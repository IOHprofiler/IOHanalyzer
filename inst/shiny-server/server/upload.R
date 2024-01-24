library(DBI)
library(dplyr)
library(odbc)
library(RSQLite)
source("/opt/IOHanalyzer/R/DataSet.R")
source("/opt/IOHanalyzer/R/DataSetList.R")

# shared reactive variables
folderList <- reactiveValues(data = list())
DataList <- reactiveValues(data = DataSetList())

observe({
  repo_dir <- get_repo_location()
  dirs <- list.dirs(repo_dir, full.names = F)
  if (length(dirs) == 0) {
    shinyjs::alert("No repository directory found. To make use of the IOHProfiler-repository,
                   please create a folder called 'repository' in your home directory
                   and make sure it contains at least one '.rds'-file of a DataSetList-object,
                   such as the ones provided on the IOHProfiler github-page.")
  }
  else
    updateSelectInput(session, 'repository.type', choices = dirs, selected = dirs[[1]])
})

# set up list of datasets (scan the repository, looking for .rds files)
observe({
  req(input$repository.type)

  # Get the available datasets from the SQLite database.
  repo_dir <- get_repo_location()
  sqlite_db_path <- sprintf('%s/db.sqlite3', repo_dir)
  sqliteConnection <- dbConnect(RSQLite::SQLite(), dbname = sqlite_db_path)
  query <- "SELECT name FROM iohdata_experimentset"
  result <- dbGetQuery(sqliteConnection, query)
  dbDisconnect(sqliteConnection)
  dataset_names <- result$name

  if (length(dataset_names) != 0) {
    updateSelectInput(session, 'repository.dataset', choices = dataset_names, selected = dataset_names[[1]])
  } else {# TODO: the alert msg should be updated
    shinyjs::alert("No repository file found. To make use of the IOHProfiler-repository,
                   please create a folder called 'repository' in your home directory
                   and make sure it contains at least one '.rds'-file of a DataSetList-object,
                   such as the ones provided on the IOHProfiler github-page.")
    updateSelectInput(session, 'repository.dataset', choices = NULL, selected = NULL)
  }
})

# load repository that is selected
observeEvent(input$repository.dataset, {
  req(input$repository.dataset)

  # Specify the path to the SQLite database
  repo_dir <- get_repo_location()  # Replace with your function to get the directory path
  sqlite_db_path <- sprintf('%s/db.sqlite3', repo_dir)

  # Establish a connection to the database
  sqliteConnection <- dbConnect(RSQLite::SQLite(), dbname = sqlite_db_path)

  # Prepare the combined query
  # This query fetches distinct algorithm names, problem IDs, and dimensions
  combined_query <- sprintf(
    "SELECT DISTINCT alg.name AS algorithm_name, exp.problem_id, run.dimension
     FROM iohdata_run AS run
     INNER JOIN (
       SELECT exp.id AS experiment_id, exp.problem_id, exp.algorithm_id
       FROM iohdata_experiment AS exp
       INNER JOIN iohdata_experimentset AS expset ON exp.experiment_set_id = expset.id
       WHERE expset.name IN (%s)
     ) AS exp ON run.experiment_id = exp.experiment_id
     INNER JOIN iohdata_algorithm AS alg ON exp.algorithm_id = alg.id",
    paste(sprintf("'%s'", input$repository.dataset), collapse = ", ")
  )

  # Execute the combined query
  combined_result <- dbGetQuery(sqliteConnection, combined_query)

  # Close the database connection
  dbDisconnect(sqliteConnection)

  # Extract distinct values into vectors
  algo_names <- unique(combined_result$algorithm_name)
  problem_ids <- unique(combined_result$problem_id)
  dimensions <- unique(combined_result$dimension)

  # Output the vectors
  internal(algo_names)
  internal(problem_ids)
  internal(dimensions)

  updateSelectInput(session, 'repository.ID', choices = algo_names, selected = algo_names)
  updateSelectInput(session, 'repository.dim', choices = dimensions, selected = dimensions)
  updateSelectInput(session, 'repository.funcId', choices = problem_ids, selected = problem_ids)
  shinyjs::enable('repository.load_button')
})

observeEvent(input$repository.load_button, {
  file_conn <- file("/home/shiny/output_data_replicate.txt", open = "a")
  on.exit(close(file_conn))

  repo_dir <- get_repo_location()
  sqlite_db_path <- sprintf('%s/db.sqlite3', repo_dir)
  sqliteConnection <- dbConnect(RSQLite::SQLite(), dbname = sqlite_db_path)

  algorithm_names <- input$repository.ID
  query <- sprintf(
    "SELECT id, name, info FROM iohdata_algorithm WHERE name IN ('%s')",
    paste(algorithm_names, collapse="', '")
  )

  # Execute the query and fetch the result
  result <- dbGetQuery(sqliteConnection, query)

  # Create the mapping from id to name
  id_to_name_mapping <- setNames(result$name, result$id)

  # Execute the query and fetch the result
  result <- dbGetQuery(sqliteConnection, query)

  # Define the variables
  dimensions <- input$repository.dim
  problems <- input$repository.funcId
  algorithm_id <- result$id
  algorithm_infos <- result$info

  # Create the query string
  query <- sprintf("SELECT p.fid, s.name FROM iohdata_problem p JOIN iohdata_suite s ON p.suite_id = s.id WHERE p.fid IN (%s)", paste(problems, collapse = ", "))

  # Execute the query and fetch results
  suite_name_mapping <- dbGetQuery(sqliteConnection, query)

  # Create the query string
  query <- sprintf("SELECT id FROM iohdata_experiment WHERE algorithm_id = %s AND problem_id = %s", algorithm_id[1], problems[1])

  # Execute the query and fetch results
  result <- dbGetQuery(sqliteConnection, query)

  # Extract the id
  # Since the combination of algorithm_id and problem_id is unique,
  # there should be only one row in the result
  experiment_id <- result$id[1]

  # Create the query string to fetch algorithm_id, problem_id, and version
  query <- sprintf(
    "SELECT algorithm_id, problem_id, version FROM iohdata_experiment WHERE algorithm_id IN (%s) AND problem_id IN (%s)",
    paste(algorithm_id, collapse = ", "),
    paste(problems, collapse = ", ")
  )

  # Execute the query and fetch results
  result <- dbGetQuery(sqliteConnection, query)

  # Initialize an empty list for version mapping
  version_mapping <- list()

  # Populate the list with version for each algorithm and problem combination
  for (row in 1:nrow(result)) {
      key <- paste(result$algorithm_id[row], result$problem_id[row], sep = ",")
      version_mapping[[key]] <- result$version[row]
  }

  # Now version_mapping is a list where you can access version with version_mapping[["algorithm_id,problem_id"]]

  # Function to get data source for a given algorithm name
  getDataSource <- function(algorithm_name) {
    query <- sprintf("SELECT data_source FROM iohdata_experimentset WHERE name = '%s'", algorithm_name)
    result <- dbGetQuery(sqliteConnection, query)
    # Extract the first value from the data_source column
    if (nrow(result) > 0) {
      return(result$data_source[1])
    } else {
      return(NA)  # Return NA if no result is found
    }
  }

  # Initialize an empty list for data_sources
  data_sources <- list()

  # Create a mapping of algorithm_ids to algorithm_names
  for (i in seq(algorithm_names)) {
    algname <- algorithm_names[i]
    algid <- algorithm_id[i]
    data_sources[[algid]] <- getDataSource(algname)
  }

  # Assuming you have already connected to your database and have access to the table

  # Create a query to fetch 'fid' and 'maximization' columns
  query <- "SELECT fid, maximization FROM iohdata_problem"

  # Execute the query and fetch results
  results <- dbGetQuery(sqliteConnection, query)

  # Initialize an empty mapping
  fid_to_maximization <- list()

  # Iterate through the results to create the mapping
  for (i in 1:nrow(results)) {
    fid <- results$fid[i]
    maximization <- as.logical(results$maximization[i])  # Convert to boolean

    # Add to the mapping
    fid_to_maximization[fid] <- maximization
  }

  # Now fid_to_maximization is a mapping from 'fid' to 'maximization' as boolean values

  # Create the query string
  query <- sprintf("SELECT instance FROM iohdata_run WHERE dimension = %s AND experiment_id = %s", dimensions[1], experiment_id)

  # Execute the query and fetch results
  instances_result <- dbGetQuery(sqliteConnection, query)

  # Extract the 'instance' column into a vector
  instances <- instances_result$instance

  # Convert the vectors to comma-separated strings
  dimensions_str <- paste(dimensions, collapse = ", ")
  problems_str <- paste(problems, collapse = ", ")
  algorithm_ids_str <- paste(algorithm_id, collapse = ", ")

  # Construct the query using sprintf
  run_id_query <- sprintf(
    "
      SELECT
        e.problem_id,
        r.dimension,
        e.algorithm_id,
        r.instance,
        r.id
      FROM
        iohdata_run r
      JOIN
        iohdata_experiment e ON r.experiment_id = e.id
      WHERE
        r.dimension IN (%s) AND
        e.problem_id IN (%s) AND
        e.algorithm_id IN (%s);
    ",
    dimensions_str,
    problems_str,
    algorithm_ids_str
  )

  # Execute the query
  run_data <- dbGetQuery(sqliteConnection, run_id_query)
  dbDisconnect(sqliteConnection)

  runs <- list()
  class(runs) <- c("DataSetList", "list")
  attr(runs, "maximization") <- FALSE

  all_combinations <- expand.grid(problem_id = problems, dimension = dimensions, algorithm_id = algorithm_id)

  rds_paths <- sprintf('%s/%s/%s.rds', repo_dir, data_sources[algorithm_id], input$repository.dataset)

  # Initialize an empty list to store the datasets
  loadedDatasets <- list()

  # Loop through the rds_paths and read each RDS file
  for (i in seq_along(rds_paths)) {
      loadedDatasets[[i]] <- readRDS(rds_paths[i])
  }

  # Iterate over each row in run_data
  for (i in 1:nrow(all_combinations)) {
    current_combination <- all_combinations[i, ]

    current_problem_id <- current_combination$problem_id
    current_dimension <- current_combination$dimension
    current_algorithm_id <- current_combination$algorithm_id

    current_algorithm_name <- id_to_name_mapping[as.character(current_algorithm_id)]

    requested_dataset_index <- NA  # Initialize to indicate no match found for dataset
    requested_inner_index <- NA    # Initialize to indicate no match found within dataset

    # Iterate over the list of datasets
    for (i in seq_along(loadedDatasets)) {
      # Access the current dataset
      current_dataset <- loadedDatasets[[i]]

      # Now iterate over elements within this dataset
      for (j in seq_along(current_dataset)) {
        # Check if 'DIM' and 'funcId' attributes match the desired values for each element
        if (attr(current_dataset[[j]], 'DIM') == current_dimension && attr(current_dataset[[j]], 'funcId') == current_problem_id && attr(current_dataset[[j]], 'ID') == current_algorithm_name) {
          requested_dataset_index <- i  # Store the index of the dataset
          requested_inner_index <- j    # Store the index within the dataset
          break  # Exit the inner loop once the match is found
        }
      }

      if (!is.na(requested_inner_index)) {
        # If a match is found in the inner loop, exit the outer loop as well
        break
      }
    }

    # Check if a match was found
    if (is.na(requested_dataset_index) || is.na(requested_inner_index)) {
      stop("No match found for the following entries: current_problem_id = ",
           current_problem_id, ", current_dimension = ", current_dimension,
           ", current_algorithm_id = ", current_algorithm_id, ".")
    }

    run <- list()
    class(run) <- c("DataSet", "list")
    run$FV <- loadedDatasets[[requested_dataset_index]][[requested_inner_index]]$FV
    run$RT <- loadedDatasets[[requested_dataset_index]][[requested_inner_index]]$RT
    run$PAR <- list(
      by_FV = structure(list(), names=character(0)),
      by_RT = structure(list(), names=character(0))
    )

    attr(run, "DIM") <- current_dimension
    attr(run, "algId") <- current_algorithm_name

    attr(run, "funcId") <- current_problem_id
    attr(run, "suite") <- suite_name_mapping[suite_name_mapping$fid == current_problem_id, "name"]

    key <- paste(current_algorithm_id, current_problem_id, sep = ",")
    attr(run, "format") <- version_mapping[[key]]

    attr(run, "maximization") <- fid_to_maximization[current_problem_id]
    attr(run, "instance") <- instances
    attr(run, "datafile") <- rds_paths[[requested_dataset_index]]
    attr(run, "comment") <- algorithm_infos[current_algorithm_id]

    runs[[length(runs) + 1]] <- run
  }

  data <- runs
  internal("333")

  if (length(DataList$data) > 0 && attr(data, 'maximization') != attr(DataList$data, 'maximization')) {
    shinyjs::alert(paste0("Attempting to add data from a different optimization type to the currently",
                   " loaded data.\nPlease either remove the currently loaded data or",
                   " choose a different dataset to load."))
    return(NULL)
  }
  internal("341")

  if (length(DataList$data) > 0 &&
      !all(attr(DataList$data, 'ID_attributes') %in% get_static_attributes(data))) {
    shinyjs::alert(paste0("Attempting to add data with different ID-attributes.
                          Please check that the attributes to create the ID
                          are present in the data you are loading."))
    return(NULL)  }
  internal("349")

  if (length(DataList$data) > 0) {
    data <- change_id(data, attr(DataList$data, 'ID_attributes'))
    temp_data <- data
  } else {
    temp_data <- change_id(data, 'algId')
  }

  if (getOption('IOHanalyzer.function_representation', 'funcId') == 'funcName') {
    for (ds in temp_data){
      attr(ds, 'funcId') <- attr(ds, 'funcName')
    }
  }

  # DataList$data <- change_id(DataList$data, getOption("IOHanalyzer.ID_vars", c("algId")))
  update_menu_visibility(attr(temp_data, 'suite'))
  # set_format_func(attr(DataList$data, 'suite'))
  IDs <- get_id(temp_data)
  if (!all(IDs %in% get_color_scheme_dt()[['ids']])) {
    set_color_scheme("Default", IDs)
  }
  DataList$data <- temp_data
})

# decompress zip files recursively and return the root directory of extracted files
unzip_fct_recursive <- function(zipfile, exdir, print_fun = print, alert_fun = print, depth = 0) {
  filetype <- basename(zipfile) %>%
    strsplit('\\.') %>% `[[`(1) %>%
    rev %>%
    `[`(1)
  folders <- list()
  if (filetype == 'zip')
    unzip_fct <- unzip
  else if (filetype %in% c('bz2', 'bz', 'gz', 'tar', 'tgz', 'tar.gz', 'xz'))
    unzip_fct <- untar

  files <- unzip_fct(zipfile, list = FALSE, exdir = file.path(exdir, rand_strings(1)))
  if (length(files) == 0) {
    alert_fun("An error occured while unzipping the provided files.\n
               Please ensure no archives are corrupted and the filenames are
               in base-64.")
    return(NULL)
  }
  print_fun(paste0('<p style="color:blue;">Succesfully unzipped ', basename(zipfile), '.<br>'))

  folders <- grep('*.info|csv|txt|json', files, value = T) %>%
    dirname %>%
    unique %>%
    grep('__MACOSX', ., value = T, invert = T) %>%  # to get rid of __MACOSX folder on MAC..
    c(folders)

  zip_files <- grep('.*\\.zip|\\.bz2|\\.bz|\\.gz|\\.tar|\\.tgz|\\.tar\\.gz|\\.xz', files, value = T, perl = T) %>%
    grep('__MACOSX', ., value = T, invert = T)

  if (depth <= 3) { # only allow for 4 levels of recursions
    for (zipfile in zip_files) {
      .folders <- unzip_fct_recursive(zipfile, dirname(zipfile), alert_fun,
                                      print_fun, depth + 1)
      folders <- c(folders, .folders)
    }
  }
  unique(folders)
}

# upload the compressed the data file and uncompress them
selected_folders <- reactive({
  if (is.null(input$upload.add_zip)) return(NULL)

  tryCatch({
    datapath <- input$upload.add_zip$datapath
    folders <- c()

    for (i in seq(datapath)) {
      filetype <- basename(datapath[i]) %>%
        strsplit('\\.') %>% `[[`(1) %>%
        rev %>%
        `[`(1)

      print_html(paste0('<p style="color:blue;">Handling ', filetype, '-data.<br>'))
      if (filetype == 'csv' || filetype == 'rds') {
        # add the data path to the folders list direct
        folders <- c(folders, datapath[[i]])
        next
      }

      .folders <- unzip_fct_recursive(datapath[i], exdir, print_html) %>% unique
      folders %<>% c(.folders)
    }
    unique(folders)
  }, error = function(e) shinyjs::alert(paste0("The following error occured when processing the uploaded data: ", e))
  )
})

# load, process the data folders, and update DataSetList
observeEvent(selected_folders(), {
  withProgress({
    folders <- selected_folders()
    req(length(folders) != 0)

    # format_selected <- input$upload.data_format
    # maximization <- input$upload.maximization

    # if (maximization == "AUTOMATIC") maximization <- NULL

    if (length(folderList$data) == 0)
      folder_new <- folders
    else
      folder_new <- setdiff(folders, intersect(folderList$data, folders))

    req(length(folder_new) != 0)
    tryCatch(
    format_detected <- lapply(folder_new, check_format) %>% unique
    , error = function(e) {
    print_html(paste('<p style="color:red;">The data verification
                     failed, please check that the uploaded files are complete
                     and not corrupted.</p>'))
      })
    if (length(format_detected) > 1)
      print_html(paste('<p style="color:red;">more than one format: <br>',
                       format_detected,
                       'is detected in the uploaded data set... skip the uploaded data'))
    else
      format_detected <- format_detected[[1]]

    print_html(paste0('<p style="color:blue;">Data processing of source type:', format_detected, ' <br>'))

    for (folder in folder_new) {
      indexFiles <- scan_index_file(folder)
      if (length(indexFiles) == 0 && !(format_detected %in% c(NEVERGRAD, "SOS", "RDS")))
        print_html(paste('<p style="color:red;">No .info-files detected in the
                         uploaded folder, while they were expected:</p>', folder))
      else {
        folderList$data <- c(folderList$data, folder)

        if (format_detected == "RDS") {
          new_data <- readRDS(folder)
          if (!("DataSetList" %in% class(new_data)))
            DataSetList()
        }
        else {
          # read the data set and handle potential errors
          new_data <- tryCatch({
            DataSetList(folder, print_fun = print_html,
                        maximization = NULL,
                        format = format_detected,
                        subsampling = F)},
            error = function(e) {
              print_html(paste('<p style="color:red;">The following error happened
                               when processing the data set:</p>'))
              print_html(paste('<p style="color:red;">', e, '</p>'))
              DataSetList()
            }
          )
        }
        tryCatch(
          DataList$data <- c(DataList$data, new_data),
          error = function(e) {
            print_html(paste('<p style="color:red;">The following error happened',
                             'when adding the uploaded data set:</p>'))
            print_html(paste('<p style="color:red;">', e,
                             '\nRemoving the old data.</p>'))
            DataList$data <- new_data
          }
        )
        shinyjs::html("upload_data_promt",
                      sprintf('%d: %s\n', length(folderList$data), folder),
                      add = TRUE)
      }
    }

    DataList$data <- clean_DataSetList(DataList$data)
    # DataList$data <- change_id(DataList$data, getOption("IOHanalyzer.ID_vars", c("algId")))
    if (is.null(DataList$data)) {
      shinyjs::alert("An error occurred when processing the uploaded data.
                     Please ensure the data is not corrupted.")
      return(NULL)
    }

    update_menu_visibility(attr(DataList$data, 'suite'))
    # set_format_func(attr(DataList$data, 'suite'))
    IDs <- get_id(DataList$data)
    if (!all(IDs %in% get_color_scheme_dt()[['ids']])) {
      set_color_scheme("Default", IDs)
    }
  }, message = "Processing data, this might take some time")
})

#Load data from custom csv
observeEvent(input$upload.custom_csv, {

  tryCatch({
    datapath <- input$upload.custom_csv$datapath
    found_columns <-  colnames(fread(datapath, nrows=0))

    options <- c(found_columns, 'None')

    if (length(found_columns) == 1)
      selected <- c('None', found_columns[[1]], 'None', 'None', 'None', 'None')
    else
      selected <- c(found_columns, 'None', 'None', 'None', 'None')

    updateSelectInput(session, 'upload.neval_name', choices = options, selected = selected[[1]])
    updateSelectInput(session, 'upload.fval_name', choices = options, selected = selected[[2]])
    updateSelectInput(session, 'upload.fname_name', choices = options, selected = selected[[3]])
    updateSelectInput(session, 'upload.algname_name', choices = options, selected = selected[[4]])
    updateSelectInput(session, 'upload.dim_name', choices = options, selected = selected[[5]])
    updateSelectInput(session, 'upload.run_name', choices = options, selected = selected[[6]])
    datapath
  }, error = function(e) shinyjs::alert(paste0("The following error occured when processing the uploaded data: ", e))
  )
})

# load, process the data folders, and update DataSetList
observeEvent(input$upload.process_csv, {
  file_name <- input$upload.custom_csv$datapath
  if (is.null(file_name)) { return(NULL)}

  neval_name <- input$upload.neval_name
  fval_name <- input$upload.fval_name
  fname_name <- input$upload.fname_name
  algname_name <- input$upload.algname_name
  dim_name <- input$upload.dim_name
  run_name <- input$upload.run_name

  static_attrs <- list()
  if (neval_name == 'None') neval_name <- NULL
  if (run_name == 'None') run_name <- NULL
  if (fname_name == 'None') {
    fname_name <- NULL
    static_attrs$fname <- input$upload.fname_static
  }
  if (algname_name == 'None') {
    algname_name <- NULL
    static_attrs$algname <- input$upload.algname_static
  }
  if (dim_name == 'None') {
    dim_name <- NULL
    static_attrs$dim <- input$upload.dim_static
  }

  data <- read_pure_csv(file_name, neval_name, fval_name,
                        fname_name, algname_name, dim_name,
                        run_name, maximization = input$upload.maximization,
                        static_attrs = static_attrs)

  if (length(DataList$data) > 0 && attr(data, 'maximization') != attr(DataList$data, 'maximization')) {
    shinyjs::alert(paste0("Attempting to add data from a different optimization type to the currently",
                          " loaded data.\nPlease either remove the currently loaded data or",
                          " choose a different dataset to load."))
    return(NULL)
  }

  if (length(DataList$data) > 0 &&
      !all(attr(DataList$data, 'ID_attributes') %in% get_static_attributes(data))) {
    shinyjs::alert(paste0("Attempting to add data with different ID-attributes.
                          Please check that the attributes to create the ID
                          are present in the data you are loading."))
    return(NULL)  }

  if (length(DataList$data) > 0) {
    data <- change_id(data, attr(DataList$data, 'ID_attributes'))
    temp_data <- c(DataList$data, data)

    temp_data <- clean_DataSetList(temp_data)
  } else {
    temp_data <- change_id(data, 'algId')
  }
  # DataList$data <- change_id(DataList$data, getOption("IOHanalyzer.ID_vars", c("algId")))
  update_menu_visibility(attr(temp_data, 'suite'))
  # set_format_func(attr(DataList$data, 'suite'))
  IDs <- get_id(temp_data)
  if (!all(IDs %in% get_color_scheme_dt()[['ids']])) {
    set_color_scheme("Default", IDs)
  }
  DataList$data <- temp_data
})


update_menu_visibility <- function(suite){
  if (all(suite == NEVERGRAD)) {
    #TODO: Better way of doing this such that these pages are not even populated with data instead of just being hidden
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "#shiny-tab-ERT"))
  }
  else{
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "#shiny-tab-ERT"))
  }
  if (all(suite == "PBO")) {
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "FCE_ECDF"))
  }
  else {
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "FCE_ECDF"))
  }
  if (any(suite == "SOS")) {
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "Positions"))
  }
  else {
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "Positions"))
  }
}

observeEvent(input$Upload.Add_to_repo, {
  data <- DATA_RAW()
  repo_dir <- get_repo_location()
  if (!file.exists(file.path(repo_dir, "public_repo"))) return(NULL)
  nr_skipped <- 0
  public_dir <- file.path(repo_dir, "public_repo")
  for (algname in get_algId(data)) {
    filename <- file.path(public_dir, paste0(algname, '.rds'))
    if (file.exists(filename)) {
      nr_skipped <- nr_skipped + 1
      next
    }
    dsl <- subset(data, algId == algname)
    saveRDS(dsl, file = filename)
  }
  nr_success <- length(get_algId(data)) - nr_skipped
  shinyjs::alert(paste0("Successfully added ", nr_success, " algorithms to the public repository.",
                        "A total of ", nr_skipped, " algorithms were not uploaded because an algorithm",
                        "of the same name already exists, and overwriting data in the public repository is not yet",
                        "supported."))
})

# remove all uploaded data set
observeEvent(input$upload.remove_data, {
  if (length(DataList$data) != 0) {
    DataList$data <- DataSetList() # NOTE: this must be a `DataSetList` object
    unlink(folderList$data, T)
    folderList$data <- list()

    updateSelectInput(session, 'Overall.Dim', choices = c(), selected = '')
    updateSelectInput(session, 'Overall.Funcid', choices = c(), selected = '')
    updateSelectInput(session, 'Overall.ID', choices = c(), selected = '')

    print_html('<p style="color:red;">all data are removed!</p>')
    print_html('', 'upload_data_promt')
  }
})

# show the detailed information on DataSetList
output$data_info <- renderDataTable({
  datasets <- DataList$data
  if (length(datasets) != 0)
    summary(datasets)
  else
    data.frame()
}, options = list(pageLength = 10, scrollX = F, autoWidth = TRUE,
                  columnDefs = list(list(width = '20px', targets = c(0, 1)))))

# update the list of dimensionality, funcId and algId and parameter list
observe({
  data <- DataList$data
  if (length(data) == 0)
    return()

  # TODO: create reactive values for them
  IDs <- get_id(data)
  # algIds <- c(IDs, 'all')
  parIds_ <- get_parId(data)
  parIds <- c(parIds_, 'all')

  parIds_RT_ <- get_parId(data, which = 'by_RT')

  funcIds <- get_funcId(data)
  DIMs <- get_dim(data)
  algIds <- get_algId(data)
  runtimes <- get_runtimes(data)
  fvals <- get_funvals(data)

  selected_ds <- data[[1]]
  selected_f <- attr(selected_ds,'funcId')
  selected_dim <- attr(selected_ds, 'DIM')
  selected_alg <- attr(selected_ds, 'algId')
  selected_ID <- get_id(selected_ds)

  updateSelectInput(session, 'Overall.Dim', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Overall.Funcid', choices = funcIds, selected = selected_f)

  if ('algId' %in% input$Settings.ID.Variables)
    updateSelectInput(session, 'Overall.ID', choices = NULL, selected = NULL)
  else
    updateSelectInput(session, 'Overall.ID', choices = algIds, selected = selected_alg)

  updateSelectInput(session, 'ERTPlot.Aggr.Funcs', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'Overview.Single.ID', choices = IDs, selected = IDs)

  updateSelectInput(session, 'RTportfolio.Shapley.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTportfolio.Shapley.Funcs', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'RTportfolio.Shapley.Dims', choices = DIMs, selected = DIMs)
  updateNumericInput(session, 'RTportfolio.Shapley.Permsize', min = 2, max = length(IDs),
                     value = min(10, length(IDs)))

  if(length(IDs) >= 2)
  {
    updateSelectInput(session, 'RTPMF.CDP.Algs', choices = IDs, selected = c(IDs[1], IDs[2]))
    updateSelectInput(session, 'FCEPDF.CDP.Algs', choices = IDs, selected = c(IDs[1], IDs[2]))
  }

  updateSelectInput(session, 'RT.MultiERT.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RT.MultiERT.FuncId', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'RT.MultiERT.DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'RT.Multisample.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RT.Multisample.FuncId', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'RT.Multisample.DIM', choices = DIMs, selected = selected_dim)

  updateSelectInput(session, 'ERTPlot.Multi.Funcs', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'FCEPlot.Multi.Funcs', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'RTECDF.Aggr.FuncIds', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'RTECDF.Aggr.DIMS', choices = DIMs, selected = DIMs)

  updateSelectInput(session, 'FV.MultiFV.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV.MultiFV.FuncId', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'FV.MultiFV.DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'FV.Multisample.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV.Multisample.FuncId', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'FV.Multisample.DIM', choices = DIMs, selected = selected_dim)

  updateSelectInput(session, 'RT_Stats.Glicko.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RT_Stats.Glicko.Funcid', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'RT_Stats.Glicko.Dim', choices = DIMs, selected = selected_dim)

  updateSelectInput(session, 'RT_NG.Heatmap.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RT_NG.Heatmap.Funcid', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'RT_NG.Heatmap.Dim', choices = DIMs, selected = DIMs)

  updateSelectInput(session, 'FV_NG.Heatmap.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV_NG.Heatmap.Funcid', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'FV_NG.Heatmap.Dim', choices = DIMs, selected = DIMs)

  updateSelectInput(session, 'RT_Stats.DSC.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RT_Stats.DSC.Funcid', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'RT_Stats.DSC.Dim', choices = DIMs, selected = DIMs)

  updateSelectInput(session, 'FV_Stats.DSC.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV_Stats.DSC.Funcid', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'FV_Stats.DSC.Dim', choices = DIMs, selected = DIMs)

  updateSelectInput(session, 'RT_Stats.Overview.ID', choices = IDs, selected = IDs)

  updateSelectInput(session, 'FV_Stats.Glicko.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV_Stats.Glicko.Funcid', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'FV_Stats.Glicko.Dim', choices = DIMs, selected = selected_dim)

  updateSelectInput(session, 'EAF.Multi.FuncIds', choices = funcIds, selected = funcIds)
  updateSelectInput(session, 'EAF.MultiCDF.FuncIds', choices = funcIds, selected = funcIds)

  updateSelectInput(session, 'FV_Stats.Overview.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTSummary.Statistics.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTSummary.Overview.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCESummary.Overview.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTSummary.Sample.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV_PAR.Plot.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV_PAR.CorrPlot.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RT_PAR.Plot.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCESummary.Statistics.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCESummary.Sample.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV_PAR.Summary.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV_PAR.Sample.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RT_PAR.Summary.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RT_PAR.Sample.ID', choices = IDs, selected = IDs)
  updateSelectInput(session, 'ERTPlot.Multi.Algs', choices = IDs, selected = selected_ID)
  updateSelectInput(session, 'ERTPlot.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'ERTPlot.Aggr.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'ERTPlot.Aggr_Dim.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCEPlot.Multi.Algs', choices = IDs, selected = selected_ID)
  updateSelectInput(session, 'FCEPlot.Aggr.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCEPlot.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCEPDF.Bar.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCEPDF.Hist.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTPMF.Bar.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTPMF.Hist.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FV_PAR.Summary.Param', choices = parIds, selected = 'all')
  updateSelectInput(session, 'FV_PAR.Sample.Param', choices = parIds, selected = 'all')
  updateSelectInput(session, 'RT_PAR.Summary.Param', choices = parIds, selected = 'all')
  updateSelectInput(session, 'RT_PAR.Sample.Param', choices = parIds, selected = 'all')
  updateSelectInput(session, 'RTECDF.Single.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTECDF.Aggr.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTECDF.AUC.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'RTECDF.Multi.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCEECDF.Single.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCEECDF.Mult.Algs', choices = IDs, selected = IDs)
  updateSelectInput(session, 'FCEECDF.AUC.Algs', choices = IDs, selected = IDs)

  updateSelectInput(session, 'EAF.Single.Algs', choices = IDs, selected = IDs[[1]])
  updateSelectInput(session, 'EAF.CDF.Algs', choices = IDs, selected = IDs[[1]])
  updateSelectInput(session, 'EAF.Multi.Algs', choices = IDs, selected = IDs[[1]])
  updateSelectInput(session, 'EAF.MultiCDF.Algs', choices = IDs, selected = IDs[[1]])
  updateSelectInput(session, 'EAF.Diff.Algs', choices = IDs, selected = IDs)

  updateSelectInput(session, 'ParCoordPlot.Algs', choices = IDs, selected = IDs[[1]])
  updateSelectInput(session, 'FV_PAR.CorrPlot.Param1', choices = c(parIds_RT_, 'f(x)'), selected = 'f(x)')
  if (length(parIds_RT_) == 0)
    updateSelectInput(session, 'FV_PAR.CorrPlot.Param2', choices = c(parIds_RT_, 'f(x)'), selected = 'f(x)')
  else
    updateSelectInput(session, 'FV_PAR.CorrPlot.Param2', choices = c(parIds_RT_, 'f(x)'), selected = parIds_RT_[[1]])
  updateSelectInput(session, 'FV_PAR.Plot.Params', choices = parIds_RT_, selected = parIds_RT_)
  updateSelectInput(session, 'RT_PAR.Plot.Params', choices = parIds_, selected = parIds_)

  updateTextInput(session, 'FCEPlot.Multi.Min', value = min(runtimes))
  updateTextInput(session, 'FCEPlot.Multi.Max', value = max(runtimes))

  attr_choices <- get_static_attributes(data)
  invalid_choices <- c('funcId', 'DIM', 'ID')
  updateSelectInput(session, 'Settings.ID.Variables', choices = attr_choices[!attr_choices %in% invalid_choices],
                    selected = attr(data, 'ID_attributes'))

  updateTextInput(session, 'EAF.MultiCDF.yMin', value = min(fvals, na.rm = T))
  updateTextInput(session, 'EAF.MultiCDF.yMax', value = max(fvals, na.rm = T))

  if (isTRUE(attr(data, 'constrained'))) {
    shinyjs::show(id = "Settings.Constrained")
    shinyjs::alert("The data you loaded seems to come from a constrained optimization problem.
                   We have added support for these types of problems by allowing for post-hoc
                   analysis of different penalization techniques (see the 'settings' page).")
  } else {shinyjs::hide(id = "Settings.Constrained")}
})



# update (filter) according to users selection DataSets
DATA <- reactive({
  dim <- input$Overall.Dim
  req(dim)

  d <- subset(DataList$data, DIM == dim)

  if (!'algId' %in% input$Settings.ID.Variables) {
    algid <- input$Overall.ID
    if (!is.null(algid)) d <- subset(d, algId == algid)
  }


    fid <- input$Overall.Funcid
    if (!is.null(fid)) d <- subset(d, funcId == fid)


  if (length(DataList$data) == 0) return(NULL)

  if (length(d) == 0 && dim != "") {
    showNotification("There is no data available for this (dimension,function)-pair")
  }
  d
})

# TODO: give a different name for DATA and DATA_RAW
DATA_RAW <- reactive({
  DataList$data
})

MAX_ERTS_FUNC <- reactive({
  dim <- input$Overall.Dim
  data <- subset(DataList$data, DIM == dim)
  max_ERTs(data, aggr_on = 'funcId', maximize = attr(data, 'maximization'))
})

MAX_ERTS_DIM <- reactive({
  func <- input$Overall.Funcid
  data <- subset(DataList$data, funcId == func)
  max_ERTs(data, aggr_on = 'DIM', maximize = attr(data, 'maximization'))
})

MEAN_FVALS_FUNC <- reactive({
  dim <- input$Overall.Dim
  data <- subset(DataList$data, DIM == dim)
  mean_FVs(data, aggr_on = 'funcId')
})

MEAN_FVALS_DIM <- reactive({
  func <- input$Overall.Funcid
  data <- subset(DataList$data, funcId == func)
  mean_FVs(data, aggr_on = 'DIM')
})

# TODO: make this urgely snippet look better...
# register the TextInput and restore them when switching funcID and DIM
observeEvent(eval(eventExpr), {
  data <- DATA()
  name <- get_data_id(data)

  if (is.null(name))
    return()

  for (id in widget_id) {
    REG[[id]][[name]] <<- input[[id]]
  }
})

# update the values for the grid of target values
observe({
  data <- DATA()
  v <- get_funvals(data)
  name <- get_data_id(data)
  req(v)

  # choose proper scaling for the function value
  # v <- trans_funeval(v)    # Do the scaling in seq_FV instead
  q <- quantile(v, probs = c(.25, .5, .75), names = F)

  # TODO: we need to fix this for the general case!!!
  length.out <- 10
  fseq <- seq_FV(v, length.out = length.out)
  start <- fseq[1]
  stop <- fseq[length.out]

  if (length(fseq) < 4) {
    step <- 0
  } else {

  #TODO: Make more general
  if (abs(2 * fseq[3] - fseq[2] - fseq[4]) < 1e-12) #arbitrary precision
    step <- fseq[3] - fseq[2]
  else
    step <- log10(fseq[3]) - log10(fseq[2])
  }

  setTextInput(session, 'RTSummary.Statistics.Min', name, alternative = format_FV(start))
  setTextInput(session, 'RTSummary.Statistics.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'RTSummary.Statistics.Step', name, alternative = format_FV(step))
  setTextInput(session, 'RTSummary.Sample.Min', name, alternative = format_FV(start))
  setTextInput(session, 'RTSummary.Sample.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'RTSummary.Sample.Step', name, alternative = format_FV(step))
  setTextInput(session, 'RTECDF.Multi.Min', name, alternative = format_FV(start))
  setTextInput(session, 'RTECDF.Multi.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'RTECDF.Multi.Step', name, alternative = format_FV(step))
  setTextInput(session, 'RTPMF.Bar.Target', name, alternative = format_FV(median(v)))
  setTextInput(session, 'RTPMF.Hist.Target', name, alternative = format_FV(median(v)))
  setTextInput(session, 'RTPMF.CDP.Target', name, alternative = format_FV(median(v)))
  setTextInput(session, 'ERTPlot.Min', name, alternative = format_FV(start))
  setTextInput(session, 'ERTPlot.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'ERTPlot.Aggr.Targets', name, alternative = "")
  setTextInput(session, 'FCEPlot.Aggr.Targets', name, alternative = "")
  setTextInput(session, 'RTECDF.Single.Target', name, alternative = format_FV(q[2]))
  setTextInput(session, 'RTECDF.AUC.Min', name, alternative = format_FV(start))
  setTextInput(session, 'RTECDF.AUC.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'RTECDF.AUC.Step', name, alternative = format_FV(step))
  setTextInput(session, 'RT_PAR.Plot.Min', name, alternative = format_FV(start))
  setTextInput(session, 'RT_PAR.Plot.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'RT_PAR.Summary.Min', name, alternative = format_FV(start))
  setTextInput(session, 'RT_PAR.Summary.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'RT_PAR.Summary.Step', name, alternative = format_FV(step))
  setTextInput(session, 'RT_PAR.Sample.Min', name, alternative = format_FV(start))
  setTextInput(session, 'RT_PAR.Sample.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'RT_PAR.Sample.Step', name, alternative = format_FV(step))
  setTextInput(session, 'RT_Stats.Overview.Target', name, alternative = format_FV(stop))
  setTextInput(session, 'RT.Multisample.Target', name, alternative = format_FV(median(v)))
  setTextInput(session, 'RT.MultiERT.Target', name, alternative = format_FV(median(v)))

  setTextInput(session, 'EAF.Single.yMin', name, alternative = format_FV(start))
  setTextInput(session, 'EAF.Single.yMax', name, alternative = format_FV(stop))
  setTextInput(session, 'EAF.CDF.yMin', name, alternative = format_FV(start))
  setTextInput(session, 'EAF.CDF.yMax', name, alternative = format_FV(stop))
  setTextInput(session, 'EAF.Diff.yMin', name, alternative = format_FV(start))
  setTextInput(session, 'EAF.Diff.yMax', name, alternative = format_FV(stop))
  setTextInput(session, 'RT.MultiERT.Target', name, alternative = format_FV(median(v)))
  setTextInput(session, 'RT.MultiERT.Target', name, alternative = format_FV(median(v)))

})

# update the values for the grid of running times
observe({
  data <- DATA()
  v <- get_runtimes(data)
  name <- get_data_id(data)

  if (length(v) == 0) return()

  # TODO: this part should be made generic!!!
  q <- quantile(v, probs = c(.25, .5, .75), names = F, type = 3)

  grid <- seq(min(v), max(v), length.out = 10)
  step <- max(1, min(grid[-1] - grid[-length(grid)]))

  start <- min(v)
  stop <- max(v)
  setTextInput(session, 'FV.Multisample.Target', name, alternative = max(v))
  setTextInput(session, 'FV.MultiFV.Target', name, alternative = max(v))
  setTextInput(session, 'FCESummary.Statistics.Min', name, alternative = min(v))
  setTextInput(session, 'FCESummary.Statistics.Max', name, alternative = max(v))
  setTextInput(session, 'FCESummary.Statistics.Step', name, alternative = step)
  setTextInput(session, 'FCESummary.Sample.Min', name, alternative = min(v))
  setTextInput(session, 'FCESummary.Sample.Max', name, alternative = max(v))
  setTextInput(session, 'FCESummary.Sample.Step', name, alternative = step)
  setTextInput(session, 'FCEPDF.Hist.Runtime', name, alternative = median(v))
  setTextInput(session, 'FCEPDF.Bar.Runtime', name, alternative = median(v))
  setTextInput(session, 'FCEPDF.CDP.Runtime', name, alternative = median(v))
  setTextInput(session, 'FCEPlot.Min', name, alternative = start)
  setTextInput(session, 'FCEPlot.Max', name, alternative = stop)
  setTextInput(session, 'FCEECDF.Mult.Min', name, alternative = min(v))
  setTextInput(session, 'FCEECDF.Mult.Max', name, alternative = max(v))
  setTextInput(session, 'FCEECDF.Mult.Step', name, alternative = step)
  setTextInput(session, 'FCEECDF.AUC.Min', name, alternative = min(v))
  setTextInput(session, 'FCEECDF.AUC.Max', name, alternative = max(v))
  setTextInput(session, 'FCEECDF.AUC.Step', name, alternative = step)
  setTextInput(session, 'FV_PAR.Plot.Min', name, alternative =  min(v))
  setTextInput(session, 'FV_PAR.Plot.Max', name, alternative = max(v))
  setTextInput(session, 'FV_PAR.Summary.Min', name, alternative =  min(v))
  setTextInput(session, 'FV_PAR.Summary.Max', name, alternative = max(v))
  setTextInput(session, 'FV_PAR.Summary.Step', name, alternative = step)
  setTextInput(session, 'FV_PAR.Sample.Min', name, alternative =  min(v))
  setTextInput(session, 'FV_PAR.Sample.Max', name, alternative = max(v))
  setTextInput(session, 'FV_PAR.Sample.Step', name, alternative = step)
  setTextInput(session, 'FV_Stats.Overview.Target', name, alternative = max(v))

  setTextInput(session, 'EAF.Single.Min', name, alternative = min(v))
  setTextInput(session, 'EAF.Single.Max', name, alternative = max(v))
  setTextInput(session, 'EAF.Diff.Min', name, alternative = min(v))
  setTextInput(session, 'EAF.Diff.Max', name, alternative = max(v))
  #TODO: remove q and replace by single number
  setTextInput(session, 'FCEECDF.Single.Target', name, alternative = q[2])
})

output$upload.Download_processed <- downloadHandler(
  filename = "DataSetList.rds",
  content = function(file) {
    saveRDS(DATA_RAW(), file = file)
  }
)
