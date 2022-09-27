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
  repo_dir <- get_repo_location()
  dir <- file.path(repo_dir, input$repository.type)

  rds_files <- list.files(dir, pattern = '.rds$') %>% sub('\\.rds$', '', .)
  if (length(rds_files) != 0) {
    updateSelectInput(session, 'repository.dataset', choices = rds_files, selected = rds_files[[1]])
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
  repo_dir <- get_repo_location()
  algs <- c()
  dims <- c()
  funcs <- c()

  for (f in input$repository.dataset) {
    rds_file <- file.path(repo_dir, input$repository.type, paste0(f, ".rds"))
    if (file.exists(paste0(rds_file, '_info'))) {
      info <- readRDS(paste0(rds_file, '_info'))
    }
    else {
      dsl <- readRDS(rds_file)
      info = list(algId = get_algId(dsl), funcId = get_funcId(dsl), DIM = get_dim(dsl))
    }
    algs <- c(algs, info$algId)
    dims <- c(dims, info$DIM)
    funcs <- c(funcs, info$funcId)
  }

  algs <- unique(algs)
  dims <- unique(dims)
  funcs <- unique(funcs)

  updateSelectInput(session, 'repository.ID', choices = algs, selected = algs)
  updateSelectInput(session, 'repository.dim', choices = dims, selected = dims)
  updateSelectInput(session, 'repository.funcId', choices = funcs, selected = funcs)
  shinyjs::enable('repository.load_button')
})

# add the data from repository
observeEvent(input$repository.load_button, {
  data <- DataSetList()
  repo_dir <- get_repo_location()
  for (f in input$repository.dataset) {
    rds_file <- file.path(repo_dir, input$repository.type, paste0(f, ".rds"))
    data <- c(data, readRDS(rds_file))
  }
  data <- subset(data, funcId %in% input$repository.funcId)
  data <- subset(data, DIM %in% input$repository.dim)
  data <- subset(data, algId %in% input$repository.ID)

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

  zip_files <- grep('.*zip|bz2|bz|gz|tar|tgz|tar\\.gz|xz', files, value = T, perl = T) %>%
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

    format_selected <- input$upload.data_format
    maximization <- input$upload.maximization

    if (maximization == "AUTOMATIC") maximization <- NULL

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
          new_data <- tryCatch(
            DataSetList(folder, print_fun = print_html,
                        maximization = maximization,
                        format = format_detected,
                        subsampling = input$upload.subsampling),
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
  if (!is.null(get_funcName(DataList$data)))
    shinyjs::enable("Settings.Use_Funcname")
  else
    shinyjs::disable("Settings.Use_Funcname")
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
    updateSelectInput(session, 'Overall.Funcname', choices = c(), selected = '')
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

  selected_ds <- data[[1]]
  selected_f <- attr(selected_ds,'funcId')
  selected_dim <- attr(selected_ds, 'DIM')
  selected_alg <- attr(selected_ds, 'algId')
  selected_ID <- get_id(selected_ds)

  updateSelectInput(session, 'Overall.Dim', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Overall.Funcid', choices = funcIds, selected = selected_f)
  if (input$Settings.Use_Funcname)
    updateSelectInput(session, 'Overall.Funcname', choices = get_funcName(data), selected = get_funcName(data[1]))

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
  updateSelectInput(session, 'ParCoordPlot.Algs', choices = IDs, selected = IDs[[1]])
  updateSelectInput(session, 'FV_PAR.CorrPlot.Param1', choices = c(parIds_RT_, 'f(x)'), selected = 'f(x)')
  if (length(parIds_RT_) == 0)
    updateSelectInput(session, 'FV_PAR.CorrPlot.Param2', choices = c(parIds_RT_, 'f(x)'), selected = 'f(x)')
  else
    updateSelectInput(session, 'FV_PAR.CorrPlot.Param2', choices = c(parIds_RT_, 'f(x)'), selected = parIds_RT_[[1]])
  updateSelectInput(session, 'FV_PAR.Plot.Params', choices = parIds_, selected = parIds_)
  updateSelectInput(session, 'RT_PAR.Plot.Params', choices = parIds_, selected = parIds_)

  updateTextInput(session, 'FCEPlot.Multi.Min', value = min(runtimes))
  updateTextInput(session, 'FCEPlot.Multi.Max', value = max(runtimes))

  attr_choices <- get_static_attributes(data)
  invalid_choices <- c('funcId', 'DIM', 'ID')
  updateSelectInput(session, 'Settings.ID.Variables', choices = attr_choices[!attr_choices %in% invalid_choices],
                    selected = attr(data, 'ID_attributes'))

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

  if (input$Settings.Use_Funcname) {
    fname <- input$Overall.Funcname
    if (!is.null(fname)) d <- subset(d, funcname == fname)
  }
  else {
    fid <- input$Overall.Funcid
    if (!is.null(fid)) d <- subset(d, funcId == fid)
  }

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

# This observe statement tries to match funcid and funcname seletions so funcId can still be used internally.
# TODO: think of a better solution to ensure this matching doesn't break.
observe({
  req(length(DATA_RAW()) > 0)
  fname <- input$Overall.Funcname
  req(fname)
  req(getOption('IOHanalyzer.function_representation', 'funcId') == 'funcname')
  dsl_sub <- subset(DATA_RAW(), funcName == fname)
  fids <- get_funcId(dsl_sub)
  if (length(fids) == 1) {
    updateSelectInput(session, 'Overall.Funcid', selected = fids)
  }
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

  #TODO: Make more general
  if (abs(2 * fseq[3] - fseq[2] - fseq[4]) < 1e-12) #arbitrary precision
    step <- fseq[3] - fseq[2]
  else
    step <- log10(fseq[3]) - log10(fseq[2])

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
  #TODO: remove q and replace by single number
  setTextInput(session, 'FCEECDF.Single.Target', name, alternative = q[2])
})

output$upload.Download_processed <- downloadHandler(
  filename = "DataSetList.rds",
  content = function(file) {
    saveRDS(DATA_RAW(), file = file)
  }
)
