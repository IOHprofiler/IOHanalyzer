# shared reactive variables
folderList <- reactiveValues(data = list())
DataList <- reactiveValues(data = DataSetList())

# set up the global variable
observe({
  sub_sampling <<- input$upload.subsampling
})

# set up list of datasets (scan the repository, looking for .rds files)
observe({
  repo_dir <<- get_repo_location()
  rds_files <- list.files(repo_dir, pattern = '.rds') %>% sub('\\.rds$', '', .)

  if (length(rds_files) != 0) {
    updateSelectInput(session, 'repository.dataset', choices = rds_files, selected = NULL)
  } else { # TODO: the alert msg should be updated
    shinyjs::alert("No repository file found. To make use of the IOHProfiler-repository,
                   please create a folder called 'repository' in your home directory
                   and make sure it contains the '2019gecco.rds'-file
                   provided on the IOHProfiler github-page.")
  }
  })

# load repository that is selected
observeEvent(input$repository.dataset, {
  req(input$repository.dataset)

  rds_file <- file.path(repo_dir, paste0(input$repository.dataset, ".rds"))
  repo_data <<- readRDS(rds_file)

  algIds <- c(get_algId(repo_data), 'all')
  dims <- c(get_dim(repo_data), 'all')
  funcIds <- c(get_funcId(repo_data), 'all')

  updateSelectInput(session, 'repository.algId', choices = algIds, selected = 'all')
  updateSelectInput(session, 'repository.dim', choices = dims, selected = 'all')
  updateSelectInput(session, 'repository.funcId', choices = funcIds, selected = 'all')
  shinyjs::enable('repository.load_button')
})

# add the data from repository
observeEvent(input$repository.load_button, {
  data <- repo_data
  if (input$repository.funcId != 'all')
    data <- subset(data, funcId == input$repository.funcId)
  if (input$repository.dim != 'all')
    data <- subset(data, DIM == input$repository.dim)
  if (input$repository.algId != 'all')
    data <- subset(data, algId == input$repository.algId)

  DataList$data <- c(DataList$data, data)
  format <<- attr(data[[1]], 'format')
})

# upload the compressed the data file and uncompress them
selected_folders <- reactive({
  if (!is.null(input$upload.add_zip)) {
    datapath <- input$upload.add_zip$datapath
    folders <- rep('', length(datapath))

    for (i in seq(datapath)) {
      filetype <- sub('[^\\.]*\\.', '', basename(datapath[i]), perl = T)

      if (filetype == 'csv'){
        folders[i] <- datapath[[i]]
        next
      }
      
      if (filetype == 'zip')
        unzip_fct <- unzip
      else if (filetype %in% c('bz2', 'bz', 'gz', 'tar', 'tgz', 'tar.gz', 'xz'))
        unzip_fct <- untar
      else{
        shinyjs::alert("This filetype is not (yet) supported.\n 
                        Please use a different format. \n
                        We support the following compression formats: \n 
                       'zip', 'bz2', 'bz', 'gz', 'tar', 'tgz', 'tar.gz' and 'xz'.\n
                       We also have limited support for csv-files (in Nevergrad format).")
        return(NULL)
      }
      if (filetype == 'zip')
        files <- unzip_fct(datapath[i], list = T)$Name
      else
        files <- unzip_fct(datapath[i], list = T)

      idx <- grep('*.info', files)[1]
      info <- files[idx]

      if (is.null(info)){
        idx <- grep('*.csv', files)[1]
        info <- files[idx]
        if (is.null(info))
          return(NULL)
      }
      
      if (basename(info) == info) {
        folder <- Sys.time()  # generate a folder name here
        .exdir <- file.path(exdir, folder)
        unzip_fct(datapath[i], list = FALSE, exdir = .exdir)
        folders[i] <- .exdir
      } else {
        folder <- dirname(info)
        unzip_fct(datapath[i], list = FALSE, exdir = exdir)
        folders[i] <- file.path(exdir, folder)
      }
    }
    folders
  } else
    NULL
})

# load, process the data folders and update DataSetList
observeEvent(selected_folders(), {
  folders <- selected_folders()
  format_selected <- input$upload.data_format
  maximization <- input$upload.maximization

  req(length(folders) != 0)

  if (length(folderList$data) == 0)
    folder_new <- folders
  else
    folder_new <- setdiff(folders, intersect(folderList$data, folders))

  req(length(folder_new) != 0)
  format_detected <- lapply(folder_new, check_format) %>% unique

  if (length(format_detected) != 1)
    print_html(paste('<p style="color:red;">more than one format: <br>',
                     format_detected,
                     'is detected in the uploaded data set... skip the uploaded data'))

  else {
    if (format_detected == IOHprofiler && format_selected == TWO_COL){
      format <<- TWO_COL
    }
    else
      format <<- format_detected   # set the global data format
  }
  if (format == NEVERGRAD){
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "RT_ECDF"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "ERT_convergence"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "ERT_data"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "ERT"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "RT_PMF"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "hide", tabName = "PARAMETER"))
    
    
  }
  else{
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "RT_ECDF"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "ERT_convergence"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "ERT_data"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "ERT"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "RT_PMF"))
    session$sendCustomMessage(type = "manipulateMenuItem", message = list(action = "show", tabName = "PARAMETER"))
  }
  
  
  for (folder in folder_new) {
    indexFiles <- scan_IndexFile(folder)

    if (length(indexFiles) == 0 && format != NEVERGRAD)
      print_html(paste('<p style="color:red;">format', format_selected,
                       'is selected, however', format_detected,
                       'is detected...<br>using the detected one...</p>'))
    else {
      folderList$data <- c(folderList$data, folder)

      if (format_selected == AUTOMATIC || (format_detected == IOHprofiler && format_selected == TWO_COL)) {
        set_format_func(format)
      } else if (format_detected != format) {
        print_html(paste('<p style="color:red;">format', format_selected,
                         'is selected, however', format,
                         'is detected...<br>using the detected one...</p>'))
      }

      if (maximization == AUTOMATIC)
        maximization <- ifelse((format == COCO || format == BIBOJ_COCO || format == NEVERGRAD)
                               , FALSE, TRUE)
      else
        maximization <- ifelse((maximization == "MAXIMIZE" || maximization == TRUE), TRUE, FALSE)

      # read the data set and handle potential errors
      new_data <- tryCatch(
        DataSetList(folder, print_fun = print_html,
                    maximization = maximization,
                    format = format,
                    subsampling = sub_sampling),
        error = function(e) {
          print_html(paste('<p style="color:red;">The following error happened
                           when processing the data set:</p>'))
          print_html(paste('<p style="color:red;">', e, '</p>'))
          DataSetList()
        }
      )

      DataList$data <- c(DataList$data, new_data)
      shinyjs::html("upload_data_promt",
                    sprintf('%d: %s\n', length(folderList$data), folder),
                    add = TRUE)
    }
    }
})

# remove all uploaded data set
observeEvent(input$upload.remove_data, {
  if (length(DataList$data) != 0) {
    DataList$data <- DataSetList() # must be a 'DataSetList'
    folderList$data <- list()
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
  algIds_ <- get_algId(data)
  algIds <- c(algIds_, 'all')
  parIds_ <- get_parId(data)
  parIds <- c(parIds_, 'all')
  funcIds <- get_funcId(data)
  DIMs <- get_dim(data)

  selected_ds <- data[[1]]
  selected_f <- attr(selected_ds,'funcId')
  selected_dim <- attr(selected_ds, 'DIM')
  selected_alg <- attr(selected_ds, 'algId')
  
  updateSelectInput(session, 'Overall.Dim', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Overall.Funcid', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'RTSummary.Statistics.Algid', choices = algIds, selected = 'all')
  updateSelectInput(session, 'RTSummary.Overview.Algid', choices = algIds, selected = 'all')
  updateSelectInput(session, 'FCESummary.Overview.Algid', choices = algIds, selected = 'all')
  updateSelectInput(session, 'RTSummary.Sample.Algid', choices = algIds, selected = 'all')
  updateSelectInput(session, 'PAR.Plot.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'FCESummary.Statistics.Algid', choices = algIds, selected = 'all')
  updateSelectInput(session, 'FCESummary.Sample.Algid', choices = algIds, selected = 'all')
  updateSelectInput(session, 'PAR.Summary.Algid', choices = algIds, selected = 'all')
  updateSelectInput(session, 'PAR.Sample.Algid', choices = algIds, selected = 'all')
  updateSelectInput(session, 'ERTPlot.Multi.Algs', choices = algIds_, selected = selected_alg)
  updateSelectInput(session, 'ERTPlot.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'ERTPlot.Aggr.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'FCEPlot.Multi.Algs', choices = algIds_, selected = selected_alg)
  updateSelectInput(session, 'FCEPlot.Aggr.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'FCEPlot.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'FCEPDF.Bar.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'FCEPDF.Hist.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'RTPMF.Bar.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'RTPMF.Hist.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'PAR.Summary.Param', choices = parIds, selected = 'all')
  updateSelectInput(session, 'PAR.Sample.Param', choices = parIds, selected = 'all')
  updateSelectInput(session, 'RTECDF.Single.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'RTECDF.Aggr.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'RTECDF.AUC.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'RTECDF.Multi.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'FCEECDF.Single.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'FCEECDF.Mult.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'FCEECDF.AUC.Algs', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'PAR.Plot.Params', choices = parIds_, selected = parIds_)
})

# update (filter) according to users selection DataSets
DATA <- reactive({
  dim <- input$Overall.Dim
  id <- input$Overall.Funcid

  if (length(DataList$data) == 0) return(NULL)

  d <- subset(DataList$data, DIM == dim, funcId == id)
  if (length(d) == 0){
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
  max_ERTs(data, aggr_on = 'funcId', maximize = !(format == COCO || format == BIBOJ_COCO))
})

MAX_ERTS_DIM <- reactive({
  func <- input$Overall.Funcid
  data <- subset(DataList$data, funcId == func)
  max_ERTs(data, aggr_on = 'DIM', maximize = !(format == COCO || format == BIBOJ_COCO))
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
  # s <- ((stop - start) * 0.1 + start)
  # e <- ((stop - start) * 0.9 + start)

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
  setTextInput(session, 'ERTPlot.Min', name, alternative = format_FV(start))
  setTextInput(session, 'ERTPlot.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'ERTPlot.Aggr.Targets', name, alternative = "")
  setTextInput(session, 'FCEPlot.Aggr.Targets', name, alternative = "")
  setTextInput(session, 'RTECDF.Single.Target', name, alternative = format_FV(q[2]))
  setTextInput(session, 'RTECDF.AUC.Min', name, alternative = format_FV(start))
  setTextInput(session, 'RTECDF.AUC.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'RTECDF.AUC.Step', name, alternative = format_FV(step))
  setTextInput(session, 'PAR.Plot.Min', name, alternative = format_FV(start))
  setTextInput(session, 'PAR.Plot.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'PAR.Summary.Min', name, alternative = format_FV(start))
  setTextInput(session, 'PAR.Summary.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'PAR.Summary.Step', name, alternative = format_FV(step))
  setTextInput(session, 'PAR.Sample.Min', name, alternative = format_FV(start))
  setTextInput(session, 'PAR.Sample.Max', name, alternative = format_FV(stop))
  setTextInput(session, 'PAR.Sample.Step', name, alternative = format_FV(step))
})

# update the values for the grid of running times
observe({
  data <- DATA()
  v <- get_runtimes(data)
  name <- get_data_id(data)
  # s <- ((max(v) - min(v)) * 0.05 + min(v)) %>% as.integer
  # e <- ((max(v) - min(v)) * 0.95 + min(v)) %>% as.integer

  if (length(v) == 0) return()

  # TODO: this part should be made generic!!!
  v <- as.integer(v)
  q <- quantile(v, probs = c(.25, .5, .75), names = F, type = 3)

  grid <- seq(min(v), max(v), length.out = 10) %>% as.integer
  step <- max(1, min(grid[-1] - grid[-length(grid)]))

  start <- min(v)
  stop <- max(v)

  setTextInput(session, 'FCESummary.Statistics.Min', name, alternative = min(v))
  setTextInput(session, 'FCESummary.Statistics.Max', name, alternative = max(v))
  setTextInput(session, 'FCESummary.Statistics.Step', name, alternative = step)
  setTextInput(session, 'FCESummary.Sample.Min', name, alternative = min(v))
  setTextInput(session, 'FCESummary.Sample.Max', name, alternative = max(v))
  setTextInput(session, 'FCESummary.Sample.Step', name, alternative = step)
  setTextInput(session, 'FCEPDF.Hist.Runtime', name, alternative = median(v))
  setTextInput(session, 'FCEPDF.Bar.Runtime', name, alternative = median(v))
  setTextInput(session, 'FCEPlot.Min', name, alternative = start)
  setTextInput(session, 'FCEPlot.Max', name, alternative = stop)
  setTextInput(session, 'FCEECDF.Mult.Min', name, alternative = min(v))
  setTextInput(session, 'FCEECDF.Mult.Max', name, alternative = max(v))
  setTextInput(session, 'FCEECDF.Mult.Step', name, alternative = step)
  setTextInput(session, 'FCEECDF.AUC.Min', name, alternative = min(v))
  setTextInput(session, 'FCEECDF.AUC.Max', name, alternative = max(v))
  setTextInput(session, 'FCEECDF.AUC.Step', name, alternative = step)

  #TODO: remove q and replace by single number
  setTextInput(session, 'FCEECDF.Single.Target', name, alternative = q[2])
})
