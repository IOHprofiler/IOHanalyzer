# shared reactive variables
folderList <- reactiveValues(data = list())
DataList <- reactiveValues(data = DataSetList())


# set up list of datasets (scan the repository, looking for .rds files)
observe({
  repo_dir <<- get_repo_location()
  rds_files <- list.files(repo_dir, pattern = '.rds') %>% sub('\\.rds$', '', .)

  if (length(rds_files) != 0) {
    updateSelectInput(session, 'repository.dataset', choices = c(rds_files, "Example_small", "Example_large"), selected = NULL)
  } else { # TODO: the alert msg should be updated
    shinyjs::alert("No repository file found. To make use of the IOHProfiler-repository,
                   please create a folder called 'repository' in your home directory
                   and make sure it contains at least one '.rds'-file, such as the ones
                   provided on the IOHProfiler github-page.")
    updateSelectInput(session, 'repository.dataset', choices = c("Example_small", "Example_large"), selected = NULL)
  }
  })

# load repository that is selected
observeEvent(input$repository.dataset, {
  req(input$repository.dataset)

  if (input$repository.dataset  == "Example_small") {
    repo_data <<- IOHanalyzer::dsl
  }
  else if (input$repository.dataset == "Example_large") {
    repo_data <<- IOHanalyzer::dsl_large
  }
  else{
    rds_file <- file.path(repo_dir, paste0(input$repository.dataset, ".rds"))
  
    repo_data <<- readRDS(rds_file)
  }
  algIds <- c(get_algId(repo_data), 'all')
  dims <- c(get_dim(repo_data), 'all')
  funcIds <- c(get_funcId(repo_data), 'all')
  if ( is.null(attr(repo_data, 'maximization'))) {
    attr(repo_data, 'maximization') <<- T
  } 
  if ( is.null(attr(repo_data, 'suite'))) {
    attr(repo_data, 'suite') <<- 'PBO'
  } 
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
  if (length(DataList$data) > 0 && attr(data, 'suite') != attr(DataList$data, 'suite')) {
    shinyjs::alert(paste0("Attempting to add data from a different suite to the currently",
                   " loaded data.\nPlease either remove the currently loaded data or", 
                   " choose a different dataset to load."))
    return(NULL)
  }
  DataList$data <- c(DataList$data, data)
  set_color_scheme("Default", get_algId(DataList$data))
})

# upload the compressed the data file and uncompress them
selected_folders <- reactive({
  if (!is.null(input$upload.add_zip)) {
    datapath <- input$upload.add_zip$datapath
    folders <- rep('', length(datapath))

    for (i in seq(datapath)) {
      filetype <- sub('[^\\.]*\\.', '', basename(datapath[i]), perl = T)

      if (filetype == 'csv') {
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

      if (is.null(info)) {
        idx <- grep('*.csv', files)[1]
        info <- files[idx]
        if (is.null(info))
          return(NULL)
      }
      
      if (basename(info) == info) {
        folder <- basename(tempfile("dir-"))  # generate a folder name here
        .exdir <- file.path(exdir, folder)
        dir.create(.exdir, recursive = T)
        unzip_fct(datapath[i], list = FALSE, exdir = .exdir)
        folders[i] <- .exdir
      } else {
        folder <- dirname(info)
        res <- unzip_fct(datapath[i], list = FALSE, exdir = exdir)
        if (length(res) == 0) {
          shinyjs::alert("An error occured while unzipping the provided files.\n
               Please ensure no archives are corrupted and the filenames are
               in base-64.")
          return(NULL)
        }
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
  else
    format_detected <- format_detected[[1]]
  
  
  
  for (folder in folder_new) {
    indexFiles <- scan_IndexFile(folder)

    if (length(indexFiles) == 0 && format_detected != NEVERGRAD)
      print_html(paste('<p style="color:red;">No .info-files detected in the
                       uploaded folder:</p>', folder))
    else {
      folderList$data <- c(folderList$data, folder)

      # read the data set and handle potential errors
      new_data <- tryCatch(
        DataSetList(folder, print_fun = print_html,
                    maximization = NULL,
                    format = format_detected,
                    subsampling = input$upload.subsampling),
        error = function(e) {
          print_html(paste('<p style="color:red;">The following error happened
                           when processing the data set:</p>'))
          print_html(paste('<p style="color:red;">', e, '</p>'))
          DataSetList()
        }
      )
      
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
  if (is.null(DataList$data)) {
    shinyjs::alert("An error occurred when processing the uploaded data. 
                   Please ensure the data is not corrupted.")
    return(NULL)
  }
  else if (attr(DataList$data, 'suite') == NEVERGRAD) {
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
  set_color_scheme("Default", get_algId(DataList$data))
  set_format_func(attr(DataList$data, 'suite'))
})

# remove all uploaded data set
observeEvent(input$upload.remove_data, {
  if (length(DataList$data) != 0) {
    DataList$data <- DataSetList() # must be a 'DataSetList'
    unlink(folderList$data, T)
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
  updateSelectInput(session, 'Report.RT.Overview-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.RT.Overview-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.Overview-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.Statistics-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.RT.Statistics-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.Statistics-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.Single_ERT-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.RT.Single_ERT-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.Single_ERT-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.Multi_ERT-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.Multi_ERT-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.Rank-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.Rank-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.Histogram-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.RT.Histogram-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.Histogram-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.PMF-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.RT.PMF-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.PMF-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.ECDF_Single_Target-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.RT.ECDF_Single_Target-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.ECDF_Single_Target-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.ECDF_Single_Function-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.RT.ECDF_Single_Function-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.ECDF_Single_Function-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.ECDF_Aggregated-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.RT.ECDF_AUC-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.RT.ECDF_AUC-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.RT.ECDF_AUC-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.Overview-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.FV.Overview-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.Overview-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.Statistics-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.FV.Statistics-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.Statistics-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.Single_FCE-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.FV.Single_FCE-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.Single_FCE-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.Multi_FCE-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.Multi_FCE-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.Rank-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.Rank-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.Histogram-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.FV.Histogram-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.Histogram-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.PMF-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.FV.PMF-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.PMF-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.ECDF_Single_Target-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.FV.ECDF_Single_Target-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.ECDF_Single_Target-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.ECDF_Single_Function-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.FV.ECDF_Single_Function-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.ECDF_Single_Function-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.ECDF_Aggregated-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.FV.ECDF_AUC-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.FV.ECDF_AUC-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.FV.ECDF_AUC-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.Param.Plot-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.Param.Plot-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.Param.Plot-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Report.Param.Statistics-FuncId', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Report.Param.Statistics-DIM', choices = DIMs, selected = selected_dim)
  updateSelectInput(session, 'Report.Param.Statistics-Alg', choices = algIds_, selected = algIds_)
  
  updateSelectInput(session, 'Stats.Glicko.Algid', choices = algIds_, selected = algIds_)
  updateSelectInput(session, 'Stats.Glicko.Funcid', choices = funcIds, selected = selected_f)
  updateSelectInput(session, 'Stats.Glicko.Dim', choices = DIMs, selected = selected_dim)
  
  updateSelectInput(session, 'Stats.Overview.Algid', choices = algIds_, selected = algIds_)
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
  if (length(d) == 0 && dim != "" && id != "") {
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
  setTextInput(session, 'Stats.Overview.Target', name, alternative = format_FV(stop))
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
  q <- quantile(v, probs = c(.25, .5, .75), names = F, type = 3)

  grid <- seq(min(v), max(v), length.out = 10)
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
