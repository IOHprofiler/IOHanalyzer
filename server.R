#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com
# 
# TODO:
#   * add 'shiny::req' to all the functions when the input might be insufficient
#   * rename most of the control widgets in a uniform and understandable scheme
suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(reshape2))
suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))

for (f in list.files('pproc', pattern = '.R', full.names = T)) {
  source(f)
}

source('plot.R')

options(width = 30)
options(shiny.maxRequestSize = 200 * 1024 ^ 2)   # maximal number of requests, this is too many...

symbols <- c("circle-open", "diamond-open", "square-open", "cross-open",
             "triangle-up-open", "triangle-down-open")

# TODO: put it as an option such that the user can select
maximization <- "MAXIMIZE"
src_format <- AUTOMATIC # TODO: this shoule be taken from the data set
selected_format <- AUTOMATIC
sub_sampling <- TRUE

# Formatter for function values
format_FV <- function(v) format(v, digits = 2, nsmall = 2)
format_RT <- function(v) as.integer(v)

# transformations applied on the function value
# trans_funeval <- `log10`
# reverse_trans_funeval <- function(x) 10 ^ x

# transformations applied on runtime values

# directory where data are extracted from the zip file
exdir <- file.path(Sys.getenv('HOME'), 'data')

# directory where rds-data is stored
# rdsdir <- file.path(Sys.getenv('HOME'), 'repository')
# repository <- NULL

setTextInput <- function(session, id, name, alternative) {
  v <- REG[[id]]
  if (name %in% names(v)) {
    updateTextInput(session, id, value = v[[name]])
  } else
    updateTextInput(session, id, value = alternative)
}

#TODO: this function could be made more clear
set_format <- function(format){
  format_FV <<- ifelse((format == COCO),
                       function(v) format(v, format = "e", digits = 5, nsmall = 2),
                       function(v) format(v, digits = 2, nsmall = 2))
}

# register previous text inputs, which is used to restore those values
REG <- lapply(widget_id, function(x) list())

# TODO: maybe give this function a better name
# get the current 'id' of the selected data: funcID + DIM 
get_data_id <- function(dsList) {
  if (is.null(dsList) | length(dsList) == 0)
    return(NULL)
  
  paste(get_funcId(dsList), get_DIM(dsList), sep = '-')
}

# gecco2019data <- readRDS('data/2019gecco-ins11-1run.rds')

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # clean up the temporarsy files on server when exiting  
  session$onSessionEnded(function() {
    unlink(exdir, recursive = T)
  })
  
  # shared reactive variables
  folderList <- reactiveValues(data = list())
  DataList <- reactiveValues(data = DataSetList())
  
  # update maximization indication, trans_funeval according to src_format 
  observe({
    selected_format <<- input$DATA_SRC_FORMAT
    maximization <<- input$DATA_SRC_MINMAX
  })
  
  # should subsamping be turned on?
  observe({
    sub_sampling <<- input$SUBSAMPLING  
  })
  
  # the folder where the uploaded zip file is uncompressed
  selected_folders <- reactive({
    if (!is.null(input$ZIP)) {
      datapath <- input$ZIP$datapath
      folders <- rep('', length(datapath))
      
      for (i in seq(datapath)) {
        filetype <- sub('[^\\.]*\\.', '', basename(datapath[i]), perl = T)
        
        if (filetype == 'zip') 
          unzip_fct <- unzip
        else if (filetype %in% c('bz2', 'bz', 'gz', 'tar', 'tgz', 'tar.gz', 'xz'))
          unzip_fct <- untar
        
        if (filetype == 'zip')
          files <- unzip_fct(datapath[i], list = T)$Name
        else 
          files <- unzip_fct(datapath[i], list = T)
        
        idx <- grep('*.info', files)[1]
        info <- files[idx]
        
        if (is.null(info)) return(NULL)
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
      return(folders)
    } else
      return(NULL)
  })
  
  # load, process the data folders and update DataSetList
  observeEvent(selected_folders(), {
    folders <- selected_folders()
    req(length(folders) != 0)
    
    if (length(folderList$data) == 0)
      folder_new <- folders
    else
      folder_new <- setdiff(folders, intersect(folderList$data, folders))
    
    req(length(folder_new) != 0)
    
    for (folder in folder_new) {
      indexFiles <- scan_IndexFile(folder)
      if (length(indexFiles) == 0) 
        shinyjs::html("process_data_promt", 
                      paste0('<p style="color:red;">No index file (.info) is found in folder ', 
                             folder, '... skip</p>'), add = TRUE)
      else {
        folderList$data <- c(folderList$data, folder)
        
        # check if the newly loaded data contradicts the selected format
        found_format <- check_format(folder)
        
        if (selected_format == AUTOMATIC) { 
          set_format(found_format)
          format <- found_format
        }
        else if (found_format != selected_format && (selected_format != TWO_COL || found_format == COCO)) {
          shinyjs::html("process_data_promt", 
                        paste0('<p style="color:red;">Format specified does not match format found (', 
                               found_format, ')... skip</p>'), add = TRUE)
          break
        }
        else{
          format <- selected_format
        }
        
        if (maximization == AUTOMATIC) {
          minmax <- ifelse((found_format == COCO), FALSE, TRUE)
        }
        else{
          minmax <- ifelse((maximization == "MAXIMIZE"), TRUE, FALSE)
        }
        print_fun <- function(s) shinyjs::html("process_data_promt", s, add = TRUE)
        
        # read the data set handles potential errors
        new_data <- tryCatch(
          read_dir(folder, print_fun = print_fun,
                   maximization = minmax,
                   format = format,
                   subsampling = sub_sampling),
          error = function(e) {
            shinyjs::html("process_data_promt",
                          paste('<p style="color:red;">The following error happened when processing the data set:</p>'), add = TRUE)
            shinyjs::html("process_data_promt",
                          paste('<p style="color:red;">', e, '</p>'),
                          add = TRUE)
            DataSetList()
          }
        )
        
        DataList$data <- c(DataList$data, new_data)
        src_format <<- format
        shinyjs::html("upload_data_promt", 
                      sprintf('%d: %s\n', length(folderList$data), folder), add = TRUE)
      }
    } 
  })
  
  # remove all uploaded data set
  observeEvent(input$RM_DATA, {
    if (length(DataList$data) != 0) {
      DataList$data <- DataSetList() # must be a 'DataSetList'
      folderList$data <- list() 
      shinyjs::html("process_data_promt", '<p style="color:red;">all data are removed!</p>', add = FALSE)
      shinyjs::html("upload_data_promt", "", add = FALSE)
    }
  })
  
  # show the detailed information on DataSetList
  output$DATASETLIST_INFO <- renderDataTable({
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
    
    dim <- get_DIM(data)
    updateSelectInput(session, 'DIM_INPUT', choices = dim, selected = dim[1])
    
    funcID <- get_funcId(data)
    updateSelectInput(session, 'FUNCID_INPUT', choices = funcID, selected = funcID[1])
    
    algId <- c(get_AlgId(data), 'all')
    updateSelectInput(session, 'ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'ALGID_INPUT_SUMMARY', choices = algId, selected = 'all')
    updateSelectInput(session, 'FCE_ALGID_INPUT_SUMMARY', choices = algId, selected = 'all')
    updateSelectInput(session, 'ALGID_RAW_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'PAR_ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'FCE_ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'FCE_ALGID_RAW_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'PAR_ALGID_INPUT_SUMMARY', choices = algId, selected = 'all')
    updateSelectInput(session, 'PAR_ALGID_INPUT_SAMPLE', choices = algId, selected = 'all')
    
    parId <- c(get_ParId(data), 'all')
    updateSelectInput(session, 'PAR_INPUT', choices = parId, selected = 'all')
    updateSelectInput(session, 'PAR_INPUT_SAMPLE', choices = parId, selected = 'all')
  })
  
  # update (filter) according to users selection DataSets 
  DATA <- reactive({
    dim <- input$DIM_INPUT
    id <- input$FUNCID_INPUT
    
    if (length(DataList$data) == 0)
      return(NULL)
    
    subset(DataList$data, DIM == dim, funcId == id)
  })

  # TODO: give a different name for DATA and DATA_UNFILTERED
  DATA_UNFILTERED <- reactive({
    DataList$data
  })
  
  MAX_ERTS_FUNC <- reactive({
    dim <- input$Overall.Dim
    data <- subset(DataList$data, DIM == dim)
    max_ERTs(data, aggr_on = 'funcId', maximize = !(src_format == COCO))
  })
  
  MAX_ERTS_DIM <- reactive({
    func <- input$Overall.Funcid
    data <- subset(DataList$data, funcId == func)
    max_ERTs(data, aggr_on = 'DIM', maximize = !(src_format == COCO))
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
    v <- get_Funvals(data)
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
    
    name <- get_data_id(data)
    setTextInput(session, 'fstart', name, alternative = format_FV(start))
    setTextInput(session, 'fstop', name, alternative = format_FV(stop))
    setTextInput(session, 'fstep', name, alternative = format_FV(step))
    
    setTextInput(session, 'F_MIN_SAMPLE', name, alternative = format_FV(start))
    setTextInput(session, 'F_MAX_SAMPLE', name, alternative = format_FV(stop))
    setTextInput(session, 'F_STEP_SAMPLE', name, alternative = format_FV(step))
    
    setTextInput(session, 'RT_fstart', name, alternative = format_FV(start))
    setTextInput(session, 'RT_fstop', name, alternative = format_FV(stop))
    setTextInput(session, 'RT_fstep', name, alternative = format_FV(step))
    setTextInput(session, 'RT_fselect', name, alternative = format_FV(median(v)))
    
    setTextInput(session, 'RT_PMF_FTARGET', name, alternative = format_FV(median(v)))
    setTextInput(session, 'RT_PMF_HIST_FTARGET', name, alternative = format_FV(median(v)))
    
    # s <- ((stop - start) * 0.1 + start)
    # e <- ((stop - start) * 0.9 + start)
    setTextInput(session, 'ERT_FSTART', name, alternative = format_FV(start))
    setTextInput(session, 'ERT_FSTOP', name, alternative = format_FV(stop))
    
    setTextInput(session, 'RT_ECDF_FTARGET1', name, alternative = format_FV(q[1]))
    setTextInput(session, 'RT_ECDF_FTARGET2', name, alternative = format_FV(q[2]))
    setTextInput(session, 'RT_ECDF_FTARGET3', name, alternative = format_FV(q[3]))
    
    setTextInput(session, 'RT_AUC_FSTART', name, alternative = format_FV(start))
    setTextInput(session, 'RT_AUC_FSTOP', name, alternative = format_FV(stop))
    setTextInput(session, 'RT_AUC_FSTEP', name, alternative = format_FV(step))
    
    setTextInput(session, 'PAR_F_MIN', name, alternative = format_FV(start))
    setTextInput(session, 'PAR_F_MAX', name, alternative = format_FV(stop))
    
    setTextInput(session, 'PAR_F_MIN_SUMMARY', name, alternative = format_FV(start))
    setTextInput(session, 'PAR_F_MAX_SUMMARY', name, alternative = format_FV(stop))
    setTextInput(session, 'PAR_F_STEP_SUMMARY', name, alternative = format_FV(step))
    
    setTextInput(session, 'PAR_F_MIN_SAMPLE', name, alternative = format_FV(start))
    setTextInput(session, 'PAR_F_MAX_SAMPLE', name, alternative = format_FV(stop))
    setTextInput(session, 'PAR_F_STEP_SAMPLE', name, alternative = format_FV(step))
    
  })
  
  # update the values for the grid of running times
  observe({
    data <- DATA()
    v <- get_Runtimes(data)
    
    if (length(v) == 0)
      return()
    
    # TODO: this part should be made generic!!!
    v <- v  %>% as.integer  
    q <- quantile(v, probs = c(.25, .5, .75), names = F, type = 3)
    
    grid <- seq(min(v), max(v), length.out = 10) %>% as.integer
    step <- max(1, min(grid[-1] - grid[-length(grid)])) 
    
    start <- min(v)
    stop <- max(v)
    
    name <- get_data_id(data)
    setTextInput(session, 'RT_MIN', name, alternative = min(v))
    setTextInput(session, 'RT_MAX', name, alternative = max(v))
    setTextInput(session, 'RT_STEP', name, alternative = step)
    
    setTextInput(session, 'RT_MIN_SAMPLE', name, alternative = min(v))
    setTextInput(session, 'RT_MAX_SAMPLE', name, alternative = max(v))
    setTextInput(session, 'RT_STEP_SAMPLE', name, alternative = step)
    
    setTextInput(session, 'FCE_HIST_RUNTIME', name, alternative = median(v))
    setTextInput(session, 'FCE_PDF_RUNTIME', name, alternative = median(v))
    
    # s <- ((max(v) - min(v)) * 0.05 + min(v)) %>% as.integer
    # e <- ((max(v) - min(v)) * 0.95 + min(v)) %>% as.integer
    setTextInput(session, 'FCE_RT_MIN', name, alternative = start)
    setTextInput(session, 'FCE_RT_MAX', name, alternative = stop)
    
    setTextInput(session, 'FCE_ECDF_RT_MIN', name, alternative = min(v))
    setTextInput(session, 'FCE_ECDF_RT_MAX', name, alternative = max(v))
    setTextInput(session, 'FCE_ECDF_RT_STEP', name, alternative = step)
    
    setTextInput(session, 'FCE_AUC_RT_MIN', name, alternative = min(v))
    setTextInput(session, 'FCE_AUC_RT_MAX', name, alternative = max(v))
    setTextInput(session, 'FCE_AUC_RT_STEP', name, alternative = step)
    
    setTextInput(session, 'FCE_ECDF_RT1', name, alternative = q[1])
    setTextInput(session, 'FCE_ECDF_RT2', name, alternative = q[2])
    setTextInput(session, 'FCE_ECDF_RT3', name, alternative = q[3])
  })
  
  # Data summary for Fixed-Target Runtime (ERT)  --------------
  runtime_summary <- reactive({
    req(input$fstart, input$fstop, input$fstep)
    
    fstart <- format_FV(input$fstart) %>% as.numeric
    fstop <- format_FV(input$fstop) %>% as.numeric
    fstep <- format_FV(input$fstep) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    
    data <- DATA()
    fall <- get_Funvals(data)
    
    if (input$singleF)
      fstop <- fstart
    
    fseq <- seq_FV(fall, fstart, fstop, fstep)
    req(fseq)
    
    df <- get_RT_summary(data, fseq, algorithm = input$ALGID_INPUT)
    df[, c('DIM', 'funcId') := NULL]
  })
  
  output$table_RT_summary <- renderTable({
    df <- runtime_summary()
    df$runs %<>% as.integer
    df$median %<>% as.integer
    df$target <- format_FV(df$target)
    
    # format the integers
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% as.integer
    }
    df
  })
  
  # Data summary for Fixed-Target Runtime (ERT)  --------------
  runtime_data_overview <- reactive({
    data <- DATA()
    fall <- get_Funvals(data)
    get_FV_overview(data, algorithm = input$ALGID_INPUT_SUMMARY)
  })
  
  output$table_RT_overview <- renderTable({
    req(input$ALGID_INPUT_SUMMARY)
    df <- runtime_data_overview()
    
    df$Budget %<>% as.integer
    df$runs %<>% as.integer
    df$`runs reached` %<>% as.integer
    df$fID %<>% as.integer
    df$DIM %<>% as.integer
    df$`Worst recorded f(x)` %<>% format_FV
    df$`Worst reached f(x)` %<>% format_FV
    df$`Best reached f(x)` %<>% format_FV
    df$`mean reached f(x)` %<>% format_FV
    df$`median reached f(x)` %<>% format_FV
    df
  })
  
  output$downloadData <- downloadHandler(
    filename = {
      data <- DATA()
      fstart <- format_FV(input$fstart)
      fstop <- format_FV(input$fstop)
      fstep <- format_FV(input$fstep) 
      eval(RT_csv_name)
    }, 
    content = function(file) {
      write.csv(runtime_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  get_RT <- reactive({
    if (input$F_SAMPLE_SINGLE) {
      req(input$fstart)
      fstart <- format_FV(input$F_MIN_SAMPLE) %>% as.numeric
      fseq <- fstart
    } else {
      req(input$fstart, input$fstop, input$fstep)
      
      fstart <- format_FV(input$F_MIN_SAMPLE) %>% as.numeric
      fstop <- format_FV(input$F_MAX_SAMPLE) %>% as.numeric
      fstep <- format_FV(input$F_STEP_SAMPLE) %>% as.numeric
      
      req(fstart <= fstop, fstep <= fstop - fstart)
      fseq <- seq_FV(fall, fstart, fstop, fstep)
    }
    
    # TODO: verify this
    # we have to remove this part from the dependency of this reactive expression
    # isolate({
    data <- DATA()
    fall <- get_Funvals(data)
    # })
    
    req(fseq)
    get_RT_sample(data, ftarget = fseq, algorithm = input$ALGID_RAW_INPUT, 
                  output = input$RT_download_format)
  })
  
  output$download_runtime <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(get_AlgId(data), collapse = ';')
      fstart <- input$F_MIN_SAMPLE %>% format_FV
      fstop <- input$F_MAX_SAMPLE %>% format_FV
      fstep <- input$F_STEP_SAMPLE %>% format_FV
      eval(RTSample_csv_name)
    },
    content = function(file) {
      write.csv(get_RT(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  output$table_RT_sample <- renderDataTable({
    df <- get_RT()
    df[is.na(df)] <- 'NA'
    df}, options = list(pageLength = 20, scrollX = T))
  
  # The expected runtime plot ---------------------
  # TODO: wrapp this as a separate function for DataSet class
  output$ERT_PER_FUN <- renderPlotly({
    render_ERT_PER_FUN()
  })
  
  output$FIG_DOWNLOAD_ERT_PER_FUN <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_ERT_PER_FUN)
    },
    content = function(file) {
      save_plotly(render_ERT_PER_FUN(), file, 
                  format = input$FIG_FORMAT_ERT_PER_FUN, 
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_ERT_PER_FUN)
  )
  
  render_ERT_PER_FUN <- reactive({
    fstart <- input$ERT_FSTART %>% as.numeric
    fstop <- input$ERT_FSTOP %>% as.numeric
    
    plot_RT_line.DataSetList(DATA(), Fstart = fstart ,Fstop = fstop,
                              show.CI = input$show.CI, show.density = input$show.density,
                              show.runs = input$show_all, show.optimal = input$show.best_of_all,
                              show.pareto = input$show.pareto_optima, show.ERT = input$show.ERT,
                              show.mean = input$show.mean, show.median = input$show.median,
                              show.grad = input$show_grad, show.intensity = input$show.intensity,
                              scale.xlog = input$semilogx, scale.ylog = input$semilogy,
                              scale.reverse = (src_format == COCO))
  })
  
  output$ERTPlot.Multi.Plot <- renderPlotly(
    render_ERTPlot_multi_plot()
  )
  
  render_ERTPlot_multi_plot <- reactive({
    req(input$ERTPlot.Multi.PlotButton)
    data <- DATA_UNFILTERED()
    data <- subset(data, algId %in% input$ERTPlot.Multi.Algs)
    
    if (length(data) == 0) return(NULL)
    if (input$ERTPlot.Multi.Aggregator == 'Functions') 
      data <- subset(data, DIM == input$Overall.Dim)
    else 
      data <- subset(data, funcId == input$Overall.Funcid)
    
    plot_ERT_MULTI(data, plot_mode = input$ERTPlot.Multi.Mode,
                   scale.xlog = input$ERTPlot.Multi.Logx, scale.ylog = input$ERTPlot.Multi.Logy,
                   scale.reverse = (src_format == COCO),
                   aggr_on = ifelse(input$ERTPlot.Multi.Aggregator == 'Functions', 'funcId', 'DIM'))
  })
  
  output$ERTPlot.Multi.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_ERT_PER_FUN_MULTI)
    },
    content = function(file) {
      save_plotly(render_ERTPlot_multi_plot(), file,
                  format = input$ERTPlot.Multi.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$ERTPlot.Multi.Format)
  )
  
  output$ERTPlot.Aggr.Plot <- renderPlotly(
    render_ERTPlot_aggr_plot()
  )
  
  get_max_targets <- function(data, aggr_on, maximize){
    targets <- c()
    aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_DIM(data)
    
    for (j in seq_along(aggr_attr)) {
      dsList_filetered <- if (aggr_on == 'funcId') subset(data,funcId == aggr_attr[[j]])
      else subset(data, DIM == aggr_attr[[j]])
      
      Fall <- get_Funvals(dsList_filetered)
      Fval <- ifelse(maximize, max(Fall), min(Fall))
      targets <- c(targets, Fval)
    }
    targets
  }
  
  render_ERTPlot_aggr_plot <- reactive({
    #TODO: figure out how to avoid plotting again when default targets are written to input
    data <- DATA_UNFILTERED()
    if (length(data) == 0) return(NULL)
    if (input$ERTPlot.Aggr.Aggregator == 'Functions') {
      data <- subset(data, DIM == input$Overall.Dim)
      erts <- MAX_ERTS_FUNC()
    }
    else{
      data <- subset(data, funcId == input$Overall.Funcid)
      erts <- MAX_ERTS_DIM()
    }
    aggr_on = ifelse(input$ERTPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
    aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_DIM(data)
    update_targets <- F
    if (input$ERTPlot.Aggr.Targets == "") {
      update_targets <- T
    } else {
      targets <- as.numeric(unlist(strsplit(input$ERTPlot.Aggr.Targets,",")))
      if (length(targets) != length(aggr_attr)) {
        update_targets <- T
      }
    }
    if (update_targets) {
      targets <- get_max_targets(data, aggr_on, maximize = !(src_format == COCO))
      updateTextInput(session, 'ERTPlot.Aggr.Targets', value = targets %>% toString)
    }
    
    plot_ERT_AGGR(data, plot_mode = input$ERTPlot.Aggr.Mode, targets = targets,
                  scale.ylog = input$ERTPlot.Aggr.Logy,
                  maximize = !(src_format == COCO), use_rank = input$ERTPlot.Aggr.Ranking,
                  aggr_on = aggr_on, erts = erts)
    
  })
  
  output$ERTPlot.Aggr.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_ERT_AGGR)
    },
    content = function(file) {
      save_plotly(render_ERTPlot_aggr_plot(), file,
                  format = input$ERTPlot.Aggr.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$ERTPlot.Aggr.Format)
  )
  
  # empirical p.m.f. of the runtime
  output$RT_PMF <- renderPlotly({
    render_RT_PMF()
  })
  
  output$FIG_DOWNLOAD_RT_PMF <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_PMF)
    },
    content = function(file) {
      save_plotly(render_RT_PMF(), file,
           format = input$FIG_FORMAT_RT_PMF, 
           width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_RT_PMF)
  )
  
  render_RT_PMF <- reactive({
    ftarget <- input$RT_PMF_FTARGET %>% as.numeric
    plot_RT_PMF.DatasetList(DATA(), ftarget, show.sample = input$RT_SHOW_SAMPLE, 
                            scale.ylog = input$RT_PMF_LOGY)
  })
  
  # historgram of the running time
  output$RT_HIST <- renderPlotly({
    render_RT_HIST()
  })
  
  output$FIG_DOWNLOAD_RT_HIST <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_HIST)
    },
    content = function(file) {
      save_plotly(render_RT_HIST(), file,
           format = input$FIG_FORMAT_RT_HIST, 
           width = fig_width2, height = fig_height2)
    },
    contentType = paste0('image/', input$FIG_FORMAT_RT_HIST)
  )
  
  render_RT_HIST <- reactive({
    req(input$RT_PMF_HIST_FTARGET)
    ftarget <- format_FV(input$RT_PMF_HIST_FTARGET) %>% as.numeric
    plot_mode <- input$ERT_illu_mode

    plot_RT_HIST(DATA(), ftarget, plot_mode = plot_mode)
  })
  
  output$RT_ECDF_MULT <- renderPlotly({
    render_RT_ECDF_MULT()
  })

  render_RT_ECDF_MULT <- reactive({

    dsList <- subset(DATA_UNFILTERED(), DIM == input$DIM_INPUT)
    targets <- uploaded_RT_ECDF_targets()
    
    if (is.null(targets)) 
      targets <- get_default_ECDF_targets(dsList)
    
    algId <- unique(attr(dsList, 'algId'))
    p <- plot_ly_default(x.title = "function evaluations",
                         y.title = "Proportion of (run, target, ...) pairs")
    
    rts <- get_Runtimes(dsList)
    x <- seq(min(rts), max(rts), length.out = 50)
    colors <- color_palettes(length(algId))
    
    for (i in seq_along(algId)) {
      Id <- algId[i]
      data <- subset(dsList, algId == Id)
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      
      fun <- ECDF(data, ftarget = targets, funcId = as.integer(names(targets)))
      if (is.null(fun)) next
      
      df_plot <- data.frame(x = x, ecdf = fun(x))
      p %<>% add_trace(data = df_plot, x = ~x, y = ~ecdf, type = 'scatter',
                       mode = 'lines+markers', name = sprintf('%s', Id), 
                       showlegend = T, 
                       line = list(color = rgb_str, width = 3),
                       marker = list(color = rgb_str, size = 10))
    }
    p
  })
  
  output$FIG_DOWNLOAD_RT_ECDF_MULT <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_ECDF_MULT)
    },
    content = function(file) {
      save_plotly(render_RT_ECDF_MULT(), file,
                  format = input$FIG_FORMAT_RT_ECDF_MULT, 
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_RT_ECDF_MULT)
  )
  
  RT_ECDF_MULTI_TABLE <- reactive({
    targets <- uploaded_RT_ECDF_targets()
    funcId <- names(targets)
    
    if (is.null(targets)) {
      data <- subset(DATA_UNFILTERED(), DIM == input$DIM_INPUT)
      targets <- get_default_ECDF_targets(data) 
      funcId <- unique(attr(data, 'funcId')) %>% sort
    }
    
    targets <- lapply(targets, function(t) {
        paste0(as.character(t), collapse = ',')
    })
    
    data.frame(funcId = funcId, target = unlist(targets))
  })
  
  output$RT_GRID_GENERATED <- renderTable({
    df <- RT_ECDF_MULTI_TABLE()
    df$funcId <- as.integer(df$funcId)
    df
  })
  
  uploaded_RT_ECDF_targets <- reactive({
    if (!is.null(input$CSV_Targets_upload)) {
      df <- read.csv(input$CSV_Targets_upload$datapath, header = T, sep = ';')
      value <- as.character(df$target)
      
      lapply(value, 
             function(v) {
               unlist(strsplit(v, '[,]')) %>% 
                 as.numeric
             }) %>% 
        set_names(df$funcId)
    } else
      NULL
  })
  
  output$TARGET_TABLE_EXAMPLE_DOWNLOAD <- downloadHandler(
    filename = 'Example_ECDF_TARGETS.csv',
    content = function(file) {
      write.table(RT_ECDF_MULTI_TABLE(), file, row.names = F, 
                  col.names = T, sep = ';')
    },
    contentType = "text/csv"
  )

  # The ECDF plots for the runtime ----------------
  output$RT_ECDF <- renderPlotly({
    req(input$RT_ECDF_FTARGET)
    ftargets <- as.numeric(format_FV(input$RT_ECDF_FTARGET))
    plot_RT_ECDF.DataSetList(DATA(), ftargets, scale.xlog = input$RT_ECDF_semilogx)
  })
  
  output$RT_GRID <- renderPrint({
    req(input$RT_fstart, input$RT_fstop, input$RT_fstep)
    
    fstart <- format_FV(input$RT_fstart) %>% as.numeric
    fstop <- format_FV(input$RT_fstop) %>% as.numeric 
    fstep <- format_FV(input$RT_fstep) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- get_Funvals(data)
    
    seq_FV(fall, fstart, fstop, by = fstep) %>% cat
  })
  
  output$RT_ECDF_AGGR <- renderPlotly({
    render_RT_ECDF_AGGR()
  })
  
  output$FIG_DOWNLOAD_RT_ECDF_AGGR <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_ECDF_AGGR)
    },
    content = function(file) {
      save_plotly(render_RT_ECDF_AGGR(), file,
           format = input$FIG_FORMAT_RT_ECDF_AGGR, 
           width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_RT_ECDF_AGGR)
  )
  
  render_RT_ECDF_AGGR <- reactive({
    req(input$RT_fstart, input$RT_fstop, input$RT_fstep)
    
    fstart <- format_FV(input$RT_fstart) %>% as.numeric
    fstop <- format_FV(input$RT_fstop) %>% as.numeric 
    fstep <- format_FV(input$RT_fstep) %>% as.numeric
    

    plot_RT_ECDF_AGGR.DataSetList(
      DATA(), fstart, fstop, fstep, 
      show.per_target = input$RT_ECDF_per_target,
      scale.xlog = input$RT_ECDF_AGGR_semilogx
    )
  })
  
  # evaluation rake of all courses 
  output$RT_AUC <- renderPlotly({
    render_RT_AUC()
  })
  
  output$FIG_DOWNLOAD_RT_AUC <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_AUC)
    },
    content = function(file) {
      save_plotly(render_RT_AUC(), file,
           format = input$FIG_FORMAT_RT_AUC, 
           width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_RT_AUC)
  )
  
  render_RT_AUC <- reactive({
    req(input$RT_AUC_FSTART, input$RT_AUC_FSTOP, input$RT_AUC_FSTEP)
    
    fstart <- format_FV(input$RT_AUC_FSTART) %>% as.numeric
    fstop <- format_FV(input$RT_AUC_FSTOP) %>% as.numeric 
    fstep <- format_FV(input$RT_AUC_FSTEP) %>% as.numeric
    
    plot_RT_AUC.DataSetList(
      DATA(), fstart, fstop, fstep, fval_formatter = format_FV
    )
  })

  # TODO: rename 'FCE'...
  # Data summary for Fixed-Budget target (FCE)  --------------
  FV_data_overview <- reactive({
    data <- DATA()
    fall <- get_Funvals(data)
    get_RT_overview(data, algorithm = input$FCE_ALGID_INPUT_SUMMARY)
  })
  
  output$table_FV_overview <- renderTable({
    req(input$FCE_ALGID_INPUT_SUMMARY)
    df <- FV_data_overview()
    
    df$Budget %<>% as.integer
    df$runs %<>% as.integer
    df$fID %<>% as.integer
    df$DIM %<>% as.integer
    df$`miminal runtime` %<>% format_RT
    df$`maximal runtime` %<>% format_RT
    df
  })
  
  get_FCE_summary <- reactive({
    req(input$RT_MIN, input$RT_MAX, input$RT_STEP)
    
    rt_min <- input$RT_MIN %>% as.integer
    rt_max <- input$RT_MAX %>% as.integer
    rt_step <- input$RT_STEP %>% as.integer
    
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- get_Runtimes(data)
    
    if (input$RT_SINGLE)
      rt_max <- rt_min
    
    rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
    req(rt_seq)
    
    get_FV_summary(data, rt_seq, algorithm = input$FCE_ALGID_INPUT)[
      , c('DIM', 'funcId') := NULL
      ]
  })
  
  output$FCE_SUMMARY <- renderTable({
    df <- get_FCE_summary()
    df$runs %<>% as.integer
    # df$runs.MaxFunvals %<>% as.integer
    df$median %<>% format(format = 'e', digits = 3)
    df$mean %<>% format(format = 'e', digits = 3)
    df$runtime %<>% as.integer
    
    # TODO: make probs as a global option
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    
    # format the integers
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% format(format = 'e', digits = 3)
    }
    df
  })
  
  output$FCE_SUMMARY_download <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(get_AlgId(data), collapse = ';')
      rt_min <- input$RT_MIN %>% as.integer %>% as.character
      rt_max <- input$RT_MAX %>% as.integer %>% as.character
      rt_step <- input$RT_STEP %>% as.integer %>% as.character
      eval(FV_csv_name)
    },
    content = function(file) {
      write.csv(get_FCE_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  get_FCE <- reactive({
    req(input$RT_MIN_SAMPLE, input$RT_MAX_SAMPLE, input$RT_STEP_SAMPLE)
    rt_min <- input$RT_MIN_SAMPLE %>% as.integer
    rt_max <- input$RT_MAX_SAMPLE %>% as.integer
    rt_step <- input$RT_STEP_SAMPLE %>% as.integer
    
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- get_Runtimes(data)
    
    if (input$RT_SINGLE_SAMPLE)
      rt_max <- rt_min
    
    rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
    req(rt_seq)
    
    get_FV_sample(data, rt_seq, algorithm = input$FCE_ALGID_RAW_INPUT,
                  output = input$download_format_FCE)
    
    # res <- list()
    # n_runs_max <- sapply(data, function(x) length(attr(x, 'instance'))) %>% max
    # 
    # for (i in seq_along(data)) {
    #   ds <- data[[i]]
    #   algId <- attr(ds, 'algId') 
    #   if (input$FCE_ALGID_RAW_INPUT != 'all' && algId != input$FCE_ALGID_RAW_INPUT)
    #     next
    #   
    #   rt <- get_FV_sample(ds, rt_seq, output = input$download_format_FCE)
    #   if (input$download_format_FCE == 'wide') {
    #     # impute the missing records
    #     n <- ncol(rt) - 2
    #     if (n < n_runs_max) 
    #       rt %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
    #   }
    #   res[[i]] <- rt
    # }
    # do.call(rbind, res) 
  })
  
  output$FCE_SAMPLE_download <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(get_AlgId(data), collapse = ';')
      rt_min <- input$RT_MIN %>% as.integer %>% as.character
      rt_max <- input$RT_MAX %>% as.integer %>% as.character
      rt_step <- input$RT_STEP %>% as.integer %>% as.character
      eval(FVSample_csv_name)
    },
    content = function(file) {
      write.csv(get_FCE(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  output$FCE_SAMPLE <- renderDataTable({
    df <- get_FCE()
    df[is.na(df)] <- 'NA'
    df}, options = list(pageLength = 20, scrollX = T))
  
  # Expected Target Value Convergence 
  output$FCE_PER_FUN <- renderPlotly({
    render_FV_PER_FUN()
  })
  
  output$FIG_DOWNLOAD_FV_PER_FUN <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_PER_FUN)
    },
    content = function(file) {
      save_plotly(render_FV_PER_FUN(), file, 
                  format = input$FIG_FORMAT_FV_PER_FUN, 
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_FV_PER_FUN)
  )
  
  render_FV_PER_FUN <- reactive({
    rt_min <- input$FCE_RT_MIN %>% as.integer
    rt_max <- input$FCE_RT_MAX %>% as.integer
    plot_FV_line.DataSetList(DATA(), RTstart = rt_min, RTstop = rt_max, show.CI = input$show.CI.FCE,
                             show.grad = input$FCE_show_grad, show.intensity = input$FCE_show.intensity,
                             show.density = input$FCE_show.density, show.pareto = input$FCE_show.pareto_optima,
                             show.runs = input$FCE_show_all, show.optimal = input$FCE_show.best_of_all,
                             show.mean = input$FCE_show.mean, show.median = input$FCE_show.median,
                             scale.xlog = input$FCE_semilogx, scale.ylog = input$FCE_semilogy)
  })
  
  # empirical p.d.f. of the target value
  render_FV_PDF <- reactive({
    req(input$FCE_PDF_RUNTIME)  
    runtime <- input$FCE_PDF_RUNTIME %>% as.integer 
    plot_FV_PDF.DataSetList(DATA(), runtime, show.sample = input$FCE_SHOW_SAMPLE,
                            scale.ylog = input$FCE_LOGY )
  })
  
  output$FIG_DOWNLOAD_FV_PDF <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_PDF)
    },
    content = function(file) {
      save_plotly(render_FV_PDF(), file, 
                  format = input$FIG_FORMAT_FV_PDF, 
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_FV_PDF)
  )
  
  output$FCE_PDF <- renderPlotly({
    render_FV_PDF()
  })
  
  # historgram of the target values -----------
  render_FV_HIST <- reactive({
    req(input$FCE_HIST_RUNTIME != "")   # require non-empty input
    runtime <- input$FCE_HIST_RUNTIME %>% as.integer 
    plot_FV_HIST.DataSetList(DATA(), runtime, plot_mode = input$FCE_illu_mode)
  })
  
  output$FIG_DOWNLOAD_FV_HIST <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_HIST)
    },
    content = function(file) {
      save_plotly(render_FV_HIST(), file, 
                  format = input$FIG_FORMAT_FV_HIST, 
                  width = fig_width2, height = fig_height2)
    },
    contentType = paste0('image/', input$FIG_FORMAT_FV_HIST)
  )
  
  output$FCE_HIST <- renderPlotly({
    render_FV_HIST()
  })
  
  # The ECDF plots for the target value ----------------
  output$FCE_ECDF_PER_TARGET <- renderPlotly({
    req(input$FCE_ECDF_RT)
    runtimes <- as.integer(input$FCE_ECDF_RT)
    plot_FCE_ECDF_PER_TARGET.DataSetList(DATA(), runtimes, scale.xlog = input$FCE_ECDF_semilogx)
  })
  
  output$FCE_RT_GRID <- renderPrint({
    req(input$FCE_ECDF_RT_MIN, input$FCE_ECDF_RT_MAX, input$FCE_ECDF_RT_STEP)
    
    rt_min <- input$FCE_ECDF_RT_MIN %>% as.integer
    rt_max <- input$FCE_ECDF_RT_MAX %>% as.integer
    rt_step <- input$FCE_ECDF_RT_STEP %>% as.integer
    
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- get_Runtimes(data)
    
    seq_RT(rt, from = rt_min, to = rt_max, by = rt_step) %>% cat
  })
  
  render_FV_ECDF_AGGR <- reactive({
    req(input$FCE_ECDF_RT_MIN, input$FCE_ECDF_RT_MAX, input$FCE_ECDF_RT_STEP)
    
    rt_min <- input$FCE_ECDF_RT_MIN %>% as.integer
    rt_max <- input$FCE_ECDF_RT_MAX %>% as.integer
    rt_step <- input$FCE_ECDF_RT_STEP %>% as.integer
    
    plot_FV_ECDF_AGGR.DataSetList(DATA(), rt_min = rt_min, 
                                  rt_max = rt_max, rt_step = rt_step, 
                                  scale.xlog = input$FCE_ECDF_AGGR_semilogx,
                                  show.per_target = input$FCE_ECDF_per_target)
  })
  
  output$FIG_DOWNLOAD_FV_ECDF_AGGR <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_ECDF_AGGR)
    },
    content = function(file) {
      save_plotly(render_FV_ECDF_AGGR(), file, 
                  format = input$FIG_FORMAT_FV_ECDF_AGGR, 
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_FV_ECDF_AGGR)
  )
  
  output$FCE_ECDF_AGGR <- renderPlotly({
    render_FV_ECDF_AGGR()
  })
  
  # evaluation rake of all courses 
  render_FV_AUC <- reactive({
    req(input$FCE_AUC_RT_MIN, input$FCE_AUC_RT_MAX, input$FCE_AUC_RT_STEP)
    
    rt_min <- input$FCE_AUC_RT_MIN %>% as.integer
    rt_max <- input$FCE_AUC_RT_MAX %>% as.integer
    rt_step <- input$FCE_AUC_RT_STEP %>% as.integer
    plot_FV_AUC.DataSetList(DATA(), rt_min = rt_min, 
                            rt_max = rt_max, rt_step = rt_step)
    
  })
  
  output$FIG_DOWNLOAD_FV_AUC <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_AUC)
    },
    content = function(file) {
      save_plotly(render_FV_AUC(), file, 
                  format = input$FIG_FORMAT_FV_AUC, 
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_FV_AUC)
  )
  
  output$FCE_AUC <- renderPlotly({
    render_FV_AUC()
  })
  
  # Expected Evolution of parameters in the algorithm
  render_PAR_PER_FUN <- reactive({
    req(input$PAR_F_MIN, input$PAR_F_MAX)
    
    f_min <- format_FV(input$PAR_F_MIN) %>% as.numeric
    f_max <- format_FV(input$PAR_F_MAX) %>% as.numeric  
    
    plot_PAR_Line.DataSetList(DATA(),f_min,f_max,algids = input$PAR_ALGID_INPUT,
                              show.mean = (input$PAR_show.mean == 'mean'),
                              show.median = (input$PAR_show.mean == 'median'),
                              show.CI = input$show.CI.PAR,
                              scale.xlog = input$PAR_semilogx,
                              scale.ylog = input$PAR_semilogy)
  })
  
  output$FIG_DOWNLOAD_PAR_PER_FUN <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_PAR_PER_FUN)
    },
    content = function(file) {
      save_plotly(render_PAR_PER_FUN(), file, 
                  format = input$FIG_FORMAT_PAR_PER_FUN, 
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FIG_FORMAT_PAR_PER_FUN)
  )
  
  output$PAR_PER_FUN <- renderPlotly({
    render_PAR_PER_FUN()
  })
  
  # TODO: add ks test for ECDF later
  # output$ks <- renderPrint({
  #   target <- input$target %>% as.numeric
  #   df.aligneds <- aligned()
  #   
  #   running_time <- list()
  #   for (i in seq_along(df.aligneds)) {
  #     df <- df.aligneds[[i]]
  #     v <- rownames(df) %>% as.numeric
  #     idx <- order(abs(target - v))[1]
  #     running_time[[i]] <- df[idx, ] %>% as.vector
  #   }
  #   algorithm1 <- running_time[[1]]
  #   algorithm2 <- running_time[[2]]
  #   a <- ks.test(algorithm1, algorithm2, alternative = 'less') 
  #   print(a)
  # })
  # 
  
  parameter_summary <- reactive({
    req(input$PAR_F_MIN_SUMMARY, input$PAR_F_MAX_SUMMARY, input$PAR_F_STEP_SUMMARY)
    
    fstart <- format_FV(input$PAR_F_MIN_SUMMARY) %>% as.numeric
    fstop <- format_FV(input$PAR_F_MAX_SUMMARY) %>% as.numeric
    fstep <- format_FV(input$PAR_F_STEP_SUMMARY) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- get_Funvals(data)
    
    if (input$PAR_F_SINGLE)
      fstop <- fstart
    
    fseq <- seq_FV(fall, fstart, fstop, by = fstep)
    req(fseq)
    
    get_PAR_summary(data, fseq, input$PAR_ALGID_INPUT_SUMMARY, input$PAR_INPUT)
  })
  
  parameter_sample <- reactive({
    req(input$PAR_ALGID_INPUT_SAMPLE, input$PAR_F_MAX_SAMPLE, 
        input$PAR_F_STEP_SAMPLE, input$PAR_F_MIN_SAMPLE,
        input$PAR_INPUT_SAMPLE)
    
    fstart <- format_FV(input$PAR_F_MIN_SAMPLE) %>% as.numeric
    fstop <- format_FV(input$PAR_F_MAX_SAMPLE) %>% as.numeric
    fstep <- format_FV(input$PAR_F_STEP_SAMPLE) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- get_Funvals(data)
    
    if (input$PAR_SAMPLE_F_SINGLE)
      fstop <- fstart
    
    fseq <- seq_FV(fall, fstart, fstop, by = fstep)
    req(fseq)
    
    get_PAR_sample(data, ftarget = fseq, 
                   algorithm = input$PAR_ALGID_INPUT_SAMPLE, 
                   parId = input$PAR_INPUT_SAMPLE,
                   output = input$PAR_download_format)
  })
  
  output$table_PAR_SAMPLE <- renderDataTable({
    dt <- parameter_sample()
    req(length(dt) != 0)
    dt[is.na(dt)] <- 'NA'
    dt}, options = list(pageLength = 20, scrollX = T))
  
  output$table_PAR_summary <- renderTable({
    dt <- parameter_summary()
    req(length(dt) != 0)
    dt$runs %<>% as.integer
    dt$mean %<>% format(digits = 2, nsmall = 2)
    dt$median %<>% format(digits = 2, nsmall = 2)
    
    # TODO: make probs as a global option
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    
    # format the integers
    # for (p in paste0(probs * 100, '%')) {
    #   df[[p]] %<>% format(digits = 2, nsmall = 2)
    # }
    dt
  })
  
  output$PAR_SAMPLE_downloadData <- downloadHandler(
    filename = {
      fstart <- format_FV(input$PAR_F_MIN_SAMPLE)
      fstop <- format_FV(input$PAR_F_MAX_SAMPLE)
      fstep <- format_FV(input$PAR_F_STEP_SAMPLE)
      eval(PARSample_csv_name)
    },
    content = function(file) {
      write.csv(parameter_sample(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  output$PAR_downloadData <- downloadHandler(
    filename = {
      fstart <- format_FV(input$PAR_F_MIN_SUMMARY)
      fstop <- format_FV(input$PAR_F_MAX_SUMMARY)
      fstep <- format_FV(input$PAR_F_STEP_SUMMARY) 
      eval(PAR_csv_name)
    }, 
    content = function(file) {
      write.csv(parameter_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
})
