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

library(shiny)
library(shinyjs)
library(reshape2)
library(magrittr)
library(dplyr)
library(plotly)

source('pproc.R')
source('plot.R')

options(width = 80)
options(shiny.maxRequestSize = 200 * 1024 ^ 2)   # maximal number of requests, this is too many...

symbols <- c("circle-open", "diamond-open", "square-open", "cross-open",
             "triangle-up-open", "triangle-down-open")

# TODO: put it as an option such that the user can select
maximization <- TRUE
src_format <- 'IOHProfiler' # TODO: this shoule be taken from the data set
sub_sampling <- TRUE

# Formatter for function values
format_funeval <- function(v) format(as.integer(as.numeric(v)))

# generate evenly space grid of function values
funeval_grid <- function(v, ...) {
  from <- min(v)
  to <- max(v)
  if (src_format == 'IOHProfiler') {
    from <- round(from)
    to <- round(to)
    grid <- seq(from, to, ...)
    step <- as.integer(grid[2] - grid[1]) 
    seq(from, to, by = step)
  } else {
    seq(from, to, ...)
  }
}

# transformations applied on the function value
# trans_funeval <- `log10`
# reverse_trans_funeval <- function(x) 10 ^ x
trans_funeval <- . %>% return
reverse_trans_funeval <- . %>% return

# transformations applied on runtime values
trans_runtime <- . %>% return
reverse_trans_runtime <- . %>% return

# directory where data are extracted from the zip file
exdir <- './data'

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # clean up the temporarsy files on server when exiting  
  session$onSessionEnded(function() {
    unlink(exdir, recursive = T)
  })
  
  # variables shared
  folderList <- reactiveValues(data = list())
  DataList <- reactiveValues(data = DataSetList())
  
  # update maximization indication, trans_funeval according to src_format 
  observe({
    src_format <<- input$DATA_SRC_FORMAT
    if (input$DATA_SRC_FORMAT == 'IOHProfiler') {
      maximization <<- TRUE
      trans_funeval <<- . %>% return
      reverse_trans_runtime <<- . %>% return
      format_funeval <<- function(v) format(as.integer(as.numeric(v)))
      
    } else if (input$DATA_SRC_FORMAT == 'COCO') {
      maximization <<- FALSE
      format_funeval <<- function(v) format(v, format = "e", digits = 5, nsmall = 2)
      # TODO: determine if we need transformations on the function values
      # trans_funeval <<- `log10`
      # reverse_trans_funeval <<- function(x) 10 ^ x
    } 
  })
  
  # is subsamping?
  observe({
    sub_sampling <<- input$SUBSAMPLING  
  })
  
  # IMPORTANT: this only works locally, keep it for the local version
  # links to users file systems
  # volumes <- getVolumes()
  # shinyDirChoose(input, 'directory', roots = volumes, session = session)
  
  # browse data directory, upload data and process data -------
  
  # the directory selected by the user
  # selected_folders <- reactive({
  #   # a <- list(root = "Macintosh HD", path = list('', "Users", "wanghao", "(1+1)-Cholesky-CMA"))
  #   # parseDirPath(volumes, a)
  #   parseDirPath(volumes, input$directory)
  # })
  
  # the folder where the uploaded zip file is uncompressed
  selected_folders <- reactive({
    if (!is.null(input$ZIP)) {
      datapath <- input$ZIP$datapath
      filename <- input$ZIP$name
      folders <- rep('', length(datapath))
      
      for (i in seq(datapath)) {
        unzip(datapath[i], list = FALSE, exdir = exdir)
        name <- strsplit(filename[i], '\\.')[[1]][1]
        folders[i] <- file.path(exdir, name)
      }
      return(folders)
    } else
      return(NULL)
  })
  
  # TODO: this part might not be
  # observe({
  #   if (input$singleF) {
  #     fstart <- input$fstart
  #     # updateTextInput(session, 'fstart', value = format_funeval(start))
  #     updateTextInput(session, 'fstop', value = format_funeval(fstart))
  #     # updateTextInput(session, 'fstep', value = format_funeval(step))
  #   }
  # })
  # 
  # # print the folderList
  # output$upload_data_promt <- renderPrint({
  #   folders <- folderList$data
  #   for (i in seq_along(folders)) {
  #     cat(sprintf('%d: %s\n', i, folders[[i]]))
  #   }
  # })
  
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
      indexFiles <- scan_indexFile(folder)
      if (length(indexFiles) == 0) 
        shinyjs::html("process_data_promt", 
                      paste0('<p style="color:red;">No index file (.info) is found in folder ', 
                             folder, '... skip</p>'), add = TRUE)
      else {
        folderList$data <- c(folderList$data, folder)
        
        # TODO: check if the newly loaded data contradicts the selected format
        print_fun <- function(s) shinyjs::html("process_data_promt", s, add = TRUE)
        DataList$data <- c(DataList$data, read_dir(folder, print_fun = print_fun,
                                                   maximization = maximization,
                                                   format = src_format,
                                                   subsampling = sub_sampling))
        
        # TODO: fix this urgly part by implementing 'c()' for DataSetList
        # class(DataList$data) <- 'DataSetList'
        shinyjs::html("upload_data_promt", 
                      sprintf('%d: %s\n', length(folderList$data), folder), add = TRUE)
      }
    } 
  })
  
  # remove all uploaded data set
  observeEvent(input$RM_DATA, {
    if (length(DataList$data) != 0) {
      DataList$data <- list()
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
    
    dim <- getDIM(data)
    updateSelectInput(session, 'DIM_INPUT', choices = dim, selected = dim[1])
    
    funcID <- getfuncId(data)
    updateSelectInput(session, 'FUNCID_INPUT', choices = funcID, selected = funcID[1])
    
    algId <- c(getAlgId(data), 'all')
    updateSelectInput(session, 'ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'ALGID_RAW_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'PAR_ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'FCE_ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'FCE_ALGID_RAW_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'PAR_ALGID_INPUT_SUMMARY', choices = algId, selected = 'all')
    
    parId <- c(getParId(data), 'all')
    updateSelectInput(session, 'PAR_INPUT', choices = parId, selected = 'all')
  })
  
  # the filtered DataSets 
  DATA <- reactive({
    dim <- input$DIM_INPUT
    id <- input$FUNCID_INPUT
    
    if (length(DataList$data) == 0)
      return(NULL)
    
    # filter(DataList$data, by = c(DIM = dim, funcId = id))
    subset(DataList$data, DIM == dim, funcId == id)
  })
  
  # compute the ERT for all DataSets after filtering
  ERT.DATA <- reactive({
    data <- DATA()
    lapply(data, ERT.DataSet)
  })
  
  # compute the FCE for all DataSets after filtering
  FCE.DATA <- reactive({
    data <- DATA()
    lapply(data, FCE.DataSet)
  })
  
  # compute the expected parameter value for all DataSets after filtering
  EPAR.DATA <- reactive({
    data <- DATA()
    lapply(data, EPAR.DataSet)
  })
  
  # update the values for the grid of target values
  observe({
    data <- DATA()
    v <- getFunvals(data)
    req(length(v) != 0)
    
    v <- trans_funeval(v)    # generate the grid in log10 scale!
    q <- quantile(v, probs = c(.25, .5, .75), names = F)
    grid <- funeval_grid(v, length.out = 15) 
    
    step <- grid[2] - grid[1]
    start <- grid[1]
    stop <- grid[length(grid)]
    
    updateTextInput(session, 'fstart', value = format_funeval(start))
    updateTextInput(session, 'fstop', value = format_funeval(stop))
    updateTextInput(session, 'fstep', value = format_funeval(step))
    
    updateTextInput(session, 'F_MIN_SAMPLE', value = format_funeval(start))
    updateTextInput(session, 'F_MAX_SAMPLE', value = format_funeval(stop))
    updateTextInput(session, 'F_STEP_SAMPLE', value = format_funeval(step))
    
    updateTextInput(session, 'RT_fstart', value = format_funeval(start))
    updateTextInput(session, 'RT_fstop', value = format_funeval(stop))
    updateTextInput(session, 'RT_fstep', value = format_funeval(step))
    updateTextInput(session, 'RT_fselect', value = format_funeval(median(v)))
    
    updateTextInput(session, 'RT_PMF_FTARGET', value = format_funeval(median(v)))
    updateTextInput(session, 'RT_PMF_HIST_FTARGET', value = format_funeval(median(v)))
    
    s <- ((stop - start) * 0.1 + start)
    e <- ((stop - start) * 0.9 + start)
    updateTextInput(session, 'ERT_FSTART', value = format_funeval(s))
    updateTextInput(session, 'ERT_FSTOP', value = format_funeval(e))
    
    updateTextInput(session, 'RT_ECDF_FTARGET1', value = format_funeval(q[1]))
    updateTextInput(session, 'RT_ECDF_FTARGET2', value = format_funeval(q[2]))
    updateTextInput(session, 'RT_ECDF_FTARGET3', value = format_funeval(q[3]))
    
    updateTextInput(session, 'RT_AUC_FSTART', value = format_funeval(start))
    updateTextInput(session, 'RT_AUC_FSTOP', value = format_funeval(stop))
    updateTextInput(session, 'RT_AUC_FSTEP', value = format_funeval(step))
    
    updateTextInput(session, 'PAR_F_MIN', value = start)
    updateTextInput(session, 'PAR_F_MAX', value = stop)
    
    updateTextInput(session, 'PAR_F_MIN_SUMMARY', value = start)
    updateTextInput(session, 'PAR_F_MAX_SUMMARY', value = stop)
    updateTextInput(session, 'PAR_F_STEP_SUMMARY', value = step)
  })
  
  # update the values for the grid of running times
  observe({
    data <- DATA()
    v <- getRuntimes(data)
    
    if (length(v) == 0)
      return()
    
    v <- trans_runtime(v)  %>% as.integer  
    q <- quantile(v, probs = c(.25, .5, .75), names = F, type = 3)
    
    grid <- seq(min(v), max(v), length.out = 15) %>% as.integer
    step <- min(grid[-1] - grid[-length(grid)]) 
    
    updateTextInput(session, 'RT_MIN', value = min(v))
    updateTextInput(session, 'RT_MAX', value = max(v))
    updateTextInput(session, 'RT_STEP', value = step)
    
    updateTextInput(session, 'RT_MIN_SAMPLE', value = min(v))
    updateTextInput(session, 'RT_MAX_SAMPLE', value = max(v))
    updateTextInput(session, 'RT_STEP_SAMPLE', value = step)
    
    updateTextInput(session, 'FCE_HIST_RUNTIME', value = median(v))
    updateTextInput(session, 'FCE_PDF_RUNTIME', value = median(v))
    
    s <- ((max(v) - min(v)) * 0.05 + min(v)) %>% as.integer
    e <- ((max(v) - min(v)) * 0.95 + min(v)) %>% as.integer
    updateTextInput(session, 'FCE_RT_MIN', value = s)
    updateTextInput(session, 'FCE_RT_MAX', value = e)
    
    updateTextInput(session, 'FCE_ECDF_RT_MIN', value = min(v))
    updateTextInput(session, 'FCE_ECDF_RT_MAX', value = max(v))
    updateTextInput(session, 'FCE_ECDF_RT_STEP', value = step)
    
    updateTextInput(session, 'FCE_AUC_RT_MIN', value = min(v))
    updateTextInput(session, 'FCE_AUC_RT_MAX', value = max(v))
    updateTextInput(session, 'FCE_AUC_RT_STEP', value = step)
    
    updateTextInput(session, 'FCE_ECDF_RT1', value = q[1])
    updateTextInput(session, 'FCE_ECDF_RT2', value = q[2])
    updateTextInput(session, 'FCE_ECDF_RT3', value = q[3])
  })
  
  # Data summary for Fixed-Target Runtime (ERT)  --------------
  # TODO: mover this part to pproc.R
  get_RT_summary <- reactive({
    data <- DATA()
    fall <- getFunvals(data)
    
    fstart <- format_funeval(input$fstart) %>% as.numeric
    fstop <- format_funeval(input$fstop) %>% as.numeric
    fstep <- format_funeval(input$fstep) %>% as.numeric
    
    if (input$singleF)
      fseq <- c(fstart) %>% 
        reverse_trans_funeval %>%
        .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)] 
    else {
      # TODO: change this to 'req'
      # when initializing or incorrect input
      if (is.na(fstart) || is.na(fstop) || is.na(fstep))
        return(data.frame())
      if (fstart >= fstop || fstep > fstop - fstart)
        return(data.frame())
      
      fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
        c(fstart, ., fstop) %>% unique %>% 
        reverse_trans_funeval %>%
        .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)] 
    }
    
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    res <- list()
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      if (input$ALGID_INPUT != 'all' && attr(df, 'algId') != input$ALGID_INPUT)
        next
      
      res[[i]] <- summarise_runtime(df, fseq, probs = probs, maximization = maximization)
    }
    do.call(rbind.data.frame, res)
  })
  
  output$table_RT_summary <- renderTable({
    df <- get_RT_summary()
    df$runs %<>% as.integer
    df$median %<>% as.integer
    df$`f(x)` <- format_funeval(df$`f(x)`)
    
    # TODO: make probs as a global option
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    
    # format the integers
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% as.integer
    }
    df
  })
  
  output$downloadData <- downloadHandler(
    filename = {
      data <- DATA()
      fstart <- format_funeval(input$fstart)
      fstop <- format_funeval(input$fstop)
      fstep <- format_funeval(input$fstep) 
      sprintf('runtime_summary_(%s,%s,%s).csv',
              fstart, fstop, fstep)
    }, 
    content = function(file) {
      write.csv(get_RT_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  get_RT <- reactive({
    data <- DATA()
    fall <- getFunvals(data)
    
    fstart <- format_funeval(input$F_MIN_SAMPLE) %>% as.numeric
    fstop <- format_funeval(input$F_MAX_SAMPLE) %>% as.numeric
    fstep <- format_funeval(input$F_STEP_SAMPLE) %>% as.numeric
    
    if (input$F_SAMPLE_SINGLE)
      fseq <- c(fstart) %>% 
      reverse_trans_funeval %>% 
      .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)] 
    else {
      # when initializing or incorrect input
      if (is.na(fstart) || is.na(fstop) || is.na(fstep))
        return(data.frame())
      if (fstart >= fstop || fstep > fstop - fstart)
        return(data.frame())
      
      fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
        c(fstart, ., fstop) %>% unique %>% 
        reverse_trans_funeval %>% 
        .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)] 
    }
    
    data <- DATA()
    res <- list()
    n_runs_max <- sapply(data, function(x) ncol(x$by_target)) %>% max
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      
      if (input$ALGID_RAW_INPUT != 'all' && attr(df, 'algId') != input$ALGID_RAW_INPUT)
        next
      
      rt <- get_runtime_sample(df, fseq, format = input$RT_download_format, 
                               maximization = maximization)
      if (input$RT_download_format == 'wide') {
        n <- ncol(rt) - 2
        if (n < n_runs_max) 
          rt %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
      }
      res[[i]] <- rt
    }
    do.call(rbind.data.frame, res) 
  })
  
  output$download_runtime <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(getAlgId(data), collapse = ';')
      fstart <- input$F_MIN_SAMPLE %>% format_funeval
      fstop <- input$F_MAX_SAMPLE %>% format_funeval
      fstep <- input$F_STEP_SAMPLE %>% format_funeval
      sprintf('runtime_(%s,%s,%s).csv', fstart, fstop, fstep)
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
  output$ERT_PER_FUN <- renderPlotly({
    Fmin <- format_funeval(input$ERT_FSTART) %>% as.numeric %>% reverse_trans_funeval
    Fmax <- format_funeval(input$ERT_FSTOP) %>% as.numeric %>% reverse_trans_funeval
    
    data <- ERT.DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(#title = "Expected Runtime Comparison",
                         x.title = "best-so-far f(x)-value", y.title = "function evaluations")
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      algId <- attr(df, 'algId')
      df_plot <- df %>% 
        mutate(upper = ERT + sd, lower = ERT - sd) %>% 
        filter(BestF >= Fmin, BestF <= Fmax) %>% 
        limit.data.frame(n = 50)
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
      
      p %<>% 
        add_trace(data = df_plot, x = ~BestF, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0),  legendgroup = algId,
                  showlegend = F, name = 'mean +/- sd') %>% 
        add_trace(x = ~BestF, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  legendgroup = algId,
                  fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd')
      
      if (input$show.mean)
        p %<>% add_trace(data = df_plot, x = ~BestF, y = ~ERT, type = 'scatter', 
                         mode = 'lines+markers', name = paste0(algId, '.mean'), 
                         marker = list(color = rgb_str), legendgroup = algId,
                         line = list(color = rgb_str))
      
      if (input$show.median)
        p %<>% add_trace(data = df_plot, x = ~BestF, y = ~median, type = 'scatter',
                         name = paste0(algId, '.median'), mode = 'lines+markers', 
                         marker = list(color = rgb_str),  legendgroup = algId,
                         line = list(color = rgb_str, dash = 'dash'))
    }
    p %<>%
      layout(xaxis = list(type = switch(input$semilogx, T = 'log', F = 'linear')),
             yaxis = list(type = switch(input$semilogy, T = 'log', F = 'linear')))
    
    # minimization for COCO
    if (src_format == 'COCO')
      p %<>% layout(xaxis = list(autorange = "reversed"))
    p
  })
  
  # empirical p.m.f. of the runtime
  output$RT_PMF <- renderPlotly({
    ftarget <- format_funeval(input$RT_PMF_FTARGET) %>% as.numeric %>% reverse_trans_funeval
    points <- ifelse(input$RT_SHOW_SAMPLE, 'all', FALSE)
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(#title = "p.m.f. of the runtime",
                         x.title = "algorithms",
                         y.title = "runtime / function evaluations")
  
    for (i in seq_along(data)) {
      df <- data[[i]]
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.5)')
      
      p %<>%
        add_trace(data = get_runtime_sample(df, ftarget, format = 'long'),
                  x = ~algId, y = ~RT, split = ~algId, type = 'violin',
                  hoveron = "points+kde",
                  box = list(visible = T),
                  points = points,
                  pointpos = 1,
                  jitter = 0.1,
                  scalemode = 'count',
                  meanline = list(visible = T),
                  line = list(color = rgb_str, width = 1),
                  marker = list(color = rgb_str))
      
      # rt <- RT(df, ftarget, format = 'long') %>% 
      #   mutate(label = i)
      
      # kernel estimation of p.m.f.
      # res <- kernel_PMF(as.numeric(rt$RT))
      # 
      # x <- res$x
      # y <- res$y
      # 
      # x <- x[y != 0]
      # y <- y[y != 0] 
      # idx <- seq(1, length(x), length.out = 50)
      # x <- x[idx]
      # y <- y[idx]
      # y <- y / max(y) * 0.8
      
      # p %<>% 
      #   add_trace(x = x, y = y + i, type = 'scatter',
      #             hoveron = "points", showlegend = F,
      #             mode = 'markers', legendgroup = paste0(i),
      #             marker = list(color = 'rgba(9,56,125,0.45)'), 
      #             line = list(color = 'rgb(9,56,125)', width = 0)) %>%
      #   add_trace(data = rt, x = ~RT, y = ~as.character(label), type = 'box',
      #             line = list(color = rgb_str, width = 0.8), legendgroup = paste0(i),
      #             marker = list(color = rgb_str), fillcolor = rgba_str, name = attr(df, 'algorithm'))
      # 
      # for (k in seq_along(x)) {
      #   p %<>% 
      #     add_segments(x = x[k], xend = x[k], y = i, yend = y[k] + i, 
      #                  line = list(color = 'rgba(9,56,125,0.4)'),
      #                  showlegend = F)
      # }
    }
    p %<>%
      layout(yaxis = list(type = switch(input$RT_PMF_LOGY, T = 'log', F = 'linear')))
  })
  
  # historgram of the running time
  output$RT_HIST <- renderPlotly({
    ftarget <- format_funeval(input$RT_PMF_HIST_FTARGET) %>% as.numeric %>% reverse_trans_funeval
    plot_mode <- input$ERT_illu_mode
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    nrows <- ceiling(n_algorithm / 2.) # keep to columns for the histograms
    
    if (plot_mode == 'overlay') {
      p <- plot_ly_default(#title = "Histogram of the runtime",
                           x.title = "function evaluations", y.title = "runs")
      
    } else if (plot_mode == 'subplot') {
      p <- lapply(seq(n_algorithm), function(x) {
        plot_ly_default(#title = "Histogram of the runtime",
                        x.title = "function evaluations", y.title = "runs")
      })
    }
    
    for (i in seq_along(data)) {
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.35)')
      
      df <- data[[i]]
      algId <- attr(df, 'algId')
      rt <- get_runtime_sample(df, ftarget, format = 'long')
      
      # skip if all runtime samples are NA
      if (sum(!is.na(rt$RT)) < 2)
        next
      
      res <- hist(rt$RT, breaks = nclass.FD, plot = F)
      breaks <- res$breaks
      plot_data <- data.frame(x = res$mids, y = res$counts, width = breaks[2] - breaks[1],
                         text = paste0('<b>count</b>: ', res$counts, '<br><b>breaks</b>: [', 
                                       breaks[-length(breaks)], ',', breaks[-1], ']')) 
      
      if (plot_mode == 'overlay') {
        p %<>%
          add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = algId, text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      } else if (plot_mode == 'subplot') {
        p[[i]] %<>% 
          add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = algId, text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      }
    }
    
    if (plot_mode == 'subplot') {
      p <- subplot(p, nrows = nrows, titleX = F, titleY = F, margin = 0.03)
    }
    p
  })
  
  # The ECDF plots for the runtime ----------------
  output$RT_ECDF <- renderPlotly({
    ftargets <- c(
      format_funeval(input$RT_ECDF_FTARGET1),
      format_funeval(input$RT_ECDF_FTARGET2),
      format_funeval(input$RT_ECDF_FTARGET3)) %>% 
      as.numeric %>% 
      reverse_trans_funeval
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(title = NULL,
      # title = "Empirical Cumulative Distribution of the runtime",
                         x.title = "function evaluations",
                         y.title = "Proportion of runs")
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.35)')
      
      for (i in seq_along(ftargets)) {
        res <- get_runtime_sample(df, ftargets[i], format = 'long') 
        rt <- sort(res$RT)
        
        if (all(is.na(rt)))
          next
        
        ecdf <- CDF_discrete(rt)
        
        # position of the markers
        x <- quantile(rt, probs = c(0.25, 0.5, 0.75), names = F, type = 3)
        y <- sapply(x, function(x) ecdf[rt == x][1])
        
        p %<>%
          add_trace(data = res, x = rt, y = ecdf, type = 'scatter',
                    mode = 'lines', name = algId, showlegend = F,
                    legendgroup = paste0(k),
                    line = list(color = rgb_str, width = 3)) %>% 
          add_trace(data = res, x = x, y = y, type = 'scatter',
                    mode = 'markers',  legendgroup = paste0(k),
                    name = sprintf('(%s, %.2e)', algId, ftargets[i]),
                    marker = list(color = rgb_str, symbol = symbols[i], size = 13))
      }
    }
    
    p %<>%
      layout(xaxis = list(type = switch(input$RT_ECDF_semilogx, 
                                        T = 'log', F = 'linear')))
  })
  
  output$RT_GRID <- renderPrint({
    data <- DATA()
    fall <- getFunvals(data)
    
    fstart <- format_funeval(input$RT_fstart) %>% as.numeric
    fstop <- format_funeval(input$RT_fstop) %>% as.numeric 
    fstep <- format_funeval(input$RT_fstep) %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
      c(fstart, ., fstop) %>% unique %>% 
      reverse_trans_funeval %>% 
      .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)] %>% 
      cat
  })
  
  output$RT_ECDF_AGGR <- renderPlotly({
    data <- DATA()
    fall <- getFunvals(data)
    
    fstart <- format_funeval(input$RT_fstart) %>% as.numeric
    fstop <- format_funeval(input$RT_fstop) %>% as.numeric 
    fstep <- format_funeval(input$RT_fstep) %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
      c(fstart, ., fstop) %>% unique %>% 
      reverse_trans_funeval %>% 
      .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)] 
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    RT.max <- sapply(data, function(d) max(d$by_target, na.rm = T)) %>% max
    RT.min <- sapply(data, function(d) min(d$by_target, na.rm = T)) %>% min
    x <- seq(RT.min, RT.max, length.out = 50)
    p <- plot_ly_default(#title = "Empirical Cumulative Distribution of the runtime",
                         x.title = "function evaluations",
                         y.title = "Proportion of (run, target) pairs")
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.15)')
      rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.8)')
      
      m <- lapply(fseq, function(f) {
        rt <- get_runtime_sample(df, f, format = 'long') %>% '$'('RT')
        if (all(is.na(rt)))
          return(rep(0, length(x)))
        fun <- ecdf(rt)
        fun(x)
      }) %>% 
        do.call(rbind, .)
      
      df_plot <- data.frame(x = x, 
                            mean = apply(m, 2, . %>% mean(na.rm = T)),
                            sd = apply(m, 2, . %>% sd(na.rm = T))) %>% 
        mutate(upper = mean + sd, lower = mean - sd)
                              
      p %<>%
        # TODO: maybe not showing the std. shade at all!
        # add_trace(data = df_plot, x = ~x, y = ~upper, type = 'scatter', mode = 'lines',
        #           line = list(color = rgba_str, width = 0),
        #           showlegend = F, name = 'mean +/- sd') %>%
        # add_trace(x = ~x, y = ~lower, type = 'scatter', mode = 'lines',
        #           fill = 'tonexty',  line = list(color = 'transparent'),
        #           fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd') %>%
        add_trace(data = df_plot, x = ~x, y = ~mean, type = 'scatter',
                  mode = 'lines+markers', name = sprintf('%s', algId), 
                  showlegend = T, legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 4.5),
                  marker = list(color = rgb_str, size = 11))
      
      if (input$RT_ECDF_per_target) {
        for (f in fseq) {
          rt <- get_runtime_sample(df, f, format = 'long') %>% '$'('RT') %>% sort
          # TODO: plot the unsuccessful ECDF
          if (all(is.na(rt)))
            next
          else 
            v <- CDF_discrete(rt)
          
          p %<>%
            add_trace(x = rt, y = v, type = 'scatter',
                      mode = 'lines', name = algId, showlegend = F,
                      line = list(color = rgba_str2, width = 1, dash = 'dot'))
        }
      }
    }
    
    p %<>%
      layout(xaxis = list(type = switch(input$RT_ECDF_AGGR_semilogx, 
                                        T = 'log', F = 'linear')))
  })
  
  # evaluation rake of all courses 
  output$RT_AUC <- renderPlotly({
    data <- DATA()
    fall <- getFunvals(data)
    
    fstart <- format_funeval(input$RT_AUC_FSTART) %>% as.numeric
    fstop <- format_funeval(input$RT_AUC_FSTOP) %>% as.numeric 
    fstep <- format_funeval(input$RT_AUC_FSTEP) %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
      c(fstart, ., fstop) %>% unique %>% 
      reverse_trans_funeval %>% 
      .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)]  

    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    RT.max <- sapply(data, function(d) max(d$by_target, na.rm = T)) %>% max
    p <- plot_ly_default()
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.2)')
      
      # calculate ECDFs on user specified targets
      funs <- lapply(fseq, function(f) {
        get_runtime_sample(df, f, format = 'long') %>% 
          '$'('RT') %>% {
          if (all(is.na(.))) NULL
          else  RT.ECDF(.)
        }
      })
      
      auc <- sapply(funs,
                    function(fun) {
                      if (is.null(fun)) 0
                      else integrate(fun, lower = attr(fun, 'min') - 1, upper = RT.max, 
                                     subdivisions = 5e3) %>% {'$'(., 'value') / RT.max}
                    })
      
      p %<>% 
        add_trace(type = 'scatterpolar', r = auc, 
                  theta = paste0('f:', format_funeval(fseq)), 
                  fill = 'toself', fillcolor = rgba_str,
                  marker = list(color = rgb_str), hoverinfo = 'text',
                  text = paste0('area: ', format(auc, digits = 2, nsmall = 2)),
                  name = algId) 
    }
    
    p %<>%
      layout(polar = list(radialaxis = list(visible = T)),
             yaxis = list(type = 'log'),
             autosize = T, hovermode = 'compare',
             paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)')
  })
  
  # Data summary for Fixed-Budget target (FCE)  --------------
  get_FCE_summary <- reactive({
    data <- DATA()
    rt <- getRuntimes(data)
    
    rt_min <- input$RT_MIN %>% as.integer
    rt_max <- input$RT_MAX %>% as.integer
    rt_step <- input$RT_STEP %>% as.integer
    
    # check for requirements
    req(rt_min < rt_max)
    req(rt_step > 0)
    
    if (input$RT_SINGLE)
      rt_seq <- c(rt_min) %>% 
      reverse_trans_runtime %>% 
      .[. >= (min(rt))] %>% .[. <= (max(rt))] 
    else {
      # TODO: remove this part and replace it with shiny::req
      # when initializing or incorrect input
      if (is.na(rt_min) || is.na(rt_max) || is.na(rt_step))
        return(data.frame())
      if (rt_min >= rt_max || rt_step > rt_max - rt_min)
        return(data.frame())
      
      rt_seq <- seq(from = rt_min, to = rt_max, by = rt_step) %>% 
        reverse_trans_runtime %>% 
        .[. >= min(rt)] %>% .[. <= max(rt)] 
    }
    
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    res <- list()
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      if (input$FCE_ALGID_INPUT != 'all' && attr(df, 'algId') != input$FCE_ALGID_INPUT)
        next
      
      res[[i]] <- summarise_target(df, rt_seq, probs = probs, maximization = maximization)
    }
    do.call(rbind.data.frame, res)
  })
  
  output$FCE_SUMMARY <- renderTable({
    df <- get_FCE_summary()
    df$runs %<>% as.integer
    df$runs.MaxFunvals %<>% as.integer
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
      algId <- paste0(getAlgId(data), collapse = ';')
      rt_min <- input$RT_MIN %>% as.integer %>% as.character
      rt_max <- input$RT_MAX %>% as.integer %>% as.character
      rt_step <- input$RT_STEP %>% as.integer %>% as.character
      sprintf('target_summary_(%s,%s,%s).csv', rt_min, rt_max, rt_step)
    },
    content = function(file) {
      write.csv(get_FCE_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  get_FCE <- reactive({
    data <- DATA()
    rt <- getRuntimes(data)
    
    rt_min <- input$RT_MIN_SAMPLE %>% as.integer
    rt_max <- input$RT_MAX_SAMPLE %>% as.integer
    rt_step <- input$RT_STEP_SAMPLE %>% as.integer
    
    # check for requirements
    req(rt_min <= rt_max)
    req(rt_step > 0)
    
    if (input$RT_SINGLE_SAMPLE)
      rt_seq <- c(rt_min) %>% 
      reverse_trans_runtime %>% 
      .[. >= (min(rt))] %>% .[. <= (max(rt))] 
    else {
      # when initializing or incorrect input
      if (is.na(rt_min) || is.na(rt_max) || is.na(rt_step))
        return(data.frame())
      if (rt_min >= rt_max || rt_step > rt_max - rt_min)
        return(data.frame())
      
      rt_seq <- seq(from = rt_min, to = rt_max, by = rt_step) %>% 
        reverse_trans_runtime %>% 
        .[. >= min(rt)] %>% .[. <= max(rt)] 
    }
    
    res <- list()
    n_runs_max <- sapply(data, function(x) length(attr(x, 'instance'))) %>% max
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      
      if (input$FCE_ALGID_RAW_INPUT != 'all' 
          && attr(df, 'algId') != input$FCE_ALGID_RAW_INPUT)
        next
      
      rt <- get_target_sample(df, rt_seq, format = input$download_format_FCE, 
                              maximization = maximization)
      if (input$download_format_FCE == 'wide') {
        # impute the missing records
        n <- ncol(rt) - 2
        if (n < n_runs_max) 
          rt %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
      }
      res[[i]] <- rt
    }
    do.call(rbind.data.frame, res) 
  })
  
  output$FCE_SAMPLE_download <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(getAlgId(data), collapse = ';')
      rt_min <- input$RT_MIN %>% as.integer %>% as.character
      rt_max <- input$RT_MAX %>% as.integer %>% as.character
      rt_step <- input$RT_STEP %>% as.integer %>% as.character
      sprintf('target_sample_(%s,%s,%s).csv', 
              rt_min, rt_max, rt_step)
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
  
  # empirical p.d.f. of the target value
  output$FCE_PDF <- renderPlotly({
    runtime <- input$FCE_PDF_RUNTIME %>% as.integer %>% reverse_trans_runtime
    points <- ifelse(input$FCE_SHOW_SAMPLE, 'all', FALSE)
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(
      x.title = "algorithms",
      y.title = "Target value")
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.5)')
      
      p %<>%
        add_trace(data = get_target_sample(df, runtime, format = 'long'),
                  x = ~algId, y = ~`f(x)`, split = ~algId, type = 'violin',
                  hoveron = "points+kde",
                  box = list(visible = T),
                  points = points,
                  pointpos = 1,
                  jitter = 0.1,
                  scalemode = 'count',
                  meanline = list(visible = T),
                  line = list(color = rgb_str, width = 1),
                  marker = list(color = rgb_str))
    }
    p %<>%
      layout(yaxis = list(type = switch(input$FCE_LOGY, T = 'log', F = 'linear')))
  })
  
  # historgram of the target values -----------
  output$FCE_HIST <- renderPlotly({
    runtime <- input$FCE_HIST_RUNTIME %>% as.integer %>% reverse_trans_runtime
    plot_mode <- input$FCE_illu_mode
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    nrows <- ceiling(n_algorithm / 2.) # keep to columns for the histograms
    
    if (plot_mode == 'overlay') {
      p <- plot_ly_default(#title = "Histogram of the runtime",
        x.title = "target values", y.title = "runs")
      
    } else if (plot_mode == 'subplot') {
      p <- lapply(seq(n_algorithm), function(x) {
        plot_ly_default(#title = "Histogram of the runtime",
          x.title = "target values", y.title = "runs")
      })
    }
    
    for (i in seq_along(data)) {
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.35)')
      
      df <- data[[i]]
      algId <- attr(df, 'algId')
      fce <- get_target_sample(df, runtime, format = 'long', maximization = maximization)
      # skip if all target samples are NA
      if (sum(!is.na(fce$`f(x)`)) < 2)
        next
      
      res <- hist(fce$`f(x)`, breaks = nclass.FD, plot = F)
      breaks <- res$breaks
      plot_data <- data.frame(x = res$mids, y = res$counts, width = breaks[2] - breaks[1],
                              text = paste0('<b>count</b>: ', res$counts, 
                                            '<br><b>breaks</b>: [', 
                                            breaks[-length(breaks)], ',', breaks[-1], ']')) 
      
      if (plot_mode == 'overlay') {
        p %<>%
          add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = algId, text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      } else if (plot_mode == 'subplot') {
        p[[i]] %<>% 
          add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = algId, text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      }
    }
    
    if (plot_mode == 'subplot') 
      p <- subplot(p, nrows = nrows, titleX = F, titleY = F, margin = 0.03)
    
    p
  })
  
  # Expected Target Value Convergence 
  output$FCE_PER_FUN <- renderPlotly({
    rt_min <- input$FCE_RT_MIN %>% as.integer %>% reverse_trans_runtime
    rt_max <- input$FCE_RT_MAX %>% as.integer %>% reverse_trans_runtime
    
    data <- FCE.DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(#title = "Expected Runtime Comparison",
      y.title = "best-so-far f(x)-value", x.title = "runtime")
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      algId <- attr(df, 'algId')
      
      fce <- df %>% 
        mutate(upper = FCE + sd, lower = FCE - sd) %>% 
        filter(runtime >= rt_min, runtime <= rt_max) %>% 
        limit.data.frame(n = 60)
      
      if (nrow(fce) == 0)
        next
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
      
      p %<>% 
        add_trace(data = fce, x = ~runtime, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0), 
                  showlegend = F, name = 'mean +/- sd') %>% 
        add_trace(x = ~runtime, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd')
      
      if (input$FCE_show.mean)
        p %<>% add_trace(data = fce, x = ~runtime, y = ~FCE, type = 'scatter', 
                         mode = 'lines+markers', name = sprintf("%s.mean", algId), 
                         marker = list(color = rgb_str), 
                         line = list(color = rgb_str))
      
      if (input$FCE_show.median)
        p %<>% add_trace(data = fce, x = ~runtime, y = ~median, type = 'scatter',
                         name = sprintf("%s.median", algId), mode = 'lines+markers', 
                         marker = list(color = rgb_str),
                         line = list(color = rgb_str, dash = 'dash'))
    }
    p %<>%
      layout(xaxis = list(type = switch(input$FCE_semilogx, T = 'log', F = 'linear')),
             yaxis = list(type = switch(input$FCE_semilogy, T = 'log', F = 'linear')))
  })
  
  # The ECDF plots for the target value ----------------
  output$FCE_ECDF_PER_TARGET <- renderPlotly({
    runtimes <- c(
      as.integer(input$FCE_ECDF_RT1),
      as.integer(input$FCE_ECDF_RT2),
      as.integer(input$FCE_ECDF_RT3)) %>% 
      reverse_trans_runtime
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(title = NULL,
                         x.title = "target value",
                         y.title = "Proportion of runs")
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.35)')
      
      for (i in seq_along(runtimes)) {
        res <- get_target_sample(df, runtimes[i], format = 'long') 
        funvals <- sort(res$`f(x)`)
        
        if (all(is.na(funvals)))
          next
        
        tmp <- ecdf(funvals) 
        density <- tmp(funvals)
        
        # position of the markers
        x <- quantile(funvals, probs = c(0.25, 0.5, 0.75), names = F, type = 3)
        y <- sapply(x, function(xx) density[funvals == xx])
        
        p %<>%
          add_trace(data = res, x = funvals, y = density, type = 'scatter',
                    mode = 'lines', name = algId, showlegend = F,
                    legendgroup = paste0(k),
                    line = list(color = rgb_str, width = 3)) %>% 
          add_trace(data = res, x = x, y = y, type = 'scatter',
                    mode = 'markers',  legendgroup = paste0(k),
                    name = sprintf('%s, %.2e', algId, runtimes[i]),
                    marker = list(color = rgb_str, symbol = symbols[i], size = 13))
      }
    }
    
    p %<>%
      layout(xaxis = list(type = switch(input$FCE_ECDF_semilogx, 
                                        T = 'log', F = 'linear')))
  })
  
  output$FCE_RT_GRID <- renderPrint({
    data <- DATA()
    rt <- getRuntimes(data)
    
    rt_min <- input$FCE_ECDF_RT_MIN %>% as.integer
    rt_max <- input$FCE_ECDF_RT_MAX %>% as.integer
    rt_step <- input$FCE_ECDF_RT_STEP %>% as.integer
    
    # when initializing or incorrect input
    if (is.na(rt_min) || is.na(rt_max) || is.na(rt_step))
      return(data.frame())
    if (rt_min >= rt_max || rt_step > rt_max - rt_min)
      return(data.frame())
    
    rt_seq <- seq(from = rt_min, to = rt_max, by = rt_step) %>% 
      reverse_trans_runtime %>% 
      .[. >= min(rt)] %>% .[. <= max(rt)] %>% 
      cat
  })
  
  output$FCE_ECDF_AGGR <- renderPlotly({
    data <- DATA()
    rt <- getRuntimes(data)
    
    rt_min <- input$FCE_ECDF_RT_MIN %>% as.integer
    rt_max <- input$FCE_ECDF_RT_MAX %>% as.integer
    rt_step <- input$FCE_ECDF_RT_STEP %>% as.integer
    
    # when initializing or incorrect input
    if (is.na(rt_min) || is.na(rt_max) || is.na(rt_step))
      return(data.frame())
    if (rt_min >= rt_max || rt_step > rt_max - rt_min)
      return(data.frame())
    
    rt_seq <- seq(from = rt_min, to = rt_max, by = rt_step) %>% 
      reverse_trans_runtime %>% 
      .[. >= min(rt)] %>% .[. <= max(rt)]
    
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    funevals.max <- sapply(data, function(d) max(d$by_runtime, na.rm = T)) %>% max
    funevals.min <- sapply(data, function(d) min(d$by_runtime, na.rm = T)) %>% min
    
    x <- seq(funevals.min, funevals.max, length.out = 40)
    p <- plot_ly_default(#title = "Empirical Cumulative Distribution of the runtime",
      x.title = "target value",
      y.title = "Proportion of (run, budget) pairs")
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.15)')
      rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.8)')
      
      fun <- get_target_sample(df, rt_seq, format = 'long',  maximization = maximization)$`f(x)` %>% 
        ecdf
      m <- fun(x)
      # m <- lapply(rt_seq, function(r) {
      #   ce <- get_target_sample(df, r, format = 'long',  maximization = maximization) %>% '$'(`f(x)`)
      #   if (all(is.na(ce)))
      #     return(rep(0, length(x)))
      #   fun <- ecdf(ce)
      #   fun(x)
      # }) %>% 
        # do.call(rbind, .)
      
      df_plot <- data.frame(x = x, mean = m)
      # df_plot <- data.frame(x = x, 
      #                       mean = apply(m, 2, . %>% mean(na.rm = T)),
      #                       sd = apply(m, 2, . %>% sd(na.rm = T))) %>% 
      #   mutate(upper = mean + sd, lower = mean - sd)
      
      p %<>%
        # add_trace(data = df_plot, x = ~x, y = ~upper, type = 'scatter', mode = 'lines',
        #           line = list(color = rgba_str, width = 0), 
        #           showlegend = F, name = 'mean +/- sd') %>% 
        # add_trace(x = ~x, y = ~lower, type = 'scatter', mode = 'lines',
        #           fill = 'tonexty',  line = list(color = 'transparent'),
        #           fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd') %>% 
        add_trace(data = df_plot, x = ~x, y = ~mean, type = 'scatter',
                  mode = 'lines+markers', name = sprintf('%s', algId), 
                  showlegend = T, legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 4.5),
                  marker = list(color = rgb_str, size = 11))
      
      if (input$FCE_ECDF_per_target) {
        for (r in rt_seq) {
          ce <- get_target_sample(df, r, format = 'long') %>% '$'(`f(x)`) %>% sort
          if (all(is.na(ce)))
            next
          else {
            fun <- ecdf(ce)
            v <- fun(ce)
          }
          
          p %<>%
            add_trace(x = ce, y = v, type = 'scatter',
                      mode = 'lines', name = algId, showlegend = F,
                      line = list(color = rgba_str2, width = 1))
        }
      }
    }
    
    p %<>%
      layout(xaxis = list(type = switch(input$FCE_ECDF_AGGR_semilogx, 
                                        T = 'log', F = 'linear')))
  })
  
  plot.DATA <- reactive({
    data <- DATA()
    for (k in seq_along(data)) {
      data[[k]]$by_runtime <- limit.data.frame(data[[k]]$by_runtime, n = 100)
    }
    data
  })
  
  # evaluation rake of all courses 
  output$FCE_AUC <- renderPlotly({
    data <- plot.DATA()
    rt <- getRuntimes(data)

    rt_min <- input$FCE_AUC_RT_MIN %>% as.integer
    rt_max <- input$FCE_AUC_RT_MAX %>% as.integer
    rt_step <- input$FCE_AUC_RT_STEP %>% as.integer

    # when initializing or incorrect input
    if (is.na(rt_min) || is.na(rt_max) || is.na(rt_step))
      return(data.frame())
    if (rt_min >= rt_max || rt_step > rt_max - rt_min)
      return(data.frame())

    rt_seq <- seq(from = rt_min, to = rt_max, by = rt_step) %>%
      reverse_trans_runtime %>%
      .[. >= min(rt)] %>% .[. <= max(rt)]

    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)

    funevals.max <- sapply(data, function(d) max(d$by_runtime, na.rm = T)) %>% max
    p <- plot_ly_default()

    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')

      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.2)')

      # calculate ECDFs on user specified targets
      funs <- lapply(rt_seq, function(r) {
        get_target_sample(df, r, format = 'long') %>%
          '$'(`f(x)`) %>% {
            if (all(is.na(.))) NULL
            else  {
              f <- ecdf(.)
              attr(f, 'min') <- min(.)
              f
            }
          }
      })

      auc <- sapply(funs,
                    function(fun) {
                      if (is.null(fun)) 0
                      else integrate(fun, lower = attr(fun, 'min') - 1, upper = funevals.max,
                                     subdivisions = 1e3) %>% {'$'(., 'value') / funevals.max}
                    })

      p %<>%
        add_trace(type = 'scatterpolar', r = auc,
                  theta = paste0('B:', as.integer(rt_seq)),
                  fill = 'toself', fillcolor = rgba_str,
                  marker = list(color = rgb_str), hoverinfo = 'text',
                  text = paste0('area: ', format(auc, digits = 2, nsmall = 2)),
                  name = algId)
    }

    p %<>%
      layout(polar = list(radialaxis = list(visible = T)),
             yaxis = list(type = 'log'),
             autosize = T, hovermode = 'compare',
             paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)')
  })
  
  # Expected Evolution of parameters in the algorithm
  output$PAR_PER_FUN <- renderPlotly({
    f_min <- input$PAR_F_MIN %>% as.numeric %>% reverse_trans_funeval
    f_max <- input$PAR_F_MAX %>% as.numeric %>% reverse_trans_funeval
    
    data <- EPAR.DATA()
    req(any(!is.null(data)))
    
    n_algorithm <- length(data)
    n_param <- sapply(data, function(d) length(unique(d$par))) %>% max
    colors <- colorspace::rainbow_hcl(n_algorithm * n_param)
    
    p <- plot_ly_default(x.title = "best-so-far f(x) value", y.title = "parameters")
    i <- 1
    
    for (df in data) {
      algId <- attr(df, 'algId')
      if (input$PAR_ALGID_INPUT != 'all' && algId != input$PAR_ALGID_INPUT)
        next
      
      ddf <- df %>% 
        mutate(upper = mean + sd, lower = mean - sd) %>% 
        filter(BestF >= f_min, BestF <= f_max)
      
      par_name <- unique(df$par)
      for (name in par_name) {
        dddf <- filter(ddf, par == name)
        req(nrow(dddf) > 0)
        
        # p %<>% 
        #   add_trace(data = dddf, x = ~runtime, y = ~upper, type = 'scatter', mode = 'lines',
        #             line = list(color = rgba_str, width = 0), 
        #             showlegend = F, name = paste0(name, '.mean +/- sd')) %>% 
        #   add_trace(x = ~runtime, y = ~lower, type = 'scatter', mode = 'lines',
        #             fill = 'tonexty',  line = list(color = 'transparent'),
        #             fillcolor = rgba_str, showlegend = T, paste0(name, '.mean +/- sd')) 
        
        rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
        rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
        
        if (input$PAR_show.mean == 'mean')
          p %<>% add_trace(data = dddf, x = ~BestF, y = ~mean, type = 'scatter', 
                           mode = 'lines+markers', name = paste0(algId, '.', name, '.mean'), 
                           marker = list(color = rgb_str),
                           line = list(color = rgb_str))
        
        else if (input$PAR_show.mean == 'median')
          p %<>% add_trace(data = dddf, x = ~BestF, y = ~median, type = 'scatter',
                           name = paste0(algId, '.', name, '.mean'), mode = 'lines+markers', 
                           marker = list(color = rgb_str),
                           line = list(color = rgb_str, dash = 'dash'))
        i <- i + 1
      }
    }
    p %<>%
      layout(xaxis = list(type = switch(input$PAR_semilogx, T = 'log', F = 'linear')),
             yaxis = list(type = switch(input$PAR_semilogy, T = 'log', F = 'linear')))
  })
  
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
  
  get_PAR_summary <- reactive({
    data <- DATA()
    fall <- getFunvals(data)
    
    fstart <- format_funeval(input$PAR_F_MIN_SUMMARY) %>% as.numeric
    fstop <- format_funeval(input$PAR_F_MAX_SUMMARY) %>% as.numeric
    fstep <- format_funeval(input$PAR_F_STEP_SUMMARY) %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
      c(fstart, ., fstop) %>% unique %>% 
      reverse_trans_funeval %>% 
      .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)] 
    
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    res <- list()
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      
      if (input$PAR_ALGID_INPUT_SUMMARY != 'all' 
          && attr(df, 'algId') != input$PAR_ALGID_INPUT_SUMMARY)
        next
      
      res[[i]] <- summarise_par(df, fseq, probs = probs, maximization = maximization)
      if (input$PAR_INPUT != 'all')
        res[[i]] %<>% filter(parId == input$PAR_INPUT)
    }
    do.call(rbind.data.frame, res)
  })
  
  output$table_PAR_summary <- renderTable({
    df <- get_PAR_summary()
    df$runs %<>% as.integer
    df$mean %<>% format(digits = 2, nsmall = 2)
    df$median %<>% format(digits = 2, nsmall = 2)
    
    # TODO: make probs as a global option
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    
    # format the integers
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% format(digits = 2, nsmall = 2)
    }
    df
  })
  
  output$PAR_downloadData <- downloadHandler(
    filename = {
      data <- DATA()
      fstart <- format_funeval(input$PAR_F_MIN_SUMMARY)
      fstop <- format_funeval(input$PAR_F_MAX_SUMMARY)
      fstep <- format_funeval(input$PAR_F_STEP_SUMMARY)
      sprintf('parameter_summary_(%s,%s,%s).csv',
              fstart, fstop, fstep)
    },
    content = function(file) {
      write.csv(get_PAR_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
   
  # get_RT <- reactive({
  #   data <- DATA()
  #   fall <- getFunvals(data)
  #   
  #   fstart <- format_funeval(input$F_MIN_SAMPLE) %>% as.numeric
  #   fstop <- format_funeval(input$F_MAX_SAMPLE) %>% as.numeric
  #   fstep <- format_funeval(input$F_STEP_SAMPLE) %>% as.numeric
  #   
  #   # when initializing or incorrect input
  #   if (is.na(fstart) || is.na(fstop) || is.na(fstep))
  #     return(data.frame())
  #   if (fstart >= fstop || fstep > fstop - fstart)
  #     return(data.frame())
  #   
  #   fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
  #     c(fstart, ., fstop) %>% unique %>% 
  #     reverse_trans_funeval %>% 
  #     .[. >= (min(fall) - 0.1)] %>% .[. <= (max(fall) + 0.1)] 
  #   
  #   data <- DATA()
  #   res <- list()
  #   n_runs_max <- sapply(data, function(x) ncol(x$by_target)) %>% max
  #   
  #   for (i in seq_along(data)) {
  #     df <- data[[i]]
  #     
  #     if (input$ALGID_RAW_INPUT != 'all' && attr(df, 'algId') != input$ALGID_RAW_INPUT)
  #       next
  #     
  #     rt <- get_runtime_sample(df, fseq, format = input$RT_download_format, 
  #                              maximization = maximization)
  #     if (input$RT_download_format == 'wide') {
  #       n <- ncol(rt) - 2
  #       if (n < n_runs_max) 
  #         rt %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
  #     }
  #     res[[i]] <- rt
  #   }
  #   do.call(rbind.data.frame, res) 
  # })
  # 
  # output$download_runtime <- downloadHandler(
  #   filename = {
  #     data <- DATA()
  #     algId <- paste0(getAlgId(data), collapse = ';')
  #     fstart <- input$F_MIN_SAMPLE %>% format_funeval
  #     fstop <- input$F_MAX_SAMPLE %>% format_funeval
  #     fstep <- input$F_STEP_SAMPLE %>% format_funeval
  #     sprintf('runtime_(%s,%s,%s).csv', fstart, fstop, fstep)
  #   },
  #   content = function(file) {
  #     write.csv(get_RT(), file, row.names = F)
  #   },
  #   contentType = "text/csv"
  # )
})
