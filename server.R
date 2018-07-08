#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com

library(shiny)
library(shinyjs)
library(shinyFiles)

library(reshape2)
library(magrittr)
library(dplyr)

library(ggplot2)
library(plotly)

source('pproc.R')
source('plot.R')

options(width = 80)
options(shiny.maxRequestSize = 100 * 1024 ^ 2) 

symbols <- c("circle-open", "diamond-open", "square-open", "cross-open",
             "triangle-up-open", "triangle-down-open")

# transformations applied on the function value
trans_funeval <- `log10`
reverse_trans_funeval <- function(x) 10 ^ x

# transformations applied on runtime values
trans_runtime <- . %>% return
reverse_trans_runtime <- . %>% return

# extraction directory
exdir <- './data'

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # clean up the temporary files on server when exiting  
  session$onSessionEnded(function() {
    unlink(exdir, recursive = T)
  })
  
  # variables shared
  folderList <- reactiveValues(data = list())
  DataList <- reactiveValues(data = list())
  
  # IMPORTANT: this only works locally, keep it for the local version
  # links to users file systems
  # volumes <- getVolumes()
  # shinyDirChoose(input, 'directory', roots = volumes, session = session)
  
  # browse data directory, upload data and process data -------
  
  # the directory selected by the user
  # selected_folder <- reactive({
  #   # a <- list(root = "Macintosh HD", path = list('', "Users", "wanghao", "(1+1)-Cholesky-CMA"))
  #   # parseDirPath(volumes, a)
  #   parseDirPath(volumes, input$directory)
  # })
  
  # the folder where the uploaded zip file is uncompressed
  selected_folder <- reactive({
    if (!is.null(input$ZIP)) {
      unzip(input$ZIP$datapath, list = FALSE, exdir = exdir)
      name <- strsplit(input$ZIP$name, '\\.')[[1]][1]
      
      return(file.path(exdir, name))
    } else
      
      return(NULL)
  })
  
  # print the folderList
  output$upload_data_promt <- renderPrint({
    folders <- folderList$data
    for (i in seq_along(folders)) {
      cat(sprintf('%d: %s\n', i, folders[[i]]))
    }
  })
  
  # load, process the data and update DataSetList
  observeEvent(selected_folder(), {
    folder <- selected_folder()
    if (length(folder) != 0 && !folder %in% folderList$data) {
      indexFiles <- scan_indexFile(folder)
      if (length(indexFiles) == 0) 
        shinyjs::html("process_data_promt", 
                      paste0('No index file (.info) is found in folder ', 
                             folder, '... skip\n'), add = TRUE)
      else {
        folderList$data <- c(folderList$data, folder)  
        print_fun <- function(s) shinyjs::html("process_data_promt", s, add = TRUE)
        DataList$data <- c(DataList$data, read_dir(folder, print_fun = print_fun))
        class(DataList$data) <- 'DataSetList'  # TODO: fix this urgly part
      }
    } 
  })
  
  # show the detailed information on DataSetList
  output$DATASETLIST_INFO <- renderDataTable({
    datasets <- DataList$data
    if (length(datasets) != 0)
      summary(datasets)
    else
      data.frame()
  }, options = list(pageLength = 10, 
                    scrollX = F, 
                    autoWidth = TRUE,
                    columnDefs = list(list(width = '20px', 
                                           targets = c(0, 1))
                                      )
                    )
  )
  
  # update the list of dimensionality, funcId and algId
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
    updateSelectInput(session, 'FCE_ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'FCE_ALGID_RAW_INPUT', choices = algId, selected = 'all')
  })
  
  # the filtered DataSets 
  DATA <- reactive({
    dim <- input$DIM_INPUT
    funcId <- input$FUNCID_INPUT
    
    if (length(DataList$data) == 0)
      return(NULL)
    
    filter(DataList$data, by = c(DIM = dim, funcId = funcId))
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
  
  # update the values for the grid of target values
  observe({
    data <- DATA()
    v <- getFunvals(data)
    
    if (length(v) == 0)
      return()
    
    v <- trans_funeval(v)    # generate the grid in log10 scale!
    q <- quantile(v, probs = c(.25, .5, .75), names = F)
    
    grid <- seq(min(v), max(v), length.out = 15)
    # step <- ceiling((max(v) - min(v)) / 10)
    step <- grid[2] - grid[1]
    
    updateTextInput(session, 'fstart', value = format(min(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'fstop', value = format(max(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'fstep', value = format(step, digits = 5, nsmall = 2))
    
    updateTextInput(session, 'fstart_raw', value = format(min(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'fstop_raw', value = format(max(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'fstep_raw', value = format(step, digits = 5, nsmall = 2))
    
    updateTextInput(session, 'RT_fstart', value = format(min(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'RT_fstop', value = format(max(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'RT_fstep', value = format(step, digits = 5, nsmall = 2))
    updateTextInput(session, 'RT_fselect', value = format(median(v), digits = 5, nsmall = 2))
    
    updateTextInput(session, 'RT_PMF_FTARGET', value = format(median(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'RT_PMF_HIST_FTARGET', value = format(median(v), digits = 5, nsmall = 2))
    
    s <- ((max(v) - min(v)) * 0.1 + min(v)) %>% format(digits = 5, nsmall = 2)
    e <- ((max(v) - min(v)) * 0.9 + min(v)) %>% format(digits = 5, nsmall = 2)
    updateTextInput(session, 'ERT_FSTART', value = s)
    updateTextInput(session, 'ERT_FSTOP', value = e)
    
    updateTextInput(session, 'RT_ECDF_FTARGET1', value = format(q[1], digits = 5, nsmall = 2))
    updateTextInput(session, 'RT_ECDF_FTARGET2', value = format(q[2], digits = 5, nsmall = 2))
    updateTextInput(session, 'RT_ECDF_FTARGET3', value = format(q[3], digits = 5, nsmall = 2))
    
    updateTextInput(session, 'RT_AUC_FSTART', value = format(min(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'RT_AUC_FSTOP', value = format(max(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'RT_AUC_FSTEP', value = format(step, digits = 5, nsmall = 2))
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
    
    s <- ((max(v) - min(v)) * 0.1 + min(v)) %>% as.integer
    e <- ((max(v) - min(v)) * 0.9 + min(v)) %>% as.integer
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
    
    fstart <- input$fstart %>% as.numeric
    fstop <- input$fstop %>% as.numeric
    fstep <- input$fstep %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
      reverse_trans_funeval %>% 
      .[. >= min(fall)] %>% .[. <= max(fall)] 
    
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    data <- DATA()
    res <- list()
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      if (input$ALGID_INPUT != 'all' && attr(df, 'algId') != input$ALGID_INPUT)
        next
      
      res[[i]] <- RT_summary(df, fseq, probs = probs)
    }
    do.call(rbind.data.frame, res)
  })
  
  output$table_RT_summary <- renderTable({
    df <- get_RT_summary()
    df$runs %<>% as.integer
    df$median %<>% as.integer
    df['f(x)'] <- format(df['f(x)'], format = 'e', digits = 2)
    
    # TODO: make probs as a global option
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    
    # format the integers
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% as.integer
    }
    df
  })
  
  output$downloadData <- downloadHandler(
    filename = 'runtime_summary.csv',
    content = function(file) {
      write.csv(get_RT_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  get_RT <- reactive({
    data <- DATA()
    fall <- getFunvals(data)
    
    fstart <- input$fstart_raw %>% as.numeric
    fstop <- input$fstop_raw %>% as.numeric
    fstep <- input$fstep_raw %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
      reverse_trans_funeval %>% 
      .[. >= min(fall)] %>% .[. <= max(fall)] 
    
    data <- DATA()
    res <- list()
    n_runs_max <- sapply(data, function(x) ncol(x$by_target)) %>% max
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      
      if (input$ALGID_RAW_INPUT != 'all' && attr(df, 'algId') != input$ALGID_RAW_INPUT)
        next
      
      rt <- RT(df, fseq, format = input$RT_download_format)
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
    filename = 'runtime_samples.csv',
    content = function(file) {
      write.csv(get_RT(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  output$table_RT_sample <- renderDataTable({
    df <- get_RT()
    df[is.na(df)] <- 'NA'
    df}, options = list(pageLength = 20, scrollX = T)
  )
  
  # The expected runtime plot ---------------------
  output$ERT_PER_FUN <- renderPlotly({
    Fmin <- input$ERT_FSTART %>% as.numeric %>% reverse_trans_funeval
    Fmax <- input$ERT_FSTOP %>% as.numeric %>% reverse_trans_funeval
    
    data <- ERT.DATA()
    # df.BestF <- BestF()
    
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(#title = "Expected Runtime Comparison",
                         x.title = "best-so-far f(x)-value", y.title = "function evaluations")
    
    for (i in seq_along(data)) {
      # raw <- df.BestF[[i]] %>% 
        # filter(BestF >= xmin, BestF <= xmax)
      df <- data[[i]]
      algId <- attr(df, 'algId')
      ert <- df %>% 
        mutate(upper = ERT + sd, lower = ERT - sd) %>% 
        filter(BestF >= Fmin, BestF <= Fmax)
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
      
      p %<>% 
        add_trace(data = ert, x = ~BestF, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0), 
                  showlegend = F, name = 'mean +/- sd') %>% 
        add_trace(x = ~BestF, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd')
      
      if (input$show.mean)
        p %<>% add_trace(data = ert, x = ~BestF, y = ~ERT, type = 'scatter', 
                         mode = 'lines+markers', name = paste0(algId, '.mean'), 
                         marker = list(color = rgb_str),
                         line = list(color = rgb_str))
      
      if (input$show.median)
        p %<>% add_trace(data = ert, x = ~BestF, y = ~median, type = 'scatter',
                         name = paste0(algId, '.median'), mode = 'lines+markers', 
                         marker = list(color = rgb_str),
                         line = list(color = rgb_str, dash = 'dash'))
      
      # if (input$show.instance)
        # p %<>% add_trace(data = raw, x = ~BestF, y = ~eval, type = 'scatter', mode = 'lines',
                         # line = list(color = rgba_str), split = ~instance, showlegend = F)
    }
    p %<>%
      layout(xaxis = list(type = switch(input$semilogx, T = 'log', F = 'linear')),
             yaxis = list(type = switch(input$semilogy, T = 'log', F = 'linear')))
  })
  
  # empirical p.m.f. of the runtime
  output$RT_PMF <- renderPlotly({
    ftarget <- input$RT_PMF_FTARGET %>% as.numeric %>% reverse_trans_funeval
    points <- switch(input$RT_SHOW_SAMPLE, T = 'all', F = F)
    
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
        add_trace(data = RT(df, ftarget, format = 'long'),
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
    p
  })
  
  # historgram of the running time
  output$RT_HIST <- renderPlotly({
    ftarget <- input$RT_PMF_HIST_FTARGET %>% as.numeric %>% reverse_trans_funeval
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
      rt <- RT(df, ftarget, format = 'long')
      
      # skip if all runtime samples are NA
      if (all(is.na(rt$RT)))
        next
      
      res <- hist(rt$RT, plot = F)
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
      p <- subplot(p, nrows = nrows, titleX = T, titleY = T, margin = 0.04)
    
    p
  })
  
  # The ECDF plots for the runtime ----------------
  output$RT_ECDF <- renderPlotly({
    ftargets <- c(
      as.numeric(input$RT_ECDF_FTARGET1),
      as.numeric(input$RT_ECDF_FTARGET2),
      as.numeric(input$RT_ECDF_FTARGET3)) %>% 
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
        res <- RT(df, ftargets[i], format = 'long') 
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
                    name = sprintf('ECDF(%s, %.2e)', algId, ftargets[i]),
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
    
    fstart <- input$RT_fstart %>% as.numeric
    fstop <- input$RT_fstop %>% as.numeric 
    fstep <- input$RT_fstep %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    seq(from = fstart, to = fstop, by = fstep) %>% 
      reverse_trans_funeval %>% 
      .[. >= min(fall)] %>% .[. <= max(fall)] %>% 
      cat
  })
  
  output$RT_ECDF_AGGR <- renderPlotly({
    data <- DATA()
    fall <- getFunvals(data)
    
    fstart <- input$RT_fstart %>% as.numeric
    fstop <- input$RT_fstop %>% as.numeric 
    fstep <- input$RT_fstep %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
      reverse_trans_funeval %>% 
      .[. >= min(fall)] %>% .[. <= max(fall)] 
    
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
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.3)')
      rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.8)')
      
      m <- lapply(fseq, function(f) {
        rt <- RT(df, f, format = 'long') %>% '$'('RT')
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
        add_trace(data = df_plot, x = ~x, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0), 
                  showlegend = F, name = 'mean +/- sd') %>% 
        add_trace(x = ~x, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd') %>% 
        add_trace(x = ~x, y = ~mean, type = 'scatter',
                  mode = 'lines', name = sprintf('ECDF.mean(%s)', algId), 
                  showlegend = T, legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 4.5))
      
      if (input$RT_ECDF_per_target) {
        for (f in fseq) {
          rt <- RT(df, f, format = 'long') %>% '$'('RT') %>% sort
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
    
    fstart <- input$RT_AUC_FSTART %>% as.numeric
    fstop <- input$RT_AUC_FSTOP %>% as.numeric 
    fstep <- input$RT_AUC_FSTEP %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep) %>% 
      reverse_trans_funeval %>% 
      .[. >= min(fall)] %>% .[. <= max(fall)] 

    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    RT.max <- sapply(data, function(d) max(d$by_target, na.rm = T)) %>% max
    p <- plot_ly_default()
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.4)')
      
      # calculate ECDFs on user specified targets
      funs <- lapply(fseq, function(f) {
        RT(df, f, format = 'long') %>% 
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
                  theta = paste0('f:', format(fseq, digits = 2, nsmall = 2)), 
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
  
  # Data summary for Fixed-Budget target (FCE)  --------------
  get_FCE_summary <- reactive({
    data <- DATA()
    rt <- getRuntimes(data)
    
    rt_min <- input$RT_MIN %>% as.integer
    rt_max <- input$RT_MAX %>% as.integer
    rt_step <- input$RT_STEP %>% as.integer
    
    # when initializing or incorrect input
    if (is.na(rt_min) || is.na(rt_max) || is.na(rt_step))
      return(data.frame())
    if (rt_min >= rt_max || rt_step > rt_max - rt_min)
      return(data.frame())
    
    rt_seq <- seq(from = rt_min, to = rt_max, by = rt_step) %>% 
      reverse_trans_runtime %>% 
      .[. >= min(rt)] %>% .[. <= max(rt)] 
    
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    res <- list()
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      if (input$FCE_ALGID_INPUT != 'all' && attr(df, 'algId') != input$FCE_ALGID_INPUT)
        next
      
      res[[i]] <- FCE_summary(df, rt_seq, probs = probs)
    }
    do.call(rbind.data.frame, res)
  })
  
  output$FCE_SUMMARY <- renderTable({
    df <- get_FCE_summary()
    df$runs %<>% as.integer
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
  
  output$FCE_downloadData <- downloadHandler(
    filename = 'fix_budget_target_summary.csv',
    content = function(file) {
      write.csv(get_FCE_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  get_FCE <- reactive({
    data <- DATA()
    rt <- getRuntimes(data)
    
    rt_min <- input$RT_MIN %>% as.integer
    rt_max <- input$RT_MAX %>% as.integer
    rt_step <- input$RT_STEP %>% as.integer
    
    # when initializing or incorrect input
    if (is.na(rt_min) || is.na(rt_max) || is.na(rt_step))
      return(data.frame())
    if (rt_min >= rt_max || rt_step > rt_max - rt_min)
      return(data.frame())
    
    rt_seq <- seq(from = rt_min, to = rt_max, by = rt_step) %>% 
      reverse_trans_runtime %>% 
      .[. >= min(rt)] %>% .[. <= max(rt)] 
    
    res <- list()
    n_runs_max <- sapply(data, function(x) length(attr(x, 'instance'))) %>% max
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      
      if (input$FCE_ALGID_RAW_INPUT != 'all' 
          && attr(df, 'algId') != input$FCE_ALGID_RAW_INPUT)
        next
      
      rt <- FCE(df, rt_seq, format = input$download_format_FCE)
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
  
  output$download_FCE_SAMPLE <- downloadHandler(
    filename = 'fix_budget_target_sample.csv',
    content = function(file) {
      write.csv(get_FCE(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  output$FCE_SAMPLE <- renderDataTable({
    df <- get_FCE()
    df[is.na(df)] <- 'NA'
    df}, options = list(pageLength = 20, scrollX = T)
  )
  
  # empirical p.d.f. of the target value
  output$FCE_PDF <- renderPlotly({
    runtime <- input$FCE_PDF_RUNTIME %>% as.integer %>% reverse_trans_runtime
    points <- switch(input$FCE_SHOW_SAMPLE, T = 'all', F = F)
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(#title = "p.m.f. of the runtime",
      x.title = "algorithms",
      y.title = "Target value")
    
    for (i in seq_along(data)) {
      df <- data[[i]]
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.5)')
      
      p %<>%
        add_trace(data = FCE(df, runtime, format = 'long'),
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
      fce <- FCE(df, runtime, format = 'long')
      
      # skip if all runtime samples are NA
      if (all(is.na(fce$`f(x)`)))
        next
      
      res <- hist(fce$`f(x)`, plot = F)
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
      p <- subplot(p, nrows = nrows, titleX = T, titleY = T, margin = 0.04)
    
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
        filter(runtime >= rt_min, runtime <= rt_max)
      
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
                         mode = 'lines+markers', name = paste0(algId, '.mean'), 
                         marker = list(color = rgb_str),
                         line = list(color = rgb_str))
      
      if (input$FCE_show.median)
        p %<>% add_trace(data = fce, x = ~runtime, y = ~median, type = 'scatter',
                         name = paste0(algId, '.median'), mode = 'lines+markers', 
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
        res <- FCE(df, runtimes[i], format = 'long') 
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
                    name = sprintf('ECDF(%s, %.2e)', algId, runtimes[i]),
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
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    funevals.max <- sapply(data, function(d) max(d$by_runtime, na.rm = T)) %>% max
    funevals.min <- sapply(data, function(d) min(d$by_runtime, na.rm = T)) %>% min
    
    x <- seq(funevals.min, funevals.max, length.out = 50)
    p <- plot_ly_default(#title = "Empirical Cumulative Distribution of the runtime",
      x.title = "target value",
      y.title = "Proportion of (run, target) pairs")
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.3)')
      rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.8)')
      
      m <- lapply(rt_seq, function(r) {
        ce <- FCE(df, r, format = 'long') %>% '$'(`f(x)`)
        if (all(is.na(ce)))
          return(rep(0, length(x)))
        fun <- ecdf(ce)
        fun(x)
      }) %>% 
        do.call(rbind, .)
      
      df_plot <- data.frame(x = x, 
                            mean = apply(m, 2, . %>% mean(na.rm = T)),
                            sd = apply(m, 2, . %>% sd(na.rm = T))) %>% 
        mutate(upper = mean + sd, lower = mean - sd)
      
      p %<>%
        add_trace(data = df_plot, x = ~x, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0), 
                  showlegend = F, name = 'mean +/- sd') %>% 
        add_trace(x = ~x, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd') %>% 
        add_trace(x = ~x, y = ~mean, type = 'scatter',
                  mode = 'lines', name = sprintf('ECDF.mean(%s)', algId), 
                  showlegend = T, legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 4.5))
      
      if (input$FCE_ECDF_per_target) {
        for (r in rt_seq) {
          ce <- FCE(df, r, format = 'long') %>% '$'(`f(x)`) %>% sort
          if (all(is.na(ce)))
            next
          else {
            fun <- ecdf(ce)
            v <- fun(ce)
          }
          
          p %<>%
            add_trace(x = ce, y = v, type = 'scatter',
                      mode = 'lines', name = algId, showlegend = F,
                      line = list(color = rgba_str2, width = 1, dash = 'dot'))
        }
      }
    }
    
    p %<>%
      layout(xaxis = list(type = switch(input$FCE_ECDF_AGGR_semilogx, 
                                        T = 'log', F = 'linear')))
  })
  
  # evaluation rake of all courses 
  output$FCE_AUC <- renderPlotly({
    data <- DATA()
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
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.4)')
      
      # calculate ECDFs on user specified targets
      funs <- lapply(rt_seq, function(r) {
        FCE(df, r, format = 'long') %>% 
          '$'(`f(x)`) %>% {
            if (all(is.na(.))) NULL
            else  RT.ECDF(.)
          }
      })
      
      auc <- sapply(funs,
                    function(fun) {
                      if (is.null(fun)) 0
                      else integrate(fun, lower = attr(fun, 'min') - 1, upper = funevals.max, 
                                     subdivisions = 5e3) %>% {'$'(., 'value') / funevals.max}
                    })
      
      p %<>% 
        add_trace(type = 'scatterpolar', r = auc, 
                  theta = paste0('f:', format(rt_seq, digits = 2, nsmall = 2)), 
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
  
})
