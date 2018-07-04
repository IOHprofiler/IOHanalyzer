#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com

library(shiny)

library(reshape2)
library(magrittr)
library(dplyr)

library(ggplot2)
library(plotly)

library(itertools)
library(iterators)

source('pproc.R')
source('plot.R')

options(width = 80)
symbols <- c("circle-open", "diamond-open", "square-open", "cross-open",
             "triangle-up-open", "triangle-down-open")

# data_src <- './data/data_f1'
# setwd('~/code_base/post-processing/')
algorithm1 <- ''
algorithm2 <- ''

if (11 < 2) {
  path1 <- './rawdata/dbopexp_f1_DIM1000_i1.dat'
  path2 <- './rawdata/dbopexp_f1_DIM1500_i1.dat'
  
  df1 <- read.data.python(path1)
  df1.aligned <- align.target(df1)
  df2 <- read.data.python(path2)
  df2.aligned <- align.target(df2)
  
  df <- list('algorithm1' = df1, 'algorithm2' = df2)
  df.aligned <- list('algorithm1' = df1.aligned, 'algorithm2' = df2.aligned)
  
  save(df, df.aligned, file = './data/tmp')
} else {
  load('./data/tmp')
}

dim <- c(1000, 1500)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  DF <- reactive({
    df
  })
  
  BestF <- reactive({
    dfs <- DF()
    res <- list()
    for (i in seq_along(dfs)) {
      df <- dfs[[i]]
      res[[i]] <- lapply(seq_along(df), function(k) {
        df[[k]] %>% select(eval, BestF) %>% 
          mutate(instance = k)
      }) %>% 
        do.call(rbind.data.frame, .)
    }
    res
  })
  
  aligned <- reactive({
    df.aligned
  })
  
  ERT <- reactive({
    df.aligned <- aligned()
    
    res <- list()
    i <- 1
    for (data in df.aligned) {
      res[[i]] <- data.frame(BestF = as.numeric(rownames(data)),
                             n.sample = apply(data, 1, . %>% {sum(!is.na(.))}), 
                 mean = rowMeans(data, na.rm = T), 
                 sd = apply(data, 1, . %>% sd(na.rm = T)),
                 se = apply(data, 1, . %>% {sd(., na.rm = T) / sqrt(length(.))}),
                 median = apply(data, 1, . %>% median(na.rm = T)))
      i <- i + 1
    }
    res
  })
  
  fvalues <- reactive({
    df.aligned <- aligned()
    lapply(df.aligned, . %>% rownames) %>% unlist %>%
      as.numeric %>% unique %>% sort %>% rev
  })
  
  # update the values for the grid of target values
  observeEvent(fvalues, {
    v <- fvalues()
    
    q <- quantile(v, names = F)
    step <- ceiling((max(v) - min(v)) / 10)
    
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
  
  # -----------------------------------------------------------
  # Data summary for Fixed-Target Runtime (ERT)
  # -----------------------------------------------------------
  get_RT_summary <- reactive({
    fall <- fvalues()  # all the aligned target values
    fstart <- input$fstart %>% as.numeric %>% max(., min(fall))
    fstop <- input$fstop %>% as.numeric %>% min(., max(fall))
    fstep <- input$fstep %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep)
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    df.aligneds <- aligned()
    res <- list()
    
    for (i in seq_along(df.aligneds)) {
      if (input$select.alg != 'all' && names(df.aligneds)[i] != input$select.alg)
        next
      
      df <- df.aligneds[[i]]
      attr(df, 'algorithm') <- paste0('algorithm', i)
      res[[i]] <- RT_summary(df, fseq, probs = probs)
    }
    
    do.call(rbind.data.frame, res)
  })
  
  output$table_RT_summary <- renderTable({
    df <- get_RT_summary()
    df$runs %<>% as.integer
    df$median %<>% as.integer
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
    fall <- fvalues()  # all the aligned target values
    fstart <- input$fstart_raw %>% as.numeric %>% max(., min(fall))
    fstop <- input$fstop_raw %>% as.numeric %>% min(., max(fall))
    fstep <- input$fstep_raw %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep)
    n_runs_max <- sapply(df.aligned, . %>% ncol) %>% max
    df.aligneds <- aligned()
    res <- list()
    
    for (i in seq_along(df.aligneds)) {
      if (input$select.alg_raw != 'all' && names(df.aligneds)[i] != input$select.alg_raw)
        next
      
      df <- df.aligneds[[i]]
      attr(df, 'algorithm') <- paste0('algorithm', i)
      data <- RT(df, fseq, format = input$RT_download_format)
      
      if (input$RT_download_format == 'wide') {
        n <- ncol(data) - 2
        if (n < n_runs_max) 
          data %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
      }
      res[[i]] <- data
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
    df
    },
    options = list(pageLength = 20, scrollX = T)
  )
  
  output$download_RT_sample <- downloadHandler(
    filename = 'RT_sample.csv',
    content = function(file) {
      df.aligneds <- aligned()
      res <- list()
      for (i in seq_along(df.aligneds)) {
        df <- df.aligneds[[i]]
        attr(df, 'algorithm') <- paste0('algorithm', i)
        res[[i]]  <- RT(df, as.numeric(input$fselect), format = 'long')
      }
      data <- do.call(rbind.data.frame, res)
      write.csv(data, file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  # -----------------------------------------------------------
  # The expected runtime plot ---------------------
  output$ERT_PER_FUN <- renderPlotly({
    xmin <- input$ERT_FSTART %>% as.numeric
    xmax <- input$ERT_FSTOP %>% as.numeric
    
    df.ERT <- ERT()
    df.BestF <- BestF()
    
    n_algorithm <- length(df.ERT)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(#title = "Expected Runtime Comparison",
                         x.title = "best-so-far f(x)-value", y.title = "function evaluations")
    
    for (i in seq_along(df.ERT)) {
      raw <- df.BestF[[i]] %>% 
        filter(BestF >= xmin, BestF <= xmax)
      
      ert <- df.ERT[[i]] %>% 
        mutate(upper = mean + sd, lower = mean - sd) %>% 
        filter(BestF >= xmin, BestF <= xmax)
      
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
        p %<>% add_trace(data = ert, x = ~BestF, y = ~mean, type = 'scatter', 
                         mode = 'lines+markers', name = paste0('algorithm', i, '.mean'), 
                         marker = list(color = rgb_str),
                         line = list(color = rgb_str))
      
      if (input$show.median)
        p %<>% add_trace(data = ert, x = ~BestF, y = ~median, type = 'scatter',
                         name = paste0('algorithm', i, '.median'), mode = 'lines+markers', 
                         marker = list(color = rgb_str),
                         line = list(color = rgb_str, dash = 'dash'))
      
      if (input$show.instance)
        p %<>% add_trace(data = raw, x = ~BestF, y = ~eval, type = 'scatter', mode = 'lines',
                         line = list(color = rgba_str), split = ~instance, showlegend = F)
    }
    p %<>%
      layout(xaxis = list(type = switch(input$semilogx, T = 'log', F = 'linear')),
             yaxis = list(type = switch(input$semilogy, T = 'log', F = 'linear')))
  })
  
  # empirical p.m.f. of the runtime
  output$RT_PMF <- renderPlotly({
    ftarget <- input$RT_PMF_FTARGET %>% as.numeric
    points <- switch(input$RT_SHOW_SAMPLE, T = 'all', F = F)
    
    df.aligneds <- aligned()
    n_algorithm <- length(df.aligneds)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(#title = "p.m.f. of the runtime",
                         x.title = "algorithms",
                         y.title = "runtime / function evaluations")
  
    for (i in seq_along(df.aligneds)) {
      df <- df.aligneds[[i]]
      # TODO: remove this, 'algorithm' attribute should be set when reading the data
      attr(df, 'algorithm') <- paste0('algorithm', i)
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.5)')
      
      p %<>%
        add_trace(data = RT(df, ftarget, format = 'long'),
                  x = ~algorithm, y = ~RT, split = ~algorithm, type = 'violin',
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
    ftarget <- input$RT_PMF_HIST_FTARGET %>% as.numeric
    plot_mode <- input$ERT_illu_mode
    
    df.aligneds <- aligned()
    n_algorithm <- length(df.aligneds)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    nrows <- ceiling(n_algorithm / 2.) # keep to columns for the histograms
    
    if (plot_mode == 'overlay') {
      p <- plot_ly_default(#title = "Histogram of the runtime",
                           x.title = "function evaluations", y.title = "counts")
      
    } else if (plot_mode == 'subplot') {
      p <- lapply(seq(n_algorithm), function(x) {
        plot_ly_default(#title = "Histogram of the runtime",
                        x.title = "function evaluations", y.title = "counts")
      })
    }
     
    for (i in seq_along(df.aligneds)) {
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.35)')
      
      df <- df.aligneds[[i]]
      attr(df, 'algorithm') <- paste0('algorithm', i)
      rt <- RT(df, ftarget, format = 'long')
      
      # skip if all runtime samples are NA
      if (all(is.na(rt$RT)))
        next
      
      res <- hist(rt$RT, plot = F)
      breaks <- res$breaks
      data <- data.frame(x = res$mids, y = res$counts, width = breaks[2] - breaks[1],
                         text = paste0('<b>count</b>: ', res$counts, 
                                       '<br><b>breaks</b>: [', 
                                       breaks[-length(breaks)], ',', breaks[-1], ']')) 
      
      if (plot_mode == 'overlay') {
        p %<>%
          add_trace(data = data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = attr(df, 'algorithm'),
                    text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      } else if (plot_mode == 'subplot') {
        p[[i]] %<>% 
          add_trace(data = data, x = ~x, y = ~y, width = ~width, type = 'bar',
                     name = attr(df, 'algorithm'),
                     text = ~text, hoverinfo = 'text',
                     marker = list(color = rgba_str,
                                   line = list(color = 'rgb(8,48,107)', width = 1.5)))
          
      }
    }
    
    if (plot_mode == 'subplot') 
      p <- subplot(p, nrows = nrows, titleX = T, titleY = T, margin = 0.04)
    
    p
  })
  
  output$RT_ECDF <- renderPlotly({
    ftargets <- c(
      as.numeric(input$RT_ECDF_FTARGET1),
      as.numeric(input$RT_ECDF_FTARGET2),
      as.numeric(input$RT_ECDF_FTARGET3))
    
    df.aligneds <- aligned()
    n_algorithm <- length(df.aligneds)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(title = NULL,
      # title = "Empirical Cumulative Distribution of the runtime",
                         x.title = "function evaluations",
                         y.title = "Proportion of instance")
    
    for (k in seq_along(df.aligneds)) {
      df <- df.aligneds[[k]]
      attr(df, 'algorithm') <- paste0('algorithm', k)
      
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
                    mode = 'lines', name = attr(df, 'algorithm'), showlegend = F,
                    legendgroup = paste0(k),
                    line = list(color = rgb_str, width = 3)) %>% 
          add_trace(data = res, x = x, y = y, type = 'scatter',
                    mode = 'markers',  legendgroup = paste0(k),
                    name = sprintf('ECDF(%s, %.2f)', attr(df, 'algorithm'), ftargets[i]),
                    marker = list(color = rgb_str, symbol = symbols[i], size = 13))
      }
    }
    
    p %<>%
      layout(xaxis = list(type = switch(input$RT_ECDF_semilogx, 
                                        T = 'log', F = 'linear')))
  })
  
  output$RT_GRID <- renderPrint({
    fall <- fvalues()  # all the aligned target values
    fstart <- input$RT_fstart %>% as.numeric %>% max(., min(fall))
    fstop <- input$RT_fstop %>% as.numeric 
    fstep <- input$RT_fstep %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    cat(seq(from = fstart, to = fstop, by = fstep))
  })
  
  output$RT_ECDF_AGGR <- renderPlotly({
    fall <- fvalues()  # all the aligned target values
    fstart <- input$RT_fstart %>% as.numeric %>% max(., min(fall))
    fstop <- input$RT_fstop %>% as.numeric 
    fstep <- input$RT_fstep %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    fseq <- seq(from = fstart, to = fstop, by = fstep)
    df.aligneds <- aligned()
    n_algorithm <- length(df.aligneds)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    RT.max <- sapply(df.aligneds, . %>% max(na.rm = T)) %>% max
    RT.min <- sapply(df.aligneds, . %>% min(na.rm = T)) %>% min
    x <- seq(RT.min, RT.max, length.out = 50)
    p <- plot_ly_default(#title = "Empirical Cumulative Distribution of the runtime",
                         x.title = "function evaluations",
                         y.title = "Proportion of instance + target pairs")
    
    for (k in seq_along(df.aligneds)) {
      df <- df.aligneds[[k]]
      attr(df, 'algorithm') <- paste0('algorithm', k)
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.3)')
      
      m <- lapply(fseq, function(f) {
        rt <- RT(df, f, format = 'long') %>% '$'('RT')
        if (all(is.na(rt)))
          return(rep(0, length(x)))
        fun <- ecdf(rt)
        fun(x)
      }) %>% 
        do.call(rbind, .)
      
      df_plot <- data.frame(x = x, mean = apply(m, 2, . %>% mean(na.rm = T)),
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
                  mode = 'lines', name = sprintf('ECDF.mean(%s)', attr(df, 'algorithm')), 
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
                      mode = 'lines', name = attr(df, 'algorithm'), showlegend = F,
                      line = list(color = rgba_str, width = 1, dash = 'dash'))
        }
      }
    }
    
    p %<>%
      layout(xaxis = list(type = switch(input$RT_ECDF_AGGR_semilogx, 
                                        T = 'log', F = 'linear')))
  })
  
  # evaluation rake of all courses 
  output$RT_AUC <- renderPlotly({
    fall <- fvalues()  # all the aligned target values
    fstart <- input$RT_AUC_FSTART %>% as.numeric %>% max(., min(fall))
    fstop <- input$RT_AUC_FSTOP %>% as.numeric 
    fstep <- input$RT_AUC_FSTEP %>% as.numeric
    
    # when initializing or incorrect input
    if (is.na(fstart) || is.na(fstop) || is.na(fstep))
      return(data.frame())
    if (fstart >= fstop || fstep > fstop - fstart)
      return(data.frame())
    
    ftargets <- seq(from = fstart, to = fstop, by = fstep)

    df.aligneds <- aligned()
    n_algorithm <- length(df.aligneds)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    RT.max <- sapply(df.aligneds, . %>% max(na.rm = T)) %>% max
    p <- plot_ly_default()
    
    for (k in seq_along(df.aligneds)) {
      df <- df.aligneds[[k]]
      attr(df, 'algorithm') <- paste0('algorithm', k)
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.4)')
      
      # calculate ECDFs on user specified targets
      funs <- lapply(ftargets, function(f) {
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
                  theta = paste0('f:', ftargets), fill = 'toself', fillcolor = rgba_str,
                  marker = list(color = rgb_str), hoverinfo = 'text',
                  text = paste0('area: ', format(auc, digits = 2, nsmall = 2)),
                  name = attr(df, 'algorithm')) 
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
})
