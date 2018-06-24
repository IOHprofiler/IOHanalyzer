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
library(rCharts)
library(plotly)

library(itertools)
library(iterators)

source('pproc.R')

options(width = 40)

t <- theme_grey() +
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5)
        # legend.position = c(0.15, 0.7),       # legend position
        # legend.key = element_blank(),         # no small box around symbol
        # legend.key.size = unit(1.3, "line"),  # bigger symbols
        # legend.background = element_rect(color = alpha("black", 0.5),
        #                                  fill = alpha('blue', 0.0),
        #                                  size = 1,
        #                                  linetype = "solid")
        )
theme_set(t)
# data_src <- './data/data_f1'

# setwd('~/code_base/post-processing/')

gg_beanplot <- function(mapping, data, p = NULL, width = 3, fill = 'grey', 
                        colour = 'grey', alpha = 1, kernel = 'gaussian', bw = 'SJ', 
                        draw_quantiles = NULL, trim = TRUE, na.rm = FALSE, 
                        show.legend = NA, point.shape = 20, show.sample = T,
                        show.violin = T, linetype = 'solid') {
  
  set.seed(42)
  x <- as.character(mapping$x)
  y <- as.character(mapping$y)
  df <- data[, c(x, y)] %>% rename_(.dots = c('x' = x, 'y' = y))
  
  if (!is.numeric(df$x)) 
    df$x <- tryCatch(as.numeric(df$x), # in case x is a factor...
                     warning = function(w) return(match(x, as.factor(x)))) 
  
  if (is.null(p))
    p <- ggplot()
  
  if (show.violin)
    p <- p + geom_violin(data = data, mapping = mapping, trim = trim, 
                         draw_quantiles = draw_quantiles, bw = bw, 
                         kernel = kernel, scale = 'width',
                         width = width, alpha = alpha)
  if (show.sample)
    p <- p + geom_jitter(data = df, aes(x, y), height = 0, width = width / 2, 
                         alpha = 0.45, shape = point.shape, size = 3.5)
    # geom_segment(aes(x = x - width / 2.2, xend = x + width / 2.2, y = y, yend = y),
    #              df, col = 'black', size = 0.2, alpha = 0.3, linetype = linetype)
  p
}

ECDF <- function(df, target) {
  v <- rownames(df) %>% as.numeric
  idx <- order(abs(target - v))[1]
  RT <- as.numeric(df[idx, ]) # running time
  ecdf(RT)
}

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
    # lapply(DF(), align.target)
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
  
  # the runtime aligned at a given target precision
  # RT <- reactive({
  #   target <- input$target %>% as.numeric
  #   lapply(aligned(), . %>% {
  #     v <- rownames(.) %>% as.numeric
  #     idx <- order(abs(target - v))[1]
  #     .[idx, ] %>% as.vector
  #     }) %>% 
  #     do.call(cbind.data.frame, .) %>% 
  #     set_colnames(c('algorithm1', 'algorithm2'))
  # })
  
  fvalues <- reactive({
    df.aligned <- aligned()
    lapply(df.aligned, . %>% rownames) %>% unlist %>%
      as.numeric %>% unique %>% sort %>% rev
  })
  
  # update the instance list 
  # observeEvent(DF, {
  #   df <- DF()
  #   instances <- unique(df$run)
  #   updateSelectInput(session = session, 'instance', choices = instances,
  #                     selected = instances[1])
  # })
  
  # update the values for the grid of target values
  observeEvent(fvalues, {
    v <- fvalues()
    step <- ceiling((max(v) - min(v)) / 10)
    updateTextInput(session, 'fstart', value = format(min(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'fstop', value = format(max(v), digits = 5, nsmall = 2))
    updateTextInput(session, 'fstep', value = format(step, digits = 5, nsmall = 2))
    updateTextInput(session, 'fselect', value = format(median(v), digits = 5, nsmall = 2))
  })
  
  observeEvent(fvalues, {
    v <- fvalues() 
    step <- ceiling((max(v) - min(v)) / 20)
    updateSliderInput(session, 'target.plot', min = min(v), max = max(v),
                      value = median(v), step = step)
  })
  
  observeEvent(fvalues, {
    v <- fvalues() 
    step <- ceiling((max(v) - min(v)) / 20)
    probs <- c(0.1, 0.3, 0.5, 0.7, 0.9)  # pre-defined targets: using quantiles
    q <- quantile(v, probs = probs, names = F)
    
    for (i in seq(5)) {
      updateSliderInput(session, paste0('target.ecdf', i), 
                        min = min(v), max = max(v),
                        value = q[i], step = step)
    }
  })
  
  # update the range of the fixed-target plot
  observeEvent(fvalues, {
    f <- fvalues()
    fmin <- min(f) %>% round(digits = 2)
    fmax <- max(f) %>% round(digits = 2)
    s <- (fmax - fmin) * 0.1 + fmin %>% round(digits = 2)
    e <- (fmax - fmin) * 0.9 + fmin %>% round(digits = 2)
    updateSliderInput(session, 'plot.range', min = fmin, max = fmax, value = c(s, e))
  })
  
  observeEvent(fvalues, {
    v <- fvalues()
    updateSelectInput(session, 'target2', choices = v, selected = v[7])
    # step <- ceiling((max(v) - min(v)) / 20)
    # updateSliderInput(session, 'target2', min = min(v), max = max(v), 
    #                   value = quantile(v, names = F)[7], step = step)
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
    fstart <- input$fstart %>% as.numeric %>% max(., min(fall))
    fstop <- input$fstop %>% as.numeric %>% min(., max(fall))
    fstep <- input$fstep %>% as.numeric
    
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
      if (input$select.alg != 'all' && names(df.aligneds)[i] != input$select.alg)
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
  
  output$table_RT_sample <- renderPrint({
    fall <- fvalues()  # all the aligned target values
    fselect <- input$fselect %>% as.numeric 
    
    # when initializing or incorrect input
    if (is.na(fselect))
      return(data.frame())
    if (fselect < min(fall) || fselect > max(fall))
      return(data.frame())
    
    df.aligneds <- aligned()
    res <- ''
    
    for (i in seq_along(df.aligneds)) {
      df <- df.aligneds[[i]]
      v <- rownames(df) %>% as.numeric
      idx <- seq_along(v)[v >= fselect][1]
      
      tmp <- df[idx, ] %>% as.vector
      res <- paste0(res, 'algorithm', i, ':', '\n')
      res <- paste0(res, paste0(tmp, collapse = ','), '\n\n')
    }
    cat(res, fill = T)
  })
  
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
  
  output$ks <- renderPrint({
    target <- input$target %>% as.numeric
    df.aligneds <- aligned()
    
    running_time <- list()
    for (i in seq_along(df.aligneds)) {
      df <- df.aligneds[[i]]
      v <- rownames(df) %>% as.numeric
      idx <- order(abs(target - v))[1]
      running_time[[i]] <- df[idx, ] %>% as.vector
    }
    algorithm1 <- running_time[[1]]
    algorithm2 <- running_time[[2]]
    a <- ks.test(algorithm1, algorithm2, alternative = 'less') 
    print(a)
  })
  
  # -----------------------------------------------------------
  # Plots for Fixed-Target Runtime (ERT)
  # -----------------------------------------------------------
  
  # The expected runtime plot 
  output$ERT_line <- renderPlotly({
    xmin <- input$plot.range[1]
    xmax <- input$plot.range[2]
    
    df.ERT <- ERT()
    df.BestF <- BestF()
    
    n_algorithm <- length(df.ERT)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    h <- 650
    w <- h / 9 * 16
    p <- plot_ly(width = w, height = h) 
    
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
                         line = list(color = rgba_str),
                         linetype = ~instance, showlegend = F)
    }
    
    xaxis.type <- switch(input$semilogx, T = 'log', F = 'linear')
    yaxis.type <- switch(input$semilogy, T = 'log', F = 'linear')
    
    p %<>% layout(title = "Expected Runtime Comparison",
                  autosize = T, hovermode = 'compare',
                  paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)',
                  xaxis = list(title = "best-so-far f(x)-value",
                               gridcolor = 'rgb(255,255,255)',
                               showgrid = TRUE,
                               showline = FALSE,
                               type = xaxis.type,
                               showticklabels = TRUE,
                               tickcolor = 'rgb(127,127,127)',
                               ticks = 'outside',
                               ticklen = 7,
                               zeroline = F),
                  yaxis = list(title = "function evaluations",
                               gridcolor = 'rgb(255,255,255)',
                               showgrid = TRUE,
                               showline = FALSE,
                               type = yaxis.type,
                               showticklabels = TRUE,
                               tickcolor = 'rgb(127,127,127)',
                               ticks = 'outside',
                               ticklen = 7,
                               zeroline = F))
  })
  
  output$ERT_violin <- renderPlotly({
    ftarget <- input$target.plot %>% as.numeric
    df.aligneds <- aligned()
    n_algorithm <- length(df.aligneds)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    h <- 650
    w <- h / 9 * 16
    p <- plot_ly(width = w, height = h)
                 
    for (i in seq_along(df.aligneds)) {
      df <- df.aligneds[[i]]
      # TODO: remove this, 'algorithm' attribute should be set when reading the data
      attr(df, 'algorithm') <- paste0('algorithm', i)
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
      
      rt <- RT(df, ftarget, format = 'long')
        
      # d <- density(rt, kernel = input$kernel)
      
      p %<>% 
        add_trace(data = rt,
                  x = ~algorithm, y = ~RT, split = ~algorithm, type = 'violin',
                  hoveron = "points+kde",
                  box = list(visible = T),
                  points = 'all',
                  pointpos = 1,
                  jitter = 0.1,
                  scalemode = 'count',
                  meanline = list(visible = T),
                  line = list(color = rgb_str, width = 1),
                  marker = list(color = rgb_str)
        )
      
      # p %<>% 
      #   add_trace(data = RT(df, ftarget, format = 'long'),
      #             x = ~algorithm, y = ~RT, split = ~algorithm, type = 'violin',
      #             hoveron = "points+kde",
      #             box = list(visible = T),
      #             points = 'all',
      #             pointpos = 1,
      #             jitter = 0.1,
      #             scalemode = 'count',
      #             meanline = list(visible = T),
      #             line = list(color = rgb_str, width = 1),
      #             marker = list(color = rgb_str)
      #           )
    }
    
    p %<>%
      layout(title = "Expected Runtime Comparison",
                  autosize = T, hovermode = 'compare',
                  paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)',
                  xaxis = list(title = "best-so-far f(x)-value",
                               gridcolor = 'rgb(255,255,255)',
                               showgrid = TRUE,
                               showline = FALSE,
                               showticklabels = TRUE,
                               tickcolor = 'rgb(127,127,127)',
                               ticks = 'outside',
                               ticklen = 7,
                               zeroline = F),
                  yaxis = list(title = "function evaluations",
                               gridcolor = 'rgb(255,255,255)',
                               showgrid = TRUE,
                               showline = FALSE,
                               showticklabels = TRUE,
                               tickcolor = 'rgb(127,127,127)',
                               ticks = 'outside',
                               ticklen = 7,
                               zeroline = F))
  })
    
  #   target <- input$target.plot %>% as.numeric
  #   df.aligneds <- aligned()
  #   
  #   p <- ggplot()
  #   colors <- 
  #   shapes <- c(20, 4)
  #   linetypes <- c('solid', 'longdash')
  #   
  #   data <- list()
  #   for (i in seq_along(df.aligneds)) {
  #     df.aligned <- df.aligneds[[i]]
  #     
  #     # select the aligned precison that is closest to the selected target
  #     v <- rownames(df.aligned) %>% as.numeric
  #     idx <- order(abs(target - v))[1]
  #     running_time <- df.aligned[idx, ] %>% as.vector
  #           
  #     data[[i]] <- data.frame(algorithm = paste0('algorithm', i), run.time = running_time)
  #     
  #     # p <- p + xlim(range = x_range) +
  #     #   labs(x = 'Best fitness', y = 'runtime / function evaluation',
  #     #        title = 'Runtime (RT) vs. target precison')
  #   }
  #   
  #   width <- 0.3
  #   data <- do.call(rbind.data.frame, data)
  #   colors <- colorspace::rainbow_hcl(2) %>% 
  #     setNames(unique(data$algorithm))
  #   
  #   p <- gg_beanplot(p = p, mapping = aes(x = algorithm, y = run.time, 
  #                                         fill = algorithm, colour = algorithm), 
  #                    data = data, width = width, trim = F, kernel = input$kernel,
  #                    show.sample = input$show.RT.sample, alpha = 0.8) + 
  #     scale_color_manual(values = colors) + 
  #     scale_fill_manual(values = colors) +
  #     labs(x = 'algorithms', y = 'running time')
  #   
  #   p
  # })
  
  # historgram of the running time
  output$histogram <- renderPlot({
    target <- input$target.plot %>% as.numeric
    df.aligneds <- aligned()
    
    data <- list()
    for (i in seq_along(df.aligneds)) {
      df.aligned <- df.aligneds[[i]]
      v <- rownames(df.aligned) %>% as.numeric
      idx <- order(abs(target - v))[1]
      
      row <- as.numeric(df.aligned[idx, ])
      res <- hist(row, plot = F)
      
      counts <- res$counts
      b <- res$breaks
      index <- seq(length(counts))
      mid_points <- sapply(index, function(i) (res$breaks[i + 1] + res$breaks[i]) / 2)
      
      data[[i]] <- data.frame(algorithm = paste0('algorithm', i),
                              width = (b[2] - b[1]) * 0.8,
                              counts = counts, x = mid_points)
    }
    
    data_plot <- do.call(rbind.data.frame, data)
    colors <- colorspace::rainbow_hcl(2) %>% 
      setNames(unique(data_plot$algorithm))
    
    ggplot(data = data_plot, aes(x, y = counts, colour = algorithm, fill = algorithm)) +
      geom_bar(stat = 'identity', mapping = aes(width = width)) + 
      facet_wrap( ~ algorithm, ncol = 2, scales = 'free_x') +
      scale_color_manual(values = colors) + 
      scale_fill_manual(values = colors) + 
      labs(x = 'algorithm')
  })
  
  output$ecdf <- renderChart2({
    targets <- c(
      as.numeric(input$target.ecdf1),
      as.numeric(input$target.ecdf2),
      as.numeric(input$target.ecdf3),
      as.numeric(input$target.ecdf4),
      as.numeric(input$target.ecdf5))
    
    dfs <- DF()
    df.aligneds <- aligned()
    colors <- colorspace::rainbow_hcl(2)
    line_style <- c('Solid',
                    'ShortDash',
                    'ShortDot',
                    'ShortDashDot',
                    'ShortDashDotDot')
    
    plot <- Highcharts$new()
    plot$chart(type = "area", height = 700, width = 700)
    plot$exporting(enable = TRUE)
    plot$title(text = 'Empirical Cumulative Distribution Functions (ECDFs)')
    plot$xAxis(title = list(text = 'log10(running time) / dim'), lineWidth = 2)
    plot$yAxis(tickmarkPlacement = 'on', max = 1.1, min = 0,
               lineWidth = 2, title = list(text = 'Proportion of instances'))
    
    for (k in seq_along(df.aligneds)) {
      df.aligned <- df.aligneds[[k]]
      df <- dfs[[k]]
      
      for (i in seq_along(targets)) {
        if (!input[[paste0('show.ecdf', i)]])
          next
        
        target <- targets[i]
        RT.max <- max(df.aligned, na.rm = T)
        f.ecdf <- ECDF(df.aligned, target)
        n <- 500
        x <- seq(0, RT.max, length.out = n)
        s <- cbind(x, f.ecdf(x)) %>% 
          split(row(.)) %>% 
          'names<-'(NULL)
        
        plot$series(data = s, name = paste0('algorithm', k, ':', 'target',
                                            format(target, digits = 5, nsmall = 2)), 
                    fillOpacity = 0, 
                    dashStyle = line_style[i],
                    color = colors[k])
      }
    }
    
    # plot$plotOptions(area = list(pointStart = x[1],
    #                              marker = list(enabled = F,
    #                                            symbol = 'circle',
    #                                            radius = 2)
    #                              )
    #                  )
    plot
  })
  
  # evaluation rake of all courses 
  output$auc.radar <- renderChart2({
    targets <- c(
      as.numeric(input$target.ecdf1),
      as.numeric(input$target.ecdf2),
      as.numeric(input$target.ecdf3), 
      as.numeric(input$target.ecdf4), 
      as.numeric(input$target.ecdf5))
    
    dfs <- DF()
    df.aligneds <- aligned()
    colors <- colorspace::rainbow_hcl(2)
    
    plot <- Highcharts$new()
    plot$chart(polar = TRUE, type = "area", width = 500, height = 500, marginLeft = 0)
    plot$exporting(enable = TRUE)
    # plot$title(text = 'Area under the ECDF curves')
    
    series <- list()
    for (k in seq_along(df.aligneds)) {
      df.aligned <- df.aligneds[[k]]
      df <- dfs[[k]]
      RT.max <- max(df.aligned, na.rm = T)
      
      # user selected target
      f.ecdfs <- lapply(targets, . %>% ECDF(df.aligned, target = .))
      auc <- sapply(f.ecdfs, 
                    function(f) {
                      res <- integrate(f, lower = 0, upper = RT.max, 
                                       subdivisions = 3e3)
                      res$value
                    })
      
      series[[k]] <- list(data = auc, name = paste0('algorithm', k),
                          fillOpacity = 0.3, color = colors[k])
    }
    auc <- sapply(series, function(s) s$data) %>% as.vector
    categories <- targets %>% format(digits = 5, nsmall = 2)
    plot$series(series)
    plot$xAxis(categories = categories,
               tickmarkPlacement = 'on', lineWidth = 2)
    plot$yAxis(gridLineInterpolation = 'circle', lineWidth = 1, min = min(auc),
               max = max(auc), endOnTick = F, tickInterval = 0.2)
    plot$tooltip(formatter = "#!function () {
                 return '<b>' + this.series.name  + '</b><br/>' +
                 '<b>' + 'area: ' + '</b>' + Highcharts.numberFormat(this.point.y, 3)}!#",
                 useHTML = T)
    return(plot)
  })
  
})
