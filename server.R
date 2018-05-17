#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(reshape2)
library(magrittr)
library(dplyr)
library(rCharts)
# library(rPython)

t <- theme_grey() +
  theme(text = element_text(size = 20)
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

gg_beanplot <- function(mapping, data, width = 3, fill = 'grey', colour = 'grey', 
                        alpha = 1, kernel = 'gaussian', bw = 'SJ', 
                        draw_quantiles = NULL, trim = TRUE, na.rm = FALSE, 
                        show.legend = NA) {
  
  x <- as.character(mapping$x)
  y <- as.character(mapping$y)
  df <- data[, c(x, y)] %>% rename_(.dots = c('x' = x, 'y' = y))
  
  if (!is.numeric(df$x)) 
    df$x <- tryCatch(as.numeric(df$x), 
                     warning = function(w) return(match(x, as.factor(x))))

  df %>% group_by(x, y) 
   
  # each segment 
  ggplot() + 
    geom_violin(data = data, mapping = mapping, fill = fill, trim = trim, 
                draw_quantiles = draw_quantiles, bw = bw, 
                colour = fill, kernel = kernel, scale = 'width',
                width = width, alpha = alpha) + 
    geom_segment(aes(x = x - width/2, xend = x + width/2, y = y, yend = y), 
                 df, col = 'black', size = 0.2, alpha = 0.3)
}

# read.data.python <- function(path) {
#   python.exec('import os')
#   python.exec("import sys; sys.path.insert(0, './')")
#   python.exec('from readalign import split')
#   python.exec("data = split(['./data/data_f2/dbopexp_f2_DIM1000_i1.dat'])")
#   data <- python.get('data')
#   
#   df <- data.frame()
#   ncol <- length(data[[1]][[1]])
#   for (i in seq(length(data))) {
#     tmp <- matrix(unlist(data[[i]]), ncol = ncol, byrow = T)[, 1:5]
#     tmp <- cbind(rep(i, nrow(tmp)), tmp) %>% as.data.frame
#     df <- rbind(df, tmp)
#   }
#   colnames(df) <- c('run', 'fct_eval', 'DeltaF', 'BestDeltaF', 'F', 'BestF')
#   df$run <- as.factor(df$run)
#   df
# }

Valign <- function(df) {
  fvalues <- df$BestF %>% unique %>% sort
  
  df.aligned <- data.frame()
  data <- tbl_df(df) %>% group_by(run)
  # TODO: handle the case where some runs failed
  for (f in fvalues) {
    data <- data %>% filter(BestF >= f)
    row <- data %>% slice(1) %>% ungroup %>% arrange(run) %>% '$'('fct_eval')
    df.aligned <- rbind(df.aligned, row)
  }
  rownames(df.aligned) <- fvalues
  colnames(df.aligned) <- seq(length(unique(df$run)))
  df.aligned
}

ECDF <- function(ftarget) {
  RT <- as.numeric(df.aligned[as.character(ftarget), ]) # running time
  ecdf(log10(RT / dim))
}


load('./data/tmp')

# df <- read.data.python()
# df.aligned <- Valign(df)

minF <- min(df$BestDeltaF)
maxF <- max(df$BestDeltaF)
dim <- 1000

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  ERT <- reactive({
    data.frame(BestF = as.numeric(rownames(df.aligned)),
               mean = rowMeans(df.aligned), 
               sd = apply(df.aligned, 1, sd))
  })
  
  fvalues <- reactive({
    df$BestF %>% unique %>% sort
  })
  
  observeEvent(fvalues, {
    v <- fvalues()
    step <- ceiling((max(v) - min(v)) / 20)
    updateSliderInput(session, 'target', min = min(v), max = max(v), 
                      value = quantile(v, names = F)[2], step = step)
  })
  
  output$convergence.raw <- renderPlot({
    p <- ggplot(df, aes(x = BestF, y = fct_eval, group = run, colour = run)) +
      geom_line(size = 1, alpha = 0.4, show.legend = F) + 
      stat_summary(fun.y = mean, geom = 'line', size = 1, 
                   aes(group = 1)) + 
      labs(x = 'best fitness measured', y = 'function evaluations') + 
      scale_color_discrete(guide = F)
      
    print(p)
  })
  
  # output$convergence.raw <- renderPlot({
  #   
  #   m1 = mPlot(x = 'date', y = c('psavert', 'uempmed'), type = 'Line',
  #              data = econ)
  #   m1$set(pointSize = 0, lineWidth = 1)
  #   m1$print(include_assets=T)
  #   
  # })
  
  output$mean.convergence <- renderPlot({
    data <- ERT() 
    target <- input$target
    
    row <- df.aligned[as.character(target), ] %>% 
      melt() %>% 
      mutate(variable = target)
    
    width <- (max(data$BestF) - min(data$BestF)) / 10
    p <- gg_beanplot(aes(x = variable, y = value), row, width = width, 
                     fill = '#F67A44', trim = F, colour = '#F67A44', alpha = 0.7) +
      geom_line(data = data, aes(x = BestF, y = mean),
                colour = 'black', size = 0.8, alpha = 0.8) + 
      geom_ribbon(aes(x = BestF, ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd),
                  data, fill = '#447DF6', alpha = 0.2) +
      labs(x = 'best fitness measured', y = 'function evaluations')

    print(p)
  })
  
  output$histogram <- renderChart2({
    f <- input$target
    row <- as.numeric(df.aligned[as.character(f), ])
    res <-  hist(row, plot = F)
    
    plot <- Highcharts$new()
    plot$chart(type = "column", height = 800, width = 600)
    plot$exporting(enable = TRUE)
    plot$xAxis(text = 'histogram',
               lineWidth = 2)
    plot$yAxis(tickmarkPlacement = 'on',
               lineWidth = 2, title = "count")
    plot$series(data = res$counts, fillOpacity = 0.4, color = '#FF5733')
    plot$plotOptions(series = list(groupPadding = 0,
                                   pointPadding = 0))
    # plot$tooltip(headerFormat = '<span style="font-size:10px">{point.key}</span><table>',
    #              pointFormat = paste0('<tr><td style="color:{series.color};',
    #                                   'padding:0">{series.name}: </td>', 
    #                                   '<td style="padding:0"><b>{point.y:.1f}</b></td></tr>'),
    #              footerFormat = '</table>',
    #              shared = T,
    #              useHTML = T)
    
    return(plot)
  })
  
  output$ecdf <- renderChart2({
    target <- input$target
  
    # RT <- as.numeric(df.aligned[as.character(target), ]) # running time
    RT.max <- log10(max(df.aligned) / dim)
    f.ecdf <- ECDF(target)
    
    n <- 100
    x <- seq(0, RT.max, length.out = n)
    v <- c(0.1, 0.3, 0.5, 0.7)
    q <- quantile(df$BestF, probs = v, names = F)
    
    f <- lapply(q, ECDF)
    
    plot.data <- c(list(x, f.ecdf(x)), lapply(f, function(f) f(x)))
    name <- c('your selection', paste0('ftarget:', q))
    names(plot.data) <- c('x', name)
    
    plot <- Highcharts$new()
    plot$chart(type = "area", height = 600, width = 800)
    plot$exporting(enable = TRUE)
    plot$xAxis(text = 'running time',
               lineWidth = 2)
    plot$yAxis(tickmarkPlacement = 'on',
               max = 1, min = 0,
               lineWidth = 2, title = "count")
    
    for (n in name) {
      d <- cbind(x, plot.data[[n]]) %>% 
        split(row(.)) %>% 
        'names<-'(NULL)
      if (n == 'your selection')
        plot$series(data = d, name = n, fillOpacity = 0.6, color = '#FF5733')
      else
        plot$series(data = d, name = n, fillOpacity = 0.2)
    }
    
    plot$plotOptions(area = list(
      pointStart = x[1],
      marker = list(
        enabled = F,
        symbol = 'circle',
        radius = 2
      )
    ))
    plot  
    
  })
})
