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

# iteration tools
library(itertools)
library(iterators)

options(width = 160)

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

# read.data.python <- function(path) {
#   python.exec('import os')
#   python.exec("import sys; sys.path.insert(0, './')")
#   python.exec('from readalign import split, VMultiReader, alignData')
#   python.exec(sprintf("data = split(['%s'])", path))
#   python.exec("df = list(map(lambda d: d.tolist(), data))")
# 
#   data <- python.get('df')
#   ncol <- length(data[[1]][[1]])
#   data <- lapply(data, function(x) matrix(unlist(x), ncol = ncol, byrow = T)[, 1:5] %>%
#                 as.data.frame %>%
#                    set_colnames(c('eval', 'DeltaF', 'BestDeltaF', 'F', 'BestF')))
# 
#   data
# }

# align all instances at a given target/precision
# TODO: implement this part in C for speeding up
# TODO: add option allowing for the minimization scenario
target_column <- 5
align.target <- function(data, targets = 'auto', nrow = 20) {
  N <- length(data)
  data.iter <- lapply(data, function(d) ihasNext(iter(as.matrix(d), by = 'row'))) # iterator of data frames
  
  if (is.numeric(targets)) {
    Fvalues <- sort(targets)
  } else {
    if (targets == 'auto') { 
      # similar to the alignment in bbob
      # produce roughly 20 data records by default
      Fstart <- sapply(data, function(x) x[, target_column][1]) %>% max(na.rm = T)
      Fend <- sapply(data, function(x) rev(x[, target_column])[1]) %>% max(na.rm = T)
      tmp <- seq(log10(Fstart), log10(Fend), length.out = nrow)
      step <- tmp[2] - tmp[1]
      idx <- log10(Fstart)
      # idxCurrentF <- max(fvalues, na.rm = T) %>% log10 %>% '*'(nbPtsF) %>% ceiling
      # t <- 10. ^ (idxCurrentF / nbPtsF)
      t <- Fstart
      
    } else if (targets == 'full') {
      # align at every observed fitness value
      # this should give roughly the same time complexity as we have to iterate the whole data set
      Fvalues <- lapply(data, function(x) unique(x[, target_column])) %>% 
        unlist %>% unique %>% sort %>% rev
    }
  }
  
  if (targets == 'auto') {
    Fvalues <- c()
    res <- c()
    curr_eval <- rep(NA, N)
    curr_fvalues <- rep(NA, N)
    
    while (t > 0 && is.finite(t)) {
      curr_eval[1:N] <- NA
      curr_fvalues[1:N] <- NA
      
      for (k in seq_along(data.iter)) {
        d <- data.iter[[k]]
        while (hasNext(d)) {
          curr_line <- nextElem(d)
          v <- curr_line[target_column]
          if (v >= t) {
            curr_eval[k] <- curr_line[1] # hitting the target
            curr_fvalues[k] <- v
            break
          }
        }
      }
      
      if (all(is.na(curr_eval)) || all(is.na(curr_fvalues))) # if the current target is not hit by any instance
        break
      
      Fvalues <- c(Fvalues, t)
      res <- rbind(res, curr_eval)
      
      # calculate the new target values
      # tmp <- max(curr_fvalues, na.rm = T) %>% log10 %>% '*'(nbPtsF) %>% ceiling 
      # make sure the new target value is bigger than the next iterate
      idx <- idx + step
      t <- max(c(10. ^ idx, curr_fvalues), na.rm = T)
      # idxCurrentF <- max(tmp) %>% '-'(1)
      # idxCurrentF <- max(idxCurrentF, tmp) %>% '+'(1)
      # t <- 10. ^ (idxCurrentF / nbPtsF)
    }
  } else {
    res <- matrix(NA, length(Fvalues), N)
    # current_eval <- rep(NA, N)
    for (i in seq_along(Fvalues)) {
      t <- Fvalues[i]
      for (k in seq_along(data)) {
        d <- data.iter[[k]]
        curr_eval <- NA
        while (hasNext(d)) {
          curr_line <- nextElem(d)
          v <- curr_line[target_column]
          if (v >= t) {
            curr_eval <- curr_line[1]
            break
          } # hitting the target 
        }
        res[i, k] <- curr_eval
      }
    }
  }
  row.names(res) <- Fvalues
  res
}

align.cost <- function(data, costs = 'auto') {
  N <- length(data)
  data.iter <- lapply(data, function(d) ihasNext(iter(as.matrix(d), by = 'row'))) # iterator of data frames
  
  if (is.numeric(targets)) {
    Fvalues <- rev(sort(targets))
  } else {
    if (targets == 'auto') { # similar to the alignment in bbob
      fvalues <- sapply(data, function(x) x$BestDeltaF[1])
      idxCurrentF <- max(fvalues, na.rm = T) %>% log10 %>% '*'(nbPtsF) %>% ceiling
      t <- 10. ^ (idxCurrentF / nbPtsF)
    }
    if (targets == 'full') {
      Fvalues <- lapply(data, function(x) unique(x$BestDeltaF)) %>% 
        unlist %>% unique %>% sort %>% rev
    }
  }
  
  if (targets == 'auto') {
    Fvalues <- c()
    res <- c()
    curr_eval <- rep(NA, N)
    curr_fvalues <- rep(NA, N)
    
    while (t > 0) {
      curr_eval[1:N] <- NA
      curr_fvalues[1:N] <- NA
      
      for (k in seq_along(data.iter)) {
        d <- data.iter[[k]]
        while (hasNext(d)) {
          curr_line <- nextElem(d)
          v <- curr_line[3]
          if (v <= t) {
            curr_eval[k] <- curr_line[1] # hitting the target
            curr_fvalues[k] <- v
            break
          }
        }
      }
      
      if (all(is.null(curr_eval))) # if the current target is not hit by any instance
        break
      
      Fvalues <- c(Fvalues, t)
      res <- rbind(res, curr_eval)
      
      # calculate the new target values
      tmp <- max(curr_fvalues, na.rm = T) %>% log10 %>% '*'(nbPtsF) %>% ceiling 
      idxCurrentF <- max(tmp) %>% '-'(1)
      t <- 10. ^ (idxCurrentF / nbPtsF)
    }
  } else {
    res <- matrix(NA, length(Fvalues), N)
    # current_eval <- rep(NA, N)
    for (i in seq_along(Fvalues)) {
      t <- Fvalues[i]
      for (k in seq_along(data)) {
        d <- data.iter[[k]]
        curr_eval <- NA
        while (hasNext(d)) {
          curr_line <- nextElem(d)
          v <- curr_line[3]
          if (v <= t) {
            curr_eval <- curr_line[1]
            break
          } # hitting the target 
        }
        res[i, k] <- curr_eval
      }
    }
  }
  row.names(res) <- Fvalues
  res
}

# Valign <- function(df) {
#   fvalues <- df$BestF %>% unique %>% sort
#   
#   df.aligned <- data.frame()
#   data <- tbl_df(df) %>% group_by(run)
#   # TODO: handle the case where some runs failed
#   for (f in fvalues) {
#     data <- data %>% filter(BestF >= f)
#     row <- data %>% slice(1) %>% ungroup %>% arrange(run) %>% '$'('fct_eval')
#     df.aligned <- rbind(df.aligned, row)
#   }
#   rownames(df.aligned) <- fvalues
#   colnames(df.aligned) <- seq(length(unique(df$run)))
#   df.aligned
# }

# ECDF <- function(ftarget) {
#   RT <- as.numeric(df.aligned[as.character(ftarget), ]) # running time
#   ecdf(log10(RT / dim))
# }

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
  
  df <- list(df1, df2)
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
                 median = apply(data, 1, . %>% median(na.rm = T)))
      i <- i + 1
    }
    res
  })
  
  # the runtime at a given target precision
  RT <- reactive({
    target <- input$target %>% as.numeric
    lapply(aligned(), . %>% {
      v <- rownames(.) %>% as.numeric
      idx <- order(abs(target - v))[1]
      .[idx, ] %>% as.vector
      }) %>% 
      do.call(cbind.data.frame, .) %>% 
      set_colnames(c('algorithm1', 'algorithm2'))
  })
  
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
  
  output$convergence.raw <- renderPlot({
    dfs <- DF()
    instance <- input$instance
    
    p <- ggplot(aes(x = BestF, y = fct_eval, group = run, colour = run))
    for (df in dfs) {
       p <- p +
        geom_line(data = df, size = 1, alpha = 0.4, show.legend = F) + 
        stat_summary(fun.y = mean, geom = 'line', size = 1, 
                     aes(group = 1)) + 
        labs(x = 'best fitness measured', y = 'function evaluations') + 
        scale_color_discrete(guide = F)
      
      if (!instance == "") {
        data <- df %>% filter(run == instance)
        p <- p + geom_line(data = data, 
                           mapping = aes(x = BestF, y = fct_eval, group = 1), 
                           size = 2, alpha = 0.9, colour = 'red',
                           show.legend = F)
      }
    }
    print(p)
  })
  
  output$data.table <- renderDataTable({
    instance <- input$instance
    dfs <- DF()
    df <- dfs[[1]]
    df %>% filter(run == instance) %>% select(-run)
  }, options = list(pageLength = 20))
  
  
  RT.summary <- reactive({
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
      v <- rownames(df) %>% as.numeric
      
      res[[i]] <- lapply(fseq, 
                         function(f) {
                           idx <- order(abs(f - v))[1]
                           RT <- df[idx, ] %>% as.vector
                           c(f, length(RT[!is.na(RT)]), 
                             mean(RT, na.rm = T), median(RT, na.rm = T), 
                             quantile(RT, probs = probs, names = F, 
                                      type = 3, na.rm = T)) # type 1 ~ 3 for the discrete r.v.
                         }) %>% 
        do.call(rbind, .) %>% 
        as.data.frame %>% 
        cbind(paste0('algorithm', i), .)
    }
    
    do.call(rbind.data.frame, res) %>% 
      set_colnames(c('algorithm name', 'f(x)', 'runs', 'mean', 'median', paste0(probs * 100, '%')))
  })
  
  output$summary <- renderTable({
    df <- RT.summary()
    df$runs %<>% as.integer
    df$median %<>% as.integer
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>%  as.integer
    }
    df
  })
  
  output$downloadData <- downloadHandler(
    filename = 'runtime_summary.csv',
    content = function(file) {
      write.csv(RT.summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  output$table.RT.sample <- renderPrint({
    fall <- fvalues()  # all the aligned target values
    fselect <- input$fselect %>% as.numeric 
    
    # when initializing or incorrect input
    if (is.na(fselect))
      return(data.frame())
    if (fselect < min(fall) || fselect > max(fall))
      return(data.frame())
    
    df.aligneds <- aligned()
    res <- list()
    
    for (i in seq_along(df.aligneds)) {
      df <- df.aligneds[[i]]
      v <- rownames(df) %>% as.numeric
      idx <- order(abs(fselect - v))[1]
      res[[i]] <- df[idx, ] %>% as.vector
    }
    names(res) <- names(df.aligneds)
    print(res)
  })
  
  output$downloadData.ert <- downloadHandler(
    filename = 'ERT.csv',
    content = function(file) {
      df <- ERT()
      res <- list()
      for (k in seq_along(df)) {
        res[[k]] <- df[[k]] %>% 
          mutate(algorithm = paste0('algorithm', k))
      }
      res <- do.call(rbind.data.frame, res)
      write.csv(res, file, row.names = F)
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
  
  # output$raw.RT <- renderDataTable({
  #   RT()
  # }, options = list(pageLength = 5))

  # the Fixed-Target convergence results
  output$mean.convergence <- renderPlot({
    target <- input$target %>% as.numeric
    show.median <- input$show.median
    show.mean <- input$show.mean
    show.original <- input$show.instance
    x_range <- input$plot.range

    df.ERT <- ERT()
    df.BestF <- BestF()
    df.aligneds <- aligned()

    p <- ggplot()
    colors <- colorspace::rainbow_hcl(2)
    shapes <- c(20, 4)
    linetypes <- c('solid', 'longdash')

    for (i in seq_along(df.aligneds)) {
      df.aligned <- df.aligneds[[i]]
      ert <- df.ERT[[i]]
      raw <- df.BestF[[i]]

      # select the aligned precison that is closest to the selected target
      v <- rownames(df.aligned) %>% as.numeric
      idx <- order(abs(target - v))[1]
      df.row <- df.aligned[idx, ] %>% melt %>% mutate(variable = target)
      width <- (max(ert$BestF) - min(ert$BestF)) / 10

      # p <- gg_beanplot(p = p, mapping = aes(x = variable, y = value), data = df.row,
      #                    width = width, fill = colors[i], trim = F,
      #                    linetype = linetypes[i], colour = colors[i],
      #                    point.shape = shapes[i], show.violin = show.beanplot,
      #                    show.sample = show.sample, alpha = 0.7)

      if (show.original)
        p <- p + geom_line(data = raw, aes(x = BestF, y = eval, group = instance),
                           colour = colors[i], size = 0.2, alpha = 0.5)
      if (show.mean)
        p <- p + geom_line(data = ert, aes(x = BestF, y = mean),
                           colour = colors[i], size = 2, alpha = 1)
      
      if (show.median)
        p <- p + geom_line(data = ert, aes(x = BestF, y = median),
                           colour = colors[i], size = 2, alpha = 1,
                           linetype = 'dashed')
        
      tmp <- ert %>% 
        filter(BestF > x_range[1], BestF < x_range[2])
      
      p <- p + geom_ribbon(aes(x = BestF, ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd),
                           data = ert, fill = '#447DF6', alpha = 0.2) +
        ylim(c(min(tmp$mean), max(tmp$mean)))

      p <- p + xlim(range = x_range) +
        labs(x = 'best-so-far f(x)-value', y = 'runtime / function evaluation',
             title = 'Runtime (RT) vs. target precison')
      
      if (input$semilogx)
        p <- p + scale_x_log10()
      
      if (input$semilogy)
        p <- p + scale_y_log10()
      
    }
    p
  })
  
  output$bean.plot <- renderPlot({
    target <- input$target.plot %>% as.numeric
    df.aligneds <- aligned()
    
    p <- ggplot()
    colors <- 
    shapes <- c(20, 4)
    linetypes <- c('solid', 'longdash')
    
    data <- list()
    for (i in seq_along(df.aligneds)) {
      df.aligned <- df.aligneds[[i]]
      
      # select the aligned precison that is closest to the selected target
      v <- rownames(df.aligned) %>% as.numeric
      idx <- order(abs(target - v))[1]
      running_time <- df.aligned[idx, ] %>% as.vector
            
      data[[i]] <- data.frame(algorithm = paste0('algorithm', i), run.time = running_time)
      
      # p <- p + xlim(range = x_range) +
      #   labs(x = 'Best fitness', y = 'runtime / function evaluation',
      #        title = 'Runtime (RT) vs. target precison')
    }
    
    width <- 0.3
    data <- do.call(rbind.data.frame, data)
    colors <- colorspace::rainbow_hcl(2) %>% 
      setNames(unique(data$algorithm))
    
    p <- gg_beanplot(p = p, mapping = aes(x = algorithm, y = run.time, 
                                          fill = algorithm, colour = algorithm), 
                     data = data, width = width, trim = F, kernel = input$kernel,
                     show.sample = input$show.RT.sample, alpha = 0.8) + 
      scale_color_manual(values = colors) + 
      scale_fill_manual(values = colors) +
      labs(x = 'algorithms', y = 'running time')
    
    p
  })
  
  # TODO: try to use ggvis
  # conv <- reactive({
  #     target <- input$target %>% as.numeric
  #     show.median <- input$show.median
  #     show.mean <- input$show.mean
  #     show.beanplot <- input$show.beanplot
  #     show.sample <- input$show.RT.sample
  #     zoomin <- input$zoomin
  #     
  #     data <- ERT()
  #     df <- DF.BestDeltaF()
  #     df.aligneds <- aligned()
  #     
  #     p <- ggvis()
  #     colors <- colorspace::rainbow_hcl(2)
  #     shapes <- c(20, 4)
  #     linetypes <- c('solid', 'longdash')
  #     
  #     for (i in seq_along(df.aligneds)) {
  #       df.aligned <- df.aligneds[[i]]
  #       ert <- data[[i]]
  #       raw <- df[[i]]
  #       
  #       # select the aligned precison that is closest to the selected target
  #       v <- rownames(df.aligned) %>% as.numeric
  #       idx <- order(abs(target - v))[1]
  #       df.row <- df.aligned[idx, ] %>% melt %>% mutate(variable = target)
  #       width <- (max(ert$BestF) - min(ert$BestF)) / 10
  #       
  #       # p <- gg_beanplot(p = p, mapping = aes(x = variable, y = value), data = df.row,
  #       #                  width = width, fill = colors[i], trim = F,
  #       #                  linetype = linetypes[i], colour = colors[i],
  #       #                  point.shape = shapes[i], show.violin = show.beanplot,
  #       #                  show.sample = show.sample, alpha = 0.7)
  #       
  #       # p <- p + geom_ribbon(aes(x = BestF, ymin = mean - 1.96 * sd, ymax = mean + 1.96 * sd),
  #       #                      ert, fill = '#447DF6', alpha = 0.2) +
  #       #   ylim(c(min(ert$mean), max(ert$mean)))
  #       
  #       if (show.mean)
  #         p <- p %>% layer_paths(data = ert, x = ~BestF, y = ~mean, stroke := colors[i])
  #       
  #       # p <- p + geom_line(data = raw, aes(x = BestDeltaF, y = eval, group = instance),
  #       #                    colour = colors[i], size = 0.2, alpha = 0.1)
  #       
  #       if (show.median)
  #         p <- p + geom_line(data = ert, aes(x = BestF, y = median),
  #                            colour = colors[i], size = 2, alpha = 0.8,
  #                            linetype = 'dashed')
  #       
  #       if (zoomin) {
  #         p <- p + scale_x_reverse(limits = c(target + width * 1.3, target - width * 1.3))
  #       }
  #       else
  #         1
  #         # p <- p + scale_x_reverse()
  #       
  #       # p <- p + labs(x = 'best fitness - Fopt', y = 'runtime / function evaluation',
  #       #               title = 'Runtime (RT) vs. target precison')
  #     }
  #     p
  # })
  # 
  # conv %>% bind_shiny("test")

  
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
