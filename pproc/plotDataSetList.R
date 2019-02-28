symbols <- c("circle-open", "diamond-open", "square-open", "cross-open",
             "triangle-up-open", "triangle-down-open")

get_legends <- function(dsList) {
  N <- length(dsList)
  legends <- sapply(dsList, function(d) attr(d, 'algId'))
  
  if (length(unique(legends)) < N) {
    funcId <- sapply(dsList, function(d) attr(d, 'funcId'))
    if (length(unique(funcId)) > 1)
      legends <- paste0(legends, '-F', funcId)
  }
  
  if (length(unique(legends)) < N) {
    DIM <- sapply(dsList, function(d) attr(d, 'DIM'))
    if (length(unique(DIM)) > 1)
      legends <- paste0(legends, '-', DIM, 'D')
  }
  legends
}

insert_best_parts <- function(from_data, to_data, best_is_min) {
  if (all(is.na(from_data)))
    to_data
  else
    if (best_is_min)
      pmin(from_data, to_data, na.rm = T)
  else
    pmax(from_data, to_data, na.rm = T)
}

#Adding S3 generics
plot_RT_line <- function(dsList,...) UseMethod("plot_RT_line", dsList)
plot_FV_line <- function(dsList,...) UseMethod("plot_FV_line", dsList)
plot_RT_PMF <- function(dsList,...) UseMethod("plot_RT_PMF", dsList)
plot_RT_HIST <- function(dsList,...) UseMethod("plot_RT_HIST", dsList)
plot_RT_ECDF <- function(dsList,...) UseMethod("plot_RT_ECDF", dsList)
plot_RT_ECDF_AGGR <- function(dsList,...) UseMethod("plot_RT_ECDF_AGGR", dsList)
plot_RT_AUC <- function(dsList,...) UseMethod("plot_RT_AUC", dsList)
plot_FV_PDF <- function(dsList,...) UseMethod("plot_FV_PDF", dsList)
plot_FV_HIST <- function(dsList,...) UseMethod("plot_FV_HIST", dsList)
plot_FCE_ECDF_PER_TARGET <- function(dsList,...) UseMethod("plot_FCE_ECDF_PER_TARGET", dsList)
plot_FV_ECDF_AGGR <- function(dsList,...) UseMethod("plot_FV_ECDF_AGGR", dsList)
plot_FV_AUC <- function(dsList,...) UseMethod("plot_FV_AUC", dsList)
plot_PAR_Line <- function(dsList,...) UseMethod("plot_PAR_Line", dsList)
plot_RT_ECDF_MULTI <- function(dsList,...) UseMethod("plot_RT_ECDF_MULTI", dsList)

plot_RT_line.DataSetList <- function(dsList, Fstart = NULL, Fstop = NULL, 
                                     show.ERT = T, show.CI = T, show.mean = F,
                                     show.runs = F, show.density = 50,
                                     show.pareto = F, show.optimal = F,
                                     show.median = F, backend = 'plotly',
                                     scale.xlog = F, scale.ylog = F,
                                     scale.reverse = F) {
  Fall <- get_Funvals(dsList)
  if (is.null(Fstart)) Fstart <- min(Fall)
  if (is.null(Fstop)) Fstop <- max(Fall)
  
  Fseq <- seq_FV(Fall, Fstart, Fstop, length.out = 60, scale = ifelse(scale.xlog,'log','linear'))
  if (length(Fseq) == 0) return(NULL)
  
  N <- length(dsList)
  legends <- get_legends(dsList)
  colors <- color_palettes(N)
  
  dt <- get_RT_summary(dsList, ftarget = Fseq)
  dt[, `:=`(upper = mean + sd, lower = mean - sd)]
  
  if (backend == 'plotly') {
    p <- plot_ly_default(x.title = "best-so-far f(x)-value", 
                         y.title = "function evaluations")
    
    for (i in seq_along(dsList)) {
      legend <- legends[i]
      ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
                   funcId == attr(dsList[[i]], 'funcId') &
                   DIM == attr(dsList[[i]], 'DIM')]

      algId <- attr(dsList[[i]], 'algId')
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.2)')
      
      if (show.CI)
        p %<>% 
          add_trace(data = ds_ERT, x = ~target, y = ~upper, type = 'scatter', mode = 'lines',
                    line = list(color = rgba_str, width = 0), legendgroup = legend,
                    showlegend = F, name = 'mean +/- sd') %>% 
          add_trace(x = ~target, y = ~lower, type = 'scatter', mode = 'lines',
                    fill = 'tonexty',  line = list(color = 'transparent'), legendgroup = legend,
                    fillcolor = rgba_str, showlegend = F, name = 'mean +/- sd')
      
      if (show.ERT)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~ERT, type = 'scatter',
                         name = paste0(legend, '.ERT'), mode = 'lines+markers',
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str))
      
      if (show.mean)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~mean, type = 'scatter', 
                         mode = 'lines+markers', name = paste0(algId, '.mean'), 
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = 'dash'))
      
      if (show.median)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~median, type = 'scatter',
                         name = paste0(legend, '.median'), mode = 'lines+markers', 
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = 'dot'))
      
      if (show.runs) {
        # TODO: Fix this for the case where algorithms do not have the same number of runs

        dr <- get_RT_sample(dsList, Fseq)
        dr_ERT <- dr[algId == attr(dsList[[i]], 'algId')&
                     funcId == attr(dsList[[i]], 'funcId') &
                     DIM == attr(dsList[[i]], 'DIM')]
        
        names_to_show = sample(colnames(dr_ERT))
        names_to_show <-
          names_to_show[!names_to_show %in% c('algId', 'target')]
        
        counter <- as.integer(length(names_to_show) * show.density / 100)
        names_to_show <- head(names_to_show, counter)
        best_parts <- NA
        mentioned <- FALSE
        
        for (run_v in names_to_show) {
          p %<>% add_trace(
            data = dr_ERT,
            x = ~ target,
            y = dr_ERT[[run_v]],
            type = 'scatter',
            mode = 'lines',
            line = list(color = rgb_str, width = 0.5),
            text = paste(run_v),
            hoverinfo = 'none',
            showlegend = !mentioned,
            name = paste("runs of ", algId)
          )
          mentioned <- TRUE
          best_parts <-
            insert_best_parts(best_parts, dr_ERT[[run_v]], TRUE)
        }
        
        if (show.optimal) {
          mentioned <- FALSE
          NonNAindex <- which(!is.na(best_parts))
          target_idx <- max(NonNAindex)
          
          check_value <- best_parts[target_idx]
          for (run_v in names_to_show)
            found_val <- dr_ERT[[run_v]][target_idx]
            if (!is.na(found_val) & check_value == found_val) {
              p %<>% add_trace(
                data = dr_ERT,
                x = ~ target,
                y = dr_ERT[[run_v]],
                type = 'scatter',
                mode = 'lines',
                line = list(color = rgb_str, width = 1.5),
                showlegend = !mentioned,
                name = paste("best.", algId)
              )
              mentioned <- TRUE
            }
        }
        if (show.pareto) {
          p %<>% add_trace(
            x = dr_ERT[['target']],
            y = best_parts,
            type = 'scatter',
            mode = 'lines',
            line = list(color = rgb_str, width = 2.5, dash = 'dot'),
            showlegend = T,
            name = paste("pareto_optima.", algId)
          )
        }
        
      }
    }
    p %<>%
      layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')),
             yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))
    
    if (scale.reverse)
      p %<>% layout(xaxis = list(autorange = "reversed"))
  } 
  else if (backend == 'ggplot2') {
    dt[, group := paste(algId, funcId, DIM, sep = '-')]
    p <- ggplot(data = dt, aes(group = group, colour = group))
    
    if (show.CI) p <- p + geom_ribbon(aes(target, ymin = lower, ymax = upper, fill = group),
                                      alpha = 0.2, colour = NA)
    if (show.ERT) p <- p + geom_line(aes(target, ERT), size = 1.2)
    if (show.mean) p <- p + geom_line(aes(target, mean), linetype = 'dashed')
    if (show.median) p <- p + geom_line(aes(target, median), linetype = 'dotted')
    
    p <- p + 
      scale_color_manual(values = colors) + 
      scale_fill_manual(values = colors)
  }
  return(p)
}

plot_FV_line.DataSetList <- function(dsList, RTstart = NULL, RTstop = NULL,
                                     show.mean = T, show.median = F,
                                     show.runs = F, show.density = 50,
                                     show.pareto = F, show.optimal = F,
                                     backend = 'plotly',
                                     scale.xlog = F, scale.ylog = F,
                                     scale.reverse = F) {
  
  RTall <- get_Runtimes(dsList)
  if (is.null(RTstart)) Fstart <- min(RTall)
  if (is.null(RTstop)) Fstop <- max(RTall)
  

  RTseq <- seq_RT(RTall, RTstart, RTstop, length.out = 60, scale = ifelse(scale.xlog,'log','linear'))
  if (length(RTseq) == 0) return(NULL)
  
  N <- length(dsList)
  colors <- color_palettes(N)
  legends <- get_legends(dsList)
  
  fce <- get_FV_summary(dsList, RTseq)
  fce[, `:=`(upper = mean + sd, lower = mean - sd)]
  
  if(backend == 'plotly'){
    p <- plot_ly_default(y.title = "best-so-far f(x)-value", x.title = "runtime")
    
    for (i in seq_along(dsList)) {
      legend <- legends[i]
      algId <- attr(dsList[[i]], 'algId')

      ds_FCE <- fce[algId == attr(dsList[[i]], 'algId') &
                     funcId == attr(dsList[[i]], 'funcId') &
                     DIM == attr(dsList[[i]], 'DIM')]
      
      if (nrow(ds_FCE) == 0)
        next
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
      
      p %<>% 
        add_trace(data = ds_FCE, x = ~runtime, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0), legendgroup = legend,
                  showlegend = F, name = 'mean +/- sd') %>% 
        add_trace(x = ~runtime, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  legendgroup = legend,
                  fillcolor = rgba_str, showlegend = F, name = 'mean +/- sd')
      
      if (show.mean)
        p %<>% add_trace(data = ds_FCE, x = ~runtime, y = ~mean, type = 'scatter', 
                         mode = 'lines+markers', name = paste0(algId, '.mean'), 
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str))

      if (show.median)
        p %<>% add_trace(data = ds_FCE, x = ~runtime, y = ~median, type = 'scatter',
                         name = paste0(legend, '.median'), mode = 'lines+markers', 
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = 'dash'))

      if (show.runs) {
        fce_runs <- get_FV_sample(dsList, RTseq)
        
        fce_runs_ERT <- fce_runs[algId == attr(dsList[[i]], 'algId') &
                                 funcId == attr(dsList[[i]], 'funcId') &
                                 DIM == attr(dsList[[i]], 'DIM')]
        names_to_show <- sample(colnames(fce_runs_ERT))
        names_to_show <-
          names_to_show[!names_to_show %in% c('algId', 'runtime')]
        
        counter <- as.integer(length(names_to_show) * show.density / 100)
        
        names_to_show <- head(names_to_show, counter)
        best_parts <- NA

        for (run_v in names_to_show) {
          p %<>% add_trace(
            data = fce_runs_ERT,
            x = ~ runtime,
            y = fce_runs_ERT[[run_v]],
            type = 'scatter',
            mode = 'lines',
            line = list(color = rgb_str, width = 0.5),
            text = paste(run_v),
            hoverinfo = 'none',
            name = paste("runs of ", algId)
          )
          best_parts <-
            insert_best_parts(best_parts, fce_runs_ERT[[run_v]], !attr(dsList[[i]],"maximization"))
        }

        if (show.optimal) {
          mentioned <- FALSE
          check_value <- tail(best_parts, 1)
          for (run_v in names_to_show)
            if (check_value == tail(fce_runs_ERT[[run_v]], 1)) {
              p %<>% add_trace(
                data = fce_runs_ERT,
                x = ~ runtime,
                y = fce_runs_ERT[[run_v]],
                type = 'scatter',
                mode = 'lines',
                line = list(color = rgb_str, width = 0.5 *
                              3),
                showlegend = !mentioned,
                name = paste("best.", algId)
              )
              mentioned = TRUE
            }
        }
        if (show.pareto) {
          p %<>% add_trace(
            x = fce_runs_ERT[['runtime']],
            y = best_parts,
            type = 'scatter',
            mode = 'lines',
            line = list(color = rgb_str, width = 0.5 *
                          5, dash = 'dot'),
            showlegend = T,
            name = paste("pareto_optima.", algId)
          )
        }
      }
    }
    p %<>%
      layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')),
             yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))
    
    if (scale.reverse)
      p %<>% layout(xaxis = list(autorange = "reversed"))
  }
  else if (backend == 'ggplot2'){
    fce[, group := paste(algId, funcId, DIM, sep = '-')]
    p <- ggplot(data = fce, aes(group = group, colour = group))
    
    if (show.mean) p <- p + geom_line(aes(runtime, mean), linetype = 'dashed')
    if (show.median) p <- p + geom_line(aes(runtime, median), linetype = 'dotted')
    
    p <- p + 
      scale_color_manual(values = colors) + 
      scale_fill_manual(values = colors)
    
    #TODO: add individual run etc
  }
  return(p)
}
     
plot_RT_PMF.DatasetList <- function(dsList, ftarget, show.sample = F, 
                                    scale.ylog = F, backend = 'plotly'){

  points <- ifelse(show.sample, 'all', FALSE)
  
  N <- length(dsList)
  colors <- color_palettes(N)
  
  p <- plot_ly_default(x.title = "algorithms",
                       y.title = "runtime / function evaluations")
  
  for (i in seq_along(dsList)) {
    ds <- dsList[[i]]
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.55)')

    p %<>%
      add_trace(data = get_RT_sample(ds, ftarget, output = 'long'),
                x = ~algId, y = ~RT, split = ~algId, type = 'violin',
                hoveron = "points+kde",
                box = list(visible = T),
                points = points,
                pointpos = 1.5,
                jitter = 0,
                name = attr(ds, 'algId'),
                scalemode = 'count',
                meanline = list(visible = F),
                fillcolor = rgba_str,
                line = list(color = 'black', width = 2),
                marker = list(color = rgb_str, size = 8))
    
  }
  p %<>%
    layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
           xaxis = list(tickangle = 45))
}

plot_RT_HIST.DataSetList <- function(dsList, ftarget, plot_mode = 'overlay'){

  N <- length(dsList)
  colors <- color_palettes(N)
  if (N <= 10)
    nrows <- ceiling(N / 2.) # keep to columns for the histograms
  else 
    nrows <- ceiling(N / 3.) # keep to columns for the histograms
  
  if (plot_mode == 'overlay') {
    p <- plot_ly_default(x.title = "function evaluations", y.title = "runs")
  } else if (plot_mode == 'subplot') {
    p <- lapply(seq(N), function(x) {
      plot_ly_default(x.title = "function evaluations", y.title = "runs")
    })
  }
  
  for (i in seq_along(dsList)) {
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.35)')
    
    df <- dsList[[i]]
    algId <- attr(df, 'algId')
    rt <- get_RT_sample(df, ftarget, output = 'long')
    
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
    p <- subplot(p, nrows = nrows, titleX = F, titleY = F, margin = 0.025)
  }
  p
}

plot_RT_ECDF.DataSetList <- function(dsList, ftargets, scale.xlog = F){
  req(length(ftargets) != 0)
  
  N <- length(data)
  colors <- color_palettes(N)
  
  p <- plot_ly_default(title = NULL,
                       x.title = "function evaluations",
                       y.title = "Proportion of runs")
  
  for (k in seq_along(dsList)) {
    df <- dsList[[k]]
    algId <- attr(df, 'algId')
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.35)')
    
    for (i in seq_along(ftargets)) {
      rt <- get_RT_sample(df, ftargets[i], output = 'long')$RT %>% sort
      if (all(is.na(rt)))
        next
      
      # TODO: ECDF computation should be put in pproc/stats.R
      ecdf <- CDF_discrete(rt)
      
      # position of the markers
      x <- quantile(rt, probs = c(0.25, 0.5, 0.75), names = F, type = 3)
      y <- sapply(x, function(x) ecdf[rt == x][1])
      
      p %<>%
        add_trace(data = NULL, x = rt, y = ecdf, type = 'scatter',
                  mode = 'lines', name = algId, showlegend = F,
                  legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 3)) %>% 
        add_trace(data = NULL, x = x, y = y, type = 'scatter',
                  mode = 'markers',  legendgroup = paste0(k),
                  name = sprintf('(%s, %.2e)', algId, ftargets[i]),
                  marker = list(color = rgb_str, symbol = symbols[i], size = 13))
    }
  }
  
  p %<>%
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')))
  p
}

plot_RT_ECDF_AGGR.DataSetList <- function(dsList, fstart = NULL, fstop = NULL,
                                          fstep = NULL, show.per_target = F,
                                          scale.xlog = F){
  fall <- get_Funvals(dsList)
  if (is.null(fstart)) fstart <- min(fall)
  if (is.null(fstop)) fstop <- max(fall)

  fseq <- seq_FV(fall, fstart, fstop, fstep, 
                 scale = ifelse(scale.xlog, 'log', 'linear'))
  req(fseq)
  
  N <- length(dsList)
  colors <- color_palettes(N)
  
  RT.max <- sapply(dsList, function(ds) max(ds$RT, na.rm = T)) %>% max
  RT.min <- sapply(dsList, function(ds) min(ds$RT, na.rm = T)) %>% min
  x <- seq(RT.min, RT.max, length.out = 50)
  p <- plot_ly_default(x.title = "function evaluations",
                       y.title = "Proportion of (run, target) pairs")
  
  for (k in seq_along(dsList)) {
    df <- dsList[[k]]
    algId <- attr(df, 'algId')
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.15)')
    rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.8)')
    
    m <- lapply(fseq, function(f) {
      rt <- get_RT_sample(df, f, output = 'long')$RT
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
    
    if (show.per_target) {
      for (f in fseq) {
        rt <- get_RT_sample(df, f, output = 'long') %>% '$'('RT') %>% sort
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
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')))
  p
}

plot_RT_AUC.DataSetList <- function(dsList, fstart = NULL, 
                                    fstop = NULL, fstep = NULL,
                                    fval_formatter = as.integer){
  fall <- get_Funvals(dsList)
  if (is.null(fstart)) fstart <- min(fall)
  if (is.null(fstop)) fstop <- max(fall)
  
  fseq <- seq_FV(fall, fstart, fstop, fstep)

  N <- length(dsList)
  colors <- color_palettes(N)
  
  RT.max <- sapply(dsList, function(ds) max(attr(ds, 'maxRT'))) %>% max
  p <- plot_ly_default()
  
  for (k in seq_along(dsList)) {
    df <- dsList[[k]]
    algId <- attr(df, 'algId')
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.2)')
    
    # calculate ECDFs on user specified targets

    # funs <- lapply(fseq, function(f) {
    #   get_RT_sample(df, f, output = 'long')$RT %>% {
    #     if (all(is.na(.))) NULL
    #     else  RT.ECDF(.)
    #   }
    # })

    auc <- sapply(fseq, function(fv) {
        ECDF(df, fv) %>% AUC(from = 1, to = RT.max)
      })
    
    # auc <- sapply(funs,
    #               function(fun) {
    #                 if (is.null(fun)) 0
    #                 else integrate(fun, lower = attr(fun, 'min') - 1, upper = RT.max, 
    #                                subdivisions = 5e3) %>% {'$'(., 'value') / RT.max}
    #               })

    p %<>% 
      add_trace(type = 'scatterpolar', r = auc, 
                theta = paste0('f:', fval_formatter(fseq)), 
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
  p

}

plot_FV_PDF.DataSetList <- function(dsList, runtime, show.sample = F, scale.ylog = F){
  points <- ifelse(show.sample, 'all', FALSE)
  
  N <- length(dsList)
  colors <- color_palettes(N)
  
  p <- plot_ly_default(x.title = "algorithms",
                       y.title = "Target value")
  
  for (i in seq_along(dsList)) {
    ds <- dsList[[i]]
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.55)')

    p %<>%
      add_trace(data = get_FV_sample(ds, runtime, output = 'long'),
                x = ~algId, y = ~`f(x)`, split = ~algId, type = 'violin',
                hoveron = "points+kde",
                box = list(visible = T),
                points = points,
                pointpos = 1.5,
                jitter = 0,
                scalemode = 'count',
                name = attr(ds, 'algId'),
                meanline = list(visible = T),
                fillcolor = rgba_str,
                line = list(color = 'black', width = 2),
                marker = list(color = rgb_str, size = 8))
  }
  p %<>%
    layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))
  p
}


plot_FV_HIST.DataSetList <- function(dsList, runtime, plot_mode='overlay'){
  n_algorithm <- length(dsList)
  colors <- color_palettes(n_algorithm)
  if (n_algorithm <= 10)
    nrows <- ceiling(n_algorithm / 2.) # keep to columns for the histograms
  else 
    nrows <- ceiling(n_algorithm / 3.) # keep to columns for the histograms
  
  if (plot_mode == 'overlay') {
    p <- plot_ly_default(x.title = "target values", y.title = "runs")
    
  } else if (plot_mode == 'subplot') {
    p <- lapply(seq(n_algorithm), function(x) {
      plot_ly_default(x.title = "target values", y.title = "runs")
    })
  }
  
  for (i in seq_along(dsList)) {
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.35)')
    
    ds <- dsList[[i]]
    algId <- attr(ds, 'algId')
    fce <- get_FV_sample(ds, runtime, output = 'long')
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
    p <- subplot(p, nrows = nrows, titleX = F, titleY = F, margin = 0.02)
  
  p
}

plot_FCE_ECDF_PER_TARGET.DataSetList <- function(dsList, runtimes, scale.xlog = F){
  #TODO: Fvals in legend need to be formatted properly
  runtimes <- runtimes[!is.na(runtimes)]
  req(length(runtimes) != 0)
  
  n_algorithm <- length(dsList)
  colors <- color_palettes(n_algorithm)
  
  p <- plot_ly_default(title = NULL,
                       x.title = "target value",
                       y.title = "Proportion of runs")
  
  for (k in seq_along(dsList)) {
    ds <- dsList[[k]]
    algId <- attr(ds, 'algId')
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.35)')
    
    for (i in seq_along(runtimes)) {
      funvals <- get_FV_sample(ds, runtimes[i], output = 'long')$`f(x)` %>% sort
      
      if (all(is.na(funvals)))
        next
      
      tmp <- ecdf(funvals) 
      density <- tmp(funvals)
      
      # position of the markers
      x <- quantile(funvals, probs = c(0.25, 0.5, 0.75), names = F, type = 3)
      y <- sapply(x, function(xx) density[funvals == xx])
      
      p %<>%
        add_trace(data = NULL, x = funvals, y = density, type = 'scatter',
                  mode = 'lines', name = algId, showlegend = F,
                  legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 3)) %>% 
        add_trace(data = NULL, x = x, y = y, type = 'scatter',
                  mode = 'markers',  legendgroup = paste0(k),
                  name = sprintf('%s, %.2e', algId, runtimes[i]),
                  marker = list(color = rgb_str, symbol = symbols[i], size = 13))
    }
  }
  
  p %<>%
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')))
  p
}

plot_FV_ECDF_AGGR.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL,
                                          rt_step = NULL, scale.xlog = F,
                                          show.per_target = F){

  rt <- get_Runtimes(dsList)
  if(is.null(rt_min)) rt_min <- min(rt)
  if(is.null(rt_max)) rt_max <- max(rt)
  
  rt_seq <- seq_RT(rt, from = rt_min, to = rt_max, by = rt_step, 
                   scale = ifelse(scale.xlog,'log','linear'))
  req(rt_seq)
  
  n_algorithm <- length(dsList)
  colors <- color_palettes(n_algorithm)
  
  funevals.max <- sapply(dsList, function(ds) max(ds$FV, na.rm = T)) %>% max
  funevals.min <- sapply(dsList, function(ds) min(ds$FV, na.rm = T)) %>% min

  x <- seq(funevals.min, funevals.max, length.out = 40)
  p <- plot_ly_default(x.title = "target value",
                       y.title = "Proportion of (run, budget) pairs")
  
  for (k in seq_along(dsList)) {
    ds <- dsList[[k]]
    algId <- attr(ds, 'algId')
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.15)')
    rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.8)')
    
    fun <- get_FV_sample(ds, rt_seq, output = 'long')$`f(x)` %>% ecdf
    m <- fun(x)

    df_plot <- data.frame(x = x, mean = m)

    
    p %<>%
       add_trace(data = df_plot, x = ~x, y = ~mean, type = 'scatter',
                mode = 'lines+markers', name = sprintf('%s', algId), 
                showlegend = T, legendgroup = paste0(k),
                line = list(color = rgb_str, width = 4.5),
                marker = list(color = rgb_str, size = 11))
    
    if (show.per_target) {
      for (r in rt_seq) {
        ce <- get_FV_sample(ds, r, output = 'long') %>% '$'(`f(x)`) %>% sort
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
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')))
  p
}

plot_FV_AUC.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL,
                                    rt_step = NULL){
  rt <- get_Runtimes(dsList)
  if(is.null(rt_min)) rt_min <- min(rt)
  if(is.null(rt_max)) rt_max <- max(rt)
  
  rt_seq <- seq_RT(rt, from = rt_min, to = rt_max, by = rt_step)
  req(rt_seq)
  
  n_algorithm <- length(dsList)
  colors <- color_palettes(n_algorithm)
  
  funevals.max <- sapply(dsList, function(ds) max(attr(ds, 'finalFV'))) %>% max
  p <- plot_ly_default()
  
  for (k in seq_along(dsList)) {
    df <- dsList[[k]]
    algId <- attr(df, 'algId')
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.2)')
    
    # calculate ECDFs on user specified targets
    funs <- lapply(rt_seq, function(r) {
      get_FV_sample(df, r, output = 'long')$`f(x)` %>% {
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
  
  p
  
}

plot_PAR_Line.DataSetList <- function(dsList, f_min = NULL, f_max = NULL, 
                                      algids = 'all',
                                      scale.xlog = F, scale.ylog = F,
                                      show.mean = T, show.median = F
                                      ){
  #TODO: clean this up
  req(xor(show.mean,show.median))
  
  fall <- get_Funvals(dsList)
  if(is.null(f_min)) f_min <- min(fall)
  if(is.null(f_max)) f_max <- max(fall)
  
  fseq <- seq_FV(fall, f_min, f_max, length.out = 50)
  req(fseq)
  
  dt <- get_PAR_summary(dsList, fseq, algids)
  req(length(dt) != 0)
  dt[, `:=`(upper = mean + sd, lower = mean - sd)]
  
  par_name <- dt[, parId] %>% unique
  n_param <- length(par_name)
  
  algorithms <- dt[, algId] %>% unique
  n_alg <- length(algorithms)
  colors <- color_palettes(n_alg)
  
  nrows <- ceiling(n_param / 2) 
  # TODO: improve the efficiency of plotting here
  p <- lapply(seq(n_param), 
              function(i) {
                plot_ly_default(y.title = par_name[i]) %>% 
                  layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')),
                         yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))
              })
  
  for (i in seq(n_alg)) {
    alg <- algorithms[i]
    
    for (j in seq(n_param)) {
      if (j == 1)
        showlegend <- T
      else 
        showlegend <- F
      
      name <- par_name[j]
      dt_plot <- dt[parId == name & algId == alg]
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
      
      p[[j]] %<>% 
        add_trace(data = dt_plot, x = ~target, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0),  
                  showlegend = F, legendgroup = ~algId, name = 'mean +/- sd') %>% 
        add_trace(x = ~target, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  fillcolor = rgba_str, showlegend = F, legendgroup = ~algId, 
                  name = 'mean +/- sd')
      
      if (show.mean)
        p[[j]] %<>% add_trace(data = dt_plot, x = ~target, y = ~mean, 
                              type = 'scatter', 
                              mode = 'lines+markers', 
                              marker = list(color = rgb_str),
                              line = list(color = rgb_str),
                              name = alg,
                              showlegend = showlegend,
                              legendgroup = ~algId)
      
      else if (show.median)
        p[[j]] %<>% add_trace(data = dt_plot, x = ~target, y = ~median,
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(color = rgb_str),
                              line = list(color = rgb_str, dash = 'dash'),
                              name = alg,
                              legendgroup = ~algId,
                              showlegend = showlegend)
    }
  }

  subplot(p, nrows = nrows, titleX = F, titleY = T, margin = 0.05) %>% 
    add_annotations(x = 0.5 , y = -0.18, text = "best-so-far f(x)-value", 
                    showarrow = F, xref = 'paper', yref = 'paper',
                    font = list(size = 22, family = 'sans-serif'))
}

plot_RT_ECDF_MULTI.DataSetList <- function(dsList, targets = NULL, dim = NULL,
                                           funcid = NULL){
  
  algId <- unique(attr(dsList,"algId"))
  p <- plot_ly_default(x.title = "function evaluations",
                       y.title = "Proportion of (run, target, ...) pairs")
  
  for (i in seq_along(algId)) {
    data <- dsList
    if(!is.null(dim))
      data <- subset(data,DIM == dim)
    if(!is.null(funcid))
      data <- subset(data,funcId == funcid)
    
    rts = get_Runtimes(data)
    
    algId <- algId[[i]]
    df_plot <- calc_ECDF_MULTI(data, algId, targets, rts )
    
    p %<>% add_trace(data = df_plot, x = ~x, y = ~mean, type = 'scatter',
                     mode = 'lines+markers',name = sprintf('%s', algId), 
                     showlegend = T, #, legendgroup = paste0(k),
                     line = list( width = 4.5),
                     marker = list(size = 11))
  }
  p

}


plot_ERT_MULTI.DataSetList <- function(dsList, plot_mode = 'subplot', scale.xlog = F,
                                       scale.ylog = F, scale.reverse = F, aggr_on = 'funcId'){
  
  N <- length(get_AlgId(dsList))
  colors <- color_palettes(N)
  
  in_legend <- integer(N)
  names(in_legend) <- get_AlgId(dsList)
  names(colors) <- get_AlgId(dsList)
  
  aggr_attr <- if(aggr_on == 'funcId') get_funcId(dsList) else get_DIM(dsList)
  M <- length(aggr_attr)
  if (M <= 10)
    nrows <- ceiling(M / 2.) # keep to columns for the histograms
  else 
    nrows <- ceiling(M / 3.) # keep to columns for the histograms
  
  if (plot_mode == 'overlay') {
    p <- plot_ly_default(x.title = "function evaluations", y.title = "ERT")
  } else if (plot_mode == 'subplot') {
    p <- lapply(seq(M), function(x) {
      plot_ly_default(x.title = "function evaluations", y.title = "ERT")
    })
  }
  
  for (j in seq_along(aggr_attr)) {
    dsList_filetered <- if(aggr_on == 'funcId') subset(dsList,funcId==aggr_attr[[j]])
                        else subset(dsList,DIM==aggr_attr[[j]])
    
    Fall <- get_Funvals(dsList_filetered)
    Fstart <- min(Fall)
    Fstop <- max(Fall)
    
    Fseq <- seq_FV(Fall, Fstart, Fstop, length.out = 60, scale = ifelse(scale.xlog,'log','linear'))
    if (length(Fseq) == 0) return(NULL)
    
    dt <- get_RT_summary(dsList_filetered, ftarget = Fseq)
    dt[, `:=`(upper = mean + sd, lower = mean - sd)]
    
    
    for (i in seq_along(dsList_filetered)){
      df <- dsList_filetered[[i]]
      algId <- attr(df, 'algId')
      to_show_legend <- (in_legend[[algId]] == 0)
      in_legend[[algId]] <- 1
      ds_ERT <- dt[algId == attr(df, 'algId') &
                     funcId == attr(df, 'funcId') &
                     DIM == attr(df, 'DIM')]
      
      color <- colors[[algId]]
      rgb_str <- paste0('rgb(', paste0(col2rgb(color), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.35)')
      


      if (plot_mode == 'overlay') {
        p %<>%
          add_trace(data = ds_ERT, x = ~target, y = ~ERT, type = 'scatter',
                    mode = 'lines+markers',
                    marker = list(color = rgb_str),
                    line = list(color = rgb_str),name = algId, showlegend=to_show_legend)
      } else if (plot_mode == 'subplot') {
        p[[j]] %<>% 
          add_trace(data = ds_ERT, x = ~target, y = ~ERT, type = 'scatter',
                    mode = 'lines+markers',
                    marker = list(color = rgb_str),
                    line = list(color = rgb_str),name = algId, showlegend=to_show_legend)
      }
    }
  }
  
  if (plot_mode == 'subplot') {
    p <- subplot(p, nrows = nrows, titleX = F, titleY = F, margin = 0.025)
  }
  p %<>%
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')),
           yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))
  if (scale.reverse)
    p %<>% layout(xaxis = list(autorange = "reversed"))
  p
}

plot_ERT_AGGR.DataSetList <- function(dsList, aggr_on = 'funcId', targets = NULL, 
                                      plot_mode = 'radar', use_rank = F,
                                      scale.ylog = T, maximize = TRUE){
  
  N <- length(get_AlgId(dsList))
  colors <- color_palettes(N)
  
  in_legend <- integer(N)
  names(in_legend) <- get_AlgId(dsList)
  names(colors) <- get_AlgId(dsList)
  
  aggr_attr <- if(aggr_on == 'funcId') get_funcId(dsList) else get_DIM(dsList)
  if(!is.null(targets) && length(targets) != length(aggr_attr)) targets <- NULL
  
  second_aggr <- if(aggr_on == 'funcId') get_DIM(dsList) else get_funcId(dsList)
  if(length(second_aggr) >1 ) return(NULL)
  
  plot_title <- paste0(ifelse(aggr_on == 'funcId', "Dimension ", "Function "), second_aggr[[1]])
  
  p <- if(plot_mode == "radar")  plot_ly_default(title = plot_title, x.title = ifelse(aggr_on == "funcid", "Function", "Dimension"), y.title = "ERT")
  else plot_ly_default(title = plot_title)
    
  erts <- c()
  ertranks <- c()
  
  for (j in seq_along(aggr_attr)) {
    dsList_filetered <- if(aggr_on == 'funcId') subset(dsList,funcId==aggr_attr[[j]])
    else subset(dsList, DIM==aggr_attr[[j]])
    
    if(is.null(targets)){
      Fall <- get_Funvals(dsList_filetered)
      Fval <- ifelse(maximize, max(Fall), min(Fall))
    }
    else
      Fval <- targets[[j]]
    ert <- get_RT_summary(dsList_filetered, ftarget = Fval)$ERT
    erts <- rbind(erts, ert)
    
    if(use_rank){ 
      ertrank <- rank(ert)
      ertranks <- rbind(ertranks,ertrank)
    }
  }
  erts[is.infinite(erts)] <- 10^12
  if (use_rank) dataert <- ertranks
  else dataert <- erts
  
  for (i in seq_along(get_AlgId(dsList))){
    algId <- get_AlgId(dsList)[[i]]
    color <- colors[[algId]]
    rgb_str <- paste0('rgb(', paste0(col2rgb(color), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.35)')
    if(plot_mode == "radar")
      p %<>% 
      add_trace(type = 'scatterpolar', r = dataert[,i], 
                theta = paste0(ifelse(aggr_on == "funcId", "F", "D"),aggr_attr), 
                fill = 'toself', connectgaps = T, fillcolor = rgba_str,
                marker = list(color = rgb_str), hoverinfo = 'text',
                text = paste0('ERT: ', format(erts[,i], digits = 3, nsmall = 3)),
                name = algId) 
    else
      p %<>% add_trace(x = aggr_attr, y = dataert[,i], type = 'scatter',
                       mode = 'lines+markers',
                       marker = list(color = rgb_str), hoverinfo = 'text',
                       text = paste0('ERT: ', format(erts[,i], digits = 3, nsmall = 3)),
                       line = list(color = rgb_str),name = algId)
  }
  if (plot_mode == "radar"){
    if(use_rank)
      p %<>%
        layout(polar = list(radialaxis = list(type = 'linear', visible=F, autorange = 'reversed')))
    else
      p %<>%
        layout(polar = list(radialaxis = list(type = 'log', visible=F, autorange = 'reverse')))
  }
  else{
    if(use_rank)
      p %<>%
        layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear', autorange = 'reverse')),
              xaxis = list(type = ifelse(aggr_on != 'funcId', 'log', 'linear')))
    else
      p %<>%
        layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
             xaxis = list(type = ifelse(aggr_on != 'funcId', 'log', 'linear')))
  }
 p
}



