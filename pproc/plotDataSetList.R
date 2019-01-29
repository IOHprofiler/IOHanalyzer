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
                         line = list(color = rgb_str, dash = 'dash'))
      
      if (show.median)
        p %<>% add_trace(data = ds_FCE, x = ~runtime, y = ~median, type = 'scatter',
                         name = paste0(legend, '.median'), mode = 'lines+markers', 
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = 'dot'))
      
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
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.5)')
    
    p %<>%
      add_trace(data = get_RT_sample(ds, ftarget, output = 'long'),
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
  }
  p %<>%
    layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))
  p
}

plot_RT_Hist.DataSetList <- function(dsList, ftarget, plot_mode = 'overlay'){

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
    p <- subplot(p, nrows = nrows, titleX = F, titleY = F, margin = 0.02)
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

  
  fseq <- seq_FV(fall, fstart, fstop, fstep, scale = ifelse(scale.xlog,'log','linear'))
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
    funs <- lapply(fseq, function(f) {
      get_RT_sample(df, f, output = 'long')$RT %>% {
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
