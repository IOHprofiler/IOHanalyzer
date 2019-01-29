
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
     
