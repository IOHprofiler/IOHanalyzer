
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

plot_RT_line.DataSetList <- function(dsList, Fstart = NULL, Fstop = NULL, 
                                     show.ERT = T, show.CI = T, show.mean = F,
                                     show.median = F, backend = 'plotly') {
  Fall <- get_Funvals(dsList)
  if (is.null(Fstart)) Fstart <- min(Fall)
  if (is.null(Fstop)) Fstop <- max(Fall)
  
  Fseq <- seq_FV(Fall, Fstart, Fstop, length.out = 60)
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
    }
    
  } else if (backend == 'ggplot2') {
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
                                     backend = 'plotly') {
  
  RTall <- get_Runtimes(dsList)
  if (is.null(RTstart)) Fstart <- min(RTall)
  if (is.null(RTstop)) Fstop <- max(RTall)
  
  
  RTseq <- seq_RT(RTall, RTstart, RTstop, length.out = 60)
  if (length(RTseq) == 0) return(NULL)
  
  
  N <- length(dsList)
  colors <- color_palettes(N)
  legends <- get_legends(dsList)
  
  fce <- get_FV_summary(dsList, RTseq)
  fce[, `:=`(upper = mean + sd, lower = mean - sd)]
  
  if(backend == 'plotly'){
    p <- plot_ly_default(y.title = "best-so-far f(x)-value", x.title = "runtime")
    
    for (i in seq_along(data)) {
      legend <- legends[i]
      algId <- attr(data[[i]], 'algId')
      # dt_plot <- fce[algId == attr(data[[i]], 'algId')]
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
    }
  }
  else if (backend == 'ggplot2'){
    #TODO: add ggplot2-plotting
    p <- NULL
  }
  return(p)
}
     
