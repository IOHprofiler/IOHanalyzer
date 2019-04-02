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

generate_rbga <- function(color,a){
  paste0('rgba(', paste0(color, collapse = ','), ',', a,')')
}

grad_functions <- c(
  scaled_edges = function(count, amount, intensity){
    scale <- (intensity + 1)/2
    color_end <- floor(scale*amount*2)
    if (count<color_end)
      1/color_end
    else
      0
  }
  ,
  fixed_edges = function(count, amount, intensity){
    scale <- (intensity + 1)/2
    color_center <- floor(scale*amount) + 1
    if (count<=color_center)
      1/(2*color_center)
    else
      1/(2*(amount-color_center))
  }
)

#S3 generics
#TODO: decide which parameters need to be in the generics

#' Plot lineplot of the ERTs of a DataSetList
#'
#' @inheritParams plot_RT_single_fct.DataSetList
#' @return A plot of ERT-values of the DataSetList
#' @export
#'
plot_RT_single_fct <- function(dsList, Fstart = NULL, Fstop = NULL,
                         show.ERT = T, show.CI = T, show.mean = F,
                         show.runs = F, show.density = 50,
                         show.grad = F, show.intensity = 0,
                         show.pareto = F, show.optimal = F,
                         show.median = F, backend = 'plotly',
                         scale.xlog = F, scale.ylog = F,
                         scale.reverse = F) UseMethod("plot_RT_single_fct", dsList)
#' Plot lineplot of the expected function values of a DataSetList
#'
#' @inheritParams plot_FV_line.DataSetList
#'
#' @return A plot of ERT-values of the DataSetList
#' @export
plot_FV_line <- function(dsList, RTstart = NULL, RTstop = NULL,
                         show.CI = T,
                         show.mean = F, show.median = F,
                         show.runs = F, show.density = 50,
                         show.grad = F, show.intensity = 0,
                         show.pareto = F, show.optimal = F,
                         backend = 'plotly',
                         scale.xlog = F, scale.ylog = F,
                         scale.reverse = F) UseMethod("plot_FV_line", dsList)
#' Plot probablity mass function of the runtimes of a DataSetList at a certain target function value
#'
#' @inheritParams plot_RT_PMF.DataSetList
#'
#' @return A plot of the probablity mass function of the runtimes at a the
#'         target function value of the DataSetList
#' @export
#'
plot_RT_PMF <- function(dsList, ftarget, show.sample = F,
                        scale.ylog = F, backend = 'plotly') UseMethod("plot_RT_PMF", dsList)
#' Plot histograms of the runtimes of a DataSetList at a certain target function value
#'
#' @inheritParams plot_RT_HIST.DataSetList
#'
#' @return A plot of the histograms of the runtimes at a the
#'         target function value of the DataSetList
#' @export
#'
plot_RT_HIST <- function(dsList, ftarget, plot_mode = 'overlay') UseMethod("plot_RT_HIST", dsList)
#' Plot the empirical cumulative distriburtion as a function of the running times of
#' a DataSetList at certain target function values
#'
#' @inheritParams plot_RT_ECDF.DataSetList
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the running times of the DataSetList at the target function values
#' @export
#'
plot_RT_ECDF <- function(dsList, ftargets, scale.xlog = F) UseMethod("plot_RT_ECDF", dsList)
#' Plot the aggregated empirical cumulative distriburtion as a function of the running times of
#' a DataSetList.
#'
#' @inheritParams plot_RT_ECDF_AGGR.DataSetList
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the running times of the DataSetList
#' @export
#'
plot_RT_ECDF_AGGR <- function(dsList, fstart = NULL, fstop = NULL,
                              fstep = NULL, show.per_target = F,
                              scale.xlog = F) UseMethod("plot_RT_ECDF_AGGR", dsList)
#' Radarplot of the area under the aggregated ECDF-curve of a DataSetList.
#'
#' @inheritParams plot_RT_AUC.DataSetList
#'
#' @return A radarplot of the area under the aggregated ECDF-curve of the DataSetList
#' @export
#'
plot_RT_AUC <- function(dsList, fstart = NULL,
                        fstop = NULL, fstep = NULL,
                        fval_formatter = as.integer) UseMethod("plot_RT_AUC", dsList)
#' Plot probablity density function of the function values of a DataSetList at
#' a certain target runtime
#'
#' @inheritParams plot_FV_PDF.DataSetList
#'
#' @return A plot of the probablity density function of the runtimes at a the
#'         target function value of the DataSetList
#' @export
#'
plot_FV_PDF <- function(dsList, runtime, show.sample = F, scale.ylog = F) UseMethod("plot_FV_PDF", dsList)
#' Plot histograms of the function values of a DataSetList at a certain target runtime
#'
#' @inheritParams plot_FV_HIST.DataSetList
#'
#' @return A plot of the histograms of the function values at a the
#'         target runtime of the DataSetList
#' @export
#'
plot_FV_HIST <- function(dsList, runtime, plot_mode='overlay') UseMethod("plot_FV_HIST", dsList)
#' Plot the empirical cumulative distriburtion as a function of the target values of
#' a DataSetList at certain target runtimes
#'
#' @inheritParams plot_FCE_ECDF_PER_TARGET.DataSetList
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the fucntion values of the DataSetList at the target runtimes
#' @export
#'
plot_FCE_ECDF_PER_TARGET <- function(dsList, runtimes, scale.xlog = F) UseMethod("plot_FCE_ECDF_PER_TARGET", dsList)
#' Plot the aggregated empirical cumulative distriburtion as a function of the function values of
#' a DataSetList.
#'
#' @inheritParams plot_FV_ECDF_AGGR.DataSetList
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the function values of the DataSetList
#' @export
#'
plot_FV_ECDF_AGGR <- function(dsList, rt_min = NULL, rt_max = NULL,
                              rt_step = NULL, scale.xlog = F,
                              show.per_target = F) UseMethod("plot_FV_ECDF_AGGR", dsList)
#' Radarplot of the area under the aggregated ECDF-curve of a DataSetList.
#'
#' @inheritParams plot_FV_AUC.DataSetList
#'
#' @return A radarplot of the area under the aggregated ECDF-curve of the DataSetList
#' @export
#'
plot_FV_AUC <- function(dsList, rt_min = NULL, rt_max = NULL,
                        rt_step = NULL) UseMethod("plot_FV_AUC", dsList)
#' Plot the parameter values recorded in a DataSetList
#'
#' @inheritParams plot_PAR_Line.DataSetList
#'
#' @return A plot of for every recorded parameter in the DataSetList
#' @export
#'
plot_PAR_Line <- function(dsList, f_min = NULL, f_max = NULL,
                          algids = 'all',
                          scale.xlog = F, scale.ylog = F,
                          show.mean = T, show.median = F) UseMethod("plot_PAR_Line", dsList)
#' Plot the aggregated empirical cumulative distriburtion as a function of the running times of
#' a DataSetList. Aggregated over multiple functions or dimensions.
#'
#' @inheritParams plot_RT_ECDF_MULTI.DataSetList
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the running times of the DataSetList
#' @export
#'
plot_RT_ECDF_MULTI <- function(dsList, targets = NULL) UseMethod("plot_RT_ECDF_MULTI", dsList)
#' Plot ERT-plots for multiple functions or dimensions
#'
#' @inheritParams plot_RT_all_fcts.DataSetList
#'
#' @return A plot of ERT-values of the DataSetList
#' @export
#'
plot_RT_all_fcts <- function(dsList, scale.xlog = F,
                             scale.ylog = F,
                             scale.reverse = F,
                             backend = 'plotly') UseMethod("plot_RT_all_fcts", dsList)
#' Plot ERT-based comparison over multiple functions or dimensions
#'
#' @inheritParams plot_ERT_AGGR.DataSetList
#'
#' @return A plot of ERT-based comparison on the provided functions or dimensions of the DataSetList
#' @export
#'
plot_ERT_AGGR <- function(dsList, aggr_on = 'funcId', targets = NULL,
                          plot_mode = 'radar', use_rank = F,
                          scale.ylog = T, maximize = T,
                          erts = NULL) UseMethod("plot_ERT_AGGR", dsList)
#' Plot expected function value-based comparison over multiple functions or dimensions
#'
#' @inheritParams plot_FCE_AGGR.DataSetList
#'
#' @return A plot of expected function value-based comparison on the provided functions
#'  or dimensions of the DataSetList
#' @export
#'
plot_FCE_AGGR <- function(dsList, aggr_on = 'funcId', runtimes = NULL,
                          plot_mode = 'radar', use_rank = F,
                          scale.ylog = T, fvs = NULL) UseMethod("plot_FCE_AGGR", dsList)
#' Plot expected function value-plots for multiple functions or dimensions
#'
#' @inheritParams plot_FV_all_fcts.DataSetList
#'
#' @return A plot of expected function values of the DataSetList
#' @export
#'
plot_FV_all_fcts <- function(dsList, scale.xlog = F,
                             scale.ylog = F,
                             backend = 'plotly') UseMethod("plot_FV_all_fcts", dsList)

##Implementations

#' Plot lineplot of the ERTs of a DataSetList
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param Fstart The starting function value.
#' @param Fstop The final function value.
#' @param show.ERT Whether or not to show the ERT-values
#' @param show.CI Whether or not to show the standard deviations
#' @param show.mean Whether or not to show the mean hitting times
#' @param show.median Whether or not to show the median hitting times
#' @param show.runs Whether or not to show the individual runs
#' @param show.density Percentage of individual runs to show when show.runs is true
#' @param show.pareto Whether or not to show the pareto-optimal run
#' @param show.optimal Whether or not to show the optimal run
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param scale.reverse Wheter or not to reverse the x-axis (when using minimization)
#' @param backend Which plotting library to use. Can be 'plotly' or 'ggplot2'
#' @param show.grad Whether or not to show the run intensity
#' @param show.intensity Intensity to use when using show.grad
#'
#' @return A plot of ERT-values of the DataSetList
#' @export
#'
plot_RT_single_fct.DataSetList <- function(dsList, Fstart = NULL, Fstop = NULL,
                                           show.ERT = T, show.CI = T, show.mean = F,
                                           show.runs = F, show.density = 50,
                                           show.grad = F, show.intensity = 0,
                                           show.pareto = F, show.optimal = F,
                                           show.median = F, backend = 'plotly',
                                           scale.xlog = F, scale.ylog = F,
                                           scale.reverse = F) {

  Fall <- get_funvals(dsList)
  if (is.null(Fstart)) Fstart <- min(Fall)
  if (is.null(Fstop)) Fstop <- max(Fall)

  Fseq <- seq_FV(Fall, Fstart, Fstop, length.out = 60, 
                 scale = ifelse(scale.xlog, 'log', 'linear'))
  
  if (length(Fseq) == 0) return(NULL)

  N <- length(dsList)
  legends <- get_legends(dsList)
  colors <- color_palettes(N)

  dt <- get_RT_summary(dsList, ftarget = Fseq)
  dt[, `:=`(upper = mean + sd, lower = mean - sd)]
  
  dr <- get_RT_sample(dsList, Fseq)
  run.names <- grep('run', names(dr),  value = T)

  if (backend == 'plotly') {
    p <- plot_ly_default(x.title = "best-so-far f(x)-value",
                         y.title = "function evaluations")
    
    # TODO: improve this part, get rid of the loop
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

      if (show.runs||show.grad) {
        
        dr_ERT <- dr[algId == attr(dsList[[i]], 'algId')&
                       funcId == attr(dsList[[i]], 'funcId') &
                       DIM == attr(dsList[[i]], 'DIM')]
        
        counter <- ceiling(length(run.names) * show.density / 100)
        names_to_show <- sample(run.names)[1:counter]
        
        fill_density <- 0
        all_names <- names_to_show
        
        index <- apply(!is.na(dr_ERT[, ..names_to_show]), 1, any)
        dr_ERT <- dr_ERT[index, ]
        best_parts <- apply(dr_ERT[, ..names_to_show], 1, . %>% min(na.rm = T))
        mentioned <- FALSE
        
        if (show.grad) {
          dr_ERT_ <- dr_ERT[complete.cases(dr_ERT)]
          sorted_dr_ERT <- apply(dr_ERT_[, -c('algId', 'target', 'funcId', 'DIM')], 1, function(x){sort(x, decreasing = FALSE, na.last = T)})
          
          if (is.matrix(sorted_dr_ERT)) {
            counter = 0
            names_amount = length(all_names)
            
            for (counter in c(1:length(all_names))){
              fill_density <- fill_density + grad_functions$fixed_edges(counter,names_amount,show.intensity)
              rgba_str_m  <- generate_rbga(col2rgb(colors[i]),fill_density)
              p %<>% add_trace(x = dr_ERT_[['target']], y = sorted_dr_ERT[counter,],
                               type = 'scatter', mode = 'none',
                               hoverinfo = 'none',
                               showlegend = F,
                               fill = 'tonexty',
                               fillcolor = rgba_str_m
              )
            }
            rgba_str_m  <- generate_rbga(col2rgb(colors[i]),1)
            upper_border <- max(sorted_dr_ERT, na.rm = T)
            p %<>% add_trace(x = dr_ERT_[['target']], y = seq(upper_border, upper_border, length.out = length(dr_ERT_[['target']])),
                             type = 'scatter', mode = 'none',
                             hoverinfo = 'none',
                             showlegend = F,
                             fill = 'tonexty',
                             fillcolor = rgba_str_m
            )
          }
        }
        if (show.runs){
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
              legendgroup = legend,
              name = paste("runs of ", algId)
            )
            mentioned <- TRUE
            # best_parts <-
              # insert_best_parts(best_parts, dr_ERT[[run_v]], attr(dsList[[i]],"maximization"))
          }
          
          if (show.optimal) {
            mentioned <- FALSE
            NonNAindex <- which(!is.na(best_parts))
            target_idx <- max(NonNAindex)
            
            check_value <- best_parts[target_idx]
            for (run_v in names_to_show){
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
    }
    p %<>%
      layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')),
             yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))

    if (scale.reverse)
      p %<>% layout(xaxis = list(autorange = "reversed"))

  } else if (backend == 'ggplot2') {
    dt[, 'group' := paste(algId, funcId, DIM, sep = '-')]
    p <- ggplot(data = dt, aes(group = 'group', colour = 'group'))

    if (show.CI) p <- p + geom_ribbon(aes(target, ymin = lower, ymax = upper, fill = 'group'),
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

#' Plot lineplot of the expected function values of a DataSetList
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param RTstart The starting runtime value.
#' @param RTstop The final runtime value.
#' @param show.CI Whether or not to show the standard deviations
#' @param show.mean Whether or not to show the mean runtimes
#' @param show.median Whether or not to show the median runtimes
#' @param show.runs Whether or not to show the individual runs
#' @param show.density Percentage of individual runs to show when show.runs is true
#' @param show.pareto Whether or not to show the pareto-optimal run
#' @param show.optimal Whether or not to show the optimal run
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param scale.reverse Wheter or not to reverse the x-axis (when using minimization)
#' @param backend Which plotting library to use. Can be 'plotly' or 'ggplot2'
#' @param show.grad Whether or not to show the run intensity
#' @param show.intensity Intensity to use when using show.grad
#'
#' @return A plot of ERT-values of the DataSetList
#' @export
#'
plot_FV_line.DataSetList <- function(dsList, RTstart = NULL, RTstop = NULL,
                                     show.CI = T,
                                     show.mean = F, show.median = F,
                                     show.runs = F, show.density = 50,
                                     show.grad = F, show.intensity = 0,
                                     show.pareto = F, show.optimal = F,
                                     backend = 'plotly',
                                     scale.xlog = F, scale.ylog = F,
                                     scale.reverse = F) {

  RTall <- get_runtimes(dsList)
  if (is.null(RTstart)) Fstart <- min(RTall)
  if (is.null(RTstop)) Fstop <- max(RTall)


  RTseq <- seq_RT(RTall, RTstart, RTstop, length.out = 60, scale = ifelse(scale.xlog,'log','linear'))
  if (length(RTseq) == 0) return(NULL)

  N <- length(dsList)
  colors <- color_palettes(N)
  legends <- get_legends(dsList)

  fce <- get_FV_summary(dsList, RTseq)
  fce[, `:=`(upper = mean + sd, lower = mean - sd)]

  if (backend == 'plotly') {
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

      if (show.CI) {
        p %<>%
          add_trace(data = ds_FCE, x = ~runtime, y = ~upper, type = 'scatter', mode = 'lines',
                    line = list(color = rgba_str, width = 0), legendgroup = legend,
                    showlegend = F, name = 'mean +/- sd') %>%
          add_trace(x = ~runtime, y = ~lower, type = 'scatter', mode = 'lines',
                    fill = 'tonexty',  line = list(color = 'transparent'),
                    legendgroup = legend,
                    fillcolor = rgba_str, showlegend = F, name = 'mean +/- sd')
      }

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

      if (show.runs || show.grad) {
        fce_runs <- get_FV_sample(dsList, RTseq)
        
        fce_runs_ERT <- fce_runs[algId == attr(dsList[[i]], 'algId') &
                                   funcId == attr(dsList[[i]], 'funcId') &
                                   DIM == attr(dsList[[i]], 'DIM')]
        names_to_show <- sample(colnames(fce_runs_ERT))
        names_to_show <-
          names_to_show[!names_to_show %in% c('algId', 'runtime', 'funcId', 'DIM')]
        
        counter <- as.integer(length(names_to_show) * show.density / 100) + 1
        fill_density <- 0
        all_names <- names_to_show
        names_to_show <- head(names_to_show, counter)
        best_parts <- NA
        mentioned <- FALSE
        
        if (show.grad) {
          sorted_fce_runs_ERT <- apply((fce_runs_ERT[, -c('algId', 'runtime', 'funcId', 'DIM')]), 1, function(x){sort(x,decreasing = TRUE)})
          counter = 0
          names_amount = length(all_names)
          
          for (counter in c(1:length(all_names))) {
            fill_density <- fill_density + grad_functions$fixed_edges(counter,names_amount,show.intensity)
            rgba_str_m  <- generate_rbga(col2rgb(colors[i]),fill_density)
            p %<>% add_trace(x = fce_runs_ERT[['runtime']], y = sorted_fce_runs_ERT[counter,],
                             type = 'scatter', mode = 'none',
                             hoverinfo = 'none',
                             showlegend = F,
                             fill = 'tonexty',
                             fillcolor = rgba_str_m
            )
          }
          rgba_str_m  <- generate_rbga(col2rgb(colors[i]),1)
          lower_border <- min(sorted_fce_runs_ERT)
          p %<>% add_trace(x = fce_runs_ERT[['runtime']], y = seq(lower_border, lower_border, length.out = length(fce_runs_ERT[['runtime']])),
                           type = 'scatter', mode = 'none',
                           hoverinfo = 'none',
                           showlegend = F,
                           fill = 'tonexty',
                           fillcolor = rgba_str_m
          )
        }
        
        if (show.runs) {
          mentioned <- FALSE
          for (run_v in names_to_show) {
            p %<>% add_trace(
              data = fce_runs_ERT,
              x = ~ runtime,
              y = fce_runs_ERT[[run_v]],
              type = 'scatter',
              mode = 'lines',
              line = list(color = rgb_str, width = 0.5),
              showlegend = !mentioned,
              text = paste(run_v),
              hoverinfo = 'none',
              name = paste("runs of ", algId)
            )
            mentioned <- TRUE
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
    }
    p %<>%
      layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')),
             yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))

    if (scale.reverse)
      p %<>% layout(xaxis = list(autorange = "reversed"))
  } else if (backend == 'ggplot2') {
    fce[, 'group' := paste(algId, funcId, DIM, sep = '-')]
    p <- ggplot(data = fce, aes(group = 'group', colour = 'group'))

    if (show.mean) p <- p + geom_line(aes(runtime, mean), linetype = 'dashed')
    if (show.median) p <- p + geom_line(aes(runtime, median), linetype = 'dotted')

    p <- p +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)

    #TODO: add individual run etc
  }
  return(p)
}

#' Plot probablity mass function of the runtimes of a DataSetList at a certain target function value
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param ftarget The target function value.
#' @param show.sample Whether or not to show the individual runtime samples
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param backend Which plotting library to use. Can be 'plotly' or 'ggplot2'
#'
#' @return A plot of the probablity mass function of the runtimes at a the
#'         target function value of the DataSetList
#' @export
#'
plot_RT_PMF.DataSetList <- function(dsList, ftarget, show.sample = F,
                                    scale.ylog = F, backend = 'plotly'){

  points <- ifelse(show.sample, 'all', FALSE)

  N <- length(dsList)
  colors <- color_palettes(N)

  p <- plot_ly_default(x.title = "algorithms",
                       y.title = "runtime / function evaluations")

  for (i in seq_along(dsList)) {
    ds <- dsList[[i]]

    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.52)')

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
                line = list(color = 'black', width = 1.1),
                marker = list(color = rgb_str, size = 6))

  }
  p %<>%
    layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
           xaxis = list(tickangle = 45))
}

#' Plot histograms of the runtimes of a DataSetList at a certain target function value
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param ftarget The target function value.
#' @param plot_mode How to plot the different hisograms for each algorithm. Can be either
#'  'overlay' to show all algorithms on one plot, or 'subplot' to have one plot per algorithm.
#'
#' @return A plot of the histograms of the runtimes at a the
#'         target function value of the DataSetList
#' @export
#'
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

#' Plot the empirical cumulative distriburtion as a function of the running times of
#' a DataSetList at certain target function values
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param ftargets The target function values
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the running times of the DataSetList at the target function values
#' @export
#'
plot_RT_ECDF.DataSetList <- function(dsList, ftargets, scale.xlog = F){
  req(length(ftargets) != 0)

  N <- length(dsList)
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

      # TODO: Fix this function to use the updated ECDF-function correctely
      ecdf <- ECDF(df, ftargets[[i]])
      vals <- ecdf(rt)
      # # position of the markers
      # x <- quantile(rt, probs = c(0.25, 0.5, 0.75), names = F, type = 3)
      # y <- sapply(x, function(x) ecdf[rt == x][1])

      p %<>%
        add_trace(data = NULL, x = rt, y = vals, type = 'scatter',
                  mode = 'lines', name = algId, showlegend = (i == 1),
                  legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 2))
      # add_trace(data = NULL, x = x, y = y, type = 'scatter',
      #           mode = 'markers',  legendgroup = paste0(k),
      #           name = sprintf('(%s, %.2e)', algId, ftargets[i]),
      #           marker = list(color = rgb_str, symbol = symbols[i], size = 13))
    }
  }

  p %<>%
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')))
  p
}

#' Plot the aggregated empirical cumulative distriburtion as a function of the running times of
#' a DataSetList.
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param fstart The starting function value
#' @param fstop The final function value
#' @param fstep The spacing between starting and final function values
#' @param show.per_target Whether or not to show the individual ECDF-curves for each target
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the running times of the DataSetList
#' @export
#'
plot_RT_ECDF_AGGR.DataSetList <- function(dsList, fstart = NULL, fstop = NULL,
                                          fstep = NULL, show.per_target = F,
                                          scale.xlog = F) {
  fall <- get_funvals(dsList)
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
                line = list(color = rgb_str, width = 2),
                marker = list(color = rgb_str, size = 9))

    if (show.per_target) {
      for (f in fseq) {
        rt <- get_RT_sample(df, f, output = 'long') %>% '$'('RT') %>% sort
        # TODO: plot the unsuccessful ECDF
        if (all(is.na(rt)))
          next
        else{
          ecdf <- ECDF(df, f)
          v <- ecdf(rt)
        }
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

#' Radarplot of the area under the aggregated ECDF-curve of a DataSetList.
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param fstart The starting function value
#' @param fstop The final function value
#' @param fstep The spacing between starting and final function values
#' @param fval_formatter Function to format the function-value labels
#'
#' @return A radarplot of the area under the aggregated ECDF-curve of the DataSetList
#' @export
#'
plot_RT_AUC.DataSetList <- function(dsList, fstart = NULL,
                                    fstop = NULL, fstep = NULL,
                                    fval_formatter = as.integer) {
  fall <- get_funvals(dsList)
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

#' Plot probablity density function of the function values of a DataSetList at
#' a certain target runtime
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param runtime The target runtime
#' @param show.sample Whether or not to show the individual function value samples
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#'
#' @return A plot of the probablity density function of the runtimes at a the
#'         target function value of the DataSetList
#' @export
#'
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

#' Plot histograms of the function values of a DataSetList at a certain target runtime
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param runtime The target runtime
#' @param plot_mode How to plot the different hisograms for each algorithm. Can be either
#'  'overlay' to show all algorithms on one plot, or 'subplot' to have one plot per algorithm.
#'
#' @return A plot of the histograms of the function values at a the
#'         target runtime of the DataSetList
#' @export
#'
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
    if (sum(!is.na(fce$'f(x)')) < 2)
      next

    res <- hist(fce$'f(x)', breaks = nclass.FD, plot = F)
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

#' Plot the empirical cumulative distriburtion as a function of the target values of
#' a DataSetList at certain target runtimes
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param runtimes The target runtimes
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the fucntion values of the DataSetList at the target runtimes
#' @export
#'
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
      funvals <- get_FV_sample(ds, runtimes[i], output = 'long')$'f(x)' %>% sort

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

#' Plot the aggregated empirical cumulative distriburtion as a function of the function values of
#' a DataSetList.
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param rt_min The starting runtime
#' @param rt_max The final runtime
#' @param rt_step The spacing between starting and final runtimes
#' @param show.per_target Whether or not to show the individual ECDF-curves for each runtime
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the function values of the DataSetList
#' @export
#'
plot_FV_ECDF_AGGR.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL,
                                          rt_step = NULL, scale.xlog = F,
                                          show.per_target = F){

  rt <- get_runtimes(dsList)
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

    fun <- get_FV_sample(ds, rt_seq, output = 'long')$'f(x)' %>% ecdf
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
        ce <- get_FV_sample(ds, r, output = 'long') %>% '$'('f(x)') %>% sort
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

#' Radarplot of the area under the aggregated ECDF-curve of a DataSetList.
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param rt_min The starting runtime
#' @param rt_max The final runtime
#' @param rt_step The spacing between starting and final runtimes
#'
#' @return A radarplot of the area under the aggregated ECDF-curve of the DataSetList
#' @export
#'
plot_FV_AUC.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL, rt_step = NULL) {
  rt <- get_runtimes(dsList)
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
      get_FV_sample(df, r, output = 'long')$'f(x)' %>% {
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

#' Plot the parameter values recorded in a DataSetList
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param f_min The starting function value.
#' @param f_max The final function value.
#' @param show.mean Whether or not to show the mean parameter values
#' @param show.median Whether or not to show the median parameter values
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param algids Which algorithms from dsList to use
#'
#' @return A plot of for every recorded parameter in the DataSetList
#' @export
#'
plot_PAR_Line.DataSetList <- function(dsList, f_min = NULL, f_max = NULL,
                                      algids = 'all',
                                      scale.xlog = F, scale.ylog = F,
                                      show.mean = T, show.median = F){
  #TODO: clean this up
  req(xor(show.mean,show.median))

  fall <- get_funvals(dsList)
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

#' Plot the aggregated empirical cumulative distriburtion as a function of the running times of
#' a DataSetList. Aggregated over multiple functions or dimensions.
#'
#' @param dsList A DataSetList.
#' @param targets The target function values. Specified in a data.frame, as can be generated
#' by the function 'get_default_ECDF_targets'
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the running times of the DataSetList
#' @export
#'
plot_RT_ECDF_MULTI.DataSetList <- function(dsList, targets = NULL){

  if (is.null(targets))
    targets <- get_default_ECDF_targets(dsList)

  algId <- unique(attr(dsList, 'algId'))
  p <- plot_ly_default(x.title = "function evaluations",
                       y.title = "Proportion of (run, target, ...) pairs")

  rts <- get_runtimes(dsList)
  x <- seq(min(rts), max(rts), length.out = 50)
  colors <- color_palettes(length(algId))

  for (i in seq_along(algId)) {
    Id <- algId[i]
    data <- subset(dsList, algId == Id)
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')

    fun <- ECDF(data, ftarget = targets, funcId = as.integer(names(targets)))
    if (is.null(fun)) next

    df_plot <- data.frame(x = x, ecdf = fun(x))
    p %<>% add_trace(data = df_plot, x = ~x, y = ~ecdf, type = 'scatter',
                     mode = 'lines+markers', name = sprintf('%s', Id),
                     showlegend = T,
                     line = list(color = rgb_str, width = 3),
                     marker = list(color = rgb_str, size = 10))
  }
  p

}

#' Plot ERT-plots for multiple functions or dimensions
#'
#' @param dsList A DataSetList (should consist of only one function OR dimension).
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param scale.reverse Wheter or not to reverse the x-axis (when using minimization)
#' @param backend Which plotting library to use. Either 'plotly' or 'ggplot2'.
#'
#' @return A plot of ERT-values of the DataSetList
#' @export
#'
plot_RT_all_fcts.DataSetList <- function(dsList, scale.xlog = F,
                                         scale.ylog = F,
                                         scale.reverse = F,
                                         backend = 'plotly') {
  xscale <- if (scale.xlog) 'log' else 'linear'
  yscale <- if (scale.ylog) 'log' else 'linear'
  funcIds <- get_funcId(dsList)
  n_fcts <- length(funcIds)

  algIds <- get_algId(dsList)
  n_algIds <- length(algIds)

  colors <- color_palettes(n_algIds)
  names(colors) <- algIds

  # how many columns do we want...
  if (n_fcts <= 10) {
    n_rows <- ceiling(n_fcts / 2.)
    n_cols <- 2
  } else if (n_fcts <= 20) {
    n_rows <- ceiling(n_fcts / 3.)
    n_cols <- 3
  } else if (n_fcts <= 30) {
    n_rows <- ceiling(n_fcts / 4.)
    n_cols <- 4
  }

  dt <- list()
  for (i in seq(n_fcts)) {
    data <- subset(dsList, funcId == funcIds[i])
    
    Fall <- get_funvals(data)
    Fstart <- min(Fall)
    Fstop <- max(Fall)
    Fseq <- seq_FV(Fall, Fstart, Fstop, length.out = 30, scale = xscale)

    if (length(Fseq) == 0) return(NULL)

    dt[[i]] <- get_RT_summary(data, ftarget = Fseq)
  }
  dt <- rbindlist(dt)

  if (backend == 'ggplot2') {
    dt[, funcId := paste0('F', funcId)]

    p <- ggplot(data = dt, aes(group = algId, colour = algId)) +
      geom_line(aes(target, ERT), linetype = 'solid') +
      facet_wrap(~funcId, scales = 'free', nrow = n_rows, ncol = n_cols) +
      scale_color_manual(values = colors)

  } else if (backend == 'plotly') {
    autorange <- ifelse(scale.reverse, 'reversed', T)
    p <- lapply(
      seq(n_fcts),
      function(x)
        plot_ly_default(x.title = "", y.title = "ERT") %>%
          layout(xaxis = list(type = xscale, tickfont = f1, ticklen = 4, autorange = autorange),
                 yaxis = list(type = yscale, tickfont = f1, ticklen = 4))
    )

    for (i in seq(n_fcts)) {
      showlegend <- ifelse(i == 1, T, F)
      dt_plot <- dt[funcId == funcIds[[i]]]

      p[[i]] %<>%
        add_trace(
          data = dt_plot, x = ~target, y = ~ERT, color = ~algId, legendgroup = ~algId,
          type = 'scatter', mode = 'lines+markers',
          line = list(width = 1.8), marker = list(size = 4), # TODO: perhaps turn off the marker here
          colors = colors, showlegend = showlegend
        ) %>%
        layout(
          annotations = list(
            text = paste0('F', funcIds[[i]]), font = f2, align = "center",
            xref = "paper", yref = "paper",
            yanchor = "bottom", xanchor = "center",
            x = 0.5, y = 1, showarrow = FALSE
          )
        )
    }

    p <- subplot(p, nrows = n_rows, titleX = F, titleY = F, margin = 0.02,
                 heights = rep(1 / n_rows, n_rows),
                 widths = rep(1 / n_cols, n_cols))
  }
  p
}

#' Plot FV-plots for multiple functions or dimensions
#'
#' @param dsList A DataSetList (should consist of only one function OR dimension).
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param backend Which plotting library to use. Either 'plotly' or 'ggplot2'.
#'
#' @return A plot of Function-values of the DataSetList
#' @export
#'
plot_FV_all_fcts.DataSetList <- function(dsList, scale.xlog = F,
                                         scale.ylog = F,
                                         backend = 'plotly') {
  xscale <- if (scale.xlog) 'log' else 'linear'
  yscale <- if (scale.ylog) 'log' else 'linear'
  funcIds <- get_funcId(dsList)
  n_fcts <- length(funcIds)

  algIds <- get_algId(dsList)
  n_algIds <- length(algIds)

  colors <- color_palettes(n_algIds)
  names(colors) <- algIds

  # how many columns do we want...
  if (n_fcts <= 10) {
    n_rows <- ceiling(n_fcts / 2.)
    n_cols <- 2
  } else if (n_fcts <= 20) {
    n_rows <- ceiling(n_fcts / 3.)
    n_cols <- 3
  } else if (n_fcts <= 30) {
    n_rows <- ceiling(n_fcts / 4.)
    n_cols <- 4
  }

  dt <- list()
  for (i in seq(n_fcts)) {
    data <- subset(dsList, funcId == funcIds[i])

    RTall <- get_runtimes(data)
    RTstart <- min(RTall)
    RTstop <- max(RTall)
    RTseq <- seq_FV(RTall, RTstart, RTstop, length.out = 30, scale = xscale)

    if (length(RTseq) == 0) return(NULL)

    dt[[i]] <- get_FV_summary(data, runtime = RTseq)
  }
  dt <- rbindlist(dt)

  if (backend == 'ggplot2') {
    dt[, funcId := paste0('F', funcId)]

    p <- ggplot(data = dt, aes(group = algId, colour = algId)) +
      geom_line(aes(runtime, `mean`), linetype = 'solid') +
      facet_wrap(~funcId, scales = 'free', nrow = n_rows, ncol = n_cols) +
      scale_color_manual(values = colors)

  } else if (backend == 'plotly') {
    p <- lapply(
      seq(n_fcts),
      function(x)
        plot_ly_default(x.title = "", y.title = "mean function value") %>%
        layout(xaxis = list(type = xscale, tickfont = f1, ticklen = 4, autorange = T),
               yaxis = list(type = yscale, tickfont = f1, ticklen = 4))
    )

    for (i in seq(n_fcts)) {
      showlegend <- ifelse(i == 1, T, F)
      dt_plot <- dt[funcId == funcIds[[i]]]

      p[[i]] %<>%
        add_trace(
          data = dt_plot, x = ~runtime, y = ~`mean`, color = ~algId, legendgroup = ~algId,
          type = 'scatter', mode = 'lines+markers',
          line = list(width = 1.8), marker = list(size = 4), # TODO: perhaps turn off the marker here
          colors = colors, showlegend = showlegend
        ) %>%
        layout(
          annotations = list(
            text = paste0('F', funcIds[[i]]), font = f2, align = "center",
            xref = "paper", yref = "paper",
            yanchor = "bottom", xanchor = "center",
            x = 0.5, y = 1, showarrow = FALSE
          )
        )
    }

    p <- subplot(p, nrows = n_rows, titleX = F, titleY = F, margin = 0.02,
                 heights = rep(1 / n_rows, n_rows),
                 widths = rep(1 / n_cols, n_cols))
  }
  p
}

#' Plot ERT-based comparison over multiple functions or dimensions
#'
#' @param dsList A DataSetList (should consist of only one function OR dimension).
#' @param plot_mode How the plots should be created. Can be 'line' or 'radar'
#' @param aggr_on Whether to compare on functions ('funcId') or dimensions ('DIM')
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param maximize Wheter or not to the data is of a maximization problem
#' @param targets Custom list of function-value targets, one for each function or dimension.
#' @param use_rank Wheter to use a ranking system. If False, the actual ERT-values will be used.
#' @param erts Pre-calculated ERT-values for the provided targets. Created by the max_ERTs function
#' of DataSetList. Can be provided to prevent needless computation in recalculating ERTs when recreating
#' this plot.
#'
#' @return A plot of ERT-based comparison on the provided functions or dimensions of the DataSetList
#' @export
#'
plot_ERT_AGGR.DataSetList <- function(dsList, aggr_on = 'funcId', targets = NULL,
                                      plot_mode = 'radar', use_rank = F,
                                      scale.ylog = T, maximize = T,
                                      erts = NULL) {
  if (is.null(erts))
    erts <- max_ERTs(dsList, aggr_on = aggr_on, targets = targets, maximize = maximize)
  if (is.null(erts))
    return(NULL)

  N <- length(get_algId(dsList))
  colors <- color_palettes(N)

  in_legend <- integer(N)
  names(in_legend) <- get_algId(dsList)
  names(colors) <- get_algId(dsList)

  aggr_attr <- if(aggr_on == 'funcId') get_funcId(dsList) else get_dim(dsList)
  if(!is.null(targets) && length(targets) != length(aggr_attr)) targets <- NULL

  second_aggr <- if(aggr_on == 'funcId') get_dim(dsList) else get_funcId(dsList)
  if(length(second_aggr) >1 ) return(NULL)

  plot_title <- paste0(ifelse(aggr_on == 'funcId', "Dimension ", "Function "), second_aggr[[1]])

  p <- if(plot_mode == "radar")  plot_ly_default(title = plot_title, x.title = ifelse(aggr_on == "funcid", "Function", "Dimension"), y.title = "ERT")
  else plot_ly_default(title = plot_title)

  if (use_rank){
    ertranks <- seq(0, 0, length.out = length(get_algId(dsList)))
    for (i in seq_along(aggr_attr)){
      ertranks <- rbind(ertranks, rank(erts[i, ]))
    }
    dataert <- ertranks[-1, ]
  }
  else {
    dataert <- erts
  }

  for (i in seq_along(get_algId(dsList))){
    algId <- get_algId(dsList)[[i]]
    color <- colors[[algId]]
    data <- dataert[,i]
    rgb_str <- paste0('rgb(', paste0(col2rgb(color), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.35)')
    if(plot_mode == "radar"){
      p %<>%
        add_trace(type = 'scatterpolar', r = data,
                  theta = paste0(ifelse(aggr_on == "funcId", "F", "D"),aggr_attr),
                  fill = 'toself', connectgaps = T, fillcolor = rgba_str,
                  marker = list(color = rgb_str), hoverinfo = 'text',
                  text = paste0('ERT: ', format(erts[,i], digits = 3, nsmall = 3)),
                  name = algId, legendgroup = algId)
      #TODO: cleaner solution!!!!!
      data2 <- data
      data2[is.infinite(data2)] <- 10e7 * (1+(i/20))
      data2[data2<10e7] <- NA
      p %<>%
        add_trace(type='scatterpolar', mode='markers', r = data2,
                  theta = paste0(ifelse(aggr_on == "funcId", "F", "D"),aggr_attr),
                  marker = list(color = rgb_str, symbol = 'diamond', size = '10'), hoverinfo = 'text',
                  text = paste0('ERT: ', format(erts[,i], digits = 3, nsmall = 3)),
                  showlegend = F, legendgroup = algId)
      data2 <- data
      data2[is.na(data2)] <- 10e7 * (1+(i/20))
      data2[data2<10e7] <- NA
      p %<>%
        add_trace(type='scatterpolar', mode='markers', r = data2,
                  theta = paste0(ifelse(aggr_on == "funcId", "F", "D"),aggr_attr),
                  marker = list(color = rgb_str, symbol = 'x', size = '10'), hoverinfo = 'text',
                  text = paste0('ERT: ', format(erts[,i], digits = 3, nsmall = 3)),
                  showlegend = F, legendgroup = algId)
    }
    else{
      p %<>% add_trace(x = aggr_attr, y = data, type = 'scatter',
                       mode = 'lines+markers',
                       marker = list(color = rgb_str), hoverinfo = 'text',
                       text = paste0('ERT: ', format(erts[,i], digits = 3, nsmall = 3)),
                       line = list(color = rgb_str), name = algId, legendgroup = algId)
      data2 <- data
      data2[is.infinite(data2)] <- 10e7 * (1+(i/20))
      data2[data2<10e7] <- NA
      p %<>%
        add_trace(type='scatter', mode='markers', x = aggr_attr, y = data2,
                  marker = list(color = rgb_str, symbol = 'diamond', size = '10'), hoverinfo = 'text',
                  text = paste0('ERT: ', format(erts[,i], digits = 3, nsmall = 3)),
                  showlegend = F, legendgroup = algId )
      data2 <- data
      data2[is.na(data2)] <- 10e7 * (1+(i/20))
      data2[data2<10e7] <- NA
      p %<>%
        add_trace(type='scatter', mode='markers', x = aggr_attr, y = data2,
                  marker = list(color = rgb_str, symbol = 'x', size = '10'), hoverinfo = 'text',
                  text = paste0('ERT: ', format(erts[,i], digits = 3, nsmall = 3)),
                  showlegend = F, legendgroup = algId )

    }
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
      layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
             xaxis = list(type = ifelse(aggr_on != 'funcId', 'log', 'linear')))
    else
      p %<>%
      layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
             xaxis = list(type = ifelse(aggr_on != 'funcId', 'log', 'linear')))
  }
  p
}

#' Plot expected function value-based comparison over multiple functions or dimensions
#'
#' @param dsList A DataSetList (should consist of only one function OR dimension).
#' @param plot_mode How the plots should be created. Can be 'line' or 'radar'
#' @param aggr_on Whether to compare on functions ('funcId') or dimensions ('DIM')
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param runtimes Custom list of function-value targets, one for each function or dimension.
#' @param use_rank Wheter to use a ranking system. If False, the actual expected function-
#' values will be used.
#' @param fvs Pre-calculated expected function-values for the provided runtimes Created by the
#' max_ERTs function of DataSetList. Can be provided to prevent needless computation
#' in recalculating ERTs when recreating this plot.
#'
#' @return A plot of expected function value-based comparison on the provided functions
#'  or dimensions of the DataSetList
#' @export
#'
plot_FCE_AGGR.DataSetList <- function(dsList, aggr_on = 'funcId', runtimes = NULL,
                                      plot_mode = 'radar', use_rank = F,
                                      scale.ylog = T, fvs = NULL){
  if (is.null(fvs))
    fvs <- mean_FVs(dsList, aggr_on = aggr_on, runtimes = runtimes)
  if (is.null(fvs))
    return(NULL)

  N <- length(get_algId(dsList))
  colors <- color_palettes(N)

  in_legend <- integer(N)
  names(in_legend) <- get_algId(dsList)
  names(colors) <- get_algId(dsList)

  aggr_attr <- if(aggr_on == 'funcId') get_funcId(dsList) else get_dim(dsList)
  if(!is.null(runtimes) && length(runtimes) != length(aggr_attr)) runtimes <- NULL

  second_aggr <- if(aggr_on == 'funcId') get_dim(dsList) else get_funcId(dsList)
  if(length(second_aggr) >1 ) return(NULL)

  plot_title <- paste0(ifelse(aggr_on == 'funcId', "Dimension ", "Function "), second_aggr[[1]])

  p <- if(plot_mode == "radar")  plot_ly_default(title = plot_title, x.title = ifelse(aggr_on == "funcid", "Function", "Dimension"), y.title = "ERT")
  else plot_ly_default(title = plot_title)

  if (use_rank){
    ertranks <- seq(0, 0, length.out = length(get_algId(dsList)))
    fvs2 <- -fvs
    fvs2[is.na(fvs2)] <- Inf
    for (i in seq_along(aggr_attr)){
      ertranks <- rbind(ertranks, rank(fvs2[i, ]))
    }
    dataert <- ertranks[-1, ]
  }
  else {
    dataert <- fvs
  }

  for (i in seq_along(get_algId(dsList))){
    algId <- get_algId(dsList)[[i]]
    color <- colors[[algId]]
    data <- dataert[,i]
    rgb_str <- paste0('rgb(', paste0(col2rgb(color), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.35)')
    if(plot_mode == "radar"){
      p %<>%
        add_trace(type = 'scatterpolar', r = data,
                  theta = paste0(ifelse(aggr_on == "funcId", "F", "D"),aggr_attr),
                  fill = 'toself', connectgaps = T, fillcolor = rgba_str,
                  marker = list(color = rgb_str), hoverinfo = 'text',
                  text = paste0('FVal: ', format(fvs[,i], digits = 3, nsmall = 3)),
                  name = algId, legendgroup = algId)
      #TODO: cleaner solution!!!!!
      data2 <- data
      data2[is.na(data2)] <- 0
      data2[!is.na(data)] <- NA
      p %<>%
        add_trace(type='scatterpolar', mode='markers', r = data2,
                  theta = paste0(ifelse(aggr_on == "funcId", "F", "D"),aggr_attr),
                  marker = list(color = rgb_str, symbol = 'x', size = '10'), hoverinfo = 'text',
                  text = paste0('FVal: ', format(fvs[,i], digits = 3, nsmall = 3)),
                  showlegend = F, legendgroup = algId, fill = 'nofill')
    }
    else{
      p %<>% add_trace(x = aggr_attr, y = data, type = 'scatter',
                       mode = 'lines+markers',
                       marker = list(color = rgb_str), hoverinfo = 'text',
                       text = paste0('FVal: ', format(fvs[,i], digits = 3, nsmall = 3)),
                       line = list(color = rgb_str), name = algId, legendgroup = algId)
      data2 <- data
      data2[is.na(data2)] <- 0
      data2[!is.na(data)] <- NA
      p %<>%
        add_trace(type='scatter', mode='markers', x = aggr_attr, y = data2,
                  marker = list(color = rgb_str, symbol = 'x', size = '10'), hoverinfo = 'text',
                  text = paste0('FVal: ', format(fvs[,i], digits = 3, nsmall = 3)),
                  showlegend = F, legendgroup = algId )

    }
  }
  if (plot_mode == "radar"){
    if(use_rank)
      p %<>%
      layout(polar = list(radialaxis = list(type = 'linear', visible=F, autorange='reversed')))
    else
      p %<>%
      layout(polar = list(radialaxis = list(type = 'log', visible=F)))
  }
  else{
    if(use_rank)
      p %<>%
      layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
             xaxis = list(type = ifelse(aggr_on != 'funcId', 'log', 'linear')))
    else
      p %<>%
      layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
             xaxis = list(type = ifelse(aggr_on != 'funcId', 'log', 'linear')))
  }
  p
}
