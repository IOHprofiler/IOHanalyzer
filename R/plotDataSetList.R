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
    if (count < color_end)
      1/color_end
    else
      0
  }
  ,
  fixed_edges = function(count, amount, intensity) {
    scale <- (intensity + 1) / 2
    color_center <- floor(scale * amount) + 1
    if (count <= color_center)
      1 / (2*color_center)
    else
      1 / (2*(amount - color_center))
  }
)

#S3 generics
# TODO: decide which parameters need to be in the generics

#' Plot lineplot of the ERTs of a DataSetList
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param Fstart The starting function value.
#' @param Fstop The final function value.
#' @param show.ERT Whether or not to show the ERT-values
#' @param show.CI Whether or not to show the standard deviations
#' @param show.mean Whether or not to show the mean hitting times
#' @param show.median Whether or not to show the median hitting times
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param scale.reverse Wheter or not to reverse the x-axis (when using minimization)
#' @param backend Which plotting library to use. Can be 'plotly' or 'ggplot2'
#' @param includeOpts Whether or not to include all best points reached by each algorithm
#' @param p Existing plot to which to add the current data
#' @return A plot of ERT-values of the DataSetList
#' @export
#' @examples 
#' Plot.RT.Single_Func(subset(dsl, funcId == 1))
Plot.RT.Single_Func <- function(dsList, Fstart = NULL, Fstop = NULL,
                                show.ERT = T, show.CI = F, show.mean = F,
                                show.median = F, backend = NULL,
                                scale.xlog = F, scale.ylog = F, scale.reverse = F,
                                includeOpts = F, p = NULL) 
  UseMethod("Plot.RT.Single_Func", dsList)
#' Plot lineplot of the expected function values of a DataSetList
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param RTstart The starting runtime value.
#' @param RTstop The final runtime value.
#' @param show.CI Whether or not to show the standard deviations
#' @param show.mean Whether or not to show the mean runtimes
#' @param show.median Whether or not to show the median runtimes
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param scale.reverse Wheter or not to reverse the x-axis (when using minimization)
#' @param backend Which plotting library to use. Can be 'plotly' or 'ggplot2'
#'
#' @return A plot of ERT-values of the DataSetList
#' @export
#' @examples 
#' Plot.FV.Single_Func(subset(dsl, funcId == 1))
Plot.FV.Single_Func <- function(dsList, RTstart = NULL, RTstop = NULL, show.CI = F, show.mean = T, 
                                show.median = F, backend = NULL, scale.xlog = F, scale.ylog = F,
                         scale.reverse = F) UseMethod("Plot.FV.Single_Func", dsList)
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
#' @examples 
#' Plot.RT.PMF(subset(dsl, funcId == 1), 14)
Plot.RT.PMF <- function(dsList, ftarget, show.sample = F, scale.ylog = F, backend = NULL) 
  UseMethod("Plot.RT.PMF", dsList)
#' Plot histograms of the runtimes of a DataSetList at a certain target function value
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param ftarget The target function value.
#' @param plot_mode How to plot the different hisograms for each algorithm. Can be either
#'  'overlay' to show all algorithms on one plot, or 'subplot' to have one plot per algorithm.
#' @param use.equal.bins Whether to determine one bin size for all plots or have individual 
#' bin sizes for each algorithm
#'
#' @return A plot of the histograms of the runtimes at a the
#'         target function value of the DataSetList
#' @export
#' @examples 
#' Plot.RT.Histogram(subset(dsl, funcId == 1), 14)
Plot.RT.Histogram <- function(dsList, ftarget, plot_mode = 'overlay', use.equal.bins = F) 
  UseMethod("Plot.RT.Histogram", dsList)
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
#' @examples 
#' Plot.RT.ECDF_Per_Target(subset(dsl, funcId == 1), 14)
Plot.RT.ECDF_Per_Target <- function(dsList, ftargets, scale.xlog = F) 
  UseMethod("Plot.RT.ECDF_Per_Target", dsList)
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
#' @examples 
#' Plot.RT.ECDF_Single_Func(subset(dsl, funcId == 1))
Plot.RT.ECDF_Single_Func <- function(dsList, fstart = NULL, fstop = NULL,
                              fstep = NULL, show.per_target = F,
                              scale.xlog = F) UseMethod("Plot.RT.ECDF_Single_Func", dsList)
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
#' @examples 
#' Plot.RT.ECDF_AUC(subset(dsl, funcId == 1))
Plot.RT.ECDF_AUC <- function(dsList, fstart = NULL,
                        fstop = NULL, fstep = NULL,
                        fval_formatter = as.integer) UseMethod("Plot.RT.ECDF_AUC", dsList)
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
#' @examples 
#' Plot.FV.PDF(subset(dsl, funcId == 1), 100)
Plot.FV.PDF <- function(dsList, runtime, show.sample = F, scale.ylog = F) 
  UseMethod("Plot.FV.PDF", dsList)
#' Plot histograms of the function values of a DataSetList at a certain target runtime
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param runtime The target runtime
#' @param plot_mode How to plot the different hisograms for each algorithm. Can be either
#'  'overlay' to show all algorithms on one plot, or 'subplot' to have one plot per algorithm.
#' @param use.equal.bins Whether to determine one bin size for all plots or have individual 
#' bin sizes for each algorithm
#' 
#' @return A plot of the histograms of the function values at a the
#'         target runtime of the DataSetList
#' @export
#' @examples 
#' Plot.FV.Histogram(subset(dsl, funcId == 1), 100)
Plot.FV.Histogram <- function(dsList, runtime, plot_mode='overlay', use.equal.bins = F) 
  UseMethod("Plot.FV.Histogram", dsList)
#' Plot the empirical cumulative distriburtion as a function of the target values of
#' a DataSetList at certain target runtimes
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param runtimes The target runtimes
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.reverse Whether or not to reverse the x-axis (when using minimization)
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the fucntion values of the DataSetList at the target runtimes
#' @export
#' @examples 
#' Plot.FV.ECDF_Per_Target(subset(dsl, funcId == 1), 10)
Plot.FV.ECDF_Per_Target <- function(dsList, runtimes, scale.xlog = F, scale.reverse = F) 
  UseMethod("Plot.FV.ECDF_Per_Target", dsList)
#' Plot the aggregated empirical cumulative distriburtion as a function of the function values of
#' a DataSetList.
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param rt_min The starting runtime
#' @param rt_max The final runtime
#' @param rt_step The spacing between starting and final runtimes
#' @param show.per_target Whether or not to show the individual ECDF-curves for each runtime
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.reverse Whether or not to reverse the x-axis (when using minimization)
#' 
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the function values of the DataSetList
#' @export
#' @examples 
#' Plot.FV.ECDF_Single_Func(subset(dsl, funcId == 1))
Plot.FV.ECDF_Single_Func <- function(dsList, rt_min = NULL, rt_max = NULL,
                              rt_step = NULL, scale.xlog = F,
                              show.per_target = F, scale.reverse = F) 
  UseMethod("Plot.FV.ECDF_Single_Func", dsList)
#' Radarplot of the area under the aggregated ECDF-curve of a DataSetList.
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param rt_min The starting runtime
#' @param rt_max The final runtime
#' @param rt_step The spacing between starting and final runtimes
#'
#' @return A radarplot of the area under the aggregated ECDF-curve of the DataSetList
#' @export
#' @examples 
#' Plot.FV.ECDF_AUC(subset(dsl, funcId == 1))
Plot.FV.ECDF_AUC <- function(dsList, rt_min = NULL, rt_max = NULL,
                        rt_step = NULL) UseMethod("Plot.FV.ECDF_AUC", dsList)
#' Plot the parameter values recorded in a DataSetList (aligned by funcion value)
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param f_min The starting function value.
#' @param f_max The final function value.
#' @param show.mean Whether or not to show the mean parameter values
#' @param show.median Whether or not to show the median parameter values
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param algids Which algorithms from dsList to use
#' @param par_name Which parameters to create plots for; set to NULL to use all 
#' parameters found in dsList.
#' @param show.CI Whether or not to show the standard deviation
#'
#' @return A plot of for every recorded parameter in the DataSetList
#' @export
#' @examples 
#' Plot.RT.Parameters(subset(dsl, funcId == 1))
Plot.RT.Parameters <- function(dsList, f_min = NULL, f_max = NULL,
                            algids = 'all', par_name = NULL,
                            scale.xlog = F, scale.ylog = F,
                            show.mean = T, show.median = F,
                            show.CI = F) UseMethod("Plot.RT.Parameters", dsList)
#' Plot the parameter values recorded in a DataSetList (aligned by budget)
#'
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param rt_min The starting budget value.
#' @param rt_max The final budget value.
#' @param show.mean Whether or not to show the mean parameter values
#' @param show.median Whether or not to show the median parameter values
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param algids Which algorithms from dsList to use
#' @param par_name Which parameters to create plots for; set to NULL to use all 
#' parameters found in dsList.
#' @param show.CI Whether or not to show the standard deviation
#'
#' @return A plot of for every recorded parameter in the DataSetList
#' @export
#' @examples 
#' Plot.FV.Parameters(subset(dsl, funcId == 1))
Plot.FV.Parameters <- function(dsList, rt_min = NULL, rt_max = NULL,
                               algids = 'all', par_name = NULL,
                               scale.xlog = F, scale.ylog = F,
                               show.mean = T, show.median = F,
                               show.CI = F) UseMethod("Plot.FV.Parameters", dsList)
#' Plot the aggregated empirical cumulative distriburtion as a function of the running times of
#' a DataSetList. Aggregated over multiple functions or dimensions.
#'
#' @param dsList A DataSetList.
#' @param targets The target function values. Specified in a data.frame, as can be generated
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' by the function 'get_default_ECDF_targets'
#'
#' @return A plot of the empirical cumulative distriburtion as a function of
#' the running times of the DataSetList
#' @export
#' @examples 
#' Plot.RT.ECDF_Multi_Func(dsl)
Plot.RT.ECDF_Multi_Func <- function(dsList, targets = NULL, scale.xlog = F) 
  UseMethod("Plot.RT.ECDF_Multi_Func", dsList)
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
#' @examples 
#' Plot.RT.Multi_Func(dsl)
Plot.RT.Multi_Func <- function(dsList, scale.xlog = F, scale.ylog = F, scale.reverse = F, 
                               backend = NULL) 
  UseMethod("Plot.RT.Multi_Func", dsList)
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
#' @param inf.action How to handle infinite ERTs ('overlap' or 'jitter')
#' @return A plot of ERT-based comparison on the provided functions or dimensions of the DataSetList
#' @export
#' @examples 
#' Plot.RT.Aggregated(dsl)
Plot.RT.Aggregated <- function(dsList, aggr_on = 'funcId', targets = NULL, 
                               plot_mode = 'radar', use_rank = F, scale.ylog = T, maximize = T,
                               erts = NULL, inf.action = 'overlap') 
  UseMethod("Plot.RT.Aggregated", dsList)
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
#' @examples 
#' Plot.FV.Aggregated(dsl)
Plot.FV.Aggregated <- function(dsList, aggr_on = 'funcId', runtimes = NULL, plot_mode = 'radar', 
                               use_rank = F, scale.ylog = T, fvs = NULL) 
  UseMethod("Plot.FV.Aggregated", dsList)

#' Plot FV-plots for multiple functions or dimensions
#'
#' @param dsList A DataSetList (should consist of only one function OR dimension).
#' @param scale.xlog Whether or not to scale the x-axis logaritmically
#' @param scale.ylog Whether or not to scale the y-axis logaritmically
#' @param backend Which plotting library to use. Either 'plotly' or 'ggplot2'.
#'
#' @return A plot of Function-values of the DataSetList
#' @export
#' @examples 
#' Plot.FV.Multi_Func(dsl)
Plot.FV.Multi_Func <- function(dsList, scale.xlog = F, scale.ylog = F, backend = NULL) 
  UseMethod("Plot.FV.Multi_Func", dsList)

#' Plot a heatmap showing the statistically different algorithms
#' 
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param ftarget The target function value to use
#' @param alpha The cutoff for statistical significance
#' @param bootstrap.size The amound of bootstrapped samples used
#' @param which Whether to use fixed-target ('by_FV') or fixed-budget ('by_RT') perspective
#'
#' @return A heatmap showing the statistical significance between algorithms
#' @export
#' @examples 
#' Plot.Stats.Significance_Heatmap(subset(dsl, funcId == 2), 16)
Plot.Stats.Significance_Heatmap <- function(dsList, ftarget, alpha = 0.01, bootstrap.size = 30, 
                                            which = 'by_FV') 
  UseMethod("Plot.Stats.Significance_Heatmap", dsList)
  
#' Plot a network graph showing the statistically different algorithms
#' 
#' @param dsList A DataSetList (should consist of only one function and dimension).
#' @param ftarget The target function value to use
#' @param alpha The cutoff for statistical significance
#' @param bootstrap.size The amound of bootstrapped samples used
#' @param which Whether to use fixed-target ('by_FV') or fixed-budget ('by_RT') perspective
#'
#' @return A graph showing the statistical significance between algorithms
#' @export
#' @examples 
#' Plot.Stats.Significance_Graph(subset(dsl, funcId == 2), 16)
Plot.Stats.Significance_Graph <- function(dsList, ftarget, alpha = 0.01, bootstrap.size = 30, 
                                          which = 'by_FV') 
  UseMethod("Plot.Stats.Significance_Graph", dsList)

#' Create a candlestick plot of Glicko2-rankings
#' 
#' @param dsList A DataSetList
#' @param nr_rounds The number of rounds in the tournament
#' @param glicko2_rank_df Optional. Dataframe containing the glicko2 rating to avoid needless recalculation.
#' @param which Whether to use fixed-target ('by_FV') or fixed-budget ('by_RT') perspective
#' @param target_dt Optional: data table containing the targets for each function and dimension
#' 
#' @export
#' @examples 
#' Plot.Stats.Glicko2_Candlestick(dsl, nr_rounds=2)
Plot.Stats.Glicko2_Candlestick <- function(dsList, nr_rounds = 100, glicko2_rank_df = NULL, 
                                           which = 'by_FV', target_dt = NULL) 
  UseMethod("Plot.Stats.Glicko2_Candlestick", dsList)


##Implementations

#' @rdname Plot.RT.Single_Func
#' @export
Plot.RT.Single_Func.DataSetList <- function(dsList, Fstart = NULL, Fstop = NULL,
                                            show.ERT = T, show.CI = T, show.mean = F,
                                            show.median = F, backend = NULL,
                                            scale.xlog = F, scale.ylog = F,
                                            scale.reverse = F, includeOpts = F, p = NULL) {
  if (is.null(backend)) backend <- getOption("IOHanalyzer.backend", default = 'plotly')

  Fall <- get_funvals(dsList)

  if (is.null(Fstart)) Fstart <- min(Fall)
  if (is.null(Fstop)) Fstop <- max(Fall)

  Fseq <- seq_FV(Fall, Fstart, Fstop, length.out = 60,
                 scale = ifelse(scale.xlog, 'log', 'linear'))
  
  if (includeOpts) {
    for (algid in get_algId(dsList)) {
      #TODO: Work for minimization
      Fseq <- c(Fseq, max(get_funvals(subset(dsList, algId == algid))))
    }
    Fseq <- unique(sort(Fseq))
  }
  
  if (length(Fseq) == 0) return(NULL)

  N <- length(dsList)
  legends <- get_legends(dsList)
  
  dt <- get_RT_summary(dsList, ftarget = Fseq)
  dt[, `:=`(upper = mean + sd, lower = mean - sd)]

  if (backend == 'plotly') {
    if (is.null(p))
      p <- IOH_plot_ly_default(x.title = "Best-so-far f(x)-value",
                               y.title = "Function evaluations")
    algnames <- get_algId(dsList)
    colors <- get_color_scheme(algnames) %>% set_names(algnames)
    dashes <- get_line_style(algnames) %>% set_names(algnames)
    # TODO: improve this part, get rid of the loop
    for (i in seq_along(dsList)) {
      legend <- legends[i]
      ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
                     funcId == attr(dsList[[i]], 'funcId') &
                     DIM == attr(dsList[[i]], 'DIM')]

      algId <- attr(dsList[[i]], 'algId')
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.2)')

      if (show.ERT)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~ERT, type = 'scatter',
                         name = legend, mode = 'lines+markers',
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = dashes[[algId]]), visible = T)

      if (show.mean)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~mean, type = 'scatter',
                         mode = 'lines+markers', name = paste0(algId, '.mean'),
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = 'dash'), visible = T)

      if (show.median)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~median, type = 'scatter',
                         name = paste0(legend, '.median'), mode = 'lines+markers',
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = 'dot'), visible = T)

      if (show.CI)
        p %<>%
        add_trace(data = ds_ERT, x = ~target, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0), legendgroup = legend,
                  showlegend = F, name = 'mean +/- sd') %>%
        add_trace(x = ~target, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'), legendgroup = legend,
                  fillcolor = rgba_str, showlegend = F, name = 'mean +/- sd')


    }
    
    p %<>%
      layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear'), showexponent = 'none'),
             yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))

    if (scale.reverse)
      p %<>% layout(xaxis = list(autorange = "reversed"))

  } else if (backend == 'ggplot2') {
    dt[, 'group' := paste(algId, funcId, DIM, sep = '-')]
    p <- ggplot(data = dt, aes(group = `group`, colour = `group`))

    if (show.CI) p <- p + geom_ribbon(aes(target, ymin = lower, ymax = upper, fill = `group`),
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

#' @rdname Plot.FV.Single_Func
#' @export
Plot.FV.Single_Func.DataSetList <- function(dsList, RTstart = NULL, RTstop = NULL,
                                     show.CI = F,
                                     show.mean = T, show.median = F,
                                     backend = NULL,
                                     scale.xlog = F, scale.ylog = F,
                                     scale.reverse = F) {
  if (is.null(backend)) backend <- getOption("IOHanalyzer.backend", default = 'plotly')
  
  RTall <- get_runtimes(dsList)
  if (is.null(RTstart)) RTstart <- min(RTall)
  if (is.null(RTstop)) RTstop <- max(RTall)


  RTseq <- seq_RT(RTall, RTstart, RTstop, length.out = 60, scale = ifelse(scale.xlog,'log','linear'))
  if (length(RTseq) == 0) return(NULL)

  N <- length(dsList)
  legends <- get_legends(dsList)

  fce <- get_FV_summary(dsList, RTseq)
  fce[, `:=`(upper = mean + sd, lower = mean - sd)]

  if (backend == 'plotly') {
    p <- IOH_plot_ly_default(y.title = "Best-so-far f(x)-value", x.title = "Runtime")
    algnames <- get_algId(dsList)
    colors <- get_color_scheme(algnames) %>% set_names(algnames)
    dashes <- get_line_style(algnames) %>% set_names(algnames)
    
    for (i in seq_along(dsList)) {
      legend <- legends[i]
      algId <- attr(dsList[[i]], 'algId')

      ds_FCE <- fce[algId == attr(dsList[[i]], 'algId') &
                      funcId == attr(dsList[[i]], 'funcId') &
                      DIM == attr(dsList[[i]], 'DIM')]

      if (nrow(ds_FCE) == 0)
        next

      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.3)')

      if (show.mean)
        p %<>% add_trace(data = ds_FCE, x = ~runtime, y = ~mean, type = 'scatter',
                         mode = 'lines+markers', name = paste0(algId, ''),
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = dashes[[algId]]), visible = T)

      if (show.median)
        p %<>% add_trace(data = ds_FCE, x = ~runtime, y = ~median, type = 'scatter',
                         name = paste0(legend, '.median'), mode = 'lines+markers',
                         marker = list(color = rgb_str), legendgroup = legend,
                         line = list(color = rgb_str, dash = 'dash'), visible = T)

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
    }
    p %<>%
      layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')),
             yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))

    if (scale.reverse)
      p %<>% layout(xaxis = list(autorange = "reversed", showexponent = 'none'))
    
  } else if (backend == 'ggplot2') {
    fce[, 'group' := paste(algId, funcId, DIM, sep = '-')]
    p <- ggplot(data = fce, aes(group = `group`, colour = `group`))

    if (show.mean) p <- p + geom_line(aes(runtime, mean), linetype = 'dashed')
    if (show.median) p <- p + geom_line(aes(runtime, median), linetype = 'dotted')

    p <- p +
      scale_color_manual(values = colors) +
      scale_fill_manual(values = colors)

    #TODO: add individual run etc
  }
  return(p)
}

#' @rdname Plot.RT.PMF
#' @export
Plot.RT.PMF.DataSetList <- function(dsList, ftarget, show.sample = F,
                                    scale.ylog = F, backend = NULL){
  if (is.null(backend)) backend <- getOption("IOHanalyzer.backend", default = 'plotly')
  
  points <- ifelse(show.sample, 'all', FALSE)

  N <- length(dsList)

  p <- IOH_plot_ly_default(x.title = "Algorithms",
                       y.title = "Runtime / function evaluations")
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)

  for (i in seq_along(dsList)) {
    ds <- dsList[[i]]
    algId <- attr(ds, "algId")
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.52)')

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
  p
}

#' @rdname Plot.RT.Histogram
#' @export
Plot.RT.Histogram.DataSetList <- function(dsList, ftarget, plot_mode = 'overlay', use.equal.bins = F){
  if (length(get_funcId(dsList)) != 1 || length(get_dim(dsList)) != 1) {
    warning("Invalid dataset uploaded. Please ensure the datasetlist contains data 
            from only one function and only one dimension.")
    return(NULL)
  }
  
  N <- length(dsList)
  if (N <= 10) {
    n_rows <- ceiling(N / 2.) # keep to columns for the histograms
    n_cols <- min(2, N)
  }
  else {
    n_rows <- ceiling(N / 3.) # keep to columns for the histograms
    n_cols <- 3
  }
  if (plot_mode == 'overlay') {
    p <- IOH_plot_ly_default(x.title = "Function evaluations", y.title = "Runs")
  } else if (plot_mode == 'subplot') {
    p <- lapply(seq(N), function(x) {
      disp_y <-  mod(x, n_cols) == 1
      disp_x <- x > (N - n_cols)
      IOH_plot_ly_default(x.title = if (disp_x) "Function evaluations" else "", 
                          y.title = if (disp_y) "Runs" else "")
    })
  }
  if (use.equal.bins) {
    res1 <- hist(get_RT_sample(dsList, ftarget, output = 'long')$RT, breaks = nclass.FD, plot = F)
  }
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  
  for (i in seq_along(dsList)) {
    df <- dsList[[i]]
    
    algId <- attr(df, "algId")
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.35)')

    algId <- attr(df, 'algId')
    rt <- get_RT_sample(df, ftarget, output = 'long')

    # skip if all runtime samples are NA
    if (sum(!is.na(rt$RT)) < 2)
      next
    if (use.equal.bins) breaks <- res1$breaks
    else breaks <- nclass.FD
    res <- hist(rt$RT, breaks = breaks, plot = F)
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
      disp_y <-  mod(i, n_cols) == 1
      disp_x <- i > (N - n_cols)
      disp <- c(disp_x, disp_y)
      
      p[[i]] %<>%
        add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                  name = algId, text = ~text, hoverinfo = 'text',
                  marker = list(color = rgba_str,
                                line = list(color = 'rgb(8,48,107)', width = 1.5))) 
      # %>%
      #   layout(
      #     annotations = list(
      #       text = c("Function Evaluations", "Runs")[disp], font = list(f2, f2)[disp], 
      #       align = "center",
      #       xref = "paper", yref = "paper",
      #       yanchor = c("top","top")[disp], xanchor = "center", textangle = c(0, -90),
      #       x = c(0.5, -0.1*n_cols)[disp], y = c(-0.065*n_rows, 0.6)[disp], showarrow = FALSE
      #     )
      #   )
    }
  }

  if (plot_mode == 'subplot') {
    p <- subplot(p, nrows = n_rows, titleX = T, titleY = T, margin = 0.05)
  }
  p %>% layout(margin = 5)
}

#' @rdname Plot.RT.ECDF_Per_Target
#' @export
Plot.RT.ECDF_Per_Target.DataSetList <- function(dsList, ftargets, scale.xlog = F){
  req(length(ftargets) != 0)

  N <- length(dsList)

  p <- IOH_plot_ly_default(title = paste('ftarget:', paste(ftargets, collapse = ' ')),
                       x.title = "Function evaluations",
                       y.title = "Proportion of runs")
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  dashes <- get_line_style(algnames) %>% set_names(algnames)
  
  for (k in seq_along(dsList)) {
    df <- dsList[[k]]
    algId <- attr(df, 'algId')

    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.35)')

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
        add_trace(x = rt, y = vals, type = 'scatter',
                  mode = 'lines+markers', name = algId, showlegend = (i == 1),
                  legendgroup = paste0(k),
                  marker = list(color = rgb_str),
                  line = list(color = rgb_str, width = 3, dash = dashes[[algId]]))
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

#' @rdname Plot.RT.ECDF_Single_Func
#' @export
Plot.RT.ECDF_Single_Func.DataSetList <- function(dsList, fstart = NULL, fstop = NULL,
                                          fstep = NULL, show.per_target = F,
                                          scale.xlog = F) {
  fall <- get_funvals(dsList)
  if (is.null(fstart)) fstart <- min(fall)
  if (is.null(fstop)) fstop <- max(fall)

  fseq <- seq_FV(fall, fstart, fstop, fstep)
  req(fseq)

  N <- length(dsList)

  RT <- get_runtimes(dsList)
  x <- seq_RT(RT, length.out = 50, scale = ifelse(scale.xlog, 'log', 'linear'))
  p <- IOH_plot_ly_default(x.title = "Function evaluations",
                       y.title = "Proportion of (run, target) pairs")
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  dashes <- get_line_style(algnames) %>% set_names(algnames)
  for (k in seq_along(dsList)) {
    df <- dsList[[k]]
    algId <- attr(df, 'algId')

    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.15)')
    rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.8)')

    m <- lapply(fseq, function(f) {
      rt <- get_RT_sample(df, f, output = 'long')$RT
      if (all(is.na(rt)))
        return(rep(0, length(x)))
      fun <- ecdf(rt)
      if (is.function(fun)) fun(x) else NA
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
                line = list(color = rgb_str, width = 2, dash = dashes[[algId]]),
                marker = list(color = rgb_str, size = 9))

    if (show.per_target) {
      for (f in fseq) {
        rt <- get_RT_sample(df, f, output = 'long') %>% '$'('RT') %>% sort
        # TODO: plot the unsuccessful ECDF
        if (all(is.na(rt)))
          next
        else{
          ecdf_temp <- ECDF(df, f)
          v <- ecdf_temp(rt)
        }
        p %<>%
          add_trace(x = rt, y = v, type = 'scatter',
                    mode = 'lines', name = algId, showlegend = F, legendgroup = paste0(k),
                    line = list(color = rgba_str2, width = 1, dash = 'dot'))
      }
    }
  }

  p %<>%
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')))
  p
}

#' @rdname Plot.RT.ECDF_AUC
#' @export
Plot.RT.ECDF_AUC.DataSetList <- function(dsList, fstart = NULL,
                                    fstop = NULL, fstep = NULL,
                                    fval_formatter = as.integer) {
  fall <- get_funvals(dsList)
  if (is.null(fstart)) fstart <- min(fall)
  if (is.null(fstop)) fstop <- max(fall)

  fseq <- seq_FV(fall, fstart, fstop, fstep)

  N <- length(dsList)

  RT.max <- sapply(dsList, function(ds) max(attr(ds, 'maxRT'))) %>% max
  p <- IOH_plot_ly_default()
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  dashes <- get_line_style(algnames) %>% set_names(algnames)
  for (k in seq_along(dsList)) {
    df <- dsList[[k]]
    algId <- attr(df, 'algId')

    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.2)')

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

#' @rdname Plot.FV.PDF
#' @export
Plot.FV.PDF.DataSetList <- function(dsList, runtime, show.sample = F, scale.ylog = F){
  points <- ifelse(show.sample, 'all', FALSE)

  N <- length(dsList)

  p <- IOH_plot_ly_default(x.title = "Algorithms",
                       y.title = "Target value")
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  dashes <- get_line_style(algnames) %>% set_names(algnames)
  for (i in seq_along(dsList)) {
    ds <- dsList[[i]]
    algId <- attr(ds, "algId")
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.55)')

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
                line = list(color = 'black', width = 2, dash = dashes[[algId]]),
                marker = list(color = rgb_str, size = 8))
  }
  p %<>%
    layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')))
  p
}

#' @rdname Plot.FV.Histogram
#' @export
Plot.FV.Histogram.DataSetList <- function(dsList, runtime, plot_mode='overlay', use.equal.bins = F){
  if (length(get_funcId(dsList)) != 1 || length(get_dim(dsList)) != 1) {
    warning("Invalid dataset uploaded. Please ensure the datasetlist contains data 
            from only one function and only one dimension.")
    return(NULL)
  }
  N <- length(dsList)
  if (N <= 10) {
    n_rows <- ceiling(N / 2.) # keep to columns for the histograms
    n_cols <- min(2, N)
  }
  else {
    n_rows <- ceiling(N / 3.) # keep to columns for the histograms
    n_cols <- 3
  }
  
  if (plot_mode == 'overlay') {
    p <- IOH_plot_ly_default(x.title = "Target values", y.title = "Runs")

  } else if (plot_mode == 'subplot') {
    p <- lapply(seq(N), function(x) {
      disp_y <-  mod(x, n_cols) == 1
      disp_x <- x > (N - n_cols)
      IOH_plot_ly_default(x.title = if (disp_x) "Target values" else "", 
                          y.title = if (disp_y) "Runs" else "")
    })
  }
  
  if (use.equal.bins) {
    res1 <- hist(get_FV_sample(dsList, runtime, output = 'long')$'f(x)', breaks = nclass.FD, plot = F)
  }
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  dashes <- get_line_style(algnames) %>% set_names(algnames)
  for (i in seq_along(dsList)) {
   

    ds <- dsList[[i]]
    algId <- attr(ds, 'algId')
    
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.35)')
    
    fce <- get_FV_sample(ds, runtime, output = 'long')
    # skip if all target samples are NA
    if (sum(!is.na(fce$'f(x)')) < 2)
      next
    
    if (use.equal.bins) breaks <- res1$breaks
    else breaks <- nclass.FD
    res <- hist(fce$'f(x)', breaks = breaks, plot = F)
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
    p <- subplot(p, nrows = n_rows, titleX = T, titleY = T, margin = 0.05)

  p %>% layout(margin = 5)
}

#' @rdname Plot.FV.ECDF_Per_Target
#' @export
Plot.FV.ECDF_Per_Target.DataSetList <- function(dsList, runtimes, scale.xlog = F, scale.reverse = F){
  #TODO: Fvals in legend need to be formatted properly
  runtimes <- runtimes[!is.na(runtimes)]
  req(length(runtimes) != 0)

  n_algorithm <- length(dsList)

  p <- IOH_plot_ly_default(title = NULL,
                       x.title = "Target value",
                       y.title = "Proportion of runs")
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  dashes <- get_line_style(algnames) %>% set_names(algnames)
  for (k in seq_along(dsList)) {
    ds <- dsList[[k]]
    algId <- attr(ds, 'algId')

    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.35)')
    show_legend <- T
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
                  mode = 'lines', name = algId, showlegend = show_legend,
                  legendgroup = algId,
                  line = list(color = rgb_str, width = 3, dash = dashes[[algId]])) %>%
        add_trace(data = NULL, x = x, y = y, type = 'scatter', showlegend = F,
                  mode = 'markers',  legendgroup = algId,
                  name = sprintf('%s, %.2e', algId, runtimes[i]),
                  marker = list(color = rgb_str, symbol = symbols[i], size = 13))
      show_legend <- F
    }
  }

  p %<>%
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear'), autorange = ifelse(scale.reverse, 'reversed', T)))
  p
}

#' @rdname Plot.FV.ECDF_Single_Func
#' @export
Plot.FV.ECDF_Single_Func.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL,
                                          rt_step = NULL, scale.xlog = F,
                                          show.per_target = F, scale.reverse = F){

  rt <- get_runtimes(dsList)
  if (is.null(rt_min)) rt_min <- min(rt)
  if (is.null(rt_max)) rt_max <- max(rt)

  rt_seq <- seq_RT(rt, from = rt_min, to = rt_max, by = rt_step,
                   scale = ifelse(scale.xlog,'log','linear'))
  
  if (!attr(dsList[[1]],"maximization"))
    rt_seq <- rev(rt_seq)
  
  req(rt_seq)

  n_algorithm <- length(dsList)

  funevals.max <- sapply(dsList, function(ds) max(ds$FV, na.rm = T)) %>% max
  funevals.min <- sapply(dsList, function(ds) min(ds$FV, na.rm = T)) %>% min
  
  if (!attr(dsList[[1]], "maximization") && funevals.min != 0)
    x <- 10 ** seq(log10(funevals.max), log10(funevals.min), length.out = 40)
  else
    x <- seq(funevals.min, funevals.max, length.out = 40)
  
  autorange <- ifelse(attr(dsList[[1]],"maximization"), T, 'reversed')
  p <- IOH_plot_ly_default(x.title = "Target value",
                       y.title = "Proportion of (run, budget) pairs") %>%
                      layout(xaxis = list(autorange = autorange))
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  dashes <- get_line_style(algnames) %>% set_names(algnames)
  for (k in seq_along(dsList)) {
    ds <- dsList[[k]]
    algId <- attr(ds, 'algId')

    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.15)')
    rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.8)')

    fun <- get_FV_sample(ds, rt_seq, output = 'long')$'f(x)' %>% ecdf
    m <- fun(x)

    df_plot <- data.frame(x = x, mean = m)


    p %<>%
      add_trace(data = df_plot, x = ~x, y = ~mean, type = 'scatter',
                mode = 'lines+markers', name = sprintf('%s', algId),
                showlegend = T, legendgroup = paste0(k),
                line = list(color = rgb_str, width = 4.5, dash = dashes[[algId]]),
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
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear'), autorange = ifelse(scale.reverse, 'reversed', T)))
  p
}

#' @rdname Plot.FV.ECDF_AUC
#' @export
Plot.FV.ECDF_AUC.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL, rt_step = NULL) {
  rt <- get_runtimes(dsList)
  if (is.null(rt_min)) rt_min <- min(rt)
  if (is.null(rt_max)) rt_max <- max(rt)

  rt_seq <- seq_RT(rt, from = rt_min, to = rt_max, by = rt_step)
  req(rt_seq)

  n_algorithm <- length(dsList)

  funevals.max <- sapply(dsList, function(ds) max(attr(ds, 'finalFV'))) %>% max
  funevals.min <- sapply(dsList, function(ds) min(attr(ds, 'finalFV'))) %>% min
  p <- IOH_plot_ly_default()
  algnames <- get_algId(dsList)
  colors <- get_color_scheme(algnames) %>% set_names(algnames)
  dashes <- get_line_style(algnames) %>% set_names(algnames)
  for (k in seq_along(dsList)) {
    df <- dsList[[k]]
    algId <- attr(df, 'algId')

    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[[algId]]), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[algId]]), collapse = ','), ',0.2)')

    # calculate ECDFs on user specified targets
    funs <- lapply(rt_seq, function(r) {
      get_FV_sample(df, r, output = 'long')$'f(x)' %>% {
        if (all(is.na(.))) NULL
        else  {
          f <- ecdf(.)
          attr(f, 'min') <- min(.)
          attr(f, 'max') <- max(.)
          f
        }
      }
    })

    auc <- sapply(funs,
                  function(fun) {
                    if (is.null(fun)) 0
                    else{ 
                      if (attr(df, 'maximization'))
                          integrate(fun, lower = attr(fun, 'min') - 1, upper = funevals.max,
                                   subdivisions = 1e3) %>% {'$'(., 'value') / funevals.max}
                      else 
                        integrate(fun, lower =  funevals.min, upper = attr(fun, 'max') + 1,
                                  subdivisions = 1e3) %>% {'$'(., 'value') / (attr(fun, 'max') + 1)}
                    }
                  })

    p %<>%
      add_trace(type = 'scatterpolar', r = auc,
                theta = paste0('B:', rt_seq),
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

#' @rdname Plot.RT.Parameters
#' @export
Plot.RT.Parameters.DataSetList <- function(dsList, f_min = NULL, f_max = NULL,
                                        algids = 'all', par_name = NULL,
                                        scale.xlog = F, scale.ylog = F,
                                        show.mean = T, show.median = F,
                                        show.CI = F) {
  # TODO: clean this up
  req(xor(show.mean, show.median))

  fall <- get_funvals(dsList)
  if (is.null(f_min)) f_min <- min(fall)
  if (is.null(f_max)) f_max <- max(fall)
  
  fseq <- seq_FV(fall, f_min, f_max, length.out = 50)
  req(fseq)

  dt <- get_PAR_summary(dsList, fseq, algids)
  req(length(dt) != 0)
  dt[, `:=`(upper = mean + sd, lower = mean - sd)]

  if (is.null(par_name)) par_name <- dt[, parId] %>% unique
  n_param <- length(par_name)

  algorithms <- dt[, algId] %>% unique
  n_alg <- length(algorithms)

  nrows <- ceiling(n_param / 2)
  # TODO: improve the efficiency of plotting here
  p <- lapply(seq(n_param),
              function(i) {
                IOH_plot_ly_default(y.title = par_name[i]) %>%
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
      rgb_str <- paste0('rgb(', paste0(col2rgb(get_color_scheme(alg)), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(get_color_scheme(alg)), collapse = ','), ',0.3)')
      
      if (show.CI) {
      p[[j]] %<>%
        add_trace(data = dt_plot, x = ~target, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0, dash = get_line_style(alg)),
                  showlegend = F, legendgroup = ~algId, name = 'mean +/- sd') %>%
        add_trace(x = ~target, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent', dash = get_line_style(alg)),
                  fillcolor = rgba_str, showlegend = F, legendgroup = ~algId,
                  name = 'mean +/- sd')
      }

      if (show.mean)
        p[[j]] %<>% add_trace(data = dt_plot, x = ~target, y = ~mean,
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(color = rgb_str),
                              line = list(color = rgb_str, dash = get_line_style(alg)),
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
      p[[j]] %<>%
        layout(
          annotations = list(
            text = "Target value", font = f1, align = "center",
            xref = "paper", yref = "paper",
            yanchor = "top", xanchor = "center",
            x = 0.5, y = -0.2, showarrow = FALSE
          )
        ) 
    }
  }

  subplot(p, nrows = nrows, titleX = F, titleY = T, margin = 0.05) 
  # %>%
  #   add_annotations(x = 0.5 , y = -0.18, text = "Best-so-far f(x)-value",
  #                   showarrow = F, xref = 'paper', yref = 'paper',
  #                   font = list(size = 22, family = 'sans-serif'))
}


#' @rdname Plot.FV.Parameters
#' @export
Plot.FV.Parameters.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL,
                                           algids = 'all', par_name = NULL,
                                           scale.xlog = F, scale.ylog = F,
                                           show.mean = T, show.median = F,
                                           show.CI = F) {
  # TODO: clean this up
  req(xor(show.mean, show.median))
  
  rtall <- get_runtimes(dsList)
  if (is.null(rt_min)) rt_min <- min(rtall)
  if (is.null(rt_max)) rt_max <- max(rtall)
  
  rtseq <- seq_FV(rtall, rt_min, rt_max, length.out = 50)
  req(rtseq)
  
  dt <- get_PAR_summary(dsList, rtseq, algids, which = 'by_RT')
  req(length(dt) != 0)
  dt[, `:=`(upper = mean + sd, lower = mean - sd)]
  
  if (is.null(par_name)) par_name <- dt[, parId] %>% unique
  n_param <- length(par_name)
  
  algorithms <- dt[, algId] %>% unique
  n_alg <- length(algorithms)
  
  nrows <- ceiling(n_param / 2)
  # TODO: improve the efficiency of plotting here
  p <- lapply(seq(n_param),
              function(i) {
                IOH_plot_ly_default(y.title = par_name[i], x.title = "Function Evaluations") %>%
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
      rgb_str <- paste0('rgb(', paste0(col2rgb(get_color_scheme(alg)), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(get_color_scheme(alg)), collapse = ','), ',0.3)')
      
      if (show.CI) {
        p[[j]] %<>%
          add_trace(data = dt_plot, x = ~runtime, y = ~upper, type = 'scatter', mode = 'lines',
                    line = list(color = rgba_str, width = 0, dash = get_line_style(alg)),
                    showlegend = F, legendgroup = ~algId, name = 'mean +/- sd') %>%
          add_trace(x = ~runtime, y = ~lower, type = 'scatter', mode = 'lines',
                    fill = 'tonexty',  line = list(color = 'transparent', dash = get_line_style(alg)),
                    fillcolor = rgba_str, showlegend = F, legendgroup = ~algId,
                    name = 'mean +/- sd')
      }
      
      if (show.mean)
        p[[j]] %<>% add_trace(data = dt_plot, x = ~runtime, y = ~mean,
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(color = rgb_str),
                              line = list(color = rgb_str, dash = get_line_style(alg)),
                              name = alg,
                              showlegend = showlegend,
                              legendgroup = ~algId)
      
      else if (show.median)
        p[[j]] %<>% add_trace(data = dt_plot, x = ~runtime, y = ~median,
                              type = 'scatter',
                              mode = 'lines+markers',
                              marker = list(color = rgb_str),
                              line = list(color = rgb_str, dash = 'dash'),
                              name = alg,
                              legendgroup = ~algId,
                              showlegend = showlegend)
      # p[[j]] %<>%
      #   layout(
      #     annotations = list(
      #       text = "Function evaluations", font = f1, align = "center",
      #       xref = "paper", yref = "paper",
      #       yanchor = "top", xanchor = "center",
      #       x = 0.5, y = -0.2, showarrow = FALSE
      #     )
      #   ) 
    }
  }
  
  subplot(p, nrows = nrows, titleX = T, titleY = T, shareX = T, margin = 0.05) 
  # %>%
  #   add_annotations(x = 0.5 , y = -0.18, text = "Best-so-far f(x)-value",
  #                   showarrow = F, xref = 'paper', yref = 'paper',
  #                   font = list(size = 22, family = 'sans-serif'))
}

#' @rdname Plot.RT.ECDF_Multi_Func
#' @export
Plot.RT.ECDF_Multi_Func.DataSetList <- function(dsList, targets = NULL,
                                                scale.xlog = F) {
  if (is.null(targets))
    targets <- get_default_ECDF_targets(dsList, as.numeric)

  algId <- unique(attr(dsList, 'algId'))
  p <- IOH_plot_ly_default(x.title = "Function evaluations",
                       y.title = "Proportion of (run, target, ...) pairs")

  rts <- get_runtimes(dsList)
  x <- seq_RT(rts, length.out = 50, scale = ifelse(scale.xlog, "log", "linear"))

  for (i in seq_along(algId)) {
    Id <- algId[i]
    data <- subset(dsList, algId == Id)
    rgb_str <- paste0('rgb(', paste0(col2rgb(get_color_scheme(Id)), collapse = ','), ')')

    fun <- ECDF(data, ftarget = targets)
    if (is.null(fun)) next

    df_plot <- data.frame(x = x, ecdf = fun(x))
    p %<>% add_trace(data = df_plot, x = ~x, y = ~ecdf, type = 'scatter',
                     mode = 'lines+markers', name = sprintf('%s', Id),
                     showlegend = T,
                     line = list(color = rgb_str, dash = get_line_style(Id)),
                     marker = list(color = rgb_str))
  }
  
  p %<>%
    layout(xaxis = list(type = ifelse(scale.xlog, 'log', 'linear')))
  p
}

#' @rdname Plot.RT.Multi_Func
#' @export
Plot.RT.Multi_Func.DataSetList <- function(dsList, scale.xlog = F,
                                           scale.ylog = F,
                                           scale.reverse = F,
                                           backend = NULL) {
  if (is.null(backend)) backend <- getOption("IOHanalyzer.backend", default = 'plotly')
  
  xscale <- if (scale.xlog) 'log' else 'linear'
  yscale <- if (scale.ylog) 'log' else 'linear'
  funcIds <- get_funcId(dsList)
  n_fcts <- length(funcIds)

  algIds <- get_algId(dsList)
  n_algIds <- length(algIds)

  colors <- get_color_scheme(algIds)
  names(colors) <- algIds
  
  dashes <- get_line_style(algIds)
  names(dashes) <- algIds
  
  # how many columns do we want...
  if (n_fcts <= 10) {
    n_rows <- ceiling(n_fcts / 2.)
    n_cols <- 2
  } else if (n_fcts <= 20) {
    n_rows <- ceiling(n_fcts / 3.)
    n_cols <- 3
  } else {
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
      function(x){
        disp_y <-  mod(x, n_cols) == 1
        disp_x <- x > (n_fcts - n_cols)
        IOH_plot_ly_default(x.title = if (disp_x) "Best-so-far f(x)" else "", 
                            y.title = if (disp_y) "ERT" else "") %>%
            layout(xaxis = list(type = xscale, tickfont = f1, ticklen = 3, autorange = autorange),
                   yaxis = list(type = yscale, tickfont = f1, ticklen = 3))
      }
    )

    for (i in seq(n_fcts)) {
      showlegend <- ifelse(i == 1, T, F)
      dt_plot <- dt[funcId == funcIds[[i]]]

      p[[i]] %<>%
        add_trace(
          data = dt_plot, x = ~target, y = ~ERT, color = ~algId, legendgroup = ~algId,
          type = 'scatter', mode = 'lines+markers',
          linetype = ~algId, marker = list(size = 4), # TODO: perhaps turn off the marker here
          colors = colors, showlegend = showlegend, linetypes = dashes
        ) 

      p[[i]] %<>%
        layout(
          annotations = list(
            text = paste0('F', funcIds[[i]]),
            font = f2,
            xref = "paper", yref = "paper", align = "center",
            yanchor =  "bottom",
            xanchor = "center", textangle = 0,
            x = 0.5,
            y = 1, showarrow = FALSE
          )
        )
    }

    p <- subplot(p, nrows = n_rows, titleX = T, titleY = T, margin = 0.05)
  }
  p %>% layout(margin = 5)
}

#' @rdname Plot.FV.Multi_Func
#' @export
Plot.FV.Multi_Func.DataSetList <- function(dsList, scale.xlog = F,
                                         scale.ylog = F,
                                         backend = NULL) {
  if (is.null(backend)) backend <- getOption("IOHanalyzer.backend", default = 'plotly')
  
  xscale <- if (scale.xlog) 'log' else 'linear'
  yscale <- if (scale.ylog) 'log' else 'linear'
  funcIds <- get_funcId(dsList)
  n_fcts <- length(funcIds)

  algIds <- get_algId(dsList)
  n_algIds <- length(algIds)

  colors <- get_color_scheme(algIds)
  names(colors) <- algIds
  
  dashes <- get_line_style(algIds)
  names(dashes) <- algIds

  # how many columns do we want...
  if (n_fcts <= 10) {
    n_rows <- ceiling(n_fcts / 2.)
    n_cols <- 2
  } else if (n_fcts <= 20) {
    n_rows <- ceiling(n_fcts / 3.)
    n_cols <- 3
  } else {
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
      function(x){
        disp_y <-  mod(x, n_cols) == 1
        disp_x <- x > (n_fcts - n_cols)
        IOH_plot_ly_default(x.title = if (disp_x) "Function Evaluations" else "", 
                          y.title = if (disp_y) "Target value" else "") %>%
        layout(xaxis = list(type = xscale, tickfont = f1, ticklen = 3, autorange = T),
               yaxis = list(type = yscale, tickfont = f1, ticklen = 3))
      }
    )

    for (i in seq(n_fcts)) {
      showlegend <- ifelse(i == 1, T, F)
      dt_plot <- dt[funcId == funcIds[[i]]]

      p[[i]] %<>%
        add_trace(
          data = dt_plot, x = ~runtime, y = ~`mean`, color = ~algId, legendgroup = ~algId,
          type = 'scatter', mode = 'lines+markers',
          linetype = ~algId, marker = list(size = 4), # TODO: perhaps turn off the marker here
          colors = colors, showlegend = showlegend, linetypes = dashes
        ) 
      
      disp_y <-  mod(i, n_cols) == 1
      disp_x <- i > (n_fcts - n_cols)
      disp <- c(disp_x, disp_y, T)
      # disp <- c(T,T,T)
      p[[i]] %<>%
        layout(
          annotations = list(
            text = paste0('F', funcIds[[i]]), 
            font = f2,
            xref = "paper", yref = "paper", align = "center",
            yanchor = "bottom", 
            xanchor = "center", textangle = 0,
            x = 0.5, y = 1, showarrow = FALSE
          )
        )
    }
    
    p <- subplot(p, nrows = n_rows, titleX = T, titleY = T, margin = 0.05)
  }
  p %>% layout(margin = 2)
}

#' @rdname Plot.RT.Aggregated
#' @export
Plot.RT.Aggregated.DataSetList <- function(dsList, aggr_on = 'funcId', targets = NULL, 
                                           plot_mode = 'radar', use_rank = F,
                                           scale.ylog = T, maximize = T, erts = NULL,
                                           inf.action = 'overlap') {
  if (is.null(erts))
    erts <- max_ERTs(dsList, aggr_on = aggr_on, targets = targets, maximize = maximize)
  
  if (is.null(erts))
    return(NULL)

  N <- length(get_algId(dsList))

  fid <- get_funcId(dsList)
  range <- c(min(fid) - .5, max(fid) + .5)
  
  in_legend <- integer(N)
  names(in_legend) <- get_algId(dsList)

  aggr_attr <- if (aggr_on == 'funcId') get_funcId(dsList) else get_dim(dsList)
  if (!is.null(targets) && length(targets) != length(aggr_attr)) targets <- NULL

  second_aggr <- if (aggr_on == 'funcId') get_dim(dsList) else get_funcId(dsList)
  if (length(second_aggr) > 1) return(NULL)

  plot_title <- paste0(ifelse(aggr_on == 'funcId', "Dimension ", "Function "), second_aggr[[1]])

  p <- if (plot_mode == "radar") {
    IOH_plot_ly_default(title = plot_title, 
                        x.title = ifelse(aggr_on == "funcid", "Function", "Dimension"), 
                        y.title = "ERT")
  } else 
    IOH_plot_ly_default(title = plot_title, x.title = ifelse(aggr_on == "funcid", "Function", "Dimension"), 
                        y.title = ifelse(use_rank, "Rank", "ERT"))
    
  if (use_rank) {
    ertranks <- seq(0, 0, length.out = length(get_algId(dsList)))
    
    for (i in seq_along(aggr_attr)) {
      ertranks <- rbind(ertranks, rank(erts[i, ]))
    }
    dataert <- ertranks[-1, ]
    
  } else {
    dataert <- erts
  }
  
  if (inf.action == 'jitter') {
    data_inf <- dataert
    idx <- apply(data_inf, 2, is.infinite)
    data_inf[idx] <- NA
    
    for (i in seq(nrow(data_inf))) {
      idx_ <- idx[i, ]
      x <- data_inf[i, ]
      max_ <- max(x[!is.infinite(x)], na.rm = T)
      n_inf <- sum(idx_)
      data_inf[i, idx_] <- 10 ^ (log10(max_ * 2) + seq(0, log10(10), length.out = n_inf))
    }
    
    dataert[idx] <- data_inf[idx]
    data_inf <- lapply(seq(N),
                       function(i) {
                         idx_ <- idx[, i]
                         v <- data_inf[idx_, i]
                         names(v) <- which(idx_)
                         v
                       })
    
    # data_na <- dataert
    # idx <- apply(data_na, 2, is.na)
    # data_na[idx] <- NA
    # for (i in seq(nrow(data_na))) {
    #   idx_ <- idx[i, ]
    #   x <- data_na[i, ]
    #   max_ <- max(x[!is.infinite(x)], na.rm = T)
    #   n_na <- sum(idx_)
    #   data_na[i, idx_] <- 10 ^ (log10(max_) + seq(0, log10(10), length.out = n_na))
    # }
    # data_na[!idx] <- NA
    # dataert[idx] <- data_na[idx]
    
  } else if (inf.action == 'overlap') {
    data_inf <- dataert
    idx <- apply(data_inf, 2, is.infinite)
    x <- as.vector(data_inf)
    data_inf[idx] <- max(x[!is.infinite(x)], na.rm = T) * 2.5
    dataert[idx] <- data_inf[idx]
    
    data_inf <- lapply(seq(N),
                      function(i) {
                        idx_ <- idx[, i]
                        v <- data_inf[idx_, i]
                        names(v) <- aggr_attr[idx_]
                        v
                      })
    
    # TODO: ask diederick when NA will be generated...
    # data_na <- dataert
    # idx <- apply(data_na, 2, is.na)
    # x <- as.vector(data_na)
    # data_na[idx] <- max(x[!is.infinite(x)], na.rm = T) * 2
    # data_na[!idx] <- NA
    # dataert[idx] <- data_na[idx]
  }
  
  for (i in seq_along(get_algId(dsList))) {
    algId <- get_algId(dsList)[[i]]
    dash <- get_line_style(algId)
    color <- get_color_scheme(algId)
    data <- dataert[, i]
    rgb_str <- paste0('rgb(', paste0(col2rgb(color), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.35)')
    
    data_inf_ <- data_inf[[i]]
    # data_na_ <- data_na[, i]
    
    if (plot_mode == "radar") {
      p %<>%
        add_trace(type = 'scatterpolar', r = data,
                  theta = paste0(ifelse(aggr_on == "funcId", "F", "D"),aggr_attr),
                  fill = 'toself', connectgaps = T, fillcolor = rgba_str,
                  marker = list(color = rgb_str), hoverinfo = 'text',
                  text = paste0('ERT: ', format(erts[, i], digits = 3, nsmall = 3)),
                  name = algId, legendgroup = algId)
      #TODO: Fix dealing with infinite ERT when radarplot is selected
      # p %<>%
      #   add_trace(type = 'scatterpolar', mode = 'markers', r = data_inf_ ,
      #             theta = paste0(ifelse(aggr_on == "funcId", "F", "D"),aggr_attr),
      #             marker = list(color = rgb_str, symbol = 'diamond', size = '10'), 
      #             text = paste0('ERT: ', format(erts[, i], digits = 3, nsmall = 3)),
      #             hoverinfo = 'text', showlegend = F, legendgroup = algId)
    } else {
      p %<>% add_trace(x = aggr_attr, y = data, type = 'scatter',
                       mode = 'lines+markers',
                       marker = list(color = rgb_str, size = 7), hoverinfo = 'text',
                       text = paste0('ERT: ', format(erts[, i], digits = 3, nsmall = 3)),
                       line = list(color = rgb_str, dash = dash), 
                       name = algId, legendgroup = algId)
      p %<>%
        add_trace(type = 'scatter', mode = 'markers', x = as.numeric(names(data_inf_)), 
                  y = data_inf_, marker = list(color = rgb_str, symbol = 'circle-open', size = 13), 
                  # text = paste0('ERT: ', format(rep(Inf, length(data_inf_)), digits = 3, nsmall = 3)),
                  hoverinfo = 'none', showlegend = F, legendgroup = algId)
      
      # p %<>%
      #   add_trace(type='scatter', mode='markers', x = aggr_attr, y = data_na_,
      #             marker = list(color = rgb_str, symbol = 'x', size = '10'),
      #             text = paste0('ERT: ', format(erts[, i], digits = 3, nsmall = 3)),
      #             hoverinfo = 'text', showlegend = F, legendgroup = algId)
    }
  }
  
  if (plot_mode == "radar") {
    if (use_rank)
      p %<>% layout(p, polar = list(radialaxis = list(type = 'linear', visible = F, 
                                                      autorange = 'reversed')))
    else
      p %<>% layout(polar = list(radialaxis = list(type = 'log', visible = F, 
                                                   autorange = 'reverse')))
    
  } else {
    if (aggr_on == 'funcId' && class(aggr_attr) == class(1))
      p %<>% layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
                    xaxis = list(tick0 = 1, dtick = 1, range = range,
                                 type = ifelse(aggr_on != 'funcId', 'log', 'linear')))
    else
      p %<>% layout(yaxis = list(type = ifelse(scale.ylog, 'log', 'linear')),
                    xaxis = list(type = ifelse(aggr_on != 'funcId', 'log', 'linear')))
  }
  p
}

#' @rdname Plot.FV.Aggregated
#' @export
Plot.FV.Aggregated.DataSetList <- function(dsList, aggr_on = 'funcId', runtimes = NULL,
                                      plot_mode = 'radar', use_rank = F,
                                      scale.ylog = T, fvs = NULL){
  if (is.null(fvs))
    fvs <- mean_FVs(dsList, aggr_on = aggr_on, runtimes = runtimes)
  if (is.null(fvs))
    return(NULL)

  N <- length(get_algId(dsList))

  in_legend <- integer(N)
  names(in_legend) <- get_algId(dsList)

  aggr_attr <- if (aggr_on == 'funcId') get_funcId(dsList) else get_dim(dsList)
  if (!is.null(runtimes) && length(runtimes) != length(aggr_attr)) runtimes <- NULL

  second_aggr <- if (aggr_on == 'funcId') get_dim(dsList) else get_funcId(dsList)
  if (length(second_aggr) > 1 ) return(NULL)

  plot_title <- paste0(ifelse(aggr_on == 'funcId', "Dimension ", "Function "), second_aggr[[1]])

  p <- if (plot_mode == "radar") 
    IOH_plot_ly_default(title = plot_title)
  else 
    IOH_plot_ly_default(title = plot_title, 
                        x.title = ifelse(aggr_on == "funcId", "Function", "Dimension"), 
                        y.title = ifelse(use_rank, "Rank", "Mean Runtime"))

  if (use_rank) {
    ertranks <- seq(0, 0, length.out = length(get_algId(dsList)))
    fvs2 <- -fvs
    fvs2[is.na(fvs2)] <- Inf
    for (i in seq_along(aggr_attr)) {
      ertranks <- rbind(ertranks, rank(fvs2[i, ]))
    }
    dataert <- ertranks[-1, ]
  }
  else {
    dataert <- fvs
  }

  for (i in seq_along(get_algId(dsList))) {
    algId <- get_algId(dsList)[[i]]
    color <- get_color_scheme(algId)
    dash <- get_line_style(algId)
    data <- dataert[,i]
    rgb_str <- paste0('rgb(', paste0(col2rgb(color), collapse = ','), ')')
    rgba_str <- paste0('rgba(', paste0(col2rgb(color), collapse = ','), ',0.35)')

    if (plot_mode == "radar") {
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
                       line = list(color = rgb_str, dash = dash), name = algId, legendgroup = algId)
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

  if (plot_mode == "radar") {
    if (use_rank)
      p %<>%
      layout(polar = list(radialaxis = list(type = 'linear', visible=F, autorange='reversed')))
    else
      p %<>%
      layout(polar = list(radialaxis = list(type = 'log', visible=F)))
  }
  else{
    if (use_rank)
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

#' @rdname Plot.Stats.Significance_Heatmap
#' @export 
Plot.Stats.Significance_Heatmap.DataSetList <- function(dsList, ftarget, alpha = 0.01,
                                            bootstrap.size = 30, which = 'by_FV'){
  if (length(get_dim(dsList)) != 1 || 
      length(get_funcId(dsList)) != 1 || 
      length(get_algId(dsList)) < 2)
    return(NULL)
  
  p_matrix <- pairwise.test(dsList, ftarget, bootstrap.size, which)
  y <- p_matrix <= alpha
  colorScale <- data.frame(x = c(-1, -0.33, -0.33, 0.33, 0.33, 1),
                           col = c('blue', 'blue', 'white', 'white', 'red', 'red')
                           )
  heatmap <-  y - t(y)
  
  p <- plot_ly(x = colnames(y), y = rownames(y), z = heatmap, type = 'heatmap',
               xgap = 0.2, ygap = 0.2, colorscale = colorScale, showscale = F) 
  p %<>% layout(yaxis = list(autorange = 'reversed', scaleratio = 1),
                xaxis = list(tickangle = 45))
  p
}

#' Helper function for Plot.Stats.Significance_Graph
#' 
#' @param x x
#' @param start default is 0
#' @param direction default is 1
#' 
#' @noRd 
radian.rescale <- function(x, start=0, direction=1) {
  c.rotate <- function(x) (x + start) %% (2 * pi) * direction
  c.rotate(rescale(x, c(0, 2 * pi), range(x)))
}

#' @rdname Plot.Stats.Significance_Graph
#' @export
Plot.Stats.Significance_Graph.DataSetList <- function(dsList, ftarget, alpha = 0.01,
                                                      bootstrap.size = 30, which = 'by_FV'){
  if (length(get_dim(dsList)) != 1 || length(get_funcId(dsList)) != 1 || length(get_algId(dsList)) < 2) {
    return(NULL)
  }
  p_matrix <- pairwise.test(dsList, ftarget, bootstrap.size, which)
  g <- graph_from_adjacency_matrix(p_matrix <= alpha, mode = 'directed', diag = F)
  lab.locs <- radian.rescale(x = 1:nrow(p_matrix), direction = -1, start = 0)
  
  plot.igraph(g, layout = layout.circle(g), vertex.size = 10, edge.arrow.size = 1,
              vertex.label.color = 'black',
              vertex.label.dist = 2,
              vertex.label.cex = 1,
              vertex.label.degree = lab.locs)
}

#' @rdname Plot.Stats.Glicko2_Candlestick
#' @export
Plot.Stats.Glicko2_Candlestick.DataSetList <- function(dsList, nr_rounds = 100, glicko2_rank_df = NULL,
                                                       which = 'by_FV', target_dt = NULL) {
  df <- glicko2_rank_df
  
  if (is.null(df)) {
    df <- glicko2_ranking(dsList, nr_rounds, which, target_dt = target_dt)$ratings
    algIds <- df$Player$algId
  }
  else{
    algIds <- df$algId
  }
  p <- IOH_plot_ly_default(title = "Glicko2-rating", 
                           x.title = "Algorithm", 
                           y.title = "Rating")
  df$Rating %<>% as.numeric
  df$Deviation %<>% as.numeric
  high <- df$Rating + 3*df$Deviation
  low <- df$Rating - 3*df$Deviation
  open <- df$Rating + df$Deviation
  close <- df$Rating - df$Deviation
  
  N <- length(df$Rating)
  colors <- get_color_scheme(algIds)
  if (length(colors != N)) {
    colors <- get_color_scheme(get_algId(dsList))
  }
  
  for (i in seq(N)) {
    # rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.52)')
    color <- list(line = list(color = colors[[i]]))
    p %<>% add_trace(type = "candlestick", x = algIds[[i]], open = open[[i]], close = close[[i]], 
                     high = high[[i]], low = low[[i]], legendgroup = algIds[[i]], 
                     name = algIds[[i]], increasing = color, decreasing = color,
                     hovertext = paste0(format(df$Rating[[i]], digits = 3), '+-', 
                                        format(df$Deviation[[i]], digits = 3)),
                     hoverinfo = "text")
  }
  p %<>% layout(xaxis = list(rangeslider = list(visible = F)))
  p
}

### _______________________ Rewritten plotting function ____________________ ###

#' Add transparancy to named list of colors
#' 
#' @param colors Named list of colors (in hex-notation)
#' @param percentage The percentage of opacity. 0 is fully transparant, 1 is fully opaque
#' 
#' @noRd
add_transparancy <- function(colors, percentage){
  hex_val <- format(as.hexmode(as.integer(255 * percentage)), upper.case = T, width = 2)
  sapply(colors, function(col) { col <- paste0('#',  substr(col, 2, 7), hex_val) })
}

#' General function for plotting within IOHanalyzer
#'
#' @param df The dataframe containing the data to plot. It should contain at least two columns:
#' 'x_attr' and 'y_attr'
#' @param x_attr The column to specify the x_axis. Default is 'algId'
#' @param legend_attr Default is 'algId' This is also used for the selection of colorschemes
#' @param y_attr The column to specify the y_axis
#' @param type The type of plot to use. Currently available: 'violin', 'line', 'radar', 'hist' and 'ribbon'
#' @param upper_attr When using ribbon-plot, this can be used to create a shaded area. 
#' Only works in combination with`lower_attr` and `type` == 'ribbon' 
#' @param lower_attr When using ribbon-plot, this can be used to create a shaded area. 
#' Only works in combination with`upper_attr` and `type` == 'ribbon' 
#' @param subplot_attr Which attribute of the dataframe to use for creating subplots
#' @param scale.xlog Logarithmic scaling of x-axis
#' @param scale.ylog Logarithmic scaling of y-axis
#' @param scale.reverse Decreasing or increasing x-axis
#' @param x_title Title of x-axis. Defaults to x_attr
#' @param y_title Title of x-axis. Defaults to x_attr
#' @param plot_title Title of x-axis. Defaults to no title
#' @param p A previously existing plot on which to add traces. If NULL, a new canvas is created
#' @param show.legend Whether or not to include a legend
#' @param ... Additional parameters for the add_trace function
#' 
#' @export
plot_general_data <- function(df, x_attr = 'algId', y_attr = 'vals', type = 'violin',
                              legend_attr = 'algId', scale.xlog = F, scale.ylog = F,
                              scale.reverse = F, p = NULL, x_title = NULL,
                              y_title = NULL, plot_title = NULL, upper_attr = NULL,
                              lower_attr = NULL, subplot_attr = NULL, show.legend = F, ...){
  
  l <- x <- NULL #Set local binding to remove warnings
  
  #Only allow valid plot types
  if (!(type %in% c('violin', 'line', 'radar', 'hist', 'ribbon', 'line+ribbon'))) {
    stop(paste0("Provided plot type ('", type, "') is not supported"))
  }
  
  #And valid number of y-attributes
  if (length(y_attr) == 0) {
    stop("At least one y-attribute is needed to plot")
  }
  
  #Deal with subplots
  if (!is.null(subplot_attr)) {
    if (!subplot_attr %in% colnames(df)) {
      stop("Provided subplot-attribut is not a colname of the selected data.table.")
    }
    colnames(df)[colnames(df) == subplot_attr] <- "subplot_attr"
    attrs <- unique(df[, subplot_attr])
    if (length(attrs) <= 1) stop("Attempting to create subplots with fewer than 2 unique values of 
                                 `subplot_attrs`-column")
    
    if (subplot_attr == legend_attr) {
      df[, l := subplot_attr]
    }
    
    #Only need one legend for the whole plot
    legends_show <- rep(F, length(attrs))
    legends_show[[1]] <- show.legend
    names(legends_show) <- attrs
    
    #Get some number of rows and columns
    n_cols <- 1 +  ceiling(length(attrs)/10)
    n_rows <- ceiling(length(attrs) / n_cols)
    
    p <- lapply(seq(length(attrs)), function(idx) {
      attr_val <- attrs[[idx]]
      df_sub <- df[subplot_attr == attr_val]
      disp_y <-  idx %% n_cols == 1
      disp_x <- idx > (length(attrs) - n_cols)
      x.title = if (disp_x) x_title else ""
      y.title = if (disp_y) y_title else ""
      
      #Generate title for the subplots
      if (stri_detect_regex(subplot_attr, "(?i)fun"))
        sub_title <- paste0('F', attr_val)
      else if (stri_detect_regex(subplot_attr, "(?i)dim"))
        sub_title <- paste0('D', attr_val)
      else
        sub_title <- paste0(attr_val)
      p <- NULL
      if (stri_detect_fixed(type, '+')) {
        type1 <- substr(type, 0, stri_locate_all(type, fixed = '+')[[1]][[1]] - 1)
        p <- plot_general_data(df_sub, x_attr, y_attr, type1, legend_attr, scale.xlog, scale.ylog, 
                               scale.reverse, NULL, x.title, y.title, plot_title, upper_attr, lower_attr, 
                               show.legend = legends_show[[attr_val]], subplot_attr = NULL, ...)
        type <- substr(type, stri_locate_all(type, fixed = '+')[[1]][[1]] + 1, nchar(type))
      }
      plot_general_data(df_sub, x_attr, y_attr, type, legend_attr, scale.xlog, scale.ylog, 
                        scale.reverse, p, x.title, y.title, plot_title, upper_attr, lower_attr, 
                        show.legend = legends_show[[attr_val]], subplot_attr = NULL, ...) %>%
        layout(
          annotations = list(
            text = sub_title, 
            font = f2,
            xref = "paper", yref = "paper", align = "center",
            yanchor = "bottom", 
            xanchor = "center", textangle = 0,
            x = 0.5, y = 1, showarrow = FALSE
          )
        )
    })
    
    
    p <- subplot(p, nrows = n_rows, titleX = T, titleY = T, margin = 0.03) %>% 
      layout(title = plot_title)
    return(p)
  }
  
  # Replace colnames to have easier matching
  if (!x_attr %in% colnames(df) || !all(y_attr %in% colnames(df))) {
    stop("Not all provided attributes are colnames of the selected data.table.")
  }
  colnames(df)[colnames(df) == x_attr] <- "x"
  
  
  if (length(y_attr) == 1 && type != 'line')
    colnames(df)[colnames(df) == y_attr] <- "y"
  else if (type != 'line') stop("Multiple y-attrs is currently only supported for line-plots")
  
  if ( !is.null(upper_attr) && !is.null(lower_attr)) {
    if (!upper_attr %in% colnames(df) || !lower_attr %in% colnames(df)) {
      stop("Provided upper and lower attributes are not colnames of the selected data.table.")
    }
    colnames(df)[colnames(df) == upper_attr] <- "upper"
    colnames(df)[colnames(df) == lower_attr] <- "lower"
  }
  
  if ( x_attr != legend_attr) {
    colnames(df)[colnames(df) == legend_attr] <- "l"
    xs <- unique(df[['l']])
  }
  else{
    xs <- unique(df[['x']])
  }
  
  #Get color and based on legend-attribute
  colors <- get_color_scheme(xs)
  names(colors) <- xs
  
  xscale <- if (scale.xlog) 'log' else 'linear'
  yscale <- if (scale.ylog) 'log' else 'linear'
  
  #If new plot is needed, create one. Store in bool to decide if axis scaling is needed.
  is_new_plot <- F
  if (is.null(p)) {
    p <- IOH_plot_ly_default(x.title = ifelse(is.null(x_title), x_attr, x_title),
                             y.title = ifelse(is.null(y_title), y_attr, y_title),
                             title = plot_title)
    is_new_plot <- T
  }
  
  switch(type,
         'violin' = {
           if (legend_attr != x_attr) {
             warning("Inconsistent attribute selected for x-axis and legend. Using x_attr as name")
           }
           p %<>%
             add_trace(data = df,
                       x = ~x, y = ~y, type = 'violin',
                       hoveron = "points+kde",
                       points = 'all',
                       pointpos = 1.5,
                       jitter = 0,
                       scalemode = 'count',
                       meanline = list(visible = F),
                       name = ~x,
                       colors = colors,
                       color = ~x,
                       split = ~x,
                       line = list(color = 'black', width = 1.1),
                       box = list(visible = T),
                       ...
             )
           if (is_new_plot) {
             p %<>% layout(yaxis = list(type = yscale, tickfont = f3, ticklen = 3))
           }
         },
         'line' = {
           if (legend_attr == x_attr) {
             stop("Duplicated attribute selected for x-axis and legend.")
           }
           #Use linestyles to differentiate traces if only one attribute is selected to be plotted
           #TODO: Combine these two options more elegantly
           if (length(y_attr) == 1) {
             dashes <- get_line_style(xs)
             names(dashes) <- xs
             colnames(df)[colnames(df) == y_attr] <- "y"
             suppressWarnings(
               p %<>%
                 add_trace(
                   data = df, x = ~x, y = ~y, color = ~l, legendgroup = ~l,
                   type = 'scatter', mode = 'lines+markers', 
                   linetype = ~l, marker = list(size = getOption('IOHanalyzer.markersize', 4)), linetypes = dashes,
                   colors = colors, showlegend = show.legend,
                   text = y_attr, line = list(width = getOption('IOHanalyzer.linewidth', 2)),
                   ...
                 ) )
           }
           else {
             dashes_full <- rep(c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"), 
                                ceiling(length(y_attr)/3))[1:length(y_attr)]
             names(dashes_full) <- y_attr
             
             for (y_atr in y_attr) {
               colnames(df)[colnames(df) == y_atr] <- "y"
               
               #TODO: Figure out how to supress warning about 6 linetypes
               dashstyle <- dashes_full[[y_atr]]
               suppressWarnings(
                 p %<>%
                   add_trace(
                     data = df, x = ~x, y = ~y, color = ~l, legendgroup = ~l,
                     type = 'scatter', mode = 'lines+markers', 
                     marker = list(size = getOption('IOHanalyzer.markersize', 4)), linetype = dashstyle,
                     colors = colors, showlegend = show.legend, name = ~l,
                     text = y_atr, line = list(width = getOption('IOHanalyzer.linewidth', 2)),
                     ...
                   )         
               )
               colnames(df)[colnames(df) == "y"] <- y_atr
               show.legend <- F
             }
           }
           if (is_new_plot) {
             p %<>% layout(xaxis = list(type = xscale, tickfont = f3, ticklen = 3,
                                        autorange = ifelse(scale.reverse, "reversed", T)),
                           yaxis = list(type = yscale, tickfont = f3, ticklen = 3))
           }
         },
         'ribbon' = {
           if (legend_attr == x_attr) {
             stop("Duplicated attribute selected for x-axis and legend.")
           }
           if (is.null(upper_attr) || is.null(lower_attr)) {
             stop("No upper or lower attribute provided for ribbon-plot")
           }
           
           for (name in xs) {
             df_small <- df[l == name]
             legend_name <- as.character(name)
             rgba_str <- paste0('rgba(', paste0(col2rgb(colors[[name]]), collapse = ','), ',0.2)')
             p %<>%
               add_trace(data = df_small, x = ~x, y = ~upper, type = 'scatter', mode = 'lines',
                         line = list(color = rgba_str, width = 0), legendgroup = legend_name,
                         showlegend = F, name = 'mean +/- sd', ...) %>%
               add_trace(x = ~x, y = ~lower, type = 'scatter', mode = 'lines',
                         fill = 'tonexty',  line = list(color = 'transparent'), legendgroup = legend_name,
                         fillcolor = rgba_str, showlegend = F, name = 'mean +/- sd', ...)
           }
           
           
           
           if (is_new_plot) {
             p %<>% layout(xaxis = list(type = xscale, tickfont = f3, ticklen = 3,
                                        autorange = ifelse(scale.reverse, "reversed", T)),
                           yaxis = list(type = yscale, tickfont = f3, ticklen = 3))
           }
         },
         'radar' = {
           if (legend_attr == x_attr) {
             stop("Duplicated attribute selected for x-axis and legend.")
           }
           #TODO: better way to force to string
           if (stri_detect_regex(x_attr, "(?i)fun"))
             df <- df[, x := paste0('F', as.character(x))]
           else if (stri_detect_regex(x_attr, "(?i)dim"))
             df <- df[, x := paste0('D', as.character(x))]
           else
             df <- df[, x := paste0('*', as.character(x))]
           
           df <- df[, col := add_transparancy(colors, 0.4)[l]]
           p %<>%
             add_trace(data = df, type = 'scatterpolar', r = ~y,
                       theta = ~x, mode = 'markers', #marker = list(color = 'lightgrey', size=0),
                       fill = 'toself', connectgaps = T, fillcolor = ~col, color = ~l, colors = colors,
                       name =  ~l, legendgroup = ~l, ...)
           if (is_new_plot) {
             p %<>% layout(polar = list(radialaxis = list(type = yscale, tickfont = f1, ticklen = 3,
                                                          autorange = ifelse(scale.reverse, "reversed", T))))
           }
         },
         'hist' = {
           if (legend_attr == x_attr) {
             stop("Duplicated attribute selected for x-axis and legend.")
           }
           if (!'width' %in% colnames(df)) {
             stop("No 'width'-column included in the provided dataframe. This is required for a histogram-plot")
           }
           p %<>%
             add_trace(data = df, x = ~x, y = ~y, width = ~width, type = 'bar',
                       name = ~l, text = ~text, hoverinfo = 'text',
                       colors = add_transparancy(colors, 0.6), color = ~l,
                       marker = list(line = list(color = 'rgb(8,48,107)', width = 1)),
                       ...)
           
           if (is_new_plot) {
             p %<>% layout(xaxis = list(type = xscale, tickfont = f3, ticklen = 3,
                                        autorange = ifelse(scale.reverse, "reversed", T)),
                           yaxis = list(type = yscale, tickfont = f3, ticklen = 3))
           }
         }
  )
  return(p)
}