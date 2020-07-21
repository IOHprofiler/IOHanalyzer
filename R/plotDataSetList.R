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
      1 / (2 * color_center)
    else
      1 / (2 * (amount - color_center))
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

  if (backend == 'plotly') {
    data <- generate_data.Single_Function(dsList, Fstart, Fstop, scale.xlog, 'by_RT', includeOpts)
    
    y_attrs <- c()
    if (show.ERT) y_attrs <- c(y_attrs, 'ERT')
    if (show.mean) y_attrs <- c(y_attrs, 'mean')
    if (show.median) y_attrs <- c(y_attrs, 'median')
    show_legend <- T
    if (length(y_attrs) > 0) {
      p <- plot_general_data(data, x_attr = 'target', y_attr = y_attrs, 
                             type = 'line', legend_attr = 'algId', show.legend = show_legend, 
                             scale.ylog = scale.ylog, p = p,
                             scale.xlog = scale.xlog, x_title = "Best-so-far f(x)-value",
                             y_title = "Function Evaluations",
                             scale.reverse = scale.reverse)
      show_legend <- F
    }
    if (show.CI) {
      p <- plot_general_data(data, x_attr = 'target', y_attr = 'mean', 
                             type = 'ribbon', legend_attr = 'algId', lower_attr = 'lower', 
                             upper_attr = 'upper', p = p, show.legend = show_legend, 
                             scale.ylog = scale.ylog,
                             scale.xlog = scale.xlog, x_title = "Best-so-far f(x)-value",
                             y_title = "Function Evaluations",
                             scale.reverse = scale.reverse)
    }
  }
  # } else if (backend == 'ggplot2') {
  #   dt[, 'group' := paste(algId, funcId, DIM, sep = '-')]
  #   p <- ggplot(data = dt, aes(group = `group`, colour = `group`))
  # 
  #   if (show.CI) p <- p + geom_ribbon(aes(target, ymin = lower, ymax = upper, fill = `group`),
  #                                     alpha = 0.2, colour = NA)
  #   if (show.ERT) p <- p + geom_line(aes(target, ERT), size = 1.2)
  #   if (show.mean) p <- p + geom_line(aes(target, mean), linetype = 'dashed')
  #   if (show.median) p <- p + geom_line(aes(target, median), linetype = 'dotted')
  # 
  #   p <- p +
  #     scale_color_manual(values = colors) +
  #     scale_fill_manual(values = colors)
  # }
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

  if (backend == 'plotly') {
    data <- generate_data.Single_Function(dsList, RTstart, RTstop, scale.xlog, 'by_FV')
    
    y_attrs <- c()
    if (show.mean) y_attrs <- c(y_attrs, 'mean')
    if (show.median) y_attrs <- c(y_attrs, 'median')
    show_legend <- T
    if (length(y_attrs) > 0) {
      p <- plot_general_data(data, x_attr = 'runtime', y_attr = y_attrs, 
                             type = 'line', legend_attr = 'algId', show.legend = show_legend, 
                             scale.ylog = scale.ylog, 
                             scale.xlog = scale.xlog, x_title = "Best-so-far f(x)-value",
                             y_title = "Function Evaluations",
                             scale.reverse = scale.reverse)
      show_legend <- F
    }
    else 
      p <- NULL
    if (show.CI) {
      p <- plot_general_data(data, x_attr = 'runtime', y_attr = 'mean', 
                             type = 'ribbon', legend_attr = 'algId', lower_attr = 'lower', 
                             upper_attr = 'upper', p = p, show.legend = show_legend, 
                             scale.ylog = scale.ylog,
                             scale.xlog = scale.xlog, x_title = "Best-so-far f(x)-value",
                             y_title = "Function Evaluations",
                             scale.reverse = scale.reverse)
    }

  }
  # } else if (backend == 'ggplot2') {
  #   fce[, 'group' := paste(algId, funcId, DIM, sep = '-')]
  #   p <- ggplot(data = fce, aes(group = `group`, colour = `group`))
  # 
  #   if (show.mean) p <- p + geom_line(aes(runtime, mean), linetype = 'dashed')
  #   if (show.median) p <- p + geom_line(aes(runtime, median), linetype = 'dotted')
  # 
  #   p <- p +
  #     scale_color_manual(values = colors) +
  #     scale_fill_manual(values = colors)
  # 
  #   #TODO: add individual run etc
  # }
  return(p)
}

#' @rdname Plot.RT.PMF
#' @export
Plot.RT.PMF.DataSetList <- function(dsList, ftarget, show.sample = F,
                                    scale.ylog = F, backend = NULL){
  if (is.null(backend)) backend <- getOption("IOHanalyzer.backend", default = 'plotly')
  
  data <- generate_data.PMF(dsList, ftarget, 'by_RT')
  
  plot_general_data(data, 'algId', 'RT', scale.ylog = scale.ylog,
                    x_title = "Algorithm", y_title = "Function Evaluations")
}

#' @rdname Plot.RT.Histogram
#' @export
Plot.RT.Histogram.DataSetList <- function(dsList, ftarget, plot_mode = 'overlay', use.equal.bins = F){
  if (length(get_funcId(dsList)) != 1 || length(get_dim(dsList)) != 1) {
    warning("Invalid dataset uploaded. Please ensure the datasetlist contains data
            from only one function and only one dimension.")
    return(NULL)
  }
  data <- generate_data.hist(dsList, ftarget, use.equal.bins, 'by_RT')
  
  subplot_attr <- if (plot_mode == 'subplot') 'algId' else NULL
  plot_general_data(data, 'x', 'y', width = 'width', type = 'hist', 
                    subplot_attr = subplot_attr, x_title = "Function Evaluations",
                    y_title = "Runs")
}

#' @rdname Plot.RT.ECDF_Per_Target
#' @export
Plot.RT.ECDF_Per_Target.DataSetList <- function(dsList, ftargets, scale.xlog = F){
  req(length(ftargets) != 0)
  data <- generate_data.ECDF(dsList, ftargets, scale.xlog)
  plot_general_data(data, 'x', 'mean', 'line', 
                    x_title = "Function Evaluations",
                    y_title = "Proportion of runs", scale.xlog = scale.xlog, show.legend = T)
}

#' @rdname Plot.RT.ECDF_Single_Func
#' @export
Plot.RT.ECDF_Single_Func.DataSetList <- function(dsList, fstart = NULL, fstop = NULL,
                                          fstep = NULL, show.per_target = F,
                                          scale.xlog = F) {
  
  targets <- seq_FV(get_funvals(dsList), fstart, fstop, fstep)
  req(targets)
  
  data <- generate_data.ECDF(dsList, targets, scale.xlog)
  
  plot_general_data(data, 'x', 'mean', 'line', 
                    x_title = "Function Evaluations",
                    y_title = "Proportion of (run, target) pairs", 
                    scale.xlog = scale.xlog, show.legend = T)
}

#' @rdname Plot.RT.ECDF_AUC
#' @export
Plot.RT.ECDF_AUC.DataSetList <- function(dsList, fstart = NULL,
                                    fstop = NULL, fstep = NULL,
                                    fval_formatter = as.integer) {
  
  targets <- seq_FV(get_funvals(dsList), fstart, fstop, fstep, length.out = 10)
  req(targets)
  
  data <- generate_data.AUC(dsList, targets)
  
  plot_general_data(data, 'x', 'AUC', 'radar')
}

#' @rdname Plot.FV.PDF
#' @export
Plot.FV.PDF.DataSetList <- function(dsList, runtime, show.sample = F, scale.ylog = F){
  
  data <- generate_data.PMF(dsList, runtime, 'by_FV')
  
  plot_general_data(data, 'algId', 'f(x)', scale.ylog = scale.ylog,
                    x_title = "Algorithm", y_title = "Target Value")
}

#' @rdname Plot.FV.Histogram
#' @export
Plot.FV.Histogram.DataSetList <- function(dsList, runtime, plot_mode='overlay', use.equal.bins = F){
  if (length(get_funcId(dsList)) != 1 || length(get_dim(dsList)) != 1) {
    warning("Invalid dataset uploaded. Please ensure the datasetlist contains data
            from only one function and only one dimension.")
    return(NULL)
  }  
  data <- generate_data.hist(dsList, runtime, use.equal.bins, 'by_FV')
  
  subplot_attr <- if (plot_mode == 'subplot') 'algId' else NULL
  plot_general_data(data, 'x', 'y', width = 'width', type = 'hist', 
                    subplot_attr = subplot_attr, x_title = "Target Values",
                    y_title = "Runs")
}

#' @rdname Plot.FV.ECDF_Per_Target
#' @export
Plot.FV.ECDF_Per_Target.DataSetList <- function(dsList, runtimes, scale.xlog = F, scale.reverse = F){
  #TODO: Fvals in legend need to be formatted properly
  runtimes <- runtimes[!is.na(runtimes)]
  req(length(runtimes) != 0)
  
  data <- generate_data.ECDF(dsList, runtimes, scale.xlog, which = 'by_FV')
  
  plot_general_data(data, 'x', 'mean', 'line', 
                    x_title = "Target Value",
                    y_title = "Proportion of runs", scale.xlog = scale.xlog, 
                    show.legend = T,
                    scale.reverse = scale.reverse)
}

#' @rdname Plot.FV.ECDF_Single_Func
#' @export
Plot.FV.ECDF_Single_Func.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL,
                                          rt_step = NULL, scale.xlog = F,
                                          show.per_target = F, scale.reverse = F){

  targets <- seq_RT(get_funvals(dsList), rt_min, rt_max, rt_step)
  req(targets)
  data <- generate_data.ECDF(dsList, targets, scale.xlog, which = 'by_FV')
  
  plot_general_data(data, 'x', 'mean', 'line', 
                    x_title = "Target Value",
                    y_title = "Proportion of (run, target) pairs", 
                    scale.xlog = scale.xlog, 
                    scale.reverse = scale.reverse, show.legend = T)
}

#' @rdname Plot.FV.ECDF_AUC
#' @export
Plot.FV.ECDF_AUC.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL, rt_step = NULL) {
  targets <- seq_RT(get_runtimes(dsList), rt_min, rt_max, rt_step, length.out = 10)
  req(targets)
  data <- generate_data.AUC(dsList, targets, which = 'by_FV')
  
  plot_general_data(data, 'x', 'AUC', 'radar')
}

#' @rdname Plot.RT.Parameters
#' @export
Plot.RT.Parameters.DataSetList <- function(dsList, f_min = NULL, f_max = NULL,
                                        algids = 'all', par_name = NULL,
                                        scale.xlog = F, scale.ylog = F,
                                        show.mean = T, show.median = F,
                                        show.CI = F) {
  data <- generate_data.Parameters(dsList, scale.xlog, which = 'by_FV')
  
  y_attrs <- c()
  if (show.mean) y_attrs <- c(y_attrs, 'mean')
  if (show.median) y_attrs <- c(y_attrs, 'median')
  show_legend <- T
  if (length(y_attrs) > 0) {
    p <- plot_general_data(data, x_attr = 'target', y_attr = y_attrs, 
                           type = 'line', legend_attr = 'algId', show.legend = show_legend, 
                           scale.ylog = scale.ylog, subplot_attr = 'parId',
                           scale.xlog = scale.xlog)
    show_legend <- F
  }
  else 
    p <- NULL
  if (show.CI) {
    p <- plot_general_data(data, x_attr = 'target', y_attr = 'mean', 
                           type = 'ribbon', legend_attr = 'algId', lower_attr = 'lower', 
                           upper_attr = 'upper', p = p, show.legend = show_legend, 
                           scale.ylog = scale.ylog, subplot_attr = 'parId',
                           scale.xlog = scale.xlog)
  }
  p
}


#' @rdname Plot.FV.Parameters
#' @export
Plot.FV.Parameters.DataSetList <- function(dsList, rt_min = NULL, rt_max = NULL,
                                           algids = 'all', par_name = NULL,
                                           scale.xlog = F, scale.ylog = F,
                                           show.mean = T, show.median = F,
                                           show.CI = F) {
  data <- generate_data.Parameters(dsList, scale.xlog, which = 'by_RT')
  
  y_attrs <- c()
  if (show.mean) y_attrs <- c(y_attrs, 'mean')
  if (show.median) y_attrs <- c(y_attrs, 'median')
  show_legend <- T
  if (length(y_attrs) > 0) {
    p <- plot_general_data(data, x_attr = 'runtime', y_attr = y_attrs, 
                           type = 'line', legend_attr = 'algId', show.legend = show_legend, 
                           scale.ylog = scale.ylog, subplot_attr = 'parId',
                           scale.xlog = scale.xlog)
    show_legend <- F
  }
  else 
    p <- NULL
  if (show.CI) {
    p <- plot_general_data(data, x_attr = 'runtime', y_attr = 'mean', 
                           type = 'ribbon', legend_attr = 'algId', lower_attr = 'lower', 
                           upper_attr = 'upper', p = p, show.legend = show_legend, 
                           scale.ylog = scale.ylog, subplot_attr = 'parId',
                           scale.xlog = scale.xlog)
  }
  p
}

#' @rdname Plot.RT.ECDF_Multi_Func
#' @export
Plot.RT.ECDF_Multi_Func.DataSetList <- function(dsList, targets = NULL,
                                                scale.xlog = F) {
  if (is.null(targets) || !is.data.table(targets)) {
    targets <- get_ECDF_targets(dsList)
  }
  
  data <- generate_data.ECDF(dsList, targets, scale.xlog)
  
  plot_general_data(data, 'x', 'mean', 'line', 
                    scale.xlog = scale.xlog, 
                    x_title = "Function Evaluations",
                    y_title = "Proportion of (run, target, ...) pairs", 
                    show.legend = T)
}

#' @rdname Plot.RT.Multi_Func
#' @export
Plot.RT.Multi_Func.DataSetList <- function(dsList, scale.xlog = F,
                                           scale.ylog = F,
                                           scale.reverse = F,
                                           backend = NULL) {
  if (is.null(backend)) backend <- getOption("IOHanalyzer.backend", default = 'plotly')
  
  data <- rbindlist(lapply(get_funcId(dsList), function(fid) {
    generate_data.Single_Function(subset(dsList, funcId == fid), scale_log = scale.xlog, 
                                  which = 'by_RT')
  }))
  
  plot_general_data(data, x_attr = 'target', y_attr = 'ERT', 
                    subplot_attr = 'funcId', type = 'line', scale.xlog = scale.xlog, 
                    scale.ylog = scale.ylog, x_title = 'Best-so-far f(x)', 
                    y_title = 'ERT', show.legend = T,
                    scale.reverse = scale.reverse)
}

#' @rdname Plot.FV.Multi_Func
#' @export
Plot.FV.Multi_Func.DataSetList <- function(dsList, scale.xlog = F,
                                         scale.ylog = F,
                                         backend = NULL) {
  if (is.null(backend)) backend <- getOption("IOHanalyzer.backend", default = 'plotly')
  
  data <- rbindlist(lapply(get_funcId(dsList), function(fid) {
    generate_data.Single_Function(subset(dsList, funcId == fid), scale_log = scale.xlog, 
                                  which = 'by_FV')
  }))
  
  plot_general_data(data, x_attr = 'runtime', y_attr = 'mean', 
                    subplot_attr = 'funcId', type = 'line', scale.xlog = scale.xlog, 
                    scale.ylog = scale.ylog, x_title = 'Runtime', 
                    y_title = 'Best-so-far f(x)', show.legend = T)
}

#' @rdname Plot.RT.Aggregated
#' @export
Plot.RT.Aggregated.DataSetList <- function(dsList, aggr_on = 'funcId', targets = NULL,
                                           plot_mode = 'radar', use_rank = F,
                                           scale.ylog = T, maximize = T, erts = NULL,
                                           inf.action = 'overlap') {
  targets <- get_target_dt(dsList)
  data <- generate_data.Aggr(dsList, aggr_on = aggr_on, targets = targets)
  y_attr <- if (use_rank) 'rank' else 'value'
  y_title <- if (use_rank) 'Rank' else 'ERT'
  plot_general_data(data, type = plot_mode, x_attr = 'funcId',
                    y_attr = y_attr, x_title = "FuncId", y_title = y_title, show.legend = T,
                    scale.ylog = scale.ylog,
                    inf.action = inf.action)
}

#' @rdname Plot.FV.Aggregated
#' @export
Plot.FV.Aggregated.DataSetList <- function(dsList, aggr_on = 'funcId', runtimes = NULL,
                                      plot_mode = 'radar', use_rank = F,
                                      scale.ylog = T, fvs = NULL){
  targets <- get_target_dt(dsList, which = 'by_FV')
  data <- generate_data.Aggr(dsList, aggr_on = aggr_on, targets = targets, which = 'by_FV')
  y_attr <- if (use_rank) 'rank' else 'value'
  y_title <- if (use_rank) 'Rank' else 'Best-so-far f(x)'
  plot_general_data(data, type = plot_mode, x_attr = 'funcId',
                    y_attr = y_attr, x_title = "FuncId", y_title = y_title, show.legend = T,
                    scale.ylog = scale.ylog)
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
  c.rotate((2 * pi * (x - min(x)) / (max(x) - min(x))))
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
#' @param type The type of plot to use. Currently available: 'violin', 'line', 'radar', 
#' 'bar', hist' and 'ribbon'
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
#' @param inf.action How to deal with infinite values. Can be 'none', 'overlap' or 'jitter'
#' @param ... Additional parameters for the add_trace function
#' 
#' @export
plot_general_data <- function(df, x_attr = 'algId', y_attr = 'vals', type = 'violin',
                              legend_attr = 'algId', scale.xlog = F, scale.ylog = F,
                              scale.reverse = F, p = NULL, x_title = NULL,
                              y_title = NULL, plot_title = NULL, upper_attr = NULL,
                              lower_attr = NULL, subplot_attr = NULL, show.legend = F,
                              inf.action = 'none', ...) {
  
  l <- x <- isinf <- y <- text <- l_orig <- NULL #Set local binding to remove warnings
  
  #Only allow valid plot types
  if (!(type %in% c('violin', 'line', 'radar', 'hist', 'ribbon', 'line+ribbon', 'bar'))) {
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
    if (length(attrs) == 0) stop("Attempting to create subplots with fewer than 2 unique values of
                                 `subplot_attrs`-column")
    if (length(attrs) == 1) return(plot_general_data(df, x_attr, y_attr, type, legend_attr, scale.xlog, scale.ylog,
                                                     scale.reverse, p, x_title, y_title, attrs, upper_attr, lower_attr,
                                                     show.legend = show.legend, subplot_attr = NULL, ...))
    if (subplot_attr == legend_attr) {
      df[, l := subplot_attr]
    }
    
    #Only need one legend for the whole plot
    legends_show <- rep(F, length(attrs))
    legends_show[[1]] <- show.legend
    names(legends_show) <- attrs
    
    #Get some number of rows and columns
    n_cols <- 1 + ceiling(length(attrs)/10)
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
    
    p <- subplot(
      p, nrows = n_rows, titleX = T, titleY = T, 
      margin = c(0.02, 0.02, 0.06, 0.06)
    ) %>% 
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
           #Update names to aviod numerical legend
           if (is.numeric(df[['x']])) {
             if (stri_detect_regex(x_attr, "(?i)fun"))
               df <- df[, x := paste0('F', sprintf("%02d", x))]
             else if (stri_detect_regex(x_attr, "(?i)dim"))
               df <- df[, x := paste0('D', as.character(x))]
             else
               df <- df[, x := as.character(x)]
           }
           #Update color names as well, since the value changed
           names(colors) <- unique(df[['x']])
           
           p %<>%
             add_trace(data = df,
                       x = ~x, y = ~y, type = 'violin',
                       hoveron = "points+kde",
                       points = F,
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
                       spanmode = 'hard',
                       showlegend = show.legend,
                       ...
             )
           if (is_new_plot) {
             p %<>% layout(yaxis = list(type = yscale, tickfont = f3(), ticklen = 3))
           }
         },
         'line' = {
           if (legend_attr == x_attr) {
             stop("Duplicated attribute selected for x-axis and legend.")
           }
           
           # Force legend to be categorical
           df[, l_orig := l]
           if (is.numeric(df[['l']])) {
             df[, l := paste0('A', l)]
             names(colors) <- paste0('A', names(colors))
           }
           
           #Use linestyles to differentiate traces if only one attribute is selected to be plotted
           #TODO: Combine these two options more elegantly
           if (length(y_attr) == 1) {
             dashes <- get_line_style(xs)
             names(dashes) <- xs
             colnames(df)[colnames(df) == y_attr] <- "y"
             
             df[, isinf := is.infinite(y)]
             df[, text := as.character(round(y, getOption("IOHanalyzer.precision", 2)))]
             
             if (inf.action == 'overlap') {
               maxval <- max(df[isinf == F, 'y'])
               df[['y']][df[['isinf']]] <- 10**(ceiling(log10(maxval)) + 1)
             }
             else if (inf.action == 'jitter') {
               #TODO: Faster way to compute this
               maxval <- max(df[isinf == F, 'y'])
               for (xval in unique(df[['x']])) {
                 tempval <- 10**(ceiling(log10(maxval)) + 1)
                 for (lval in unique(df[['l']])) {
                   temp <- df[l == lval][x == xval]
                   if (nrow(temp) > 0 && df[l == lval][x == xval][['isinf']]) {
                     df[l == lval][x == xval][['y']] <- tempval
                     tempval <- 1.2 * tempval
                   }
                 }
               }
             }
             
             suppressWarnings(
               p %<>%
                 add_trace(
                   data = df, x = ~x, y = ~y, color = ~l, legendgroup = ~l_orig, name = ~l_orig,
                   type = 'scatter', mode = 'lines+markers',
                   linetype = ~l_orig, marker = list(size = getOption('IOHanalyzer.markersize', 4)), 
                   linetypes = dashes,
                   colors = colors, showlegend = show.legend,
                   text = ~text, line = list(width = getOption('IOHanalyzer.linewidth', 2)),
                   hovertemplate = '%{text}',
                   ...
                 ) 
            )
             if (inf.action != 'none') {
               p %<>% add_trace(data = df[isinf == T], x = ~x, y = ~y, legendgroup = ~l_orig, name = ~l_orig,
                    type = 'scatter', mode = 'markers',  color = ~l,
                    marker = list(symbol = 'circle-open', size = 8 + getOption('IOHanalyzer.markersize', 4)),
                    colors = colors, showlegend = F, text = 'Inf', hoverinfo = 'none',
                    ...
               )
             }   
             
           }
           else {
             if (inf.action != 'none') {
               warning("inf.action is not yet supported for multiple y-attributes")
             }
             
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
                     data = df, x = ~x, y = ~y, color = ~l, legendgroup = ~l_orig, name = ~l_orig,
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
             p %<>% layout(xaxis = list(type = xscale, tickfont = f3(), ticklen = 3,
                                        autorange = ifelse(scale.reverse, "reversed", T)),
                           yaxis = list(type = yscale, tickfont = f3(), ticklen = 3))
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
                         showlegend = F, name = 'upper', ...) %>%
               add_trace(x = ~x, y = ~lower, type = 'scatter', mode = 'lines',
                         fill = 'tonexty',  line = list(color = 'transparent'), legendgroup = legend_name,
                         fillcolor = rgba_str, showlegend = F, name = 'lower', ...)
           }
           
           
           
           if (is_new_plot) {
             p %<>% layout(xaxis = list(type = xscale, tickfont = f3(), ticklen = 3,
                                        autorange = ifelse(scale.reverse, "reversed", T)),
                           yaxis = list(type = yscale, tickfont = f3(), ticklen = 3))
           }
         },
         'radar' = {
           if (legend_attr == x_attr) {
             stop("Duplicated attribute selected for x-axis and legend.")
           }
           if (is.numeric(df[['x']])) {
             if (stri_detect_regex(x_attr, "(?i)fun"))
               df <- df[, x := paste0('F', sprintf("%02d", x))]
             else if (stri_detect_regex(x_attr, "(?i)dim"))
               df <- df[, x := paste0('D', as.character(x))]
             else
               df <- df[, x := as.character(x)]
           }
           df <- df[, col := add_transparancy(colors, 0.4)[l]]
           p %<>%
             add_trace(data = df, type = 'scatterpolar', r = ~y,
                       theta = ~x, mode = 'markers', #marker = list(color = 'lightgrey', size=0),
                       fill = 'toself', connectgaps = T, fillcolor = ~col, color = ~l, colors = colors,
                       name =  ~l, legendgroup = ~l, ...)
           if (is_new_plot) {
             p %<>% layout(polar = list(radialaxis = list(type = yscale, tickfont = f3(), ticklen = 3,
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
                       marker = list(line = list(color = 'rgb(8,48,107)')),
                       ...)
           
           if (is_new_plot) {
             p %<>% layout(xaxis = list(type = xscale, tickfont = f3(), ticklen = 3,
                                        autorange = ifelse(scale.reverse, "reversed", T)),
                           yaxis = list(type = yscale, tickfont = f3(), ticklen = 3))
           }
         },
        'bar' = {
          if (legend_attr != x_attr) {
            warning("Inconsistent attribute selected for x-axis and legend. Using x_attr as name")
          }
          p %<>%
            add_trace(data = df, x = ~x, y = ~y, type = 'bar',
                      name = ~x,
                      colors = add_transparancy(colors, 0.6), color = ~x,
                      marker = list(line = list(color = 'rgb(8,48,107)')),
                      ...)
          
          if (is_new_plot) {
            p %<>% layout(xaxis = list(tickfont = f3(), ticklen = 3),
                          yaxis = list(type = yscale, tickfont = f3(), ticklen = 3))
          }
        }
  )
  return(p)
}


#' Create the PerformViz plot
#' 
#' From the paper: 
#' 
#' @param DSC_rank_result The result from a call to DSCtool rank service (`get_dsc_rank`)
#' 
#' @return A performviz plot
#' @export
#' @examples
#' \dontrun{
#' Plot.Performviz(get_dsc_rank(dsl))
#' }
Plot.Performviz <- function(DSC_rank_result) {
  if (!requireNamespace("ComplexHeatmap", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("reshape2", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if (!requireNamespace("grid", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  mlist <- DSC_rank_result$ranked_matrix
  
  problem <- NULL #Assign variable to remove warnings
  # df_temp <- rbindlist(lapply(mlist[[problem_idx]]$result, 
  #                             function(x) {
  #                               list(algorithm = x$algorithm, rank =  x$rank)
  #                             }))
  # df_temp[, problem := mlist[[problem_idx]]$problem]
  
  df <- rbindlist(lapply(seq(length(mlist)), function(problem_idx) {
    df_temp <- rbindlist(lapply(mlist[[problem_idx]]$result, 
                                function(x) {
                                  list(algorithm = x$algorithm, rank =  x$rank)
                                }))
    df_temp[, problem := mlist[[problem_idx]]$problem]
  }))
  
  rank_matrix <- reshape2::acast(df, algorithm ~ problem, value.var = 'rank')
  df <- rank_matrix
  # colnames(df)<-index
  # rownames(df)<-vector
  # Define some graphics to display the distribution of columns
  # library(ComplexHeatmap)
  .hist = ComplexHeatmap::anno_histogram(df, gp = grid::gpar(fill = "lightblue"))
  .density = ComplexHeatmap::anno_density(df, type = "line", gp = grid::gpar(col = "blue"))
  ha_mix_top = ComplexHeatmap::HeatmapAnnotation(hist = .hist, density = .density)
  # Define some graphics to display the distribution of rows
  .violin = ComplexHeatmap::anno_density(df, type = "violin", 
                         gp = grid::gpar(fill = "lightblue"), which = "row")
  .boxplot = ComplexHeatmap::anno_boxplot(df, which = "row")
  ha_mix_right = ComplexHeatmap::HeatmapAnnotation(violin = .violin, bxplt = .boxplot,
                                   which = "row", width = grid::unit(4, "cm"))
  # Combine annotation with heatmap
  ComplexHeatmap::Heatmap(df, name = "Ranking", 
          column_names_gp = grid::gpar(fontsize = 8),
          top_annotation = ha_mix_top, 
          top_annotation_height = grid::unit(3.8, "cm")) + ha_mix_right
  
}
