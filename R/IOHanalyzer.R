#' @importFrom stats dt ecdf integrate median quantile sd 
#' @importFrom grDevices col2rgb colors nclass.FD
#' @importFrom graphics hist
#' @importFrom utils data head read.csv tail
#' @importFrom dplyr %>% mutate
#' @importFrom magrittr set_names set_rownames set_colnames %<>% mod
#' @importFrom colorspace sequential_hcl
#' @importFrom RColorBrewer brewer.pal
#' @importFrom colorRamps primary.colors
#' @importFrom data.table as.data.table rbindlist data.table fread := melt is.data.table
#' @importFrom plotly add_annotations add_trace orca plot_ly rename_ subplot layout
#' @importFrom ggplot2 aes geom_jitter geom_line geom_ribbon geom_violin ggplot element_text
#' @importFrom ggplot2 guides scale_color_manual scale_colour_manual scale_fill_manual
#' @importFrom ggplot2 scale_x_continuous scale_x_log10 facet_wrap theme_set theme_grey theme
#' @importFrom shiny req
#' @importFrom Rcpp sourceCpp
#' @importFrom withr with_dir
#' @useDynLib IOHanalyzer
NULL
#Ugly hack, but appears to be required to appease CRAN
utils::globalVariables(c(".", "algId", "run", "ERT", "RT", "group",
                         "DIM", "Fvalue", "lower", "upper", "target", "format",
                         "runtime", "parId", "instance", "input", "funcId",
                         "budget", "dimension", "loss", "name", "optimizer_name",
                         "rescale"))

options(shiny.port = 4242)

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.IOHanalyzer <- list(
    IOHanalyzer.quantiles = c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.,
    IOHanalyzer.max_samples = 100,
    IOHanalyzer.backend = 'plotly',
    IOHanalyzer.bgcolor = 'rgb(229,229,229)',
    IOHanalyzer.gridcolor = 'rgb(255,255,255)',
    IOHanalyzer.tickcolor = 'rgb(127,127,127)',
    IOHanalyzer.figure_width = NULL,
    IOHanalyzer.figure_height = NULL,
    IOHanalyzer.legend_location = 'outside_right',
    IOHanalyzer.legend_fontsize = 13,
    IOHanalyzer.label_fontsize = 16,
    IOHanalyzer.title_fontsize = 16,
    IOHanalyzer.tick_fontsize = 16
  )
  toset <- !(names(op.IOHanalyzer) %in% names(op))
  if (any(toset)) options(op.IOHanalyzer[toset])
  
  invisible()
}


IOHanalyzer_env <- new.env(parent = emptyenv())

# IOHanalyzer_env$probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.

.mean <- function(x) mean(x, na.rm = T)
.median <- function(x) median(x, na.rm = T)
.sd <- function(x) sd(x, na.rm = T)
.sum <- function(x) sum(x, na.rm = T)

IOHanalyzer_env$D_quantile <- function(x, pct = NULL){
  if (is.null(pct)) pct <- getOption("IOHanalyzer.quantiles")
  quantile(x, pct, names = F, type = 3, na.rm = T)
}
IOHanalyzer_env$C_quantile <- function(x, pct = NULL){
  if (is.null(pct)) pct <- getOption("IOHanalyzer.quantiles")
  quantile(x, pct, names = F, na.rm = T)
}

IOHprofiler <- 'IOHprofiler'
COCO <- 'COCO'
BIBOJ_COCO <- 'BIBOJ_COCO'
TWO_COL <- 'TWO_COL'
AUTOMATIC <- 'AUTOMATIC'
NEVERGRAD <- 'NEVERGRAD'

# IOHanalyzer_env$max_samples <- 100
# IOHanalyzer_env$default_backend <- 'plotly'

#' is.wholenumber <-
#'   function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
#' 
#' #' Set the value of some global IOHanalyzer properties
#' #' 
#' #' @param prop_name The property to set. Current options are:
#' #' \itemize{
#' #' \item 'probs': The probabilities in the displayed quantiles (RT and FV summaries). Needs a vector of probabilities
#' #' \item 'max_samples': The maximum number of samples to generate for each algorithm. Needs a single integer
#' #' }
#' #' @param value The value to set the property to
#' #' @param ... Addition arguments
#' #' 
#' #' @export
#' #' @examples 
#' #' set_property("probs",c(0.1,0.25,0.5,0.75,0.9))
#' set_property <- function(prop_name, value, ...){
#'     switch(prop_name, 
#'         "probs" = {
#'             req(length(value) > 0 && !any(value >= 1 | value <= 0))
#'             IOHanalyzer_env$probs <- value
#'             IOHanalyzer_env$D_quantile <- function(x, pct = IOHanalyzer_env$probs) quantile(x, pct, names = F, type = 3, na.rm = T)
#'             IOHanalyzer_env$C_quantile <- function(x, pct = IOHanalyzer_env$probs) quantile(x, pct, names = F, na.rm = T)
#'         },
#'         "max_samples" = {
#'             req(is.wholenumber(value))
#'             IOHanalyzer_env$max_samples <- value
#'         },
#'         "backend" = {
#'           req(value %in% c('plotly', 'ggplot2'))
#'           IOHanalyzer_env$default_backend <- value
#'         },
#'         "colorscheme" = {
#'           set_colorScheme(value, ...)
#'         }
#'     )
#' }
#' 
#' #' Get the value of some global IOHanalyzer properties
#' #' 
#' #' @param prop_name The property to get. Current options are:
#' #' \itemize{
#' #' \item 'probs': The probabilities in the displayed quantiles (RT and FV summaries).
#' #' \item 'max_samples': The maximum number of samples to generate for each algorithm.
#' #' }
#' #' @param ... Arguments passed to underlying functions (i.e. number of colors to get colorscheme for)
#' #' 
#' #' @export
#' #' @examples 
#' #' get_property("probs")
#' get_property <- function(prop_name, ...){
#'     switch(prop_name, 
#'         "probs" = {
#'             IOHanalyzer_env$probs
#'         },
#'         "max_samples" = {
#'             IOHanalyzer_env$max_samples
#'         },
#'         "backend" = {
#'           IOHanalyzer_env$default_backend
#'         },
#'         "colorscheme" = {
#'           IOHanalyzer_env$used_colorscheme(...)
#'         }
#'     )
#' }

#' IOHanalyzer: Data Analysis Part of IOHprofiler
#'
#' The data analysis module for the Iterative Optimization Heuristics Profiler (IOHprofiler).
#' This module provides statistical analysis methods for the benchmark data generated by
#' optimization heuristics, which can be visualized through a
#' web-based interface. The benchmark data is usually generated by the
#' experimentation module, called IOHexperimenter. IOHanalyzer also supports
#' the widely used COCO (Comparing Continuous Optimisers) data format for benchmarking.
#'
#' @section Functions:
#' The IOHanalyzer consists of 3 main functionalities:
#' \itemize{
#' \item Reading and alligning data from different heuristics, such as IOHExperimenter.
#' This is done using the \code{\link{DataSet}} and \code{\link{DataSetList}} functions
#' \item Processing and summarizing this data
#' \item Creating various plots
#' }
#'
#' @docType package
#' @name IOHanalyzer
#' @examples 
#' path <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package="IOHanalyzer")
#' dsList <- DataSetList(path)
#' summary(dsList)
#' Plot.RT.Single_Func(dsList[1])
#' 
#' @examples 
#' \donttest{
#' runServer()
#' }
NULL
