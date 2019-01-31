#' Estimator 'SP' for the Expected Running Time (ERT)
#'
#' @param data A dataframe or matrix
#' @param max_runtime A Numerical vector. Should has the same size as columns of data 
#'
#' @return
#' @export
#'
#' @examples
SP <- function(data, max_runtime) {
  N <- ncol(data)
  succ <- apply(data, 1, function(x) sum(!is.na(x)))
  succ_rate <- succ / N
  
  idx <- is.na(data)
  for (i in seq(N)) {
    data[idx[, i], i] <- max_runtime[i]
  }
  
  list(ERT = rowSums(data) / succ, runs = succ, succ_rate = succ_rate)
}

ECDF <- function(ds, ...) UseMethod("ECDF", ds)
# TODO: also implement the ecdf functions for function values and parameters
#' Empirical Cumulative Dsitribution Function of Runtime of a single data set
#'
#' @param ds A DataSet object.
#' @param ftarget A Numerical vector. Function values at which runtime values are consumed
#'
#' @return a object of type 'ECDF'
#' @export
#'
#' @examples
ECDF.DataSet <- function(ds, ftarget) {
  runtime <- get_RT_sample(ds, ftarget, output = 'long')$RT
  fun <- ecdf(runtime)
  class(fun)[1] <- 'ECDF'
  attr(fun, 'min') <- min(runtime)
  attr(fun, 'max') <- max(runtime)  # the sample can be retrieved by knots(fun)
  fun
}

#' Empirical Cumulative Dsitribution Function of Runtime of a list of data sets
#'
#' @param dsList A DataSetList object
#' @param ftarget A Numerical vector or a list of numerical vector. 
#'                Function values at which runtime values are consumed. When it is a list,
#'                it should have the same length as dsList
#'
#' @return a object of type 'ECDF'
#' @export
#'
#' @examples
ECDF.DataSetList <- function(dsList, ftarget) {
  if (is.list(ftarget)) {
    stopifnot(length(dsList) == length(ftarget))
    runtime <- sapply(ftarget, function(v) get_RT_sample(dsList, v, output = 'long')$RT)
  } else {
    runtime <- get_RT_sample(dsList, ftarget, output = 'long')$RT
  }
  
  fun <- ecdf(runtime)
  class(fun)[1] <- 'ECDF'
  attr(fun, 'min') <- min(runtime)
  attr(fun, 'max') <- max(runtime)  # the sample can be retrieved by knots(fun)
  fun
}

# calculate the area under ECDFs on user specified targets
AUC <- function(fun, ...) UseMethod('AUC', fun)

#' Area Under Curve (Empirical Cumulative Dsitribution Function)
#'
#' @param fun A ECDF object.
#' @param from double Starting point of the area on x-axis
#' @param to   double. Ending point of the area on x-axis
#'
#' @return a object of type 'ECDF'
#' @export
#'
#' @examples
AUC.ECDF <- function(fun, from = NULL, to = NULL) {
  if (is.null(from)) 
    from <- attr(fun, 'min')
  if (is.null(to))
    to <- attr(fun, 'max')
  
  if (is.null(fun)) 
    0
  else 
    integrate(fun, lower = from, upper = to, subdivisions = 1e3L)$value / (to - from) 
}

# TODO: remove the function, deprecated!
CDF_discrete <- function(x) {
  x <- sort(x)
  x.unique <- unique(x)
  res <- seq_along(x) / length(x)
  for (v in x.unique) {
    res[x == v] <- max(res[x == v])
  }
  res
}
