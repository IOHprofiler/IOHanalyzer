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

# adding S3 generics
ECDF <- function(ds, ...) UseMethod('ECDF', ds)

# TODO: 
#' Empirical Cumulative Dsitribution Function of Runtime of a single data set
#'
#' @param ds A DataSet object.
#' @param ftarget A Numerical vector. Function values at which runtime values are consumed
#'
#' @return a object of type 'ecdf'
#' @export
#'
#' @examples
ECDF.DataSet <- function(ds, ftarget) {
  runtime <- get_RT_sample(ds, ftarget, output = 'long')$RT
  fun <- ecdf(runtime)
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
#' @return a object of type 'ecdf'
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
  attr(fun, 'min') <- min(runtime)
  attr(fun, 'max') <- max(runtime)  # the sample can be retrieved by knots(fun)
  fun
}

# TODO: remove the function, deprecated!
# functions to compute statistics from the data set
RT.ECDF <- function(x) {
  x <- sort(x)
  x.unique <- unique(x)
  p <- seq_along(x) / length(x)
  for (v in x.unique) {
    p[x == v] <- max(p[x == v])
  }
  
  f <- ecdf(x)
  attr(f, 'x') <- x.unique
  attr(f, 'p') <- p
  attr(f, 'min') <- min(x)
  attr(f, 'max') <- max(x)
  f
}

# calculate the area under ECDFs on user specified targets
AUC <- function(fun, ...) UseMethod('AUC', fun)

AUC.ecdf <- function(fun, from = NULL, to = NULL) {
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
ECDF_AUC <- function(df, ftargets) {
  funs <- lapply(ftargets, function(f) {
    RT(df, f, format = 'long') %>% 
      '$'('RT') %>% {
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
