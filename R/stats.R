#' Estimator 'SP' for the Expected Running Time (ERT)
#'
#' @param data A dataframe or matrix
#' @param max_runtime A Numerical vector. Should have the same size as columns of data
#'
#' @return A list containing ERTs, number of succesfull runs and the succes rate
#' @export
#' @examples 
#' SP(dsl[[1]]$RT, max(dsl[[1]]$RT))
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

#TODO: improve formatting
#' Function for generating sequences of function values
#'
#' @param FV A list of function values
#' @param from Starting function value. Will be replaced by min(FV) if it is NULL or too small
#' @param to Stopping function value. Will be replaced by max(FV) if it is NULL or too large
#' @param by Stepsize of the sequence. Will be replaced if it is too small
#' @param length.out Number of values in the sequence.
#'   'by' takes preference if both it and length.out are provided.
#' @param scale Scaling of the sequence. Can be either 'linear' or 'log', indicating a
#'   linear or log-linear spacing respectively. If NULL, the scale will be predicted
#'   based on FV
#'
#' @return A sequence of function values
#' @export
#' @examples 
#' FVall <- get_runtimes(dsl)
#' seq_FV(FVall, 10, 16, 1, scale='linear')
seq_FV <- function(FV, from = NULL, to = NULL, by = NULL, length.out = NULL, scale = NULL) {
  from <- max(from, min(FV))
  to <- min(to, max(FV))

  rev_trans <- function(x) x

  # Auto detect scaling
  # TODO: Improve this detection (based on FV?). Currently very arbitrary
  if (is.null(scale)) {
    if (to < 0 || from < 0)
      scale <- 'linear'
    else if (abs(log10(mean(FV)) - log10(median(FV))) > 1)
      scale <- 'log'
    else
      scale <- 'linear'
  }

  if (scale == 'log') {
    trans <- log10
    rev_trans <- function(x) 10 ^ x
    # TODO: Better way to deal with negative values
    #       set lowest possible target globally instead of arbitrary 1e-12
    from <- max(1e-12, from)
    to <- max(1e-12 ,to)
    from <- trans(from)
    to <- trans(to)
  }

  #Avoid generating too many samples
  if(!is.null(by)){
    nr_samples_generated <- (to-from)/by
    if (nr_samples_generated > getOption("IOHanalyzer.max_samples", default = 100)){
      by <- NULL
      if(is.null(length.out))
        length.out <- getOption("IOHanalyzer.max_samples", default = 100)
    }
  }

  if (is.null(by) || by > to - from) {
    if (is.null(length.out)) {
      length.out <- 10
      args <- list(from = from, to = to, by = (to - from) / (length.out - 1))
    } else
      args <- list(from = from, to = to, length.out = length.out)
  } else
    args <- list(from = from, to = to, by = by)

  # tryCatch({
  do.call(seq, args) %>%
    c(from, ., to) %>%    # always include the starting / ending value
    unique %>%
    rev_trans
  # }, error = function(e) {
  # c()
  # })
}

#' Function for generating sequences of runtime values
#'
#' @param RT A list of runtime values
#' @param from Starting runtime value. Will be replaced by min(RT) if it is NULL or too small
#' @param to Stopping runtime value. Will be replaced by max(RT) if it is NULL or too large
#' @param by Stepsize of the sequence. Will be replaced if it is too small
#' @param length.out Number of values in the sequence.
#'   'by' takes preference if both it and length.out are provided.
#' @param scale Scaling of the sequence. Can be either 'linear' or 'log', indicating a
#'   linear or log-linear spacing respectively.
#'
#' @return A sequence of runtime values
#' @export
#' @examples 
#' RTall <- get_runtimes(dsl)
#' seq_RT(RTall, 0, 500, length.out=10, scale='log')
seq_RT <- function(RT, from = NULL, to = NULL, by = NULL, length.out = NULL,
                   scale = 'linear') {
  rev_trans <- function(x) x

  # Do this first to avoid the log-values being overwritten.
  from <- max(from, min(RT))
  to <- min(to, max(RT))

  if (scale == 'log') {
    RT <- log10(RT)
    rev_trans <- function(x) 10 ^ x
    if (!is.null(from))
      from <- log10(from)
    if (!is.null(to))
      to <- log10(to)
    if (!is.null(by))
      by <- log10(by)
  }

  #Avoid generating too many samples
  if(!is.null(by)){
    nr_samples_generated <- (to-from)/by
    if (nr_samples_generated > getOption("IOHanalyzer.max_samples", default = 100)){
      by <- NULL
      if(is.null(length.out))
        length.out <- getOption("IOHanalyzer.max_samples", default = 100)
    }
  }

  # Also reset by if it is too large
  if (is.null(by) || by > to - from) {
    if (is.null(length.out)) {
      length.out <- 10
      args <- list(from = from, to = to, by = (to - from) / (length.out - 1))
    } else
      args <- list(from = from, to = to, length.out = length.out)
  } else
    args <- list(from = from, to = to, by = by)

  do.call(seq, args) %>%
    c(from, ., to) %>%    # always include the starting / ending value
    unique %>%
    rev_trans
}

# TODO: implement the empirical p.m.f. for runtime
EPMF <- function() {

}

#' Empirical Cumulative Dsitribution Function of Runtime of a single data set
#'
#' @param ds A DataSet or DataSetList object.
#' @param ftarget A Numerical vector. Function values at which runtime values are consumed
#' @param ... Arguments passed to other methods
#'
#' @return a object of type 'ECDF'
#' @export
#' @examples 
#' ECDF(dsl,c(12,14))
#' ECDF(dsl[[1]],c(12,14))
ECDF <- function(ds, ftarget, ...) UseMethod("ECDF", ds)

# TODO: also implement the ecdf functions for function values and parameters
#' @rdname ECDF
#' @export
ECDF.DataSet <- function(ds, ftarget, ...) {
  runtime <- get_RT_sample(ds, ftarget, output = 'long')$RT
  runtime <- runtime[!is.na(runtime)]
  if (length(runtime) == 0)
    fun <- ecdf(Inf)
  else
    fun <- ecdf(runtime)

  class(fun)[1] <- 'ECDF'
  attr(fun, 'min') <- min(runtime)
  attr(fun, 'max') <- max(runtime)  # the sample can be retrieved by knots(fun)
  fun
}

#TODO: better description of funcId parameter
#' @rdname ECDF
#' @param funcId Function Ids to use
#' @export
ECDF.DataSetList <- function(ds, ftarget, funcId = NULL, ...) {
  if (length(ds) == 0) return(NULL)

  if (is.list(ftarget)) {
    runtime <- sapply(seq_along(ftarget), function(i) {
      Id <- funcId[i]
      data <- subset(ds, funcId == Id)
      if (length(data) == 0) return(NA)
      res <- get_RT_sample(data, ftarget[[i]], output = 'long')$RT
      res[is.na(res)] <- Inf
      res
    }) %>%
      unlist
  } else {
    runtime <- get_RT_sample(ds, ftarget, output = 'long')$RT
    runtime[is.na(runtime)] <- Inf
  }


  if (length(runtime) == 0) return(NULL)

  fun <- ecdf(runtime)
  class(fun)[1] <- 'ECDF'
  attr(fun, 'min') <- min(runtime)
  attr(fun, 'max') <- max(runtime)  # the sample can be retrieved by knots(fun)
  fun
}

#' Area Under Curve (Empirical Cumulative Dsitribution Function)
#'
#' @param fun A ECDF object.
#' @param from double. Starting point of the area on x-axis
#' @param to   double. Ending point of the area on x-axis
#'
#' @return a object of type 'ECDF'
#' @export
#' @examples 
#' ecdf <- ECDF(dsl,c(12,14))
#' AUC(ecdf, 0, 100)
AUC <- function(fun, from = NULL, to = NULL) UseMethod('AUC', fun)

#' @rdname AUC
#' @export
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

#TODO: inconsistent use of format_func gives slightly different results between
#generated and uploaded targets
#' Generate ECDF targets for a DataSetList
#'
#' @param data A DataSetList
#' @param format_func function to format the targets
#'
#' @return a vector of targets
#' @export
#' @examples 
#' get_default_ECDF_targets(dsl)
get_default_ECDF_targets <- function(data, format_func = as.integer){
  funcIds <- get_funcId(data)
  dims <- get_dim(data)

  targets <- list()
  for (i in seq_along(funcIds)) {
    Id <- funcIds[[i]]
    data_sub <- subset(data, funcId == Id)
    for (j in seq_along(dims)) {
      dim <- dims
      data_subsub <- subset(data_sub, DIM == dim)
      fall <- get_funvals(data_subsub)
      #TODO: Account for minimization / maximization
      fmin <- min(fall)
      fmax <- max(fall)

      fseq <- seq_FV(fall, fmin, fmax, length.out = 10) %>% format_func

      targets <- append(targets, list(fseq))
    }
  }
  targets %>% set_names(funcIds)
}

