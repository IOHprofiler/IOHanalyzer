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

# function for generating sequences for RT and FV ---------------------
# TODO: add Roxygen docs...
# TODO: maybe merge 'seq_FV' and 'seq_RT'...
# TODO: determine when the sequence should be generate in log-linear way
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
    if (nr_samples_generated > max_samples){
      by <- NULL
      if(is.null(length.out))
        length.out <- max_samples
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

# TODO: Roxygen doc...
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
    if (nr_samples_generated > max_samples){
      by <- NULL
      if(is.null(length.out))
        length.out <- max_samples
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
ECDF.DataSetList <- function(dsList, ftarget, funcId = NULL) {
  if (length(dsList) == 0) return(NULL)
  
  if (is.list(ftarget)) {
    runtime <- sapply(seq_along(ftarget), function(i) {
      Id <- funcId[i]
      data <- subset(dsList, funcId == Id)
      if (length(data) == 0) return(NA)
      res <- get_RT_sample(data, ftarget[[i]], output = 'long')$RT
      res[is.na(res)] <- Inf
      res
    }) %>%
      unlist
  } else {
    runtime <- get_RT_sample(dsList, ftarget, output = 'long')$RT
    runtime[is.na(runtime)] <- Inf
  }


  if (length(runtime) == 0) return(NULL)
  
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

#TODO: inconsistent use of format_func gives slightly different results between
#generated and uploaded targets
get_default_ECDF_targets <- function(data, format_func = as.integer){
  funcIds <- get_funcId(data)
  dims <- get_DIM(data)
  
  targets <- list()
  for (i in seq_along(funcIds)) {
    Id <- funcIds[[i]]
    data_sub <- subset(data, funcId == Id)
    for (j in seq_along(dims)) {
      dim <- dims
      data_subsub <- subset(data_sub, DIM == dim)
      fall <- get_Funvals(data_subsub)
      #TODO: Account for minimization / maximization
      fmin <- min(fall)
      fmax <- max(fall)
      
      fseq <- seq_FV(fall, fmin, fmax, length.out = 10) %>% format_func

      targets <- append(targets, list(fseq))
    }
  }
  targets %>% set_names(funcIds)
}
