#' Estimator 'SP' for the Expected Running Time (ERT)
#'
#' @param data A dataframe or matrix. Each row stores the runtime sample points from
#' several runs
#' @param max_runtime A Numerical vector. Should have the same size as columns of data
#'
#' @return A list containing ERTs, number of succesfull runs and the succes rate
#' @export
#' @examples
#' SP(dsl[[1]]$RT, max(dsl[[1]]$RT))
SP <- function(data, max_runtime) {
  N <- ncol(data)
  M <- nrow(data)

  succ <- apply(data, 1, function(x) sum(!is.na(x)))
  succ_rate <- succ / N
  idx <- is.na(data)

  for (i in seq(M)) {
    data[i, idx[i, ]] <- max_runtime[idx[i, ]]
  }

  list(ERT = rowSums(data) / succ, runs = succ, succ_rate = succ_rate)
}

#' Bootstrapping for running time samples
#'
#' @param x A numeric vector. A sample of the running time.
#' @param max_eval A numeric vector, containing the maximal running time in
#' each run. It should have the same size as x
#' @param bootstrap.size integer, the size of the bootstrapped sample
#'
#' @return A numeric vector of the bootstrapped running time sample
#' @export
#' @examples
#' ds <- dsl[[1]]
#' x <- get_RT_sample(ds, ftarget = 16, output = 'long')
#' max_eval <- get_maxRT(dsl, output = 'long')
#' bootstrap_RT(x$RT, max_eval$maxRT, bootstrap.size = 30)
bootstrap_RT <- function(x, max_eval, bootstrap.size) {
  x_succ <- x[!is.na(x)]
  x_unsucc <- max_eval[is.na(x)]
  n_succ <- length(x_succ)
  n_unsucc <- length(x_unsucc)

  p <- n_succ / length(x)
  N <- rgeom(bootstrap.size, p)

  if (n_succ == 0){
    return (rep(Inf, bootstrap.size))
  }

  sapply(N,
         function(size) {
           if (size > 0)
             x <- sum(sample(x_unsucc, size, replace = T))
           else
             x <- 0
           x <- x + sample(x_succ, 1, replace = T)
         })
}

# TODO: remove the bootstrapping part as it does not make much sense here...
#' Performs a pairwise Kolmogorov-Smirnov test on the bootstrapped running times
#' among a data set
#'
#' @description This function performs a Kolmogorov-Smirnov test on each pair of
#' algorithms in the input x to determine which algorithm gives a significantly
#' smaller running time. The resulting p-values are arranged in a matrix, where
#' each cell (i, j) contains a p-value from the test with alternative hypothesis:
#' the running time of algorithm i is smaller (thus better) than that of j.
#'
#' @param x either a list that contains running time sample for each algorithm as
#' sub-lists, or a DataSetList object
#' @param bootstrap.size integer, the size of the bootstrapped sample. Set to 0 to disable bootstrapping
#' @param ... all other options
#' @return A matrix containing p-values of the test
#' @export
#' @examples
#' pairwise.test(subset(dsl, funcId == 1), 16)
pairwise.test <- function(x, ...) UseMethod('pairwise.test', x)

#' @param max_eval list that contains the maximal running time for each algorithm
#' as sub-lists
#' @export
#' @rdname pairwise.test
pairwise.test.list <- function(x, max_eval, bootstrap.size = 30, ...) {
  if ("DataSetList" %in% class(x)) {
    class(x) <- rev(class(x))
    pairwise.test.DataSetList(x)
  }

  N <- length(x)
  p.value <- matrix(NA, N, N)

  for (i in seq(1, N - 1)) {
    for (j in seq(i + 1, N)) {
      if (bootstrap.size == 0) {
        x1 <- x[[i]]
        x2 <- x[[j]]
      } else {
        x1 <- bootstrap_RT(x[[i]], max_eval[[i]], bootstrap.size)
        x2 <- bootstrap_RT(x[[j]], max_eval[[j]], bootstrap.size)
      }
      if (all(is.na(x1))) {
        if (all(is.na(x2))) {
          next
        }
        else {
          p.value[i, j] <- 1
          p.value[j, i] <- 0
        }
      }
      else if (all(is.na(x2))) {
        p.value[i, j] <- 0
        p.value[j, i] <- 1
      }
      else {
        options(warn = -1)
        p.value[i, j] <- ks.test(x1, x2, alternative = 'greater', exact = F)$p.value
        p.value[j, i] <- ks.test(x1, x2, alternative = 'less', exact = F)$p.value
        options(warn = 0)
      }
    }
  }

  p.value.adjust <- p.adjust(as.vector(p.value), method = 'holm')
  p.value <- matrix(p.value.adjust, N, N)
  colnames(p.value) <- rownames(p.value) <- names(x)
  p.value
}

#' @param ftarget float, the target value used to determine the running / hitting
#' @param which wheter to do fixed-target ('by_FV') or fixed-budget ('by_RT') comparison
#' time
#' @export
#' @rdname pairwise.test
pairwise.test.DataSetList <- function(x, ftarget, bootstrap.size = 0, which = 'by_FV', ...) {
  if (which == 'by_FV') {
    dt <- get_RT_sample(x, ftarget, output = 'long')
    maxRT <- get_maxRT(x, output = 'long')
    maxRT <- split(maxRT$maxRT, maxRT$algId)
    s <- split(dt$RT, dt$algId)
  }
  else if (which == 'by_RT') {
    dt <- get_FV_sample(x, ftarget, output = 'long')
    maxRT <- NULL
    if (bootstrap.size > 0) warning("Bootstrapping is currently not supported for
                                    fixed-budget statistics.")
    bootstrap.size = 0
    s <- split(dt$`f(x)`, dt$algId)
  }
  else stop("Unsupported argument 'which'. Available options are 'by_FV' and 'by_RT'")
  
  return(pairwise.test.list(s, maxRT, bootstrap.size))
}

# TODO: move those two functions to a separate file
# TODO: review / re-write this function
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
  if (!is.null(by)) {
    nr_samples_generated <- (to - from) / by
    if (nr_samples_generated > getOption("IOHanalyzer.max_samples", default = 100)) {
      by <- NULL
      if (is.null(length.out))
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

# TODO: review / re-write this function
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
  if (!is.null(by)) {
    nr_samples_generated <- (to - from) / by
    if (nr_samples_generated > getOption("IOHanalyzer.max_samples", default = 100)) {
      by <- NULL
      if (is.null(length.out))
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
    rev_trans %>%
    round
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
  attr(fun, 'samples') <- length(runtime)
  attr(fun, 'max') <- max(runtime)  # the sample can be retrieved by knots(fun)
  fun
}

# TODO: review / re-write this function
#' @rdname ECDF
#' @export
ECDF.DataSetList <- function(ds, ftarget, ...) {
  if (length(ds) == 0) return(NULL)

  dims <- unique(get_dim(ds))
  funcs <- unique(get_funcId(ds))

  if (is.data.table(ftarget)) {
    runtime <- sapply(seq(nrow(ftarget)), function(i) {
      if (length(dims) > 1 && length(funcs) > 1) {
        names_temp <- ftarget[i][[1]] %>%
          strsplit(., ';')
        FuncId <- names_temp[[1]][[1]]
        Dim <- names_temp[[1]][[2]]
      }
      else if (length(dims) > 1) {
        FuncId <- funcs[[1]]
        Dim <- ftarget[i][[1]]
      }
      else if (length(funcs) > 1) {
        FuncId <- ftarget[i][[1]]
        Dim <- dims[[1]]
      }
      data <- subset(ds, funcId == FuncId, DIM == Dim)
      if (length(data) == 0) return(NA)
      temp_targets <- ftarget[i] %>%
        unlist %>%
        as.numeric
      names(temp_targets) <- NULL
      res <- get_RT_sample(data, temp_targets[2:11], output = 'long')$RT
      res[is.na(res)] <- Inf
      res
    }) %>%
      unlist
  } else if (is.list(ftarget)) {
    runtime <- sapply(seq_along(ftarget), function(i) {
      if(length(dims) > 1 && length(funcs) >1){
        names_temp <- names(ftarget[i])[[1]] %>%
          strsplit(., ';')
        FuncId <- names_temp[[1]][[1]]
        Dim <- names_temp[[1]][[2]]
      }
      else if(length(dims) > 1){
        FuncId <- funcs[[1]]
        Dim <- names(ftarget[i])[[1]]
      }
      else if(length(funcs) > 1){
        FuncId <- names(ftarget[i])[[1]]
        Dim <- dims[[1]]
      } else {
        FuncId <- funcs[[1]]
        Dim <- dims[[1]]
      }
      data <- subset(ds, funcId == FuncId, DIM == Dim)
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

#' Generate datatables of runtime or function value targets for a DataSetList
#'
#' Only one target is generated per (function, dimension)-pair, as opposed to the
#' function `get_default_ECDF_targets`, which generates multiple targets.
#'
#' @param dsList A DataSetList
#' @param which Whether to generate fixed-target ('by_FV') or fixed-budget ('by_RT') targets
#'
#' @return a data.table of targets
#' @export
#' @examples
#' get_target_dt(dsl)
get_target_dt <- function(dsList, which = 'by_RT') {
  vals <- c()
  funcs <- get_funcId(dsList)
  dims <- get_dim(dsList)
  dt <- as.data.table(expand.grid(funcs, dims))
  colnames(dt) <- c("funcId", "DIM")
  if (which == 'by_RT') {
    target_func <- get_target_FV
  }
  else if (which == 'by_FV') {
    target_func <- get_target_RT
  }
  else stop("Invalid argument for `which`; can only be `by_FV` or `by_RT`.")
  targets <- apply(dt, 1,
                   function(x) {target_func(subset(dsList, funcId == x[[1]], DIM == x[[2]]))})
  dt[, target := targets]

  return(dt)
}

#' Helper function for `get_target_dt`
#' @noRd
get_target_FV <- function(dsList){
  vals <- get_FV_summary(dsList, Inf)[[paste0(100*getOption("IOHanalyzer.quantiles", 0.02)[[1]], '%')]]
  if (is.null(attr(dsList, 'maximization')) || attr(dsList, 'maximization')) {
    return(max(vals))
  }
  else {
    return(min(vals))
  }
}

#' Helper function for `get_target_dt`
#' @noRd
get_target_RT <- function(dsList){
  return(max(get_runtimes(dsList)))
}

#' Glicko2 raning of algorithms
#'
#' This procedure ranks algorithms based on a glicko2-procedure.
#' Every round (total nr_rounds), for every function and dimension of the datasetlist,
#' each pair of algorithms competes. This competition samples a random runtime for the
#' provided target (defaults to best achieved target). Whichever algorithm has the lower
#' runtime wins the game. Then, from these games, the glicko2-rating is determined.
#'
#' @param dsl The DataSetList, can contain multiple functions and dimensions, but should have the
#' same algorithms for all of them
#' @param nr_rounds The number of rounds to run. More rounds leads to a more accurate ranking.
#' @param which Whether to use fixed-target ('by_FV') or fixed-budget ('by_RT') perspective
#' @param target_dt Custom data.table target value to use. When NULL, this is selected automatically.
#' @return A dataframe containing the glicko2-ratings and some additional info
#'
#' @export
#' @examples
#' glicko2_ranking(dsl, nr_round = 25)
#' glicko2_ranking(dsl, nr_round = 25, which = 'by_RT')
glicko2_ranking <- function(dsl, nr_rounds = 100, which = 'by_FV', target_dt = NULL){
  if (!requireNamespace("PlayerRatings", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }
  req(length(get_algId(dsl)) > 1)

  if (!is.null(target_dt) && !('data.table' %in% class(target_dt))) {
    warning("Provided `target_dt` argument is not a data.table")
    target_dt <- NULL
  }

  if (is.null(target_dt))
    target_dt <- get_target_dt(dsl, which)

  if (!(which %in% c('by_FV', 'by_RT')))
    stop("Invalid argument: 'which' can only be 'by_FV' or 'by_RT'")
  p1 <- NULL
  p2 <- NULL
  scores <- NULL
  weeks <- NULL
  get_dims <- function(){
    dims <- get_dim(dsl)
    if (length(dims) > 1) {
      dims <- sample(dims)
    }
    dims
  }
  get_funcs <- function(){
    funcs <- get_funcId(dsl)
    if (length(funcs) > 1) {
      funcs <- sample(funcs)
    }
    funcs
  }
  n_algs = length(get_algId(dsl))
  alg_names <- NULL
  for (k in seq(1,nr_rounds)) {
    for (dim in get_dims()) {
      targets_temp <- target_dt[target_dt$DIM == dim]
      for (fid in get_funcs()) {
        dsl_s <- subset(dsl, funcId == fid && DIM == dim)
        if (which == 'by_FV') {
          target <- targets_temp[targets_temp$funcId == fid]$target
          x_arr <- get_RT_sample(dsl_s, target)
          win_operator <- `<`
        }
        else {
          target <- targets_temp[targets_temp$funcId == fid]$target
          x_arr <- get_FV_sample(dsl_s, target)
          win_operator <- ifelse(attr(dsl, 'maximization'), `>`, `<`)
        }
        vals = array(dim = c(n_algs,ncol(x_arr) - 4))
        for (i in seq(1,n_algs)) {
          z <- x_arr[i]
          y <- as.numeric(z[,5:ncol(x_arr)])
          vals[i,] = y
        }
        if (is.null(alg_names)) alg_names <- x_arr[,3]

        for (i in seq(1,n_algs)) {
          for (j in seq(i,n_algs)) {
            if (i == j) next
            weeks <- c(weeks, k)
            s1 <- sample(vals[i,], 1)
            s2 <- sample(vals[j,], 1)
            if (is.na(s1)) {
              if (is.na(s2)) {
                won <- 0.5
              }
              else{
                won <- 0
              }
            }
            else{
              if (is.na(s2)) {
                won <- 1
              }
              else if (s1 == s2) {
                won <- 0.5 #Tie
              }
              else {
                won <- win_operator(s1, s2)
              }
            }
            p1 <- c(p1, i)
            p2 <- c(p2, j)
            scores <- c(scores, won)

          }
        }
      }
    }
  }
  # weeks <- seq(1,1,length.out = length(p1))
  games <- data.frame(Weeks = weeks, Player1 = p1, Player2 = p2, Scores = as.numeric(scores))
  lout <- PlayerRatings::glicko2(games, init =  c(1500,350,0.06))
  lout$ratings$Player <- alg_names[lout$ratings$Player]
  lout
}


#' Verify that the credentials for DSCtool have been set
#' 
#' This uses the keyring package to store and load credentials. 
#' If the keyring package does not exists, it will default to look for
#' a config-file in the 'repository'-folder, under your home directory.
#' This can be changed by setting the option IOHprofiler.config_dir
#' If you already have an account, please call `set_DSC_credentials`
#' with the corresponding username and password. 
#' If you don't have an account, you can register for one using `register_DSC`
#' 
#' 
#' 
#' @export
#' @examples 
#' check_dsc_configured()
check_dsc_configured <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    warning("It is recommended to have the 'keyring' package installed to store
            DSCtool settings. Since this package is not found, we default
            to a local settings-file instead.")
    repo_dir <- paste0(file.path(Sys.getenv('HOME'), 'repository'))
    if (!is.null(getOption("IOHprofiler.repo_dir"))) {
      repo_dir <- getOption("IOHprofiler.repo_dir")
    }
    if (!file.exists(repo_dir, "/config.rds")) {
      return(FALSE)
    }
    data <- readRDS(paste0(find.package("IOHanalyzer"), "/config.rds"))
    if (is.null(data$DSCusername) || is.null(data$DSCpassword)) {
      return(FALSE)
    } 
    return(TRUE)
  }
  tryCatch({
    username <- keyring::key_get("DSCtool_name")
    pwd <- keyring::key_get("DSCtool")
    return(TRUE)
    },
    error = function(e) {
      print("DSCtool connection not yet set. Please use the function `register_DSC` to 
            create an account to use the DSC tool locally (or use 'add_DSC_credentials'
            to use an existing account)")
      return(FALSE)
    }
  )
}

#' Register an account to the DSCtool API
#' 
#' This uses the keyring package to store and load credentials. 
#' If you already have an account, please call `set_DSC_credentials` instead
#' 
#' @param name Your name
#' @param username A usename to be identified with. Will be stored on keyring under 'DSCtool_name'
#' @param affiliation Your affiliation (university / company)
#' @param email Your email adress
#' @param password The password to use. If NULL, this will be generated at random. 
#' Will be stored on keyring under 'DSCtool'
#' 
#' @export
#' @examples
#' \dontrun{ 
#' register_DSC('John Doe', 'jdoe', 'Sample University', "j.doe.sample.com")
#' }
register_DSC <- function(name, username, affiliation, email, password = NULL) {
  if (is.null(password)) password <- stri_rand_strings(1, 10)
  url <- "https://ws.ijs.si:8443/dsc-1.5/service/manage/user"
  result_json <- POST(url, add_headers(.headers = c('Content-Type' = "application/json",
                                                    'Accept' = "application/json")),
                      body = list(name = name, affiliation = affiliation, email = email,
                                  username = username, password = password), 
                      encode = "json")
  if (result_json$status_code == 200) {
    if (!requireNamespace("keyring", quietly = TRUE)) {
      repo_dir <- paste0(file.path(Sys.getenv('HOME'), 'repository'))
      if (!is.null(getOption("IOHprofiler.repo_dir"))) {
        repo_dir <- getOption("IOHprofiler.repo_dir")
      }
      saveRDS(list(DSCusername = username, DSCpassword = password), 
              paste0(repo_dir, "/config.rds"))
      return(TRUE)
    }
    else {
      keyring::key_set_with_value("DSCtool", password = password)
      keyring::key_set_with_value("DSCtool_name", password = username)
      return(TRUE)
    }
  }
  else {
    stop("Something went wrong in registering for DSCtool")
  }
}

#' Register an account to the DSCtool API
#' 
#' This uses the keyring package to store and load credentials. 
#' If you already have an account, please call `add_DSC_credentials` instead
#' 
#' @param username The usename you use on DSCtool. Will be stored on keyring under 'DSCtool_name'
#' @param password The password you use on DSCtool. Will be stored on keyring under 'DSCtool'
#' 
#' @export
#' @examples 
#' \dontrun{
#' set_DSC_credentials('jdoe', 'monkey123')
#' }
set_DSC_credentials <- function(username, password) {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    repo_dir <- paste0(file.path(Sys.getenv('HOME'), 'repository'))
    if (!is.null(getOption("IOHprofiler.repo_dir"))) {
      repo_dir <- getOption("IOHprofiler.repo_dir")
    }
    saveRDS(list(DSCusername = username, DSCpassword = password), 
            paste0(repo_dir, "/config.rds"))
  }
  else {
    keyring::key_set_with_value("DSCtool", password = password)
    keyring::key_set_with_value("DSCtool_name", password = username)
  }
}

#' Load stored credentials for DSCtool
#' 
#' 
#' @noRd
#' @examples 
#' \dontrun{
#' get_DSC_credentials()
#' }
get_DSC_credentials <- function() {
  if (!requireNamespace("keyring", quietly = TRUE)) {
    repo_dir <- paste0(file.path(Sys.getenv('HOME'), 'repository'))
    if (!is.null(getOption("IOHprofiler.repo_dir"))) {
      repo_dir <- getOption("IOHprofiler.repo_dir")
    }
    data <- readRDS(paste0(repo_dir, "/config.rds"))
    return(list(name = data$DSCusername, pwd = data$DSCpassword))
  }
  else {
    return(list(name = keyring::key_get("DSCtool_name"), 
                pwd = keyring::key_get("DSCtool")))
  }
}

#' Convert a DataSetList to the format needed for the DSCtool
#' 
#' 
#' @param dsList The DataSetList object
#' @param targets Optional list of target values (Runtime or target value)
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' @param na.correction How to deal with missing values. Only used in fixed-target perspective.
#' 
#' @noRd
#' @examples 
#' convert_to_dsc_compliant(dsl)
convert_to_dsc_compliant <- function(dsList, targets = NULL, which = 'by_RT', 
                                     na.correction = NULL) {
  maximize <- attr(dsList, 'maximization')
  variable <- fid <- value <- NULL #Set local binding to remove warnings
  by_rt <- which == 'by_RT'
  
  if (is.null(targets)) {
    targets <- get_target_dt(dsList, which)
  }
  
  final_list <- lapply(get_algId(dsList), function(algname) {
    dsl_sub <- subset(dsList, algId == algname)
    problems <- lapply(dsl_sub, function(ds) {
      target <- targets[funcId == attr(ds, 'funcId') & DIM == attr(ds, 'DIM'), 'target']
      if (by_rt) {
        data <- get_RT_sample(ds, target, output = 'long')$RT
        if (any(is.na(data)) & !is.null(na.correction)) {
          if (na.correction == 'PAR-1') {
            budgets <- attr(ds, 'maxRT')
            data[is.na(data)] <- budgets[is.na(data)]
          }
          else if (na.correction == 'PAR-10') {
            budgets <- attr(ds, 'maxRT')
            data[is.na(data)] <- 10 * budgets[is.na(data)]
          }
          else if (na.correction == 'ERT') {
            ert <- as.numeric(get_ERT(ds, target)$ERT)
            if (is.infinite(ert)) {
              ert <- sum(attr(ds, 'maxRT'))
            }
            data[is.na(data)] <- ert 
          }
          else if (na.correction == 'Remove-na') {
            data <- data[!is.na(data)]
          }
          else {
            stop('This value for `na.correction` is not supported')
          }
        }
      }
      else {
        data <- get_FV_sample(ds, target, output = 'long')$`f(x)`
      }
      if (any(is.na(data))) {
        warning("NA-values detected in data preparation for DSCtool. This will likely result in an 
                  error. Please verify the provided DataSetList.")
      }
      return(list(name = paste0("F", attr(ds, 'funcId'), "_", attr(ds, 'DIM'),"D"),
                  data = data))
    })
    return(list(algorithm = algname, problems = problems))
  })
  
  
  return(final_list)
}

#' Get the matrix of rankings using the DSCtool api for a DataSetList
#' 
#' 
#' @param dsList The DataSetList object
#' @param targets Optional list of target values (Runtime or target value)
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' @param test_type Either 'AD' for Anderson-Darling or KS for Kolmogorov-Smirnov tests
#' @param alpha Threshold value for statistical significance
#' @param epsilon Minimum threshold to have practical difference between algorithms (eDSC)
#' @param monte_carlo_iterations How many monte-carlo-simulations to perform 
#' (set to 0 to use regular DSC)
#' @param na.correction How to deal with missing values. Only used in fixed-target perspective.
#' Options are: 
#' - 'NULL': No correction is done. This will likely result in an error, as the DSCtool
#' does not allow for na values
#' - 'PAR-1' Replace missing values with Budget (budget taken from relevant DataSet)
#' - 'PAR-10' Replace missing values with 10*Budget (budget taken from relevant DataSet)
#' - 'ERT' Replace NA values with the Expected Running Time. If all values are NA, this 
#' reverts to nr_runs * budget
#' - 'Remove-na' Removes all NA values
#' 
#' @return A named list containing a ranked-matrix which has the rankin of each algorithm
#' on each problem, as well as a list of which omnibus tests can be used to further process
#' this data. This can be further analyzed using `get_dsc_omnibus`
#' 
#' @export
#' @examples 
#' get_dsc_rank(dsl)
get_dsc_rank <- function(dsList, targets = NULL, which = 'by_RT', test_type = "AD", alpha = 0.05,
                         epsilon = 0, monte_carlo_iterations = 0, na.correction = NULL) {
  if (!check_dsc_configured()) return(NULL)
  url <- "https://ws.ijs.si:8443/dsc-1.5/service/rank"
  dsc_list <- convert_to_dsc_compliant(dsList, targets, which, na.correction = na.correction)
  json_list <- list(method = list(name = test_type, alpha = alpha), epsilon = epsilon, 
                    data = dsc_list, monte_carlo_iterations = monte_carlo_iterations)
  
  result_json <- POST(url,
                      authenticate(get_DSC_credentials()$name, get_DSC_credentials()$pwd),
                      add_headers(.headers = c('Content-Type' = "application/json",
                                               'Accept' = "application/json")),
                      body = json_list, encode = "json")
  
  return(content(result_json)$result)
}


#' Perform omnibus statistical tests on the matrix of rankings from the DSCtool api 
#' 
#' 
#' @param res The result of a call to the `get_dsc_rank`
#' @param method Which method to use to do the tests. 
#' Has be be one of the allowed ones in `res$valid_methods`. 
#' When NULL, the first valid option is chosen by default
#' @param alpha Threshold value for statistical significance
#' 
#' @return A named list containing the algorithm means
#' 
#' @export
#' @examples 
#' get_dsc_omnibus(get_dsc_rank(dsl))
get_dsc_omnibus <- function(res, method = NULL, alpha = 0.05) {
  if (!check_dsc_configured()) return(NULL)
  if (is.null(method)) method <- res$valid_methods[[1]]
  else req(method %in% res$valid_methods)
  url <- "https://ws.ijs.si:8443/dsc-1.5/service/omnibus"
  new_json <- list(method = list(name = method, alpha = alpha),
                   ranked_matrix = res$ranked_matrix,
                   number_algorithms = res$number_algorithms,
                   parametric_tests = res$parametric_tests)
  result_json <- POST(url,
                      authenticate(get_DSC_credentials()$name, get_DSC_credentials()$pwd),
                      add_headers(.headers = c('Content-Type' = "application/json",
                                               'Accept' = "application/json")),
                      body = new_json, encode = "json")
  
  return(content(result_json)$result)
}

#' Perform post-hoc processing on data from DSCtool
#' 
#' 
#' @param omni_res The result from a call to `get_dsc_omnibus`
#' @param nr_algs The number of algorithms present in `omni_res`
#' @param nr_problems The number of problems present in `omni_res`
#' @param base_algorithm The base algorithm to which the other are compared. 
#' This has to be present in `omni_res$algorithm_means` as an `algorithm` property
#' @param method Either 'friedman' or 'friedman-aligned-rank'
#' @param alpha Threshold value for statistical significance
#' 
#' @return A named list containing 4 types of analyses:
#' * Zvalue
#' * UnadjustedPValue
#' * Holm
#' * Hochberg
#' 
#' @export
#' @examples 
#' get_dsc_posthoc(get_dsc_omnibus(get_dsc_rank(dsl)), 2, 2)
get_dsc_posthoc <- function(omni_res, nr_algs, nr_problems, base_algorithm = NULL, 
                            method = "friedman", alpha = 0.05) {
  if (!check_dsc_configured()) return(NULL)
  if (is.null(base_algorithm)) base_algorithm <- omni_res$algorithm_means[[1]]$algorithm
  else req(base_algorithm %in% unlist(omni_res$algorithm_means))
  url <- "https://ws.ijs.si:8443/dsc-1.5/service/posthoc"
  new_json <- list(method = list(name = method, alpha = alpha),
                   algorithm_means = omni_res$algorithm_means,
                   n = nr_algs,
                   k = nr_problems,
                   base_algorithm = base_algorithm)
  result_json <- POST(url,
                      authenticate(get_DSC_credentials()$name, get_DSC_credentials()$pwd),
                      add_headers(.headers = c('Content-Type' = "application/json",
                                               'Accept' = "application/json")),
                      body = new_json, encode = "json")
  
  return(content(result_json)$result)
}