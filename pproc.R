# This file contains some functions for reading, aligning, analyzing the raw data
# from the pseudo-boolean benchmarking
#
# Author: Hao Wang
# Email: wangronin@gmail.com
# 
# Remark:
#   1. library 'itertool' is way two slow and thus is not used here
#   2. Rcpp is used for the data alignment function

library(magrittr)
library(reshape2)
library(data.table)
library(Rcpp)

source('readFiles.R')

# TODO: perhaps migrate to data.table for speed concern and simplicity
# TODO: find better name to replace FCE

# global variables for the alignment
idxEvals <- 1
idxTarget <- 3

# constructor of S3 class 'DataSet' ---------------------------
# Attributes
#   funId
#   DIM
#   algId
#   Precision
#   datafile
#   instance
#   maxEvals
#   finalFunEvals
#   comment
DataSet <- function(info, verbose = F) {
  if (!is.null(info)) {
    datFile <- info$datafile
    path <- dirname(info$datafile)
    filename <- basename(info$datafile)
    tdatFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.tdat'))
    
    dat <- read_dat(datFile)          # read the dat file
    tdat <- read_dat(tdatFile)        # read the tdat file
    
    by_target <- align_target(dat)      # aligned by target values
    by_runtime <- align_runtime(tdat)   # aligned by runtime
    
    maxEvals <- sapply(dat, function(d) d[nrow(d), idxEvals]) %>% 
      set_names(NULL)
    finalFunvals <- sapply(tdat, function(d) d[nrow(d), idxTarget]) %>% 
      set_names(NULL)
    
    # if (any(maxEvals != info$maxEvals)) 
    #   warning('Inconsitent maxEvals in .info file and .dat file')
    
    do.call(function(...) structure(list(by_target = by_target, by_runtime = by_runtime), 
                                    class = c('DataSet'), ...), 
            c(info, list(maxEvals = maxEvals, finalFunvals = finalFunvals)))
     
  } else {
    structure(list(), class = c('DataSet'))
  }
}

print.DataSet <- function(data, verbose = F) {
  # TODO: implement the verbose mode
  cat(sprintf('DataSet(%s on f%s %dD)', 
              attr(data, 'algId'),
              attr(data, 'funcId'),
              attr(data, 'DIM')))
}

# TODO:
summary.DataSet <- function(data) {
  
}

# TODO:
plot.DataSet <- function(data) {
  
}

`==.DataSet` <- function(dsL, dsR) {
  if (length(dsL) == 0 || length(dsR) == 0) 
    return(FALSE)
  
   attr(dsL, 'funcId') == attr(dsR, 'funcId') &&
    attr(dsL, 'DIM') == attr(dsR, 'DIM') &&
    attr(dsL, 'Precision') == attr(dsR, 'Precision') &&
    attr(dsL, 'algId') == attr(dsR, 'algId') &&
    attr(dsL, 'comment') == attr(dsR, 'comment')
}

# calculate the basic statistics of the runtime samples from an aligned data set
RT_summary <- function(dataset, ftarget, maximization = FALSE,
                       probs = c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.) {
  df <- dataset$by_target
  algId <- attr(dataset, 'algId')
  
  ftarget <- c(ftarget)
  f_aligned <- rownames(df) %>% as.numeric
  op <- ifelse(maximization, `>=`, `<=`)
  
  if (is.null(algId)) 
    algId <- 'unknown'
  
  lapply(ftarget, 
         function(f) {
           seq_along(f_aligned)[op(f_aligned, f)][1] %>% 
             # order(abs(f - f_aligned))[1] %>% 
             df[., ] %>% 
             as.vector %>% {
               c(f, length(.[!is.na(.)]), 
                 mean(., na.rm = T), median(., na.rm = T), 
                 # type 1 ~ 3 for the discrete r.v.
                 quantile(., probs = probs, names = F, type = 3, na.rm = T)) 
             }
         }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame %>% 
    cbind(algId, .) %>%
    set_colnames(c('algId', 'f(x)', 'runs', 'mean', 'median', paste0(probs * 100, '%')))
}

# Retrieve the runtime samples from an aligned data set
# wide-format is set by default
RT <- function(dataset, ftarget, format = 'wide', maximization = FALSE) {
  data <- dataset$by_target
  algId <- attr(dataset, 'algId')
  
  ftarget <- c(ftarget)
  n_run <- ncol(data)
  f_aligned <- rownames(data) %>% as.numeric
  op <- ifelse(maximization, `>=`, `<=`)
  
  if (is.null(algId)) 
    algId <- 'unknown'
  
  df <- lapply(ftarget, 
               function(f) {
                 seq_along(f_aligned)[op(f_aligned, f)][1] %>% 
                   data[., ] %>% 
                   as.vector %>% 
                   c(f, .)
               }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame %>% 
    cbind(algId, .) %>%
    set_colnames(c('algId', 'f(x)', paste0('run.', seq(n_run))))
  
  if (format == 'long') {
    df <- melt(df, id = c('algId', 'f(x)'), variable.name = 'run', 
               value.name = 'RT') %>% 
      mutate(run = as.numeric(gsub('run.', '', run)) %>% as.integer)
  }
  df
}

# TODO: find a better name for those two functions
# calculate the basic statistics of the runtime samples from an aligned data set
FCE_summary <- function(dataset, runtimes, maximization = FALSE,
                        probs = c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.) {
  df <- dataset$by_runtime
  algId <- attr(dataset, 'algId')
  
  runtimes <- c(runtimes)
  RT_aligned <- rownames(df) %>% as.numeric
  
  if (is.null(algId)) 
    algId <- 'unknown'
  
  lapply(runtimes, 
         function(r) {
           idx <- seq_along(RT_aligned)[RT_aligned >= r]
           ifelse(length(idx) == 0, nrow(df), idx[1]) %>% 
             df[., ] %>% 
             as.vector %>% {
               c(r, length(.[!is.na(.)]), 
                 mean(., na.rm = T), median(., na.rm = T), 
                 quantile(., probs = probs, names = F, na.rm = T))
             }
         }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame %>% 
    cbind(algId, .) %>%
    set_colnames(c('algId', 'runtime', 'runs', 'mean', 'median', paste0(probs * 100, '%')))
}

#' Fixed Cost Error
#'
#' @param dataset 
#' @param runtimes 
#' @param format 
#' @param maximization 
#'
#' @return
#' @export
#'
#' @examples
FCE <- function(dataset, runtimes, format = 'wide', maximization = FALSE) {
  data <- dataset$by_runtime
  algId <- attr(dataset, 'algId')
  
  runtimes <- c(runtimes)
  n_run <- ncol(data)
  RT_aligned <- rownames(data) %>% as.numeric
  
  if (is.null(algId)) 
    algId <- 'unknown'
  
  df <- lapply(runtimes, 
               function(r) {
                 idx <- seq_along(RT_aligned)[RT_aligned >= r]
                 ifelse(length(idx) == 0, nrow(data), idx[1]) %>%
                   data[., ] %>% 
                   as.vector %>% 
                   c(r, .)
               }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame %>% 
    cbind(algId, .) %>%
    set_colnames(c('algId', 'runtime', paste0('run.', seq(n_run))))
  
  if (format == 'long') {
    df <- melt(df, id = c('algId', 'runtime'), variable.name = 'run', 
               value.name = 'f(x)') %>% 
      mutate(run = as.numeric(gsub('run.', '', run)) %>% as.integer)
  }
  df
}

# the estimator for the expected running time
# TODO: add the variance of the estimate
ERT <- function(x, succ = NULL, maxEval = Inf) {
  x <- na.omit(x)
  drop <- attr(x, 'na.action')
  N <- length(x)
  
  if (!is.null(succ) && !is.null(drop))
    succ <- succ[-drop]
  
  if (is.null(succ))
    succ <- x < maxEval
  
  N_succ <- sum(succ)
  list(ps = N_succ / N, ERT = sum(x) / N_succ, N_succ = N_succ)
}

# compute the expected running time for a single DataSet
ERT.DataSet <- function(data) {
  df <- data$by_target
  succ <- matrix(0, nrow(df), ncol(df))
  
  for (i in 1:nrow(df)) {
    idx <- is.na(df[i, ])
    succ[i, ] <- !idx
    df[i, idx] <- attr(data, 'maxEvals')[idx]
  }
  
  object <- lapply(seq(nrow(df)),
                   function(idx) 
                     as.data.frame(ERT(df[idx, ]), succ = succ[idx, ])
    ) %>%
    do.call(rbind.data.frame, .) %>% 
    cbind(data.frame(BestF = row.names(df) %>% as.numeric,
                     sd = apply(df, 1, . %>% sd(na.rm = T)),
                     se = apply(df, 1, . %>% {sd(., na.rm = T) / sqrt(length(.))}),
                     median = apply(df, 1, . %>% median(na.rm = T))))
  
  class(object) %<>% c('ERT')
  attr(object, 'DIM') <- attr(data, 'DIM')
  attr(object, 'algId') <- attr(data, 'algId')
  attr(object, 'funcId') <- attr(data, 'funcId')
  object
}

# compute the fixed cost error for a single DataSet
FCE.DataSet <- function(data) {
  df <- data$by_runtime
  succ <- matrix(0, nrow(df), ncol(df))
  
  for (i in 1:nrow(df)) {
    idx <- is.na(df[i, ])
    succ[i, ] <- !idx
    df[i, idx] <- attr(data, 'maxEvals')[idx]
  }
  
  object <- data.frame(runtime = row.names(df) %>% as.integer,
                       FCE = apply(df, 1, . %>% mean(na.rm = T)),
                       median = apply(df, 1, . %>% median(na.rm = T)),
                       sd = apply(df, 1, . %>% sd(na.rm = T)),
                       se = apply(df, 1, . %>% {sd(., na.rm = T) / sqrt(length(.))})
                       )
  
  class(object) %<>% c('FCE')
  attr(object, 'DIM') <- attr(data, 'DIM')
  attr(object, 'algId') <- attr(data, 'algId')
  attr(object, 'funcId') <- attr(data, 'funcId')
  object
}

# read all raw data files in a give directory
read_dir <- function(args, verbose = T, print_fun = NULL) {
  DataSetList(args, verbose, print_fun)
}

# S3 constructoer of the 'DataSetList' ----------------------------
DataSetList <- function(args = NULL, verbose = T, print_fun = NULL) {
  if (is.null(args))
    return(structure(list(), class = c('DataSetList', 'list')))
  
  args <- trimws(args)
  indexFiles <- file.path(args, dir(args, pattern = '.info'))  # scan all .info files
  
  if (is.null(print_fun))
    print_fun <- cat
  
  object <- list()
  DIM <- c()
  algId <- c()
  funcId <- c()
  i <- 1
  
  for (file in indexFiles) {
    indexInfo <- read_IndexFile(file)
    
    if (verbose) {
      print_fun(paste('processing', file, '...\n'))
      print_fun(sprintf('   algorithm %s...\n', indexInfo[[1]]$algId))
    }
    
    for (info in indexInfo) {
      if (verbose) {
        print_fun(sprintf('      %d instances on f%d %dD...\n',
                          length(info$instance), info$funcId, info$DIM))
      }
      
      copy_flag <- TRUE
      data <- DataSet(info)
      DIM[i] <- attr(data, 'DIM')
      funcId[i] <- attr(data, 'funcId')
      algId[i] <- attr(data, 'algId')
      instance <- attr(data, 'instance')
      
      # check for duplicated instances
      if (length(object) != 0) {
        idx <- sapply(object, . %>% `==`(data)) %>% which
        for (k in idx) {
          instance_ <- attr(object[[k]], 'instance')
          if (all(instance == instance_)) {
            copy_flag <- FALSE
            warning('duplicated instances!')
            break
          }
          
          if (length(intersect(instance, instance_)) != 0) {
            warning('duplicated instances!')
          }
        }
      }
      
      if (copy_flag) {
        object[[i]] <- data
        i <- i + 1
      }
    }
  }
  
  # TODO: sort all DataSet by key order: algId, funcId and DIM
  class(object) %<>% c('DataSetList')
  attr(object, 'DIM') <- DIM
  attr(object, 'funcId') <- funcId
  attr(object, 'algId') <- algId
  object
}

`[.DataSetList` <- function(x, i, drop = FALSE) {
  obj <- unclass(x)[i]
  class(obj) %<>% c('DataSetList')
  attr(obj, 'DIM') <- attr(x, 'DIM')[i]
  attr(obj, 'funId') <- attr(x, 'funId')[i]
  attr(obj, 'algId') <- attr(x, 'algId')[i]
  obj
}

# print.DataSetList <- function(ds) {
#   
# }

summary.DataSetList <- function(data) {
  sapply(data, function(d) {
    list(funcId = attr(d, 'funcId'), 
      DIM = attr(d, 'DIM'),
      algId = attr(d, 'algId'),
      datafile = attr(d, 'datafile'),
      comment = attr(d, 'comment'))
  }) %>% 
    t %>% 
    as.data.frame
}

getDIM <- function(data) {
  sapply(data, function(d) attr(d, 'DIM')) %>% unique %>% sort
}

getfuncId <- function(data) {
  sapply(data, function(d) attr(d, 'funcId')) %>% unique %>% sort
}

getAlgId <- function(data) {
  sapply(data, function(d) attr(d, 'algId')) %>% unique %>% sort
}

getFunvals <- function(data) {
  lapply(data, function(x) rownames(x$by_target)) %>% unlist %>%
    as.numeric %>% unique %>% sort %>% rev
}

getRuntimes <- function(data) {
  lapply(data, function(x) rownames(x$by_runtime)) %>% unlist %>%
    as.numeric %>% unique %>% sort
}

filter.DataSetList <- function(data, by) {
  on <- names(by)
  idx <- rep(TRUE, length(data))

  for (i in seq_along(on)) {
    idx <- idx & sapply(data, . %>% attr(on[i])) == by[i]
  }
  data[idx]
}

subset.DataSetList <- function(ds, ...) {
  n <- nargs() - 1
  condition_call <- substitute(list(...))
  idx <- eval(condition_call, attributes(ds)) %>% 
    unlist %>% 
    matrix(nrow = n, byrow = T) %>% 
    apply(MARGIN = 2, all)
  ds[idx]
}
 
# functions to align the raw data -----------------------

# align all instances at a given target/precision
# TODO: implement this part in C for speeding up
# TODO: add option allowing for the minimization scenario
# TODO: remove the option 'full' for targets
# TODO: write the main loop using Rcpp
# TODO: automatically determine how many rows to align 
align_target <- function(data, targets = 'auto', nrow = 50, maximization = FALSE) {
  
  N <- length(data) 
  data <- lapply(data, as.matrix)   # matrices are faster for indexing?
  idx <- c(idxTarget, idxEvals)
  next_lines <- lapply(data, function(x) x[1, idx]) %>% unlist %>% 
    matrix(nrow = N, byrow = T)
  
  func <- ifelse(maximization, min, max)
  op <- ifelse(maximization, `>=`, `<=`)
  inc <- ifelse(maximization, function(x) x + 1, function(x) x - 1)
  
  if (is.numeric(targets)) {
    Fvalues <- sort(targets)
  } else {
    if (targets == 'auto') { 
      Fstart <- func(next_lines[, 1], na.rm = T)
      
      if (!maximization) {
        step <- 5.
        idxCurrentF <- ceiling(log10(Fstart) * step)
        t <- 10. ^ (idxCurrentF / step)
      } else {
        # similar to the alignment in bbob
        # produce roughly 50 data records by default
        Fend <- sapply(data, function(x) rev(x[, idxTarget])[1]) %>% func(na.rm = T)
        tmp <- seq(log10(Fstart), log10(Fend), length.out = nrow)
        step <- tmp[2] - tmp[1]
        idxCurrentF <- log10(Fstart)
        t <- Fstart
      }
    } else if (targets == 'full') {
      # align at every observed fitness value
      # this should give roughly the same time complexity 
      # as we have to iterate the whole data set
      Fvalues <- lapply(data, function(x) unique(x[, idxTarget])) %>% 
        unlist %>% unique %>% sort
      
      if (!maximization)
        Fvalues <- rev(Fvalues)
    }
  }
  
  if (targets == 'auto') {
    nrow_max <- sapply(data, function(x) tail(x[, 1], 1)) %>% max
    res <- matrix(NA, nrow = nrow_max, ncol = N)
    Fvalues <- rep(NA, nrow_max)
    
    curr_eval <- rep(NA, N)
    curr_fvalues <- rep(NA, N)
    finished <- rep(FALSE, N)
    index <- rep(1, N)  # 'iterator'
    
    i <- 0
    while (t > 0 && is.finite(t)) {
      curr_eval[1:N] <- NA
      curr_fvalues[1:N] <- NA
      
      for (k in seq_along(data)) {
        d <- data[[k]]
        iter <- index[k]
        while (!finished[k]) {
          # hitting the target
          if (`op`(next_lines[k, 1], t)) { 
            curr_eval[k] <- next_lines[k, 2]
            curr_fvalues[k] <- next_lines[k, 1]
            break
          }
          
          if (iter < nrow(d)) {
            iter <- iter + 1
            next_lines[k, ] <- d[iter, idx]
          } else {
            finished[k] <- TRUE
          }
        }
        index[k] <- iter
      }
      
      # if the current target is not hit by any instance
      if (all(is.na(curr_eval)))
        break
      
      i <- i + 1
      res[i, ] <- curr_eval
      if (!maximization) {
        idxCurrentF_new <- ceiling(log10(func(curr_fvalues, na.rm = T)) * step)
        if (maximization)
          idxCurrentF <- max(idxCurrentF_new, idxCurrentF)
        else
          idxCurrentF <- min(idxCurrentF_new, idxCurrentF)
        
        Fvalues[i] <-  10. ^ (idxCurrentF / step)
        idxCurrentF <- inc(idxCurrentF)
        t <- 10. ^ (idxCurrentF / step)
      } else {
        # make sure the new target value is bigger than the next iterate
        Fvalues[i] <- t
        idxCurrentF <- idxCurrentF + step
        t <- 10. ^ idxCurrentF
      }
    }
  } else {
    res <- matrix(NA, length(Fvalues), N)
    # current_eval <- rep(NA, N)
    for (i in seq_along(Fvalues)) {
      t <- Fvalues[i]
      for (k in seq_along(data)) {
        d <- data.iter[[k]]
        curr_eval <- NA
        while (hasNext(d)) {
          curr_line <- nextElem(d)
          v <- curr_line[idxTarget]
          if (`op`(v, t)) {
            curr_eval <- curr_line[1]
            break
          } # hitting the target 
        }
        res[i, k] <- curr_eval
      }
    }
  }
  res <- res[1:i, ]
  Fvalues <- Fvalues[1:i]
  rownames(res) <- Fvalues
  res
}

cppFunction(
'NumericVector align_runtime_loop(int r, List data, NumericVector Nrows, NumericVector index,
                       LogicalVector finished, NumericMatrix next_lines,
                       NumericVector curr_fvalues
                       ) {
  int N = data.size();
  NumericVector out = clone(curr_fvalues);
  for (int k = 0; k < N; k++) {
    NumericMatrix d = as<NumericMatrix>(data[k]);
    int Nrow = Nrows[k];
    int iter = index[k];
    while (!finished[k]) {
      if (next_lines(k, 0) > r) {
        out[k] = next_lines(k, 1); 
        break;
      }
      
      if (iter < (Nrow - 1)) {
        iter++;
        for (int j = 0; j < 2; j++) {
          next_lines(k, j) = d(iter, j);
        }
        
      } else {
        out[k] = next_lines(k, 1); 
        next_lines(k, 0) = NA_REAL;
        finished[k] = true;
      }
    }
    index[k] = iter;
  }
  return out;
}')

align_runtime <- function(data, runtime = 'full') {
  N <- length(data) 
  data <- lapply(data, as.matrix)   # matrices are faster for indexing?
  colnames(data) <- NULL
  idx <- c(idxEvals, idxTarget)
  
  if (runtime == 'full') {
    nrow_max <- sapply(data, function(x) tail(x[, 1], 1)) %>% max
    res <- matrix(NA, nrow = nrow_max, ncol = N)
    runtime <- rep(NA, nrow_max)
    index <- rep(1, N)  # index of the 'iterator'
    
    curr_fvalues <- rep(NA, N)
    finished <- rep(FALSE, N)
    
    next_lines <- lapply(data, function(x) x[1, idx]) %>% 
      unlist %>% matrix(nrow = N, byrow = T)
    r <- min(next_lines[, 1], na.rm = T)
    Nrows <- sapply(data, nrow)
    
    i <- 1
    while (r > 0) {
      curr_fvalues[] <- align_runtime_loop(r, data, Nrows, 
                                           index, finished, 
                                           next_lines, curr_fvalues)
      # k <- 1
      # for (d in data) {
      #   Nrow <- Nrows[k]
      #   iter <- index[k]
      #   while (!finished[k]) {
      #     if (next_lines[k, 1] > r) {
      #       curr_fvalues[k] <- next_lines[k, 2] 
      #       break
      #     }
      #     
      #     if (iter < Nrow) {
      #       iter <- iter + 1
      #       next_lines[k, ] <- d[iter, idx]
      #     } else {
      #       curr_fvalues[k] <- next_lines[k, 2]  # !IMPORTANT: store the last Target value
      #       next_lines[k, 1] <- NA
      #       finished[k] <- TRUE
      #     }
      #   }
      #   index[k] <- iter
      #   k <- k + 1
      # }
      
      runtime[i] <- r
      res[i, ] <- curr_fvalues
      
      if (all(finished)) 
        break
      
      i <- i + 1
      r <- min(next_lines[1:N, 1], na.rm = T)
    }
    
  }
  res <- res[1:i, ]
  runtime <- runtime[1:i]
  
  rownames(res) <- runtime
  res
}

# functions to compute statistics from the data set ----
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

CDF_discrete <- function(x) {
  x <- sort(x)
  x.unique <- unique(x)
  res <- seq_along(x) / length(x)
  for (v in x.unique) {
    res[x == v] <- max(res[x == v])
  }
  res
}





