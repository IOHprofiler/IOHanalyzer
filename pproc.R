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

source('readFiles.R')

# TODO: perhaps migrate to data.table for speed concern and simplicity
# TODO: find better name to replace FCE

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
#   
#   TODO: maybe also store the raw data sets
DataSet <- function(info, verbose = F, maximization = TRUE) {
  if (!is.null(info)) {
    datFile <- info$datafile
    path <- dirname(info$datafile)
    filename <- basename(info$datafile)
    
    # TODO: do we need to read .idat file here?
    idatFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.idat'))
    tdatFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.tdat'))
    cdatFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.cdat'))
    cdatFile <- ifelse(file.exists(cdatFile), cdatFile, tdatFile)
    
    # TODO: whether to keep the raw data set list?
    tdat <- read_dat(tdatFile)         # read the tdat file
    cdat <- read_dat(cdatFile)         # read the cdat file
    
    by_target <- align_by_target(tdat, maximization = maximization) # aligned by target values
    by_runtime <- align_by_runtime(cdat)   # aligned by runtime
    
    # TODO: remove this and include the parameters that are aligned by runtimes
    by_runtime <- by_runtime[1]            
    
    maxEvals <- sapply(tdat, function(d) d[nrow(d), idxEvals]) %>% set_names(NULL)
    finalFunvals <- sapply(cdat, function(d) d[nrow(d), idxTarget]) %>% set_names(NULL)
    
    if (length(info$instance) == 0 || length(info$instance) != length(tdat)) {
      warning('The number of instances found in the info is inconsistent with the data!')
      info$instance <- seq(length(dat))
    }
    
    # if (any(maxEvals != info$maxEvals))
    #   warning('Inconsitent maxEvals in .info file and .dat file')
    
    do.call(function(...) structure(c(by_target, by_runtime), class = c('DataSet'), ...), 
            c(info, list(maxEvals = maxEvals, finalFunvals = finalFunvals)))
     
  } else {
    structure(list(), class = c('DataSet'))
  }
}

getParId2 <- function(data) {
  setdiff(names(data), c('by_runtime', 'by_target'))
}

print.DataSet <- function(data, verbose = F) {
  # TODO: implement the verbose mode
  cat(as.character.DataSet(data))
}

as.character.DataSet <- function(data, verbose = F) {
  # TODO: implement the verbose mode
  sprintf('DataSet(%s on f%s %dD)', attr(data, 'algId'), attr(data, 'funcId'),
          attr(data, 'DIM'))
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
summarise_runtime <- function(dataset, ftarget, maximization = TRUE, probs = NULL) {
  df <- dataset$by_target
  algId <- attr(dataset, 'algId')
  
  ftarget <- c(ftarget) %>% as.double
  f_aligned <- rownames(df) %>% as.double
  idx <- seq_along(f_aligned)
  op <- ifelse(maximization, `>=`, `<=`)
  
  if (is.null(algId)) 
    algId <- 'unknown'
  if (is.null(probs))
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
  
  lapply(ftarget, 
         function(f) {
           matched <- idx[`op`(f_aligned, f)][1]
           target <- ifelse(is.na(matched), f, f_aligned[matched])
           df[matched, ] %>% 
             as.vector %>% {
               c(target, 
                 length(.[!is.na(.)]), 
                 mean(., na.rm = T), 
                 median(., na.rm = T), 
                 sd(., na.rm = T),
                 quantile(., probs = probs, names = F, type = 3, na.rm = T) # type 1 ~ 3 for the discrete r.v.
                 ) 
             }
         }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame %>% 
    cbind(algId, .) %>%
    set_colnames(c('algId', 'f(x)', 'runs', 'mean', 'median', 'sd', paste0(probs * 100, '%')))
}

# Retrieve the runtime samples from an aligned data set
# wide-format is set by default
get_runtime_sample <- function(dataset, ftarget, format = 'wide', maximization = TRUE) {
  df <- dataset$by_target
  N <- ncol(df)
  algId <- attr(dataset, 'algId')
  
  ftarget <- c(ftarget) %>% as.double
  f_aligned <- rownames(df) %>% as.double
  idx <- seq_along(f_aligned)
  op <- ifelse(maximization, `>=`, `<=`)
  
  if (is.null(algId)) 
    algId <- 'unknown'
  
  df <- lapply(ftarget, 
               function(f) {
                 matched <- idx[`op`(f_aligned, f)][1] 
                 target <- ifelse(is.na(matched), f, f_aligned[matched])
                 df[matched, ] %>% 
                   as.vector %>% 
                   c(algId, target, .)
               }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame %>% 
    set_colnames(c('algId', 'f(x)', paste0('run.', seq(N))))
  
  if (format == 'long') {
    df <- melt(df, id = c('algId', 'f(x)'), variable.name = 'run', value.name = 'RT') %>% 
      mutate(run = as.numeric(gsub('run.', '', run)) %>% as.integer,
             RT = as.integer(RT)) %>% 
      dplyr::arrange(`f(x)`, run)
  }
  df
}

# calculate the basic statistics of the runtime samples from an aligned data set
summarise_target <- function(dataset, runtimes, maximization = TRUE, probs = NULL) {
  df <- as.data.frame(dataset$by_runtime)
  N <- nrow(df)
  algId <- attr(dataset, 'algId')
  
  runtimes <- c(runtimes)
  runtimes_aligned <- rownames(df) %>% as.numeric
  idx <- seq_along(runtimes_aligned)
  
  if (is.null(algId)) 
    algId <- 'unknown'
  
  if (is.null(probs))
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
  
  matched <- sapply(runtimes, function(r) {
    res <- idx[runtimes_aligned >= r]
    ifelse(length(res) == 0, N, res[1]) 
  }) %>% 
    unique
  
  if (length(matched) == 0)
    return(data.frame())
  
  df <- df[matched, ]
  q <- apply(df, 1, . %>% quantile(., probs = probs, names = F, na.rm = T)) %>% 
    as.data.frame %>% 
    t %>%
    set_colnames(paste0(probs * 100, '%'))
  
  data.frame(algId = algId,
             runtime = runtimes_aligned[matched],
             runs = apply(df, 1, . %>% {length(.[!is.na(.)])}),
             mean = apply(df, 1, . %>% mean(., na.rm = T)), 
             median = apply(df, 1, . %>% median(., na.rm = T)), 
             sd = apply(df, 1, function(x) sd(x, na.rm = T))) %>% 
    cbind(q)
}

get_target_sample <- function(dataset, runtimes, format = 'wide', maximization = TRUE) {
  df <- as.data.frame(dataset$by_runtime)
  n_run <- ncol(df)
  n_row <- nrow(df)
  algId <- attr(dataset, 'algId')
  
  runtimes <- c(runtimes)
  runtimes_aligned <- rownames(df) %>% as.numeric
  idx <- seq_along(runtimes_aligned)
  
  if (is.null(algId)) 
    algId <- 'unknown'
  
  matched <- sapply(runtimes, function(r) {
    res <- idx[runtimes_aligned >= r]
    ifelse(length(res) == 0, n_row, res[1])
  }) %>% 
    unique
  
  if (length(matched) == 0)
    return(data.frame())
  
  df <- df[matched, ] %>% 
    cbind(data.frame(algId = algId, runtime = runtimes_aligned[matched]), .) %>% 
    set_colnames(c('algId', 'runtime', paste0('run.', seq(n_run))))
  
  if (format == 'long') {
    df <- melt(df, id = c('algId', 'runtime'), variable.name = 'run', 
               value.name = 'f(x)') %>% 
      mutate(run = as.numeric(gsub('run.', '', run)) %>% as.integer) %>% 
      arrange(runtime, run)
  }
  df
}

summarise_par <- function(data, ftarget, maximization = TRUE, probs = NULL) {
  algId <- attr(data, 'algId')
  data <- data[!(names(data) %in% c('by_runtime', 'by_target'))] # drop the aligned data set
  par_name <- names(data)
  n_par <- length(par_name)
  
  op <- ifelse(maximization, `>=`, `<=`)
  ftarget <- c(ftarget) %>% as.double
  
  if (is.null(algId)) 
    algId <- 'unknown'
  if (is.null(probs))
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
  
  res <- list()
  for (i in seq(n_par)) {
    par <- par_name[i]
    df <- data[[par]]
    
    f_aligned <- rownames(df) %>% as.double
    idx <- seq_along(f_aligned)
    
    res[[i]] <- lapply(ftarget, 
                       function(f) {
                         matched <- idx[`op`(f_aligned, f)][1]
                         target <- ifelse(is.na(matched), f, f_aligned[matched])
                         df[matched, ] %>% 
                           as.vector %>% {
                             c(target, 
                               length(.[!is.na(.)]), 
                               mean(., na.rm = T), 
                               median(., na.rm = T), 
                               sd(., na.rm = T),
                               quantile(., probs = probs, names = F, na.rm = T)
                             ) 
                           }
                       }) %>% 
      do.call(rbind, .) %>% 
      as.data.frame %>% 
      cbind(algId, par, .) %>% 
      set_colnames(c('algId', 'parId', 'f(x)', 'runs', 'mean', 'median', 'sd', paste0(probs * 100, '%'))) 
  }
  do.call(rbind.data.frame, res)
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
  N <- ncol(df)
  N_succ <- apply(df, 1, function(x) sum(!is.na(x)))
  
  # ERT = apply(df, 1, . %>% sum(na.rm = T)) / N_succ
  object <- data.frame(ERT = rowSums(df, na.rm = T) / N_succ,
                       ps = N_succ / N,
                       N_succ = N_succ) %>% 
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

EPAR.DataSet <- function(data) {
  data.ori <- data
  data <- data[!(names(data) %in% c('by_runtime', 'by_target'))] # drop the aligned data set
  par_name <- names(data)
  n_par <- length(par_name)
  
  df <- lapply(names(data), function(n) as.data.frame(data[[n]])) %>% 
    do.call(rbind.data.frame, .)
  
  object <- data.frame(BestF = rep(rownames(data[[1]]), n_par) %>% as.numeric,
                       mean = apply(df, 1, . %>% mean(na.rm = T)),
                       median = apply(df, 1, . %>% median(na.rm = T)),
                       sd = apply(df, 1, . %>% sd(na.rm = T))) %>% 
    cbind(par = rep(par_name, each = nrow(data[[1]])), .)
  
  class(object) %<>% c('EPAR')
  attr(object, 'DIM') <- attr(data.ori, 'DIM')
  attr(object, 'algId') <- attr(data.ori, 'algId')
  attr(object, 'funcId') <- attr(data.ori, 'funcId')
  object
}

# read all raw data files in a give directory
read_dir <- function(args, verbose = T, print_fun = NULL, maximization = TRUE) {
  DataSetList(args, verbose, print_fun, maximization = maximization)
}

# S3 constructoer of the 'DataSetList' 
# Attributes
#   funId
#   DIM
#   algId
#   Precision
#   datafile
#   instance
#   maxEvals
#   finalFunEvals
DataSetList <- function(args = NULL, verbose = T, print_fun = NULL, maximization = TRUE) {
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
      data <- DataSet(info, maximization = maximization)
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

print.DataSetList <- function(ds) {
  cat('DataSetList:\n')
  for (i in seq_along(ds)) {
    cat(sprintf('%d: %s', i, as.character(ds[[i]])))
  }
}

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

getParId <- function(data) {
  lapply(data, function(d) setdiff(names(d), c('by_runtime', 'by_target'))) %>% unlist %>% unique
}

# TODO: let the user choose/detect whether the problem is subject to maximization
# and determine whether to sort the function values in ascending/desceding order
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
