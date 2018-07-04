# This file contains some functions for reading, aligning, analyzing the raw data
# from the pseudo-boolean benchmarking
#
# Author: Hao Wang
# Email: wangronin@gmail.com
# 
# Remark:
#   1. library 'itertool' is way two slow and thus is not used here

library(magrittr)
library(reshape2)
library(data.table)

# TODO: perhaps migrate to data.table for speed concern and simplicity
# TODO: maybe use S4 later for 'DataSet'?

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
    
    maxEvals <- sapply(dat, function(d) d[nrow(d), idxEvals])
    finalFunvals <- sapply(tdat, function(d) d[nrow(d), idxTarget])
    
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

`==.DataSet` <- function(set1, set2) {
  if (length(set1) == 0 || length(set2) == 0) 
    return(FALSE)
  
   attr(set1, 'funcId') == attr(set2, 'funcId') &&
    attr(set1, 'DIM') == attr(set2, 'DIM') &&
    attr(set1, 'Precision') == attr(set2, 'Precision') &&
    attr(set1, 'algId') == attr(set2, 'algId') &&
    attr(set1, 'comment') == attr(set2, 'comment')
}

# the estimator for expected running time
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

# compute the expected running time for a DataSet
ERT.DataSet <- function(data) {
  df <- data$by_target
  succ <- matrix(0, nrow(df), ncol(df))
  for (i in 1:nrow(df)) {
    idx <- is.na(df[i, ])
    succ[i, ] <- !idx
    df[i, idx] <- attr(data, 'maxEvals')[idx]
  }
  
  lapply(seq(nrow(df)), function(idx) as.data.frame(ERT(df[idx, ], succ = succ[idx, ]))) %>%
    do.call(rbind.data.frame, .) %>% 
    set_rownames(row.names(df))
}

# functions to read the .info, .dat files --------------------------

# read .dat, .rdat, .tdat files
# Return: a list of data.frames
read_dat <- function(fname) {
  df <- fread(fname, header = FALSE, sep = ' ', colClasses = 'character', fill = T,
              select = 1:5) %>% 
    as.data.frame
  
  columns <- colnames(df) <- as.matrix(df)[1, ]
  idx <- grepl('function', df[, 1]) %>% 
    which %>% c(nrow(df) + 1)
  
  res <- lapply(seq(length(idx) - 1), function(i) {
    i1 <- idx[i] + 1
    i2 <- idx[i + 1] - 1
    ans <- df[i1:i2, ] %>% sapply(as.numeric)
    
    if (i1 == i2)
      ans <- t(ans)
    
    ans
  })
  res
}

# read .info files and extract information
read_IndexFile <- function(fname) {
  f <- file(fname, 'r')
  path <- dirname(fname)
  
  data <- list()
  i <- 1
  while (TRUE) {
    lines <- readLines(f, n = 3)
    if (length(lines) == 0 ) 
      break
    
    header <- strsplit(lines[1] , ',')[[1]] %>% trimws %>% 
      strsplit(split = '=') %>% unlist %>% trimws %>% 
      matrix(nrow = 2) %>% {
        ans <- as.list(.[2, ])
        names(ans) <- .[1, ]
        for (name in .[1, ]) {
          v <- suppressWarnings(as.numeric(ans[[name]])) # convert quoted numeric values to numeric
          if (!is.na(v))
            ans[[name]] <- v
        }
        ans
      }
    
    record <- strsplit(lines[3], ',')[[1]] %>% trimws
    res <- strsplit(record[-1], ':') %>% unlist %>% matrix(nrow = 2)
    info <- strsplit(res[2, ], '\\|') %>% unlist %>% matrix(nrow = 2)
    datafile <- file.path(path, record[1])
    
    # TODO: check the name of the attributes and fix them!
    data[[i]] <- c(header,
                   list(comment = lines[2], 
                        datafile = datafile,
                        instance = as.numeric(res[1, ]),
                        maxEvals = as.numeric(info[1, ]),
                        bestdeltaf = as.numeric(info[2, ]))
    )
    
    i <- i + 1
  }
  close(f)
  data
}

# TODO: create class DataSetList class
# read all raw data files in a give directory
read_dir <- function(args) {
  args <- trimws(args)
  indexFiles <- file.path(args, dir(args, pattern = '.info'))  # scan all .info fils
  
  DataSetList <- list()
  i <- 1
  for (file in indexFiles) {
    indexInfo <- read_IndexFile(file)
    
    for (info in indexInfo) {
      copy_flag <- TRUE
      data <- DataSet(info)
      instance <- attr(data, 'instance')
      
      # check for duplicated instances
      if (length(DataSetList) != 0) {
        idx <- sapply(DataSetList, . %>% `==`(data)) %>% which
        for (k in idx) {
          instance_ <- attr(DataSetList[[k]], 'instance')
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
        DataSetList[[i]] <- data
        i <- i + 1
      }
    }
  }
  # TODO: sort all DataSet
  DataSetList
}

# functions to align the raw data -----------------------

# align all instances at a given target/precision
# TODO: implement this part in C for speeding up
# TODO: add option allowing for the minimization scenario
# TODO: remove the option 'full' for targets
align_target <- function(data, targets = 'auto', nrow = 20, maximization = FALSE) {
  step <- 5.
  N <- length(data) 
  data <- lapply(data, as.matrix)   # matrices are faster for indexing?
  next_lines <- lapply(data, function(x) x[1, ]) %>% unlist %>% 
    matrix(nrow = N, byrow = T)
  
  func <- ifelse(maximization, min, max)
  op <- ifelse(maximization, `>=`, `<=`)
  inc <- ifelse(maximization, function(x) x + 1, function(x) x - 1)
  
  if (is.numeric(targets)) {
    Fvalues <- sort(targets)
  } else {
    if (targets == 'auto') { 
      Fstart <- func(next_lines[, idxTarget], na.rm = T)
      
      if (1 < 2) {
        idxCurrentF <- ceiling(log10(Fstart) * step)
        t <- 10. ^ (idxCurrentF / step)
      } else {
        # similar to the alignment in bbob
        # produce roughly 20 data records by default
        Fend <- sapply(data, function(x) rev(x[, idxTarget])[1]) %>% func(na.rm = T)
        tmp <- seq(log10(Fstart), log10(Fend), length.out = nrow)
        step <- tmp[2] - tmp[1]
        idx <- log10(Fstart)
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
    Fvalues <- c()
    res <- c()
    curr_eval <- rep(NA, N)
    curr_fvalues <- rep(NA, N)
    finished <- rep(FALSE, N)
    index <- rep(1, N)  # 'iterator'
    
    while (t > 0 && is.finite(t)) {
      curr_eval[1:N] <- NA
      curr_fvalues[1:N] <- NA
      
      for (k in seq_along(data)) {
        d <- data[[k]]
        iter <- index[k]
        while (!finished[k]) {
          curr_line <- next_lines[k, ]
          
          # hitting the target
          if (`op`(curr_line[idxTarget], t)) { 
            curr_eval[k] <- curr_line[idxEvals] 
            curr_fvalues[k] <- curr_line[idxTarget]
            break
          }
          
          if (iter < nrow(d)) {
            iter <- iter + 1
            next_lines[k, ] <- d[iter, ]
          } else {
            finished[k] <- TRUE
          }
        }
        index[k] <- iter
      }
      
      # if the current target is not hit by any instance
      if (all(is.na(curr_eval)))
        break
      
      res <- rbind(res, curr_eval)
      if (1 < 2) {
        idxCurrentF_new <- ceiling(log10(func(curr_fvalues, na.rm = T)) * step)
        if (maximization)
          idxCurrentF <- max(idxCurrentF_new, idxCurrentF)
        else
          idxCurrentF <- min(idxCurrentF_new, idxCurrentF)
        
        Fvalues <- c(Fvalues, 10. ^ (idxCurrentF / step))
        idxCurrentF <- inc(idxCurrentF)
        t <- 10. ^ (idxCurrentF / step)
      } else {
        # make sure the new target value is bigger than the next iterate
        Fvalues <- c(Fvalues, t)
        idx <- idx + step
        t <- 10. ^ idx
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
  row.names(res) <- Fvalues
  res
}

align_runtime <- function(data, runtime = 'full') {
  N <- length(data) 
  data <- lapply(data, as.matrix)   # matrices are faster for indexing?
  
  if (runtime == 'full') {
    runtime <- c()
    res <- c()
    curr_fvalues <- rep(NA, N)
    finished <- rep(FALSE, N)
    index <- rep(1, N)  # index of the 'iterator'
    
    next_lines <- lapply(data, function(x) x[1, ]) %>% unlist %>% matrix(nrow = N, byrow = T)
    r <- min(next_lines[, idxEvals], na.rm = T)
    
    while (r > 0) {
      # TODO: use foreach / parallel for loop
      for (k in seq_along(data)) {
        d <- data[[k]]
        while (!finished[k]) {
          curr_line <- next_lines[k, ]
          if (curr_line[idxEvals] > r) {
            curr_fvalues[k] <- curr_line[idxTarget] 
            break
          }
          
          if (index[k] < nrow(d)) {
            index[k] <- index[k] + 1
            next_lines[k, ] <- d[index[k], ]
          } else {
            curr_fvalues[k] <- curr_line[idxTarget]  # !IMPORTANT: store the last Target value
            finished[k] <- TRUE
          }
        }
      }
      
      runtime <- c(runtime, r)
      res <- rbind(res, curr_fvalues)
      
      if (all(finished)) 
        break
      
      r <- min(next_lines[, idxEvals], na.rm = T)
    }
    
  }
  row.names(res) <- runtime
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

# calculate the basic statistics of the runtime samples from an aligned data set
RT_summary <- function(df, ftarget, 
                       probs = c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.) {
  ftarget <- c(ftarget)
  f_aligned <- rownames(df) %>% as.numeric
  algorithm.name <- attr(df, 'algorithm')
  
  if (is.null(algorithm.name)) 
    algorithm.name <- 'unknown'
  
  lapply(ftarget, 
         function(f) {
           seq_along(f_aligned)[f_aligned >= f][1] %>% 
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
    cbind(algorithm.name, .) %>%
    set_colnames(c('algorithm', 'f(x)', 'runs', 'mean', 'median', paste0(probs * 100, '%')))
}

# Retrieve the runtime samples from an aligned data set
# wide-format is set by default
RT <- function(data, ftarget, format = 'wide') {
  ftarget <- c(ftarget)
  f_aligned <- rownames(data) %>% as.numeric
  algorithm <- attr(data, 'algorithm')
  n_run <- ncol(data)
  
  if (is.null(algorithm)) 
    algorithm <- 'unknown'
  
  df <- lapply(ftarget, 
         function(f) {
           seq_along(f_aligned)[f_aligned >= f][1] %>% 
             data[., ] %>% 
             as.vector %>% 
             c(f, .)
         }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame %>% 
    cbind(algorithm, .) %>%
    set_colnames(c('algorithm', 'f(x)', paste0('run.', seq(n_run))))
  
  if (format == 'long') {
    df <- melt(df, id = c('algorithm', 'f(x)'), variable.name = 'run', 
               value.name = 'RT') %>% 
      mutate(run = as.numeric(gsub('run.', '', run)) %>% as.integer)
  }
  df
}






