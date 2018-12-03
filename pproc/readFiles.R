# This file contains some functions for reading, aligning, analyzing the raw data
# from the pseudo-boolean benchmarking
#
# Author: Hao Wang
# Email: wangronin@gmail.com
# 
# TODO: maybe I should always use data.table as it is very fast

suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))
suppressMessages(library(Rcpp))
suppressMessages(library(ggplot2))

sourceCpp('C/align.cc')
sourceCpp('C/read.cc')
source('global.R')

# reduce the size of the data set by evenly subsampling the records
limit.data <- function(df, n) {
  N <- nrow(df)
  if (N > n) {
    idx <- c(1, seq(1, N, length.out = n), N) %>% unique
    df[idx, ]
  } else 
    df
}

# scan *.info files for IOHProfiler or COCO
scan_indexFile <- function(folder) {
  folder <- trimws(folder)
  file.path(folder, dir(folder, pattern = '.info'))
}

# read .info files and extract information
read_IndexFile <- function(fname) {
  f <- file(fname, 'r')
  path <- dirname(fname)
  data <- list()
  i <- 1
  
  while (TRUE) {
    # TODO: remove suppressWarnings later
    lines <- suppressWarnings(readLines(f, n = 3))
    if (length(lines) == 0) 
      break
    
    header <- strsplit(lines[1] , ',')[[1]] %>% trimws %>% 
      strsplit(split = '=') %>% unlist %>% trimws %>% 
      matrix(nrow = 2) %>% {
        ans <- as.list(.[2, ])
        names(ans) <- .[1, ]
        for (name in .[1, ]) {
          value <- ans[[name]]
          ans[[name]] <- gsub("'", '', value)
          value <- suppressWarnings(as.numeric(value)) # convert quoted numeric values to numeric
          if (!is.na(value))
            ans[[name]] <- value
        }
        ans
      }
    
    record <- strsplit(lines[3], ',')[[1]] %>% trimws
    
    # TODO: this must also be removed...
    if (record[2] == "") {
      warning(sprintf('File %s is incomplete!', fname))
      res <- NULL
      info <- NULL
    } else {
      res <- strsplit(record[-1], ':') %>% unlist %>% matrix(nrow = 2)
      info <- strsplit(res[2, ], '\\|') %>% unlist %>% matrix(nrow = 2)
    }
    
    datafile <- file.path(path, record[1])
    # TODO: check the name of the attributes and fix them!
    data[[i]] <- c(
      header,
      list(
        comment = lines[2], 
        datafile = datafile,
        instance = as.numeric(res[1, ]),
        maxRT = as.numeric(info[1, ]),
        finalFV = as.numeric(info[2, ])
      )
    )
    i <- i + 1
  }
  close(f)
  data
}

# check the format of data
check_format <- function(path) {
  index_files <- scan_indexFile(path)
  info <- lapply(index_files, read_IndexFile) %>% unlist(recursive = F)
  datafile <- sapply(info, . %>% `$`(datafile))
  
  format <- lapply(datafile, function(file) {
    first_line <- scan(file, what = 'character', sep = '\n', n = 1, quiet = T)
    if (startsWith(first_line, '%'))
      COCO
    else if (startsWith(first_line, 'function'))
      IOHprofiler
  }) %>% unlist 
  
  res <- data.frame(datafile = datafile, format = format)
  
  if (length(levels(res$format)) > 1) {
    warning(paste(path, 'contains multiple data formats. This is not allowed for data processing.
                  Please check the returned dataframe for more information.'))
    res
  } else
    levels(res$format)[1]
}

# read IOHProfiler *.dat files
# Return a list of data.frames
read_dat <- function(fname, subsampling = FALSE) {
  # TODO: use the same data loading method as in read_COCO_dat
  df <- fread(fname, header = FALSE, sep = ' ', colClasses = 'character', fill = T)
  idx <- which(df[, 1] == 'function evaluation')

  # check for data consistence
  header_len <- apply(df[idx, ] != "", 1, sum) %>% min
  idx %<>% c(nrow(df) + 1)
  df <- df[, 1:header_len]
  
  # turn off the warnings of the data coersion below
  options(warn = -1)
  columns <- colnames(df) <- as.matrix(df[1, ])
  # TOOD: this opeartor is the bottelneck
  df %<>% sapply(function(c) {class(c) <- 'numeric'; c})
  # df <- df[, lapply(.SD, as.numeric)]
  # df %<>% mutate_all(funs(as.numeric(.)))
  options(warn = 0)
  
  res <- lapply(seq(length(idx) - 1), function(i) {
    i1 <- idx[i] + 1
    i2 <- idx[i + 1] - 1
    ans <- df[i1:i2, ]
    if (i1 == i2)
      ans <- t(ans) %>% as.matrix
    
    # TODO: determine the number of record in the 'efficient mode'
    if (subsampling)
      ans <- limit.data(ans, n = 500)
    else 
      ans
  })
  res 
}

# TODO: maybe not subsampling for COCO data
# read COCO '.dat'-like file
read_COCO_dat <- function(fname, subsampling = FALSE) {
  # read the file as a character vector (one string per row)
  # X <- fread(fname, header = FALSE, sep = '\n', colClasses = 'character') %>% 
  #   apply(1, as.character)
  # 
  # idx <- which(startsWith(X, '%'))
  # df <- paste(X[-idx], collapse = '\n') %>% 
  #   fread(header = FALSE, sep = ' ', colClasses = 'numeric')
  # 
  # idx %<>% c(length(X) + 1) %>% `-`(seq(0, length(idx)))
  # 
  # lapply(seq(length(idx) - 1), 
  #        function(i) {
  #          i1 <- idx[i]
  #          i2 <- idx[i + 1] - 1
  #          ans <-  df[i1:i2, ] %>% as.matrix
  #          
  #          # TODO: determine the number of records automatically, the same here
  #          if (subsampling)
  #            ans %<>% limit.data(n = 500)
  #          else 
  #            ans
  #        })
  # NOTE: the C implementation is only fast for small data set
  c_read_dat(path.expand(fname), 7, '%')
}

# TODO: double check the index of the target column
# global variables for the alignment
idxEvals <- 1
idxTarget <- 3  
n_data_column <- 5

# faster alignment for runtimes
align_runtime <- function(data, format = IOHprofiler) {
  
  if (format == IOHprofiler) {
    maximization <- TRUE
    idxTarget <- 3
  } else if (format == COCO) {
    maximization <- FALSE
    idxTarget <- 3
  }
  
  FV <- lapply(data, function(x) x[, idxTarget]) %>%
    unlist %>% unique %>% sort(decreasing = !maximization)
  
  n_rows <- sapply(data, nrow)
  n_column <- sapply(data, ncol) %>% unique
  
  if (format == COCO) {
    n_param <- 0
    idxValue <- idxEvals
    param_names <- NULL
  } else {
    n_param <- n_column - n_data_column
    if (n_param > 0) {
      param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
      idxValue <- c(idxEvals, (n_data_column + 1):n_column)
    } else {
      param_names <- NULL
      idxValue <- idxEvals
    }
  }
  
  c_align_runtime(data, FV, idxValue - 1, maximization) %>% set_names(c('RT', param_names))
}

# TODO: deprecated! this function should be also removed...
# align all instances at a given target/precision
align_by_target <- function(data, targets = 'full', nrow = 100, maximization = TRUE,
                            format = IOHprofiler) {
  
  if (format == IOHprofiler) {
    maximization <- TRUE
    idxTarget <- 5
  } else if (format == COCO) {
    maximization <- FALSE
    idxTarget <- 3
  }
  
  N <- length(data) 
  data <- lapply(data, as.matrix)   # TODO: matrices are faster for indexing?
  next_lines <- lapply(data, function(x) x[1, ]) %>% unlist %>% 
    matrix(nrow = N, byrow = T)
  
  n_rows <- sapply(data, nrow)
  n_column <- sapply(data, . %>% ncol) %>% unique
  
  if (format == COCO)
    n_param <- 0
  else
    n_param <- n_column - n_data_column
  
  if (length(n_column) > 1)
    stop('inconsistent number of columns in each run!')
  
  func <- ifelse(maximization, min, max)
  op <- ifelse(maximization, `>=`, `<=`)
  inc <- ifelse(maximization, function(x) x + 1, function(x) x - 1)
  
  if (is.numeric(targets)) {  # given target values to be aligned
    Fvalues <- sort(targets)
  } else {
    if (targets == 'auto') {  # automatically detemined alignment values
      Fstart <- func(next_lines[, idxTarget], na.rm = T)
      
      if (!maximization) {
        step <- 5.
        idxCurrentF <- ceiling(log10(Fstart) * step)
        t <- 10. ^ (idxCurrentF / step)
      } else {
        # similar to the alignment in bbob
        # produce roughly 50 data records by default
        Fend <- sapply(data, function(x) rev(x[, idxTarget])[1]) %>% max(na.rm = T)
        if (Fstart == 0)
          Fstart <- Fstart + 1
        
        tmp <- seq(log10(Fstart), log10(Fend), length.out = nrow)
        step <- tmp[2] - tmp[1]
        idxCurrentF <- log10(Fstart)
        t <- Fstart
      }
      nrow_max <- lapply(data, function(x) unique(x[, idxTarget])) %>% 
        unlist %>% unique %>% length
      res <- matrix(NA, nrow = nrow_max, ncol = N)
      Fvalues <- rep(NA, nrow_max)
      
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
  
  index <- rep(1, N)  # 'iterator'
  res <- matrix(NA, length(Fvalues), N)
  curr_eval <- rep(NA, N)
  
  if (n_param > 0) { # the aligned parameters
    param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
    param <- lapply(seq(n_param), . %>% {matrix(NA, nrow = length(Fvalues), ncol = N)}) %>% 
      set_names(param_names)  
  }
  
  if (targets == 'auto') {
    curr_fvalues <- rep(NA, N)
    i <- 1
    
    while (all(index < n_rows)) {
      curr_eval[1:N] <- NA
      curr_fvalues[1:N] <- NA
      
      for (k in seq_along(data)) {
        d <- data[[k]]
        iter <- index[k]
        
        while (TRUE) {
          # hitting the target
          # TODO: solve this issue!
          if (`op`(next_lines[k, idxTarget], t)) { 
            curr_eval[k] <- next_lines[k, idxEvals]
            curr_fvalues[k] <- next_lines[k, idxTarget]
            break
          }
          
          if (iter < nrow(d)) {
            iter <- iter + 1
            next_lines[k, ] <- d[iter, ]
          } else {
            break
          }
        }
        index[k] <- iter
      }
      
      i <- i + 1
      res[i, ] <- curr_eval
      
      if (n_param > 0) {
        for (k in seq(n_param)) {
          param[[k]][i, ] <- next_lines[, k + n_data_column]
        }
      }
      
      # if the all the iterations are completed
      # if (all(index == n_rows))
      #   break
      
      # setup the new target value
      if (!maximization) {
        idxCurrentF_new <- ceiling(log10(func(curr_fvalues, na.rm = T)) * step)
        # if (maximization)
        # idxCurrentF <- max(idxCurrentF_new, idxCurrentF)
        # else
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
    t <- Fvalues[1]
    i <- 1
    
    while (is.finite(t)) {
      curr_eval[1:N] <- NA
      
      # Rcpp implementation
      curr_eval[] <- align_by_target_inner_loop(t, idxEvals - 1, idxTarget - 1,
                                                data, index - 1, next_lines, curr_eval,
                                                maximization)
      
      # slow R alternative
      # for (k in seq_along(data)) {
      #   d <- data[[k]]
      #   iter <- index[k]
      #   while (TRUE) {
      #     # if hitting the target
      #     if (`op`(next_lines[k, idxTarget], t)) {
      #       curr_eval[k] <- next_lines[k, idxEvals]
      #       break
      #     }
      # 
      #     # otherwise, is the iterator finished?
      #     if (iter < nrow(d)) {
      #       iter <- iter + 1
      #       next_lines[k, ] <- d[iter, ]
      #     } else {
      #       break
      #     }
      #   }
      #   index[k] <- iter
      # }
      
      res[i, ] <- curr_eval[1:N]
      if (n_param > 0) {
        for (k in seq(n_param)) {
          param[[k]][i, ] <- next_lines[, k + n_data_column]
        }
      }
      
      if (i == length(Fvalues)) 
        break
      
      i <- i + 1
      t <- Fvalues[i]
    }
  }
  
  # the return values
  res <- res[1:i, ]
  Fvalues <- Fvalues[1:i]
  rownames(res) <- Fvalues
  
  if (n_param > 0) {
    for (k in seq(n_param)) {
      param[[k]] <- param[[k]][1:i, ] %>% 
        set_rownames(Fvalues) 
    }
    c(list(RT = res), param)
  } else 
    list(RT = res)
}

check_contiguous <- function(data) {
  sapply(data, 
         function(d) {
           v <- d[, idxEvals]
           N <- length(v)
           v[1] == 1 && v[N] == N
         }) %>% 
    all
}

align_contiguous <- function(data, idx, rownames) {
  N <- length(data)
  nrow <- length(rownames)
  lapply(data, 
         function(d) {
           v <- d[, idx]
           r <- nrow - length(v)
           if (r > 0) {
             v <- c(v, rep(v[length(v)], r))
           }
           v
         }) %>% 
    unlist %>%
    matrix(nrow = nrow, ncol = N) %>%
    set_rownames(rownames)
}

align_non_contiguous <- function(data, idx, rownames) {
  N <- length(data)
  nrow <- length(rownames)
  lapply(data, 
         function(d) {
           c_impute(d[, idx], d[, idxEvals], rownames)
         }) %>% 
    unlist %>%
    matrix(nrow = nrow, ncol = N) %>%
    set_rownames(rownames)
}

align_function_value <- function(data, include_param = TRUE, format = IOHprofiler) {
  N <- length(data) 
  n_column <- sapply(data, ncol) %>% unique
  stopifnot(length(n_column) == 1)
  
  if (format == COCO) {
    maximization <- FALSE
    idxTarget <- 3
    n_param <- 0
  } else if (format == IOHprofiler) {
    maximization <- TRUE
    idxTarget <- 5
    n_param <- n_column - n_data_column
  }
  
  include_param <- include_param && (n_param > 0)
  
  if (check_contiguous(data)) {
    nrow <- sapply(data, nrow) %>% max
    runtime <- seq(nrow)
    align_func <- align_contiguous
  } else {
    runtime <- lapply(data, function(x) x[, idxEvals]) %>% unlist %>% unique %>% sort
    nrow <- length(runtime)
    align_func <- align_non_contiguous
  }
  
  FV <- align_func(data, idxTarget, runtime)
  
  if (include_param) {
    param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
    param <- list()
    for (i in seq(n_param)) {
      name <- param_names[i]
      param[[name]] <- align_func(data, i + n_data_column, runtime)
    }
  }
  
  if (include_param) {
    c(list(FV = FV), param)
  } else {
    list(FV = FV)
  }
}

# NOTE: this is slow and incorrect...
# TODO: remove this in the next commit
align_function_value_old <- function(data, type = 'full', format = IOHprofiler) {
  
  if (format == IOHprofiler) {
    maximization <- TRUE
    idxTarget <- 5
  } else if (format == COCO) {
    maximization <- FALSE
    idxTarget <- 3
  }
  
  N <- length(data) 
  data <- lapply(data, as.matrix)   # matrices are faster for indexing?
  
  n_rows <- sapply(data, nrow)
  n_column <- sapply(data, . %>% ncol) %>% unique
  
  if (format == COCO)
    n_param <- 0
  else
    n_param <- n_column - n_data_column

  if (length(n_column) > 1)
    stop('inconsistent number of columns in each run!')
  
  index <- rep(1, N)  # index of the 'iterator'
  curr_fvalues <- rep(NA_real_, N)  # NOTE: only NA_real_ work with the Rcpp code below
  next_lines <- lapply(data, function(x) x[1, ]) %>% 
    unlist %>% matrix(nrow = N, byrow = T)
  
  runtime <- lapply(data, function(x) x[, idxEvals]) %>% unlist %>% unique %>% sort
  nrow_max <- length(runtime)
  res <- matrix(NA, nrow = nrow_max, ncol = N)  # the aligned data
  
  if (n_param > 0) { # the aligned parameters
    param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
    param <- lapply(seq(n_param), . %>% {matrix(NA, nrow = nrow_max, ncol = N)}) %>% 
      set_names(param_names)  
  }
  
  if (type == 'full') {
    for (i in seq(nrow_max)) {
      r <- runtime[i]
      
      # Rcpp implementation
      align_function_value_inner_loop(r, idxEvals - 1, idxTarget - 1, data, n_rows,
                                  index - 1, next_lines, curr_fvalues)
      
      # the slower implementation
      # for (k in seq_along(data)) {
      #   d <- data[[k]]
      #   n_row <- n_rows[k]
      #   iter <- index[k]
      # 
      #   while (!is.na(next_lines[k, idxEvals])) {
      #     if (next_lines[k, idxEvals] >= r) {
      #       curr_fvalues[k] <- next_lines[k, idxTarget]
      #       break
      #     }
      # 
      #     if (iter < n_row) {
      #       iter <- iter + 1
      #       next_lines[k, ] <- d[iter, ]
      #     } else {
      #       curr_fvalues[k] <- next_lines[k, idxTarget]  # !IMPORTANT: store the last Target value
      #       next_lines[k, idxEvals] <- NA
      #     }
      #   }
      #   index[k] <- iter
      # }
      # 
      runtime[i] <- r
      res[i, ] <- curr_fvalues
      
      # store the aligned parameters
      if (n_param > 0) {
        for (k in seq(n_param)) {
          param[[k]][i, ] <- next_lines[, k + n_data_column]
        }
      }
      
      if (all(is.na(next_lines[, idxEvals])))
        break 
    }
  }
  
  res <- res[1:i, ]
  runtime <- runtime[1:i]
  rownames(res) <- runtime
  
  # store the aligned parameters first
  if (n_param > 0) {
    for (k in seq(n_param)) {
      param[[k]] <- param[[k]][1:i, ] %>% 
        set_rownames(runtime) 
    }
    c(list(by_runtime = res), param)
  } else {
    list(by_runtime = res)
  }
}

# align_runtime <- function(data, format = IOHprofiler) {
#   
#   if (format == IOHprofiler) {
#     maximization <- TRUE
#     idxTarget <- 3
#   } else if (format == COCO) {
#     maximization <- FALSE
#     idxTarget <- 3
#   }
#   
#   FV <- lapply(data, function(x) x[, idxTarget]) %>%
#     unlist %>% unique %>% sort(decreasing = !maximization)
#   
#   n_rows <- sapply(data, nrow)
#   n_column <- sapply(data, ncol) %>% unique
#   
#   if (format == COCO) {
#     n_param <- 0
#     idxValue <- idxEvals
#     param_names <- NULL
#   } else {
#     idxValue <- c(idxEvals, (n_data_column + 1):n_column)
#     n_param <- n_column - n_data_column
#     param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
#   }
#     
#   N <- length(data)
#   NR <- length(FV)
#   
#   tmp <- lapply(data,
#          function(d) {
#            c_impute_runtime(d[, idxTarget], d[, idxValue, drop = F], FV, maximization)
#          })
#   
#   res <- list()
#   for (k in seq(length(n_param + 1))) {
#     res[[k]] <- lapply(tmp, function(col) col[, k]) %>% 
#       unlist %>%
#       matrix(nrow = NR, ncol = N) %>%
#       set_rownames(FV)
#   }
#   set_names(res, c('RT', param_names))
# }
