# This file contains some functions for reading, aligning, analyzing the raw data
# from the pseudo-boolean benchmarking
#
# Author: Hao Wang
# Email: wangronin@gmail.com

library(magrittr)
library(dplyr)
library(reshape2)
library(data.table)
library(Rcpp)

# reduce the size of the data set by evenly subsampling the records
limit.data.frame <- function(df, n) {
  N <- nrow(df)
  if (N > n) {
    idx <- c(1, seq(1, N, length.out = n), N) %>% unique
    df[idx, ]
  } else 
    df
}

# functions to read the .info, .dat files 
scan_indexFile <- function(folder) {
  folder <- trimws(folder)
  file.path(folder, dir(folder, pattern = '.info'))
}

# reading .dat, .rdat, .tdat files ----

# for IOHProfiler format
# Return: a list of data.frames
read_dat <- function(fname, subsampling = FALSE) {
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
      ans <- t(ans)
    
    # TODO: determine the number of record...
    if (subsampling)
      ans %<>% limit.data.frame(n = 500)
    else 
      ans
  })
  res 
}

# TODO: think of even faster methods...
# fast data reading for COCO format
read_COCO_dat <- function(fname, subsampling = FALSE) {
  # improve the speed of 'scan'
  X <- scan(fname, what = '', sep = '\n', quiet = T)
  idx <- which(startsWith(X, '%'))
  
  df <- paste(X[-idx], collapse = '\n') %>% 
    fread(header = FALSE, sep = ' ', colClasses = 'numeric', fill = T) %>% 
    as.data.frame
  
  idx %<>% c(length(X) + 1) %>% 
    `-`(seq(0, length(idx)))
  
  lapply(seq(length(idx) - 1), 
         function(i) {
           i1 <- idx[i]
           i2 <- idx[i + 1] - 1
           ans <-  df[i1:i2, ]
           
           if (subsampling)
             ans %<>% limit.data.frame(n = 500)
           else 
             ans
         })
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
    data[[i]] <- c(header,
                   list(comment = lines[2], 
                        datafile = datafile,
                        # TODO: REMOVE this in the future for Furong's data set
                        instance = seq(ncol(res)))  
                        # instance = as.numeric(res[1, ]),
                        # maxEvals = as.numeric(info[1, ]),
                        # bestdeltaf = as.numeric(info[2, ]))
    )
    i <- i + 1
  }
  close(f)
  data
}

# functions to align the raw data ----

# global variables for the alignment
idxEvals <- 1
idxTarget <- 5
n_data_column <- 5

# align all instances at a given target/precision
align_by_target <- function(data, targets = 'full', nrow = 100, maximization = TRUE,
                            format = 'IOHProfiler') {
  idxTarget <- switch(format,
                      IOHProfiler = 5,
                      COCO = 3)
  N <- length(data) 
  data <- lapply(data, as.matrix)   # TODO: matrices are faster for indexing?
  next_lines <- lapply(data, function(x) x[1, ]) %>% unlist %>% 
    matrix(nrow = N, byrow = T)
  
  n_rows <- sapply(data, nrow)
  n_column <- sapply(data, . %>% ncol) %>% unique
  
  if (format == 'COCO')
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
      # curr_eval[] <- align_by_target_inner_loop(t, idxEvals - 1, idxTarget - 1,
      #                                           data, index, next_lines, curr_eval)
  
      for (k in seq_along(data)) {
        d <- data[[k]]
        iter <- index[k]
        while (TRUE) {
          # if hitting the target
          # TODO: solve this issue (+0.001) precision issue!
          if (`op`(next_lines[k, idxTarget], t)) {
            curr_eval[k] <- next_lines[k, idxEvals]
            break
          }
          
          # otherwise, is the iterator finished?
          if (iter < nrow(d)) {
            iter <- iter + 1
            next_lines[k, ] <- d[iter, ]
          } else {
            break
          }
        }
        index[k] <- iter
      }
      
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
    c(list(by_target = res), param)
  } else 
    list(by_target = res)
}

# Use Rcpp to speed up 'align_by_runtime'
cppFunction('
NumericVector align_by_target_inner_loop(double t, int idxEvals, int idxTarget, 
  List data, NumericVector index, NumericMatrix next_lines, NumericVector curr_eval) {

  int N = data.size();
  NumericVector out = clone(curr_eval);

  for (int k = 0; k < N; k++) {
    NumericMatrix d = as<NumericMatrix>(data[k]);
    int n_row = d.nrow();
    int n_col = d.ncol();
    int iter = index[k];

    while (true) {
      if (next_lines(k, idxTarget) >= t) {
        out[k] = next_lines(k, idxEvals);
        break;
      }

      if (iter < (n_row - 1)) {
        iter++;
        for (int j = 0; j < n_col; j++) {
          next_lines(k, j) = d(iter, j);
        }
      } else {
        break;
      }
    }
    index[k] = iter;
  }
  return out;
}
')

# TODO: find a better way to organize the output
align_by_runtime <- function(data, runtime = 'full', format = 'IOHProfiler') {
  idxTarget <- switch(format,
                      IOHProfiler = 5,
                      COCO = 3)
  N <- length(data) 
  data <- lapply(data, as.matrix)   # matrices are faster for indexing?
  
  n_rows <- sapply(data, nrow)
  n_column <- sapply(data, . %>% ncol) %>% unique
  
  if (format == 'COCO')
    n_param <- 0
  else
    n_param <- n_column - n_data_column

  if (length(n_column) > 1)
    stop('inconsistent number of columns in each run!')
  
  index <- rep(1, N)  # index of the 'iterator'
  curr_fvalues <- rep(NA_real_, N)  # NOTE: only NA_real_ work with the Rcpp code below
  next_lines <- lapply(data, function(x) x[1, ]) %>% 
    unlist %>% matrix(nrow = N, byrow = T)
  
  nrow_max <- lapply(data, function(x) x[, idxEvals]) %>% unlist %>% unique %>% length 
  res <- matrix(NA, nrow = nrow_max, ncol = N)  # the aligned data
  
  if (n_param > 0) { # the aligned parameters
    param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
    param <- lapply(seq(n_param), . %>% {matrix(NA, nrow = nrow_max, ncol = N)}) %>% 
      set_names(param_names)  
  }
  
  if (runtime == 'full') {
    runtime <- rep(NA, nrow_max)
    r <- min(next_lines[, idxEvals], na.rm = T)
    i <- 1
    while (TRUE) {
      # Rcpp implementation
      align_by_runtime_inner_loop(r, idxEvals - 1, idxTarget - 1, data, n_rows, 
                                  index - 1, next_lines, curr_fvalues)
      
      # the slower implementation
      # for (k in seq_along(data)) {
      #   d <- data[[k]]
      #   n_row <- n_rows[k]
      #   iter <- index[k]
      #   
      #   while (!is.na(next_lines[k, idxEvals])) {
      #     if (next_lines[k, idxEvals] > r) {
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
      
      i <- i + 1
      r <- min(next_lines[, idxEvals], na.rm = T)
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

# TODO: verify this function
# Use Rcpp to speed up 'align_by_runtime'
cppFunction('
void align_by_runtime_inner_loop(int r, int idxEvals, int idxTarget, 
  List data, NumericVector n_rows, NumericVector index, NumericMatrix next_lines,
  NumericVector curr_fvalues) {

  int N = data.size();
  for (int k = 0; k < N; k++) {
    NumericMatrix d = as<NumericMatrix>(data[k]);
    int n_row = n_rows[k];
    int iter = index[k];
    int n_col = d.ncol();

    while (!NumericVector::is_na(next_lines(k, idxEvals))) {
      if (next_lines(k, idxEvals) > r) {
        curr_fvalues[k] = next_lines(k, idxTarget);
        break;
      }

      if (iter < (n_row - 1)) {
        iter++;
        for (int j = 0; j < n_col; j++) {
          next_lines(k, j) = d(iter, j);
        }
      } else {
        curr_fvalues[k] = next_lines(k, idxTarget);
        next_lines(k, idxEvals) = NA_REAL;
      }
    }
    index[k] = iter;
  }
}
')
