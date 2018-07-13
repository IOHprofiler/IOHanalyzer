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

# functions to read the .info, .dat files --------------------------
scan_indexFile <- function(folder) {
  folder <- trimws(folder)
  file.path(folder, dir(folder, pattern = '.info'))
}

# read .dat, .rdat, .tdat files
# Return: a list of data.frames
read_dat <- function(fname) {
  df <- fread(fname, header = FALSE, sep = ' ', colClasses = 'character', fill = T)
  
  idx <- grepl('function', df[, V1]) %>% which 
  
  # check for data consistence
  header_len <- sapply(idx[], function(i) sum(!df[i, ] == "")) %>% min
  idx %<>% c(nrow(df) + 1)
  df <- df[, 1:header_len]
  
  # turn off the warnings of the data coersion below
  options(warn = -1)
  columns <- colnames(df) <- as.matrix(df[1, ])
  df %<>% sapply(function(c) {class(c) <- 'numeric'; c}) 
  options(warn = 0)
  
  res <- lapply(seq(length(idx) - 1), function(i) {
    i1 <- idx[i] + 1
    i2 <- idx[i + 1] - 1
    ans <- df[i1:i2, ]
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

# functions to align the raw data -----------------------
# global variables for the alignment
idxEvals <- 1
idxTarget <- 5
n_data_column <- 5
# align all instances at a given target/precision
# TODO: implement this part in C for speeding up
# TODO: add option allowing for the minimization scenario
# TODO: remove the option 'full' for targets
# TODO: write the main loop using Rcpp
# TODO: automatically determine how many rows to align 
align_by_target <- function(data, targets = 'full', nrow = 100, maximization = TRUE) {
  N <- length(data) 
  data <- lapply(data, as.matrix)   # matrices are faster for indexing?
  # idx <- c(idxTarget, idxEvals)
  next_lines <- lapply(data, function(x) x[1, ]) %>% unlist %>% 
    matrix(nrow = N, byrow = T)
  
  n_column <- sapply(data, . %>% ncol) %>% unique
  if (length(n_column) > 1)
    stop('inconsistent number of columns in each run!')
  
  func <- ifelse(maximization, min, max)
  op <- ifelse(maximization, `>=`, `<=`)
  inc <- ifelse(maximization, function(x) x + 1, function(x) x - 1)
  
  if (is.numeric(targets)) {  # if target values are given
    Fvalues <- sort(targets)
  } else {
    if (targets == 'auto') {  # automatically detemined alignment values
      Fstart <- func(next_lines[, 1], na.rm = T)
      
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
          # TODO: solve this issue!
          if (`op`(next_lines[k, 1], as.integer(t))) { 
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
    res <- res[1:i, ]
    Fvalues <- Fvalues[1:i]
  } else {
    n_param <- n_column - n_data_column
    if (n_param > 0) { # the aligned parameters
      param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
      param <- lapply(seq(n_param), . %>% {matrix(NA, nrow = length(Fvalues), ncol = N)}) %>% 
        set_names(param_names)  
    }
    
    res <- matrix(NA, length(Fvalues), N)
    curr_eval <- rep(NA, N)
    finished <- rep(FALSE, N)
    
    index <- rep(1, N)  # 'iterator'
    t <- Fvalues[1]
    i <- 1
    
    while (is.finite(t)) {
      curr_eval[1:N] <- NA
      # curr_eval[1:N] <- align_by_target_inner_loop(t, data, index, finished,
      #                                              next_lines, curr_eval)
      for (k in seq_along(data)) {
        d <- data[[k]]
        iter <- index[k]
        while (!finished[k]) {
          # hitting the target
          # TODO: solve this issue!
          if (`op`(next_lines[k, idxTarget], as.integer(t))) {
            curr_eval[k] <- next_lines[k, idxEvals]
            if (iter == nrow(d))
              finished[k] <- TRUE
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
      
      if (n_param > 0) {
        for (k in seq(n_param)) {
          param[[k]][i, ] <- next_lines[, k + n_data_column]
        }
      }
      
      res[i, ] <- curr_eval
      i <- i + 1
      t <- Fvalues[i]
      
      if (all(finished) || i > length(Fvalues))
        break
    }
  }
  
  rownames(res) <- Fvalues
  if (n_param > 0) {
    for (k in seq(n_param)) {
      param[[k]] <- set_rownames(param[[k]], Fvalues)
    }
    return(c(list(by_target = res), param))
  }
  list(by_target = res)
}

# Use Rcpp to speed up 'align_by_runtime'
# cppFunction(
# 'NumericVector align_by_target_inner_loop(double t, List data, NumericVector index,
#                        LogicalVector finished, NumericMatrix next_lines,
#                        NumericVector curr_eval
#                        ) {
#   int N = data.size();
#   NumericVector out = clone(curr_eval);
# 
#   for (int k = 0; k < N; k++) {
#     NumericMatrix d = as<NumericMatrix>(data[k]);
#     int Nrow = d.nrow();
#     int iter = index[k];
# 
#     while (!finished[k]) {
#       if (next_lines(k, 0) >= t) {
#         out[k] = next_lines(k, 1);
#         if (iter == Nrow) {
#           finished[k] = true;
#         }
#         break;
#       }
# 
#       if (iter < (Nrow - 1)) {
#         iter++;
#         next_lines(k, 0) = d(iter, 2);
#         next_lines(k, 1) = d(iter, 0);
#       } else {
#         finished[k] = true;
#       }
#     }
#     index[k] = iter;
#   }
#   return out;
# }')

# TODO: find a better way to organize the output
align_by_runtime <- function(data, runtime = 'full') {
  N <- length(data) 
  data <- lapply(data, as.matrix)   # matrices are faster for indexing?
  idx <- c(idxEvals, idxTarget)
  
  n_column <- sapply(data, . %>% ncol) %>% unique
  if (length(n_column) > 1)
    stop('inconsistent number of columns in each run!')
  
  n_param <- n_column - n_data_column
  
  if (runtime == 'full') {
    nrow_max <- sapply(data, function(x) tail(x[, 1], 1)) %>% max
    res <- matrix(NA, nrow = nrow_max, ncol = N)  # the aligned data
    
    if (n_param > 0) { # the aligned parameters
      param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
      param <- lapply(seq(n_param), . %>% {matrix(NA, nrow = nrow_max, ncol = N)}) %>% 
        set_names(param_names)  
    }
    
    runtime <- rep(NA, nrow_max)
    index <- rep(1, N)  # index of the 'iterator'
    
    curr_fvalues <- rep(NA, N)
    finished <- rep(FALSE, N)
    
    next_lines <- lapply(data, function(x) x[1, ]) %>% 
      unlist %>% matrix(nrow = N, byrow = T)
    r <- min(next_lines[, 1], na.rm = T)
    Nrows <- sapply(data, nrow)
    
    i <- 1
    while (r > 0) {
      
      # store the aligned parameters first
      if (n_param > 0) {
        for (k in seq(n_param)) {
          param[[k]][i, ] <- next_lines[, k + n_data_column]
        }
      }
      
      # curr_fvalues[] <- align_runtime_loop(r, data, Nrows, 
      #                                      index, finished, 
      #                                      next_lines, curr_fvalues)
      
      # the slower implementation
      k <- 1
      for (d in data) {
        Nrow <- Nrows[k]
        iter <- index[k]
        while (!finished[k]) {
          if (next_lines[k, 1] > r) {
            curr_fvalues[k] <- next_lines[k, 2]
            break
          }
          
          if (iter < Nrow) {
            iter <- iter + 1
            next_lines[k, ] <- d[iter, ]
          } else {
            curr_fvalues[k] <- next_lines[k, 2]  # !IMPORTANT: store the last Target value
            next_lines[k, 1] <- NA
            finished[k] <- TRUE
          }
        }
        index[k] <- iter
        k <- k + 1
      }
      
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
  
  # store the aligned parameters first
  if (n_param > 0) {
    for (k in seq(n_param)) {
      param[[k]] <- param[[k]][1:i, ] %>% 
        set_rownames(runtime) 
    }
    return(c(list(by_runtime = res), param))
  }
  list(by_runtime = res)
}

# Use Rcpp to speed up 'align_by_runtime'
# cppFunction(
# 'NumericVector align_runtime_loop(int r, List data, NumericVector Nrows, NumericVector index,
#                        LogicalVector finished, NumericMatrix next_lines,
#                        NumericVector curr_fvalues
#                        ) {
#   int N = data.size();
#   NumericVector out = clone(curr_fvalues);
#   for (int k = 0; k < N; k++) {
#     NumericMatrix d = as<NumericMatrix>(data[k]);
#     int Nrow = Nrows[k];
#     int iter = index[k];
#     while (!finished[k]) {
#       if (next_lines(k, 0) > r) {
#         out[k] = next_lines(k, 1); 
#         break;
#       }
#       
#       if (iter < (Nrow - 1)) {
#         iter++;
#         for (int j = 0; j < Nrow; j++) {
#           next_lines(k, j) = d(iter, j);
#         }
#         
#       } else {
#         out[k] = next_lines(k, 1); 
#         next_lines(k, 0) = NA_REAL;
#         finished[k] = true;
#       }
#     }
#     index[k] = iter;
#   }
#   return out;
# }')
