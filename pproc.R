#
# This file contains some functions for reading, aligning, analyzing the raw data
# from the pseudo-boolean benchmarking
#
# Author: Hao Wang
# Email: wangronin@gmail.com

library(magrittr)
library(reshape2)
library(itertools)
library(iterators)
# library(rPython)

# TODO: migrate to data.table for speed concern and simplicity
# TODO: implement the S3 generics 

# read.data.python <- function(path) {
#   python.exec('import os')
#   python.exec("import sys; sys.path.insert(0, './')")
#   python.exec('from readalign import split, VMultiReader, alignData')
#   python.exec(sprintf("data = split(['%s'])", path))
#   python.exec("df = list(map(lambda d: d.tolist(), data))")
#   
#   data <- python.get('df')
#   ncol <- length(data[[1]][[1]])
#   data <- lapply(data, function(x) matrix(unlist(x), ncol = ncol, byrow = T)[, 1:5] %>%
#                    as.data.frame %>%
#                    set_colnames(c('eval', 'DeltaF', 'BestDeltaF', 'F', 'BestF')))
#   data
# }

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

CDF_discrete <- function(x) {
  x <- sort(x)
  x.unique <- unique(x)
  res <- seq_along(x) / length(x)
  for (v in x.unique) {
    res[x == v] <- max(res[x == v])
  }
  res
}


# align all instances at a given target/precision
# TODO: implement this part in C for speeding up
# TODO: add option allowing for the minimization scenario

target_column <- 5
align_target <- function(data, targets = 'auto', nrow = 50) {
  N <- length(data)
  # iterator of data frames
  data.iter <- lapply(data, function(d) ihasNext(iter(as.matrix(d), by = 'row'))) 
  
  if (is.numeric(targets)) {
    Fvalues <- sort(targets)
  } else {
    if (targets == 'auto') { 
      # similar to the alignment in bbob
      # produce roughly 20 data records by default
      Fstart <- sapply(data, function(x) x[, target_column][1]) %>% max(na.rm = T)
      Fend <- sapply(data, function(x) rev(x[, target_column])[1]) %>% max(na.rm = T)
      tmp <- seq(log10(Fstart), log10(Fend), length.out = nrow)
      step <- tmp[2] - tmp[1]
      idx <- log10(Fstart)
      # idxCurrentF <- max(fvalues, na.rm = T) %>% log10 %>% '*'(nbPtsF) %>% ceiling
      # t <- 10. ^ (idxCurrentF / nbPtsF)
      t <- Fstart
      
    } else if (targets == 'full') {
      # align at every observed fitness value
      # this should give roughly the same time complexity 
      # as we have to iterate the whole data set
      Fvalues <- lapply(data, function(x) unique(x[, target_column])) %>% 
        unlist %>% unique %>% sort %>% rev
    }
  }
  
  if (targets == 'auto') {
    Fvalues <- c()
    res <- c()
    curr_eval <- rep(NA, N)
    curr_fvalues <- rep(NA, N)
    
    while (t > 0 && is.finite(t)) {
      curr_eval[1:N] <- NA
      curr_fvalues[1:N] <- NA
      
      for (k in seq_along(data.iter)) {
        d <- data.iter[[k]]
        while (hasNext(d)) {
          curr_line <- nextElem(d)
          v <- curr_line[target_column]
          if (v >= t) {
            curr_eval[k] <- curr_line[1] # hitting the target
            curr_fvalues[k] <- v
            break
          }
        }
      }
      
      # if the current target is not hit by any instance
      if (all(is.na(curr_eval)) || all(is.na(curr_fvalues)))
        break
      
      Fvalues <- c(Fvalues, t)
      res <- rbind(res, curr_eval)
      
      # calculate the new target values
      # tmp <- max(curr_fvalues, na.rm = T) %>% log10 %>% '*'(nbPtsF) %>% ceiling 
      # make sure the new target value is bigger than the next iterate
      idx <- idx + step
      t <- max(c(10. ^ idx, curr_fvalues), na.rm = T)
      # idxCurrentF <- max(tmp) %>% '-'(1)
      # idxCurrentF <- max(idxCurrentF, tmp) %>% '+'(1)
      # t <- 10. ^ (idxCurrentF / nbPtsF)
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
          v <- curr_line[target_column]
          if (v >= t) {
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

align.cost <- function(data, costs = 'auto') {
  N <- length(data)
  data.iter <- lapply(data, function(d) ihasNext(iter(as.matrix(d), by = 'row'))) # iterator of data frames
  
  if (is.numeric(targets)) {
    Fvalues <- rev(sort(targets))
  } else {
    if (targets == 'auto') { # similar to the alignment in bbob
      fvalues <- sapply(data, function(x) x$BestDeltaF[1])
      idxCurrentF <- max(fvalues, na.rm = T) %>% log10 %>% '*'(nbPtsF) %>% ceiling
      t <- 10. ^ (idxCurrentF / nbPtsF)
    }
    if (targets == 'full') {
      Fvalues <- lapply(data, function(x) unique(x$BestDeltaF)) %>% 
        unlist %>% unique %>% sort %>% rev
    }
  }
  
  if (targets == 'auto') {
    Fvalues <- c()
    res <- c()
    curr_eval <- rep(NA, N)
    curr_fvalues <- rep(NA, N)
    
    while (t > 0) {
      curr_eval[1:N] <- NA
      curr_fvalues[1:N] <- NA
      
      for (k in seq_along(data.iter)) {
        d <- data.iter[[k]]
        while (hasNext(d)) {
          curr_line <- nextElem(d)
          v <- curr_line[3]
          if (v <= t) {
            curr_eval[k] <- curr_line[1] # hitting the target
            curr_fvalues[k] <- v
            break
          }
        }
      }
      
      if (all(is.null(curr_eval))) # if the current target is not hit by any instance
        break
      
      Fvalues <- c(Fvalues, t)
      res <- rbind(res, curr_eval)
      
      # calculate the new target values
      tmp <- max(curr_fvalues, na.rm = T) %>% log10 %>% '*'(nbPtsF) %>% ceiling 
      idxCurrentF <- max(tmp) %>% '-'(1)
      t <- 10. ^ (idxCurrentF / nbPtsF)
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
          v <- curr_line[3]
          if (v <= t) {
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
