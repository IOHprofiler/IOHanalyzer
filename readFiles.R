# This file contains some functions for reading, aligning, analyzing the raw data
# from the pseudo-boolean benchmarking
#
# Author: Hao Wang
# Email: wangronin@gmail.com

library(magrittr)
library(dplyr)
library(reshape2)
library(data.table)

# functions to read the .info, .dat files --------------------------
scan_indexFile <- function(folder) {
  folder <- trimws(folder)
  file.path(folder, dir(folder, pattern = '.info'))
}

# read .dat, .rdat, .tdat files
# Return: a list of data.frames
read_dat <- function(fname) {
  df <- fread(fname, header = FALSE, sep = ' ', colClasses = 'character', fill = T,
              select = 1:5) 
  
  idx <- grepl('function', df[, V1]) %>% 
    which %>% c(nrow(df) + 1)
  
  options(warn = -1)
  columns <- colnames(df) <- as.matrix(df[1, ])
  df %<>% sapply(function(c) {class(c) <- 'numeric'; c}) # many warnings here...
  # df %<>% sapply(as.numeric) # many warnings here...
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