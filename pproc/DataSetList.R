# This file contains some functions for reading, aligning, analyzing the raw data
# from the pseudo-boolean benchmarking
#
# Author: Hao Wang
# Email: wangronin@gmail.com
# 
# TODO:
#   1. add Roxygen docs
#   2. perhaps migrate to data.table completely for speed concern and simplicity
#   3. maybe separate DataSetList class from DataSet class

suppressMessages(library(magrittr))
suppressMessages(library(reshape2))
suppressMessages(library(data.table))

# read all raw data files in a give directory
read_dir <- function(path, verbose = T, print_fun = NULL, maximization = TRUE, 
                     format = IOHprofiler, subsampling = FALSE) {
  DataSetList(path, verbose, print_fun, maximization = maximization,
              format = format, subsampling = subsampling)
}

# TODO: find a better name for this function
# TODO: implement this
load_index <- function(file) {
  
}

# TODO: put class DataSetList in a separate file

#' S3 constructor of the 'DataSetList' 
#'
#' Attributes
#'   funId
#'   DIM
#'   algId
#   
#' @param path 
#' @param verbose 
#' @param print_fun 
#' @param maximization 
#' @param format 
#' @param subsampling 
#'
#' @return
#' @export
#'
#' @examples
DataSetList <- function(path = NULL, verbose = T, print_fun = NULL, maximization = TRUE,
                        format = IOHprofiler, subsampling = FALSE) {
  if (is.null(path))
    return(structure(list(), class = c('DataSetList', 'list')))
  
  path <- trimws(path)
  indexFiles <- file.path(path, dir(path, pattern = '.info'))  # scan all .info files
  
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
      print_fun(paste('Processing', file, '...\n'))
      print_fun(sprintf('   algorithm %s...\n', indexInfo[[1]]$algId))
    }
    
    for (info in indexInfo) {
      if (verbose) {
        print_fun(sprintf('      %d instances on f%d %dD...\n',
                          length(info$instance), info$funcId, info$DIM))
      }
      
      copy_flag <- TRUE
      data <- DataSet(info, maximization = maximization, format = format, 
                      subsampling = subsampling)
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
    
    if (verbose) {
      print_fun("\n")
    }
  }
  
  # TODO: sort all DataSet by multiple attributes: algId, funcId and DIM
  class(object) %<>% c('DataSetList')
  attr(object, 'DIM') <- DIM
  attr(object, 'funcId') <- funcId
  attr(object, 'algId') <- algId
  object
}

c.DataSetList <- function(...) {
  # TODO: maybe remove duplicated dataset in the further
  # remove the empty list first
  dsl <- list(...)
  dsl <- dsl[sapply(dsl, length) != 0]  
  
  if (length(dsl) == 0)
    return()
  
  object <- unlist(dsl, recursive = F)
  class(object) %<>% c('DataSetList')
  
  for (attr_str in c('DIM', 'funcId', 'algId')) {
    attr(object, attr_str) <- unlist(lapply(dsl, function(x) attr(x, attr_str)))
  }
  object
}

`[.DataSetList` <- function(x, i, drop = FALSE) {
  # remove the attributes firstly
  obj <- unclass(x)[i] 
  class(obj) %<>% c('DataSetList')
  
  # also slice the attributes accordingly
  attr(obj, 'DIM') <- attr(x, 'DIM')[i]
  attr(obj, 'funcId') <- attr(x, 'funcId')[i]
  attr(obj, 'algId') <- attr(x, 'algId')[i]
  obj
}

print.DataSetList <- function(ds) {
  cat('DataSetList:\n')
  for (i in seq_along(ds)) {
    cat(sprintf('%d: %s\n', i, as.character(ds[[i]])))
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

get_FV_overview.DataSetList <- function(dsList, algorithm = 'all') {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_FV_overview(ds)) %>% rbindlist
}

get_RT_overview.DataSetList <- function(dsList, algorithm = 'all') {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_RT_overview(ds)) %>% rbindlist
}

get_RT_summary.DataSetList <- function(dsList, ftarget, algorithm = 'all') {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) {
    res <- cbind(attr(ds, 'DIM'), attr(ds, 'funcId'), get_RT_summary(ds, ftarget))
    colnames(res)[1] <- 'DIM'
    colnames(res)[2] <- 'funcId'
    res
  }) %>% rbindlist
}

get_RT_sample.DataSetList <- function(dsList, ftarget, algorithm = 'all', ...) {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds){
    res <- cbind(attr(ds, 'DIM'), attr(ds, 'funcId'), get_RT_sample(ds, ftarget, ...))
    colnames(res)[1] <- 'DIM'
    colnames(res)[2] <- 'funcId'
    res
  }) %>% rbindlist(fill = T)
}

get_FV_summary.DataSetList <- function(dsList, runtime, algorithm = 'all') {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  

  lapply(dsList, function(ds) {
    res <- cbind(attr(ds, 'DIM'), attr(ds, 'funcId'), get_FV_summary(ds, runtime))
    colnames(res)[1] <- 'DIM'
    colnames(res)[2] <- 'funcId'
    res
  }) %>% rbindlist
}

get_FV_overview.DataSetList <- function(dsList, algorithm = 'all') {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_FV_overview(ds)) %>% rbindlist

}

get_RT_overview.DataSetList <- function(dsList, algorithm = 'all') {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_RT_overview(ds)) %>% rbindlist
}

# get_FV_runs.DataSetList <- function(dsList, runtime, algorithm = 'all') {
#   if (algorithm != 'all')
#     dsList <- subset(dsList, algId == algorithm)
  
#   lapply(dsList, function(ds) get_FV_runs(ds, runtime)) %>% rbindlist
# }


get_FV_sample.DataSetList <- function(dsList, runtime, algorithm = 'all', ...) {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds){
    res <- cbind(attr(ds, 'DIM'), attr(ds, 'funcId'), get_FV_sample(ds, runtime, ...))
    colnames(res)[1] <- 'DIM'
    colnames(res)[2] <- 'funcId'
    res
  }) %>% rbindlist(fill = T)
}

get_PAR_summary.DataSetList <- function(dsList, ftarget, algorithm = 'all', ...) {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_PAR_summary(ds, ftarget, ...)) %>% rbindlist
}

get_PAR_sample.DataSetList <- function(dsList, ftarget, algorithm = 'all', ...) {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_PAR_sample(ds, ftarget, ...)) %>% rbindlist(fill = T)
}

get_DIM <- function(data) {
  sapply(data, function(d) attr(d, 'DIM')) %>% unique %>% sort
}

get_funcId <- function(data) {
  sapply(data, function(d) attr(d, 'funcId')) %>% unique %>% sort
}

get_AlgId <- function(data) {
  sapply(data, function(d) attr(d, 'algId')) %>% unique %>% sort
}

get_ParId <- function(data) {
  lapply(data, function(d) setdiff(names(d), c('RT', 'FV', 'RT.summary'))) %>% unlist %>% unique
}

# TODO: let the user choose/detect whether the problem is subject to maximization
# and determine whether to sort the function values in ascending/desceding order
get_Funvals <- function(data) {
  lapply(data, function(x) rownames(x$RT)) %>% unlist %>%
    as.numeric %>% unique %>% sort %>% rev
}

get_Runtimes <- function(data) {
  lapply(data, function(x) rownames(x$FV)) %>% unlist %>%
    as.numeric %>% unique %>% sort
}

# TODO: this is deprecated! remove it
filter.DataSetList <- function(data, by) {
  on <- names(by)
  idx <- rep(TRUE, length(data))
  for (i in seq_along(on)) {
    idx <- idx & sapply(data, . %>% attr(on[i])) == by[i]
  }
  data[idx]
}

# filter the DataSetList
subset.DataSetList <- function(dsList, ...) {
  n <- nargs() - 1
  condition_call <- substitute(list(...))
  enclos <- parent.frame()
  idx <- sapply(dsList, 
                function(ds) 
                  eval(condition_call, attributes(ds), enclos) %>% 
                  unlist %>% 
                  all
  )
  dsList[idx]
}

max_ERTs <- function(dsList, aggr_on = 'funcId', targets = NULL, maximize = T) UseMethod("max_ERTs", dsList)


max_ERTs.DataSetList <- function(dsList, aggr_on = 'funcId', targets = NULL, maximize = T) {
  N <- length(get_AlgId(dsList))
  
  aggr_attr <- if(aggr_on == 'funcId') get_funcId(dsList) else get_DIM(dsList)
  if(!is.null(targets) && length(targets) != length(aggr_attr)) targets <- NULL
  
  second_aggr <- if(aggr_on == 'funcId') get_DIM(dsList) else get_funcId(dsList)
  if(length(second_aggr) >1 ) return(NULL)
  
  erts <- seq(0, 0, length.out = length(get_AlgId(dsList)))
  names(erts) <- get_AlgId(dsList)

  for (j in seq_along(aggr_attr)) {
    dsList_filetered <- if(aggr_on == 'funcId') subset(dsList, funcId==aggr_attr[[j]])
    else subset(dsList, DIM==aggr_attr[[j]])
    
    if(is.null(targets)){
      Fall <- get_Funvals(dsList_filetered)
      Fval <- ifelse(maximize, max(Fall), min(Fall))
    }
    else
      Fval <- targets[[j]]
    summary <- get_RT_summary(dsList_filetered, ftarget = Fval)
    ert <- summary$ERT
    names(ert) <- summary$algId
    erts <- rbind(erts, ert[get_AlgId(dsList)])
  }
  return(erts[-1,])
}
