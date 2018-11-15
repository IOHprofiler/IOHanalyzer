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

source('readFiles.R')
source('global.R')
source('plot.R')

#' Estimator 'SP' for the Expected Running Time (ERT)
#'
#' @param data A dataframe or matrix
#' @param max_runtime A Numerical vector. Should has the same size as columns of data 
#'
#' @return
#' @export
#'
#' @examples
SP <- function(data, max_runtime) {
  N <- ncol(data)
  succ <- apply(data, 1, function(x) sum(!is.na(x)))
  succ_rate <- succ / N
  
  idx <- is.na(data)
  for (i in seq(N)) {
    data[idx[, i], i] <- max_runtime[i]
  }
  
  list(ERT = rowSums(data) / succ, runs = succ, succ_rate = succ_rate)
}

#' Constructor of S3 class 'DataSet'
#'
#' DataSet contains the following attributes
#'  * funId
#'  * DIM
#'  * algId
#'  * Precision
#'  * datafile
#'  * instance
#'  * maxEvals
#'  * finalFunEvals
#'  * comment
#'  
#' @param info A List. Contains a set of in a *.info file.
#' @param verbose Logical. 
#' @param maximization Logical. Whether the underlying optimization algorithm performs a maximization?
#' @param format A character. The format of data source, either 'IOHProfiler' or 'COCO'.
#' @param subsampling Logical. Whether *.cdat files are subsampled?
#'
#' @return A S3 object 'DataSet'
#' @export
#'
#' @examples
DataSet <- function(info, verbose = F, maximization = TRUE, format = IOHprofiler,
                    subsampling = FALSE) {
  if (!is.null(info)) {
    datFile <- info$datafile
    path <- dirname(info$datafile)
    filename <- basename(info$datafile)
    
    # Data source:  
    # alignment by target values:
    #   IOHprofiler: *.dat
    #   COCO: *.dat
    # alignment by runtimes:
    #   IHprofiler: *.cdat
    #   COCO: *.tdat
    if (format == IOHprofiler) {
      datFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.dat'))
      tdatFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.tdat'))
      cdatFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.cdat'))
      
      # priority for the runtime alignment: *.cdat > *.tdat > *.dat
      cdatFile <- ifelse(file.exists(cdatFile), cdatFile, tdatFile)
      cdatFile <- ifelse(file.exists(cdatFile), cdatFile, datFile)
    } else if (format == COCO) {
      datFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.dat'))
      tdatFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.tdat'))
    }
    
    # TODO: whether to keep the raw data set list?
    if (format == IOHprofiler) {
      dat <- read_dat(datFile, subsampling)         # read the dat file
      cdat <- read_dat(cdatFile, subsampling)       # read the cdat file
    } else if (format == COCO) {
      dat <- read_COCO_dat(datFile, subsampling)    # read the dat file
      cdat <- read_COCO_dat(tdatFile, subsampling)   # read the tdat file
    }
    
    # check if the number of instances does not match
    stopifnot(length(dat) == length(cdat))
    
    if (format == IOHprofiler)
      maximization <- TRUE
    else if (format == COCO)
      maximization <- FALSE
    
    RT <- align_by_target(dat, maximization = maximization, format = format) # runtime
    FV <- align_by_runtime(cdat, format = format)  # function value
    
    # TODO: remove this and incorporate the parameters aligned by runtimes
    FV[names(FV) != 'FV'] <- NULL
    
    maxRT <- sapply(cdat, function(d) d[nrow(d), idxEvals]) %>% set_names(NULL)
    if (any(maxRT != info$maxRT))
      warning('Inconsitent maxRT in *.info file and *.cdat file')
    
    finalFV <- sapply(dat, function(d) d[nrow(d), idxTarget]) %>% set_names(NULL)
    if (any(finalFV != info$finalFV))
      warning('Inconsitent finalFvalue in *.info file and *.dat file')
    
    if (length(info$instance) != length(dat)) {
      warning('The number of instances found in the info is inconsistent with the data!')
      info$instance <- seq(length(dat))
    }
    
    # NOTE: the RT summary is always pre-computed 
    AUX <- list()
    data <- RT$RT
    n_instance <- length(info$instance)
    RT.summary <- data.table(target = rownames(data) %>% as.double,
                             mean = apply(data, 1, .mean),
                             median = apply(data, 1, .median),
                             sd = apply(data, 1, .sd))
    
    names <- paste0(probs * 100, '%')
    RT.summary[, (names) := t(apply(data, 1, D_quantile)) %>% split(c(col(.)))]
    RT.summary[, c('ERT', 'runs', 'ps') := SP(data, maxRT)]
    AUX$RT.summary <- RT.summary
    
    # data <- FV$FV
    # if (nrow(data) > 500) {
    #   data <- limit.data(data, n = 500)
    # }
    # 
    # FV.summary <- data.table(budget = rownames(data) %>% as.double,
    #                          mean = apply(data, 1, .mean),
    #                          median = apply(data, 1, .median),
    #                          sd = apply(data, 1, .sd))
    # 
    # names <- paste0(probs * 100, '%')
    # FV.summary[, (names) := t(apply(data, 1, C_quantile)) %>% split(c(col(.)))]
    # AUX$FV.summary  <- FV.summary
        
    do.call(function(...) structure(c(RT, FV, AUX), class = c('DataSet'), ...), 
            c(info, list(maxRT = maxRT, finalFV = finalFV, src = format,
                         maximization = maximization)))
     
  } else {
    structure(list(), class = c('DataSet'))
  }
}

print.DataSet <- function(ds, verbose = F) {
  # TODO: implement the verbose mode
  print(as.character.DataSet(ds))
}

cat.DataSet <- function(ds) cat(as.character(ds))

as.character.DataSet <- function(ds, verbose = F) {
  # TODO: implement the verbose mode
  sprintf('DataSet(%s on f%s %dD)', attr(ds, 'algId'), attr(ds, 'funcId'),
          attr(ds, 'DIM'))
}

summary.DataSet <- function(ds) {
  ds_attr <- attributes(ds)
  cat('DataSet Object:\n')
  cat(sprintf('Source: %s\n', ds_attr$src))
  cat(sprintf('Algorithm: %s\n', ds_attr$algId))
  cat(sprintf('Function ID: %d\n', ds_attr$funcId))
  cat(sprintf('Dimension: %dD\n', ds_attr$DIM))
  
  n_instance <- length(ds_attr$instance)
  if (n_instance >= 15) {
    inst <- paste0(paste(ds_attr$instance[1:7], collapse = ','), 
                   ',...,', 
                   paste(ds_attr$instance[(n_instance - 7):n_instance], collapse = ','))
    cat(sprintf('%d instance found: %s\n\n', n_instance, inst))
  }
  else
    cat(sprintf('%d instance found: %s\n\n', n_instance, paste(ds_attr$instance, collapse = ',')))
  
  cat('runtime summary:\n')
  print(ds$RT.summary)
  cat('\n')
  
  cat('function value summary:\n')
  runtimes <- rownames(ds$FV) %>% as.numeric
  if (length(runtimes) > 100) {
    runtimes <- runtimes[seq(1, length(runtimes), length.out = 100)]
  }
  
  FV.summary <- get_FV_summary(ds, runtimes)
  print(FV.summary)
  cat('\n')
  
  cat(paste('Attributes:', paste0(names(ds_attr), collapse = ', ')))
}

plot.DataSet <- function(ds, ask = TRUE) {
  dt <- data.table(ds$RT) 
  colnames(dt) <- as.character(seq(ncol(dt)))
  dt[, target := as.numeric(rownames(ds$RT))]
  
  target <- dt[, target]
  N <- length(target)
  if (N >= 15) # limit the number of point to plot
    target <- as.numeric(target[seq(1, N, by = ceiling(N / 15))])
  
  # plot by_target curves
  p <- melt(dt, id.vars = 'target', variable.name = 'instance', value.name = 'runtime') %>% 
    ggplot(aes(target, runtime, colour = instance)) + 
    geom_line(aes(group = instance)) +
    scale_x_continuous(breaks = target) + 
    guides(colour = FALSE)
    
  print(p)
  
  if (ask) x <- readline("show data aligned by runtime?")
  
  df <- ds$FV
  if (nrow(df) > 500) {
    idx <- c(1, seq(1, nrow(df), length.out = 500), nrow(df)) %>% unique
    df <- df[idx, ]
  }
  
  dt <- data.table(df)
  colnames(dt) <- as.character(seq(ncol(dt)))
  dt[, budget := as.numeric(rownames(df))]
  
  budget <- dt[, budget]
  N <- length(budget)
  if (N >= 15) {
    N.log10 <- log10(N)
    index <- floor(10 ^ seq(0, N.log10, by = N.log10 / 15))
    budget <- as.numeric(budget[index])
  }
  
  # plot by_runtime curves
  p <- melt(dt, id.vars = 'budget', variable.name = 'instance', value.name = 'Fvalue') %>%
    ggplot(aes(budget, Fvalue, colour = instance)) +
    geom_line(aes(group = instance)) +
    # scale_x_continuous(breaks = budget) + 
    scale_x_log10() +
    guides(colour = FALSE)
  
  print(p)
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

# S3 generics
get_RT_sample <- function(ds, ...) UseMethod("get_RT_sample", ds)
get_RT_summary <- function(ds, ...) UseMethod("get_RT_summary", ds)
get_FV_sample <- function(ds, ...) UseMethod("get_FV_sample", ds)
get_FV_summary <- function(ds, ...) UseMethod("get_FV_summary", ds)
get_PAR_sample <- function(ds, ...) UseMethod("get_PAR_sample", ds)
get_PAR_summary <- function(ds, ...) UseMethod("get_PAR_summary", ds)

#' Get RunTime Summary
#'
#' @param ds 
#' @param ftarget 
#'
#' @return
#' @export
#'
#' @examples
get_RT_summary.DataSet <- function(ds, ftarget) {
  data <- ds$RT.summary
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')
  
  ftarget <- c(ftarget) %>% as.double %>% sort(decreasing = !maximization)
  FValues <- data[, target]
  idx <- seq_along(FValues)
  op <- ifelse(maximization, `>=`, `<=`)
  
  matched <- sapply(
    ftarget,
    function(f) {
      idx[`op`(FValues, f)][1]
    }
  )
  
  NAs <- is.na(matched)
  if (any(NAs)) {
    rbindlist(
      list(
        data[matched[!NAs], ],
        data.table(cbind(t(t(ftarget[NAs])), matrix(NA, sum(NAs), ncol(data) - 1)))
      )
    ) %>% cbind(algId, .)
  } else {
    data[matched, ] %>% cbind(algId, .)
  }
}

#' Get RunTime Sample
#'
#' @param ds 
#' @param ftarget 
#' @param output 
#'
#' @return
#' @export
#'
#' @examples
get_RT_sample.DataSet <- function(ds, ftarget, output = 'wide') {
  data <- ds$RT
  N <- ncol(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')
  
  ftarget <- c(ftarget) %>% unique %>% as.double %>% sort(decreasing = !maximization)
  FValues <- rownames(data) %>% as.double
  idx <- seq_along(FValues)
  op <- ifelse(maximization, `>=`, `<=`)
  
  matched <- sapply(
    ftarget,
    function(f) {
      idx[`op`(FValues, f)][1]
    }
  )
  
  res <- data[matched, , drop = FALSE] %>% 
    as.data.table %>% 
    cbind(algId, ftarget, .) %>% 
    set_colnames(c('algId', 'target', paste0('run.', seq(N))))
  
  if (output == 'long') {
    res <- melt(res, id = c('algId', 'target'), variable.name = 'run', value.name = 'RT')
    res[, run := as.numeric(gsub('run.', '', run)) %>% as.integer
        ][, RT := as.integer(RT)
          ][order(target, run)]
  }
  res
}

#' Get Function Value Summary
#'
#' @param ds 
#' @param runtimes 
#'
#' @return
#' @export
#'
#' @examples
get_FV_summary.DataSet <- function(ds, runtimes) {
  data <- ds$FV
  NC <- ncol(data)
  NR <- nrow(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')
  
  runtimes <- c(runtimes) %>% unique %>% as.numeric %>% sort
  RT <- rownames(data) %>% as.numeric
  idx <- seq_along(RT)
  
  matched <- sapply(runtimes, function(r) {
    res <- idx[RT >= r][1]
    ifelse(is.na(res), NR, res)
  })
  
  data <- data[matched, , drop = FALSE]
  apply(data, 1, C_quantile) %>% 
    t %>% 
    as.data.table %>% 
    cbind(algId, runtimes, NC, 
          apply(data, 1, .mean),
          apply(data, 1, .median),
          apply(data, 1, .sd), .) %>% 
    set_colnames(c('algId', 'runtime', 'runs', 'mean', 'median', 'sd', paste0(probs * 100, '%')))
}

#' Get Funtion Value Samples
#'
#' @param ds A DataSet object
#' @param runtimes A Numerical vector. Function values reached taking the given runtime are extracted from the data
#' @param output A character. The format of the output data: 'wide' or 'long' 
#'
#' @return A data.table object
#' @export
#'
#' @examples
get_FV_sample.DataSet <- function(ds, runtimes, output = 'wide') {
  data <- ds$FV
  N <- ncol(data)
  n_row <- nrow(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')
  
  runtimes <- c(runtimes) %>% unique %>% as.numeric %>% sort
  RT <- rownames(data) %>% as.numeric
  idx <- seq_along(RT)
  
  matched <- sapply(runtimes, function(r) {
    res <- idx[RT >= r][1]
    ifelse(is.na(res), n_row, res)
  })
  
  res <- data[matched, , drop = FALSE] %>% 
    as.data.table %>% 
    cbind(algId, runtimes, .) %>% 
    set_colnames(c('algId', 'runtime', paste0('run.', seq(N))))
  
  if (output == 'long') {
    res <- melt(res, id = c('algId', 'runtime'), variable.name = 'run', value.name = 'f(x)')
    res[, run := as.numeric(gsub('run.', '', run)) %>% as.integer
        ][order(runtime, run)]
  }
  res
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

get_PAR_sample.DataSet <- function() {}
get_PAR_summary.DataSet <- function() {}

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

EPAR.DataSet <- function(data) {
  data.ori <- data
  data <- data[!(names(data) %in% c('by_runtime', 'by_target'))] # drop the aligned data set
  par_name <- names(data)
  n_par <- length(par_name)
  
  if (n_par == 0) return(NULL)
  
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
  lapply(data, function(d) setdiff(names(d), c('RT', 'FV', 'RT.summary'))) %>% unlist %>% unique
}

# TODO: let the user choose/detect whether the problem is subject to maximization
# and determine whether to sort the function values in ascending/desceding order
getFunvals <- function(data) {
  lapply(data, function(x) rownames(x$RT)) %>% unlist %>%
    as.numeric %>% unique %>% sort %>% rev
}

getRuntimes <- function(data) {
  lapply(data, function(x) rownames(x$FV)) %>% unlist %>%
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
