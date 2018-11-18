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
    
    do.call(function(...) structure(c(RT, FV, AUX), class = c('DataSet', 'list'), ...), 
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

# TODO: implement save
plot.DataSet <- function(ds, ask = TRUE, save = FALSE) {
  dt <- data.table(ds$RT) 
  NC <- ncol(dt)
  colnames(dt) <- as.character(seq(ncol(dt)))
  dt[, target := as.numeric(rownames(ds$RT))]
  dt_mean <- data.table(target = dt$target, mean = rowMeans(dt[, -c('target')], na.rm = T))
  
  target <- dt[, target]
  N <- length(target)
  if (N >= 30) # limit the number of point to plot
    target <- as.numeric(target[seq(1, N, by = ceiling(N / 30))])
  
  # plot runtime curves
  p <- melt(dt, id.vars = 'target', variable.name = 'instance', value.name = 'runtime') %>% 
    ggplot(aes(target, runtime, colour = as.factor(instance))) + 
    geom_line(aes(group = instance), alpha = 0.8) +
    geom_line(data = dt_mean, aes(target, mean), colour = 'black', size = 1.5, alpha = 0.8) +
    scale_colour_manual(values = colorspace::rainbow_hcl(NC)) + 
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
  dt_mean <- data.table(budget = dt$budget, mean = rowMeans(dt[, -c('budget')], na.rm = T))
  
  budget <- dt[, budget]
  N <- length(budget)
  if (N >= 30) {
    N.log10 <- log10(N)
    index <- floor(10 ^ seq(0, N.log10, by = N.log10 / 30))
    budget <- as.numeric(budget[index])
  }
  
  # plot function value curves
  p <- melt(dt, id.vars = 'budget', variable.name = 'instance', value.name = 'Fvalue') %>%
    ggplot(aes(budget, Fvalue, colour = instance)) +
    geom_line(aes(group = instance), alpha = 0.8) +
    geom_line(data = dt_mean, aes(budget, mean), colour = 'black', size = 1.5, alpha = 0.8) +
    scale_colour_manual(values = colorspace::rainbow_hcl(NC)) + 
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
get_PAR_name <- function(ds) UseMethod("get_PAR_name", ds)

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
  # maxRT <- attr(ds, 'maxRT')
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')
  
  ftarget <- c(ftarget) %>% as.double %>% sort(decreasing = !maximization)
  # FValues <- rownames(data) %>% as.numeric
  FValues <- data[, target]
  idx <- seq_along(FValues)
  op <- ifelse(maximization, `>=`, `<=`)
  
  matched <- sapply(
    ftarget,
    function(f) {
      idx[`op`(FValues, f)][1]
    }
  )
  
  # data <- data[matched, , drop = FALSE]
  # apply(data, 1, D_quantile) %>% 
  #   t %>% 
  #   as.data.table %>% 
  #   cbind(as.data.table(SP(data, maxRT))) %>% 
  #   cbind(algId, ftarget, 
  #         apply(data, 1, .mean),
  #         apply(data, 1, .median),
  #         apply(data, 1, .sd), .) %>% 
  #   set_colnames(c('algId', 'target', 'mean', 'median', 
  #                  'sd', paste0(probs * 100, '%'), 'ERT', 'runs', 'ps'))
  
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
get_FV_summary.DataSet <- function(ds, runtime) {
  data <- ds$FV
  NC <- ncol(data)
  NR <- nrow(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')
  
  runtime <- c(runtime) %>% unique %>% as.numeric %>% sort
  RT <- rownames(data) %>% as.numeric
  idx <- seq_along(RT)
  
  matched <- sapply(runtime, function(r) {
    res <- idx[RT >= r][1]
    ifelse(is.na(res), NR, res)
  })
  
  data <- data[matched, , drop = FALSE]
  apply(data, 1, C_quantile) %>% 
    t %>% 
    as.data.table %>% 
    cbind(algId, runtime, NC, 
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
get_FV_sample.DataSet <- function(ds, runtime, output = 'wide') {
  data <- ds$FV
  N <- ncol(data)
  n_row <- nrow(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')
  
  runtime <- c(runtime) %>% unique %>% as.numeric %>% sort
  RT <- rownames(data) %>% as.numeric
  idx <- seq_along(RT)
  
  matched <- sapply(runtime, function(r) {
    res <- idx[RT >= r][1]
    ifelse(is.na(res), n_row, res)
  })
  
  res <- data[matched, , drop = FALSE] %>% 
    as.data.table %>% 
    cbind(algId, runtime, .) %>% 
    set_colnames(c('algId', 'runtime', paste0('run.', seq(N))))
  
  if (output == 'long') {
    res <- melt(res, id = c('algId', 'runtime'), variable.name = 'run', value.name = 'f(x)')
    res[, run := as.numeric(gsub('run.', '', run)) %>% as.integer
        ][order(runtime, run)]
  }
  res
}

# TODO: perhaps this function can be removed...
get_PAR_name.DataSet <- function(ds) {
  name <- names(ds)
  name[!(name %in% c('RT', 'RT.summary', 'FV'))]
}

#' Get Parameter Value Summary
#'
#' @param ds A DataSet object
#' @param ftarget A Numerical vector. Function values used to align parameter values
#' @param parId A character vector. Either 'all' or the name of parameters to be retrieved
#'
#' @return A data.table object containing basic statistics of parameter values aligned at each given target value
#' @export
#'
#' @examples
get_PAR_summary.DataSet <- function(ds, ftarget, parId = 'all') {
  FValues <- rownames(ds$RT) %>% as.numeric
  idx <- seq_along(FValues)
  
  algId <- attr(ds, 'algId')
  par_name <- get_PAR_name(ds)
  if (parId != 'all')
    par_name <- intersect(par_name, parId)
  if (length(par_name) == 0)
    return(data.table())
  
  maximization <- attr(ds, 'maximization')
  ftarget <- c(ftarget) %>% as.numeric %>% sort(decreasing = !maximization)
  op <- ifelse(maximization, `>=`, `<=`)
  
  matched <- sapply(
    ftarget,
    function(f) {
      idx[`op`(FValues, f)][1]
    }
  )
  
  lapply(par_name,
         function(par) {
           data <- ds[[par]][matched, , drop = FALSE]
           apply(data, 1, C_quantile) %>% 
             t %>% 
             as.data.table %>% 
             cbind(algId, par, ftarget,
                   apply(data, 1, function(x) length(x[!is.na(x)])),
                   apply(data, 1, .mean),
                   apply(data, 1, .median),
                   apply(data, 1, .sd), .) %>% 
             set_colnames(c('algId', 'parId', 'target', 'runs', 'mean', 'median', 'sd', 
                            paste0(probs * 100, '%')))
         }) %>% 
    rbindlist
}

#' Get Parameter Value Samples
#'
#' @param ds A DataSet object
#' @param ftarget A Numerical vector. Function values used to align parameter values
#' @param parId A character vector. Either 'all' or the name of parameters to be retrieved
#' @param output A character. The format of the output data: 'wide' or 'long' 
#'
#' @return A data.table object containing parameter values aligned at each given target value
#' @export
#'
#' @examples
get_PAR_sample.DataSet <- function(ds, ftarget, parId = 'all', output = 'wide') {
  N <- length(attr(ds, 'instance'))
  FValues <- rownames(ds$RT) %>% as.numeric
  idx <- seq_along(FValues)
  
  algId <- attr(ds, 'algId')
  par_name <- get_PAR_name(ds)
  if (parId != 'all')
    par_name <- intersect(par_name, parId)
  if (length(par_name) == 0)
    return(data.table())
  
  maximization <- attr(ds, 'maximization')
  ftarget <- c(ftarget) %>% as.numeric %>% sort(decreasing = !maximization)
  op <- ifelse(maximization, `>=`, `<=`)
  
  matched <- sapply(
    ftarget,
    function(f) {
      idx[`op`(FValues, f)][1]
    }
  )
  
  res <- lapply(par_name,
         function(parId) {
           data <- ds[[parId]]
           data[matched, , drop = FALSE] %>% 
             as.data.table %>% 
             cbind(algId, parId, ftarget, .) %>% 
             set_colnames(c('algId', 'parId', 'target', paste0('run.', seq(N))))
         }) %>% 
    rbindlist
  
  if (output == 'long') {
    res <- melt(res, id = c('algId', 'parId', 'target'), variable.name = 'run', value.name = 'value')
    res[, run := as.numeric(gsub('run.', '', run)) %>% as.integer
        ][order(target, run)]
  }
  res
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

get_RT_summary.DataSetList <- function(dsList, ftarget, algorithm = 'all') {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_RT_summary(ds, ftarget)) %>% rbindlist
}

get_RT_sample.DataSetList <- function(dsList, ftarget, algorithm = 'all', ...) {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_RT_sample(ds, ftarget, ...)) %>% rbindlist
}

get_FV_summary.DataSetList <- function(dsList, runtime, algorithm = 'all') {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_RT_summary(ds, runtime)) %>% rbindlist
}

get_FV_sample.DataSetList <- function(dsList, runtime, algorithm = 'all', ...) {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_RT_sample(ds, runtime, ...)) %>% rbindlist
}

get_PAR_summary.DataSetList <- function(dsList, ftarget, algorithm = 'all', ...) {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_PAR_summary(ds, ftarget, ...)) %>% rbindlist
}

get_PAR_sample.DataSetList <- function(dsList, ftarget, algorithm = 'all', ...) {
  if (algorithm != 'all')
    dsList <- subset(dsList, algId == algorithm)
  
  lapply(dsList, function(ds) get_PAR_sample(ds, ftarget, ...)) %>% rbindlist
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
