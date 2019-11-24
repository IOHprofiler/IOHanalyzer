#' Read all the data file in a folfer
#
#' @param path Path to the data files. Will look for all .info-files in this directory and use
#'  the corresponding datafiles to create the DataSetList
#' @param verbose Logical.
#' @param maximization Logical. Whether the underlying optimization algorithm performs a maximization?
#' @param format A character. The format of data source, either 'IOHProfiler', 'COCO' or 'TWO_COL"
#' @param subsampling Logical. Whether *.cdat files are subsampled?
#' @param print_fun Function used to print output when in verbose mode
#' @noRd
#' @return A DataSetList object
read_dir <- function(path, verbose = T, print_fun = NULL, maximization = TRUE,
                     format = IOHprofiler, subsampling = FALSE) {
    DataSetList(
      path,
      verbose,
      print_fun,
      maximization = maximization,
      format = format,
      subsampling = subsampling
    )
  }

# TODO: improve documentation of this S3 class
#' S3 constructor of the 'DataSetList'
#'
#' Attributes
#'   funId
#'   DIM
#'   algId
#
#' @param path Path to the data files. Will look for all .info-files in this directory and use
#'  the corresponding datafiles to create the DataSetList
#' @param verbose Logical.
#' @param maximization Logical. Whether the underlying optimization algorithm performs a maximization?
#' @param format A character. The format of data source, options are:
#'  \itemize{
#'  \item'IOHProfiler'
#'  \item'COCO'
#'  \item'TWO_COL'
#'  \item'COCO_BIOBJ'
#'  \item'NEVERGRAD'
#'  }
#'  These formats are specified in more detail in our github wiki.
#' @param subsampling Logical. Whether *.cdat files are subsampled?
#' @param print_fun Function used to print output when in verbose mode
#'
#' @return A DataSetList object
#' @export
#' @examples
#' path <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package = "IOHanalyzer")
#' DataSetList(path)
DataSetList <- function(path = NULL, verbose = T, print_fun = NULL, maximization = NULL,
                        format = IOHprofiler, subsampling = FALSE) {
    if (is.null(path))
      return(structure(list(), class = c('DataSetList', 'list')))
    
    path <- trimws(path)
    if (format == NEVERGRAD) {
      if (sub('[^\\.]*\\.', '', basename(path), perl = T) == "csv")
        return(read_nevergrad(path))
      else
        indexFiles <-
          file.path(path, list.files(path, pattern = '.csv', recursive = T))
    }
    else
      indexFiles <- scan_index_file(path)
    
    if (is.null(print_fun))
      print_fun <- cat
    
    object <- list()
    class(object) <- c('DataSetList', class(object))
    DIM <- c()
    algId <- c()
    funcId <- c()
    suites <- c()
    maximizations <- c()
    i <- 1
    
    for (file in indexFiles) {
      if (verbose) {
        print_fun(paste('Processing', file, '...\n'))
      }
      
      if (format == NEVERGRAD) {
        dsl <- read_nevergrad(file)
        object %<>% c(., dsl)
        suites[i] <- NEVERGRAD
      }
      else {
        indexInfo <- read_index_file(file)
        if (verbose) 
          print_fun(sprintf('   algorithm %s...\n', indexInfo[[1]]$algId))
        
        for (info in indexInfo) {
          if (verbose) {
            print_fun(
              sprintf(
                '      %d instances on f%d %dD...\n',
                length(info$instance),
                info$funcId,
                info$DIM
              )
            )
          }
          
          copy_flag <- TRUE
          data <- DataSet(info, maximization = maximization,
                          format = format, subsampling = subsampling)
          
          DIM[i] <- attr(data, 'DIM')
          funcId[i] <- attr(data, 'funcId')
          algId[i] <- attr(data, 'algId')
          # TODO: double-check the following treatment on `instance`!!!
          instance <- attr(data, 'instance') #Was instance without index?
          suites[i] <- attr(data, 'suite')
          maximizations[i] <- attr(data, 'maximization')
          
          # check for duplicated instances
          if (length(object) != 0) {
            idx <- which(sapply(object, function(obj) obj == data))
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
      
      if (verbose) 
        print_fun("\n")
    }
    
    # TODO: sort all DataSet by multiple attributes: algId, funcId and DIM
    if (format != NEVERGRAD) {
      attr(object, 'DIM') <- DIM
      attr(object, 'funcId') <- funcId
      attr(object, 'algId') <- algId
    }
    
    suite <- unique(suites)
    maximization <- unique(maximizations)
    if (length(suite) != 1 || length(maximization) != 1) {
      warning("Multipe different suites detected!")
    }
    
    attr(object, 'suite') <- suite
    attr(object, 'maximization') <- maximization
    object
  }

#' S3 concatenation function for DataSetList
#'
#' @param ... The DataSetLists to concatenate
#' @return A new DataSetList
#' @export
#' @examples
#' c(dsl[1],dsl[3])
c.DataSetList <- function(...) {
  # TODO: maybe remove duplicated dataset in the further
  # remove the empty list first
  dsl <- list(...)
  dsl <- dsl[sapply(dsl, length) != 0]
  
  if (length(dsl) == 0)
    return()
  
  object <- unlist(dsl, recursive = F)
  if (!any((class(object)) == 'DataSetList'))
    class(object) <- c('DataSetList', class(object))
  
  for (attr_str in c('DIM', 'funcId', 'algId')) {
    attr(object, attr_str) <-
      unlist(lapply(dsl, function(x)
        attr(x, attr_str)))
  }
  
  for (attr_str in c('suite', 'maximization')) {
    temp  <-
      unique(
        unlist(lapply(dsl, function(x)
          attr(x, attr_str))))
    if (length(temp) > 1) {
      stop(paste0("Attempted to add datasets with different ", attr_str, 
                  "-attributes! This will lead to errors when processing
                  this data!"))
    }
    attr(object, attr_str) <- temp[[1]]
  }
  object
}

#' S3 extraction function for DataSetList
#'
#' @param x The DataSetList to use
#' @param i The indices to extract
#' @param drop Currently unused parameter
#' @return The DataSetList of the DataSets at indices i of DataSetList x
#' @export
#' @examples
#' dsl[c(1, 3)]
`[.DataSetList` <- function(x, i, drop = FALSE) {
  # remove the attributes firstly
  obj <- unclass(x)[i]
  class(obj) <- c('DataSetList', class(obj))
  
  # also slice the attributes accordingly
  attr(obj, 'DIM') <- attr(x, 'DIM')[i]
  attr(obj, 'funcId') <- attr(x, 'funcId')[i]
  attr(obj, 'algId') <- attr(x, 'algId')[i]
  attr(obj, 'suite') <- attr(x, 'suite')
  attr(obj, 'maximization') <- attr(x, 'maximization')
  
  obj
}

#' S3 print function for DataSetList
#'
#' @param x The DataSetList to print
#' @param ... Arguments for underlying print function?
#' @export
#' @examples
#' print(dsl)
print.DataSetList <- function(x, ...) {
  cat('DataSetList:\n')
  cat(paste0('Suite: ', attr(x, 'suite'), '\n'))
  for (i in seq_along(x)) {
    cat(sprintf('%d: %s\n', i, as.character(x[[i]])))
  }
}

# TODO: consistent use of ds, data, dsList etc.
# TODO: make decision on `DIM` or `dim`? funcId or fId or fID? algID / algid / algId?

#' S3 summary function for DataSetList
#'
#' Prints the Function ID, Dimension, Algorithm Id, datafile location and comment for every
#' DataSet in the DataSetList
#' @param object The DataSetList to print
#' @param ... Arguments for underlying summary function?
#' @export
#' @examples
#' summary(dsl)
summary.DataSetList <- function(object, ...) {
  as.data.frame(
    t(
      sapply(
        object, 
        function(d) {
          list(
            suite = attr(d, 'suite'),
            funcId = attr(d, 'funcId'),
            DIM = attr(d, 'DIM'),
            algId = attr(d, 'algId'),
            datafile = attr(d, 'datafile'),
            comment = attr(d, 'comment')
          )
        }
      )
    )
  )
}

#' S3 sort function for DataSetList
#'
#' Sorts a DataSetList based on the custom specified attributes ('algId', 'DIM' or 'funcId'). 
#' Default is as ascending, can be made descending by adding a - in front of the attribute.
#' Sorting accross multiple attributes is supported, in the order they are specified.
#' 
#' @param dsl The DataSetList to sort
#' @param ... attribute by which `dsl` is sorted. Multiple attributes can be specified.
#' @export
#' @examples 
#' arrange(dsl, DIM, -funcId, algId) 
arrange <- function(dsl, ...) UseMethod('arrange', dsl)

#' @rdname arrange
#' @export
#'
arrange.DataSetList <- function(dsl, ...) {
  cols <- substitute(list(...))[-1L]
  if (identical(as.character(cols), "NULL")) 
    return(dsl)
  
  cols <- as.list(cols)
  order <- as.list(rep(1L, length(cols)))
  
  for (i in seq_along(cols)) {
    v <- as.list(cols[[i]])
    if (length(v) > 1L) {
      if (v[[1L]] == '-') order[[i]] <- -1L
      v <- v[[-1L]]
    }
    
    v <- as.character(v)
    if (v %in% c('DIM', 'funcId', 'algId'))
      cols[[i]] <- v
    else {
      cols[[i]] <- NULL
      order[[i]] <- NULL
    }
  }
  
  cols <- unlist(cols, use.names = F)
  order <- unlist(order)
  x <- c(list(1:length(dsl)), lapply(cols, function(col) attr(dsl, col)))
  names(x) <- c('index', cols) 
  
  DT <- rbindlist(list(x))
  data.table::setorderv(DT, cols, order)
  idx <- DT[[1]]
  dsl <- dsl[idx]
  
  # TODO: perhaps we do not need those attributes at all...
  for (v in c('DIM', 'funcId', 'algId'))
    attr(dsl, v) <- sapply(dsl, function(d) attr(d, v))
  dsl
}

#' @rdname get_ERT
#' @param algorithm Which algorithms in the DataSetList to consider.
#' @export
#'
get_ERT.DataSetList <-
  function(ds, ftarget, algorithm = 'all', ...) {
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'), attr(ds, 'funcId'), get_ERT(ds, ftarget))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }))
  }

#' @rdname get_RT_summary
#' @param algorithm Which algorithms in the DataSetList to consider.
#' @export
get_RT_summary.DataSetList <-
  function(ds, ftarget, algorithm = 'all', ...) {
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'),
              attr(ds, 'funcId'),
              get_RT_summary(ds, ftarget))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }))
  }

#' @rdname get_RT_sample
#' @param algorithm Which algorithms in the DataSetList to consider.
#'
#' @export
get_RT_sample.DataSetList <-
  function(ds, ftarget, algorithm = 'all', ...) {
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'),
              attr(ds, 'funcId'),
              get_RT_sample(ds, ftarget, ...))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }),
    fill = T)
  }

#' @rdname get_maxRT
#' @param algorithm Which algorithms in the DataSetList to consider.
#'
#' @export
get_maxRT.DataSetList <- function(ds, algorithm = 'all', ...) {
  if (algorithm != 'all')
    ds <- subset(ds, algId == algorithm)
  
  rbindlist(lapply(ds, function(ds_) {
    res <-
      cbind(attr(ds_, 'DIM'), attr(ds_, 'funcId'), get_maxRT(ds_, ...))
    colnames(res)[1] <- 'DIM'
    colnames(res)[2] <- 'funcId'
    res
  }),
  fill = T)
}

#' @rdname get_FV_summary
#' @param algorithm Which algorithms in the DataSetList to consider.
#' @export
#'
get_FV_summary.DataSetList <-
  function(ds, runtime, algorithm = 'all', ...) {
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'),
              attr(ds, 'funcId'),
              get_FV_summary(ds, runtime))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }))
}

#' @param algorithm Which algorithms in the DataSetList to consider.
#' @export
#' @rdname get_FV_overview
#'
get_FV_overview.DataSetList <-
  function(ds, algorithm = 'all', ...) {
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds)
      get_FV_overview(ds)))
    
}

#' @rdname get_RT_overview
#' @param algorithm Which algorithms in the DataSetList to consider.
#' @export
get_RT_overview.DataSetList <-
  function(ds, algorithm = 'all', ...) {
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds)
      get_RT_overview(ds)))
}

#' @rdname get_overview
#' @export
get_overview.DataSetList <-
  function(ds, ...) {
    df <- rbindlist(lapply(ds, function(ds)
      get_overview(ds)))
    if (length(get_funcId(ds)) > 1 || length(get_dim(ds)) > 1) {
      p1 <- df[,lapply(.SD, max, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('budget', 'best reached')]
      p2 <- df[,lapply(.SD, mean, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('mean reached')]
      p3 <- df[,lapply(.SD, min, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('worst recorded', 'worst reached')]
      return(merge(merge(p1,p2),p3))
    }
    else {
      return(df)
    }
}

#' @rdname get_FV_sample
#' @param algorithm Which algorithms in the DataSetList to consider.
#' @export
#'
get_FV_sample.DataSetList <-
  function(ds, runtime, algorithm = 'all', ...) {
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'),
              attr(ds, 'funcId'),
              get_FV_sample(ds, runtime, ...))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }),
    fill = T)
  }

#' @rdname get_PAR_summary
#' @param algorithm Which algorithms in the DataSetList to consider.
#' @export
get_PAR_summary.DataSetList <- function(ds, idxValue, algorithm = 'all', ...) {
  if (algorithm != 'all')
    ds <- subset(ds, algId == algorithm)
    
  rbindlist(lapply(ds, function(ds) get_PAR_summary(ds, idxValue, ...)))
}

#' @rdname get_PAR_sample
#' @param algorithm Which algorithms in the DataSetList to consider.
#' @export
get_PAR_sample.DataSetList <- function(ds, idxValue, algorithm = 'all', ...) {
  if (algorithm != 'all')
    ds <- subset(ds, algId == algorithm)
    
  rbindlist(lapply(ds, function(ds) get_PAR_sample(ds, idxValue, ...)), fill = T)
}

#' Get all dimensions present in a DataSetList
#'
#' @param dsList The DataSetLsit
#'
#' @return A sorted list of all unique dimensions which occur in the DataSetList
#' @export
#' @examples
#' get_dim(dsl)
get_dim <- function(dsList) {
  sort(unique(sapply(dsList, function(d) attr(d, 'DIM'))))
}

#' Get all function ids present in a DataSetList
#'
#' @param dsList The DataSetLsit
#'
#' @return A sorted list of all unique function ids which occur in the DataSetList
#' @export
#' @examples
#' get_funcId(dsl)
get_funcId <- function(dsList) {
  sort(unique(sapply(dsList, function(d) attr(d, 'funcId'))))
}

#' Get all algorithm ids present in a DataSetList
#'
#' @param dsList The DataSetLsit
#'
#' @return A sorted list of all unique algorithm ids which occur in the DataSetList
#' @export
#' @examples
#' get_algId(dsl)
get_algId <- function(dsList) {
  unique(sapply(dsList, function(d) attr(d, 'algId')))
}

#' Get all parameter ids present in a DataSetList
#'
#' @param dsList The DataSetList
#' @param which A string takes values in `c('by_FV', 'by_RT')`. To choose the parameters aligned 
#' by the running time (RT) or the function value (FV). Note that parameters in each case are 
#' not necessary the same.
#'
#' @return A sorted list of all unique parameter ids which occur in the DataSetList
#' @export
#' @examples
#' get_parId(dsl)
get_parId <- function(dsList, which = 'by_FV') {
  unique(
    unlist(
      lapply(dsList, 
             function(d) {
               if (which == 'by_FV') 
                 names(d$PAR$by_FV)
               else if (which == 'by_RT') 
                 names(d$PAR$by_RT)
             }
      )
    )
  )
}

# TODO: let the user choose/detect whether the problem is subject to maximization
# and determine whether to sort the function values in ascending/desceding order

#' Get all function values present in a DataSetList
#'
#' @param dsList The DataSetLsit
#'
#' @return A list matrices of all function values which occur in the DataSetList
#' @export
#' @examples
#' get_funvals(dsl)
get_funvals <- function(dsList) {
  if (length(dsList) == 0)
    return(NULL)
  if (length(dsList[[1]]$RT) == 0) {
    sort(unique(as.numeric(unlist(
      lapply(dsList, function(x) 
        as.vector(x$FV))
    ))))
  }
  else
    (sort(unique(as.numeric(unlist(
      lapply(dsList, function(x)
        rownames(x$RT))
    )))))
}

#' Get all runtime values present in a DataSetList
#'
#' @param dsList The DataSetLsit
#'
#' @return A list matrices of all runtime values which occur in the DataSetList
#' @export
#' @examples
#' get_runtimes(dsl)
get_runtimes <- function(dsList) {
  sort(unique(as.numeric(unlist(
    lapply(dsList, function(x)
      rownames(x$FV))
  ))))
}

# #' Get the best function value reached in a DataSetList
# #'
# #' @param dsList The DataSetLsit
# #'
# #' @return A list matrices of all runtime values which occur in the DataSetList
# #' @export
# get_best_targets <- function(dsList, by = 'funcId', maximize = T) {
#   targets <- c()
#   funcIds <- get_funcId(dsList)

#   for (i in seq_along(aggr_attr)) {
#     data <- subset(dsList, funcId == funcIds[i])

#     Fall <- get_funvals(data)
#     Fval <- ifelse(maximize, max(Fall), min(Fall))
#     targets <- c(targets, Fval)
#   }
#   targets
# }

# TODO: the attribute list should als be sliced here...
#' Filter a DataSetList by some criteria
#'
#' @param x The DataSetLsit
#' @param ... The condition to filter on. Can be any expression which assigns True or False
#' to a DataSet object, such as DIM == 625 or funcId == 2
#'
#' @return The filtered DataSetList
#' @export
#' @examples
#' subset(dsl, funcId == 1)
subset.DataSetList <- function(x, ...) {
  condition_call <- substitute(list(...))
  enclos <- parent.frame()
  idx <- sapply(x,
                function(ds)
                  all(unlist(
                    eval(condition_call, attributes(ds), enclos)
                  )))
  x[idx]
}

#' Get the ERT-values for all DataSets in a DataSetList at certain targets
#'
#' @param dsList The DataSetLsit
#' @param aggr_on Whether to aggregate on 'funcId' or 'DIM'.
#' @param targets Predifined target function-values. Should be one for each function/dimension
#' @param maximize Whether the DataSetList is from a maximization or minimization problem
#'
#' @return A data.table containing ERT-values
#' @export
#' @examples
#' max_ERTs(dsl)
max_ERTs <-
  function(dsList,
           aggr_on = 'funcId',
           targets = NULL,
           maximize = T)
    UseMethod("max_ERTs", dsList)

#TODO: rename this function! this function needs to be rewritten
#' @rdname max_ERTs
#' @export
max_ERTs.DataSetList <-
  function(dsList,
           aggr_on = 'funcId',
           targets = NULL,
           maximize = T) {
    N <- length(get_algId(dsList))
    
    aggr_attr <-
      if (aggr_on == 'funcId')
        get_funcId(dsList)
    else
      get_dim(dsList)
    if (!is.null(targets) &&
        length(targets) != length(aggr_attr))
      targets <- NULL
    
    second_aggr <-
      if (aggr_on == 'funcId')
        get_dim(dsList)
    else
      get_funcId(dsList)
    if (length(second_aggr) > 1)
      return(NULL)
    
    erts <- seq(0, 0, length.out = length(get_algId(dsList)))
    names(erts) <- get_algId(dsList)
    
    for (j in seq_along(aggr_attr)) {
      dsList_filetered <-
        if (aggr_on == 'funcId')
          subset(dsList, funcId == aggr_attr[[j]])
      else
        subset(dsList, DIM == aggr_attr[[j]])
      
      if (is.null(targets)) {
        Fall <- get_funvals(dsList_filetered)
        Fval <- ifelse(maximize, max(Fall), min(Fall))
      }
      else
        Fval <- targets[[j]]
      summary <- get_RT_summary(dsList_filetered, ftarget = Fval)
      ert <- summary$ERT
      names(ert) <- summary$algId
      erts <- rbind(erts, ert[get_algId(dsList)])
    }
    return(erts[-1, ])
}

#' Get the expected function-values for all DataSets in a DataSetList at certain runtimes
#'
#' @param dsList The DataSetLsit
#' @param aggr_on Whether to aggregate on 'funcId' or 'DIM'.
#' @param runtimes Predifined target runtimes-values. Should be one for each function/dimension
#'
#' @return A data.table containing expected fucntion-values
#' @export
#' @examples
#' mean_FVs(dsl)
mean_FVs <-
  function(dsList,
           aggr_on = 'funcId',
           runtimes = NULL)
    UseMethod("mean_FVs", dsList)

#' @rdname mean_FVs
#' @export
mean_FVs.DataSetList <-
  function(dsList,
           aggr_on = 'funcId',
           runtimes = NULL) {
    N <- length(get_algId(dsList))
    
    aggr_attr <-
      if (aggr_on == 'funcId')
        get_funcId(dsList)
    else
      get_dim(dsList)
    if (!is.null(runtimes) &&
        length(runtimes) != length(aggr_attr))
      targets <- NULL
    
    second_aggr <-
      if (aggr_on == 'funcId')
        get_dim(dsList)
    else
      get_funcId(dsList)
    if (length(second_aggr) > 1)
      return(NULL)
    
    erts <- seq(0, 0, length.out = length(get_algId(dsList)))
    names(erts) <- get_algId(dsList)
    
    for (j in seq_along(aggr_attr)) {
      dsList_filetered <-
        if (aggr_on == 'funcId')
          subset(dsList, funcId == aggr_attr[[j]])
      else
        subset(dsList, DIM == aggr_attr[[j]])
      
      if (is.null(runtimes)) {
        RTall <- get_runtimes(dsList_filetered)
        RTval <- max(RTall)
      }
      else
        RTval <- runtimes[[j]]
      summary <- get_FV_summary(dsList_filetered, runtime = RTval)
      ert <- summary$mean
      names(ert) <- summary$algId
      erts <- rbind(erts, ert[get_algId(dsList)])
    }
    return(erts[-1, ])
}
