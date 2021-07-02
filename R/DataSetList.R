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
#'  \item'SOS'
#'  }
#'  These formats are specified in more detail in our github wiki.
#' @param subsampling Logical. Whether *.cdat files are subsampled?
#' @param print_fun Function used to print output when in verbose mode
#' @param full_aggregation If True, individual DataSets are aggregated as much as possible: all DataSets
#' with the same algorithmname, function id and dimension are combined together. This leads to information loss
#' related to static variables, so only use if that information is not required. 
#'
#' @return A DataSetList object
#' @export
#' @examples
#' path <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package = "IOHanalyzer")
#' DataSetList(path)
DataSetList <- function(path = NULL, verbose = T, print_fun = NULL, maximization = NULL,
                        format = IOHprofiler, subsampling = FALSE, full_aggregation = TRUE) {
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
  else if (format == SOS) {
    return(read_datasetlist_SOS(path, locate_corrections_files(path)))
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
              '      %d instances on f%s %dD...\n',
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
  if (length(maximization) != 1) {
    warning("Multipe different optimization types detected!")
  }
  
  attr(object, 'suite') <- suite
  attr(object, 'maximization') <- maximization
  if (full_aggregation)
    clean_DataSetList(object)
  else
    object
}


#' Clean DataSetList object by concatenating DataSets
#' 
#' Concatenates all DataSets with the same algorith name, function id and dimension
#'  
#' @param dsList The DataSetList object to clean
#' @export
#' @examples 
#' clean_DataSetList(dsl)
clean_DataSetList <- function(dsList) {
  #To ensure no uninitialized variables are present
  .I <- NULL
  cases <- mapply(
    function(...) paste0(list(...), collapse = ','),
    attr(dsList, 'funcId'), 
    attr(dsList, 'DIM'),
    attr(dsList, 'algId'),
    SIMPLIFY = T,
    USE.NAMES = F
  )
    
  dt <- as.data.table(cases)[, list(list(.I)), by = cases]
  idx_to_del <- c()
  
  for (idx in dt$V1) {
    if (length(idx) > 1) {
      dsList[[idx[1]]] <- c.DataSet(dsList[idx])
      idx_to_del <- c(idx_to_del, idx[-1])
    }
  }  
  dsList[idx_to_del] <- NULL
  
  if (length(idx_to_del) > 0) {
    attr(dsList, 'DIM') <- sapply(dsList, function(ds) attr(ds, 'DIM'))
    attr(dsList, 'funcId') <- sapply(dsList, function(ds) attr(ds, 'funcId'))
    attr(dsList, 'algId') <- sapply(dsList, function(ds) attr(ds, 'algId'))
  }
  dsList
}

#' S3 concatenation function for DataSetList
#'
#' @param ... The DataSetLists to concatenate
#' @return A new DataSetList
#' @export
#' @examples
#' c(dsl[1], dsl[3])
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
    attr(object, attr_str) <- unlist(lapply(dsl, function(x) attr(x, attr_str)))
  }

  # Deal with Suites on the datasetlist level
  attr(object, "suite") <- unique(
    unlist(
      lapply(dsl, function(x) attr(x, "suite"))
    )
  )
  
  # These attributes NEED to be the same across the datasetlist
  for (attr_str in c('maximization')) {
    temp <- unique(
        unlist(lapply(dsl, function(x) attr(x, attr_str)))
    )

    if (length(temp) > 1) {
      stop(paste0("Attempted to add datasetlists with different ", attr_str,
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
  N <- length(x)
  
  # TODO: add an option for the following numbers: 15 and 5
  if (N <= 15) 
    idx <- seq_along(x)
  else 
    idx <- c(1:5, '---', (N-4):N)
  idx <- format(idx, justify = 'right')
    
  for (i in idx) {
    if (trimws(i) == '---')
      cat(paste0(i), '\n')
    else {
      .i <- as.integer(trimws(i))
      cat(sprintf('%s: %s\n', i, as.character(x[[.i]])))
    }
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
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#' @export
#'
get_ERT.DataSetList <- function(ds, ftarget, budget = NULL, algorithm = 'all', ...) {
    if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'), attr(ds, 'funcId'), get_ERT(ds, ftarget, budget))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }))
  }

#' @rdname get_RT_summary
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#' @export
get_RT_summary.DataSetList <- function(ds, ftarget, budget = NULL, algorithm = 'all', ...) {
    if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)
    
    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'),
              attr(ds, 'funcId'),
              get_RT_summary(ds, ftarget, budget))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }))
  }

#' @rdname get_RT_sample
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#'
#' @export
get_RT_sample.DataSetList <- function(ds, ftarget, algorithm = 'all', ...) {
    if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
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
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#'
#' @export
get_maxRT.DataSetList <- function(ds, algorithm = 'all', ...) {
  if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
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
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#' @export
#'
get_FV_summary.DataSetList <- function(ds, runtime, algorithm = 'all', ...) {
    if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
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

#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#' @export
#' @rdname get_FV_overview
#'
get_FV_overview.DataSetList <- function(ds, algorithm = 'all', ...) {
    if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)

    rbindlist(lapply(ds, function(ds)
      get_FV_overview(ds)))

}

#' @rdname get_RT_overview
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#' @export
get_RT_overview.DataSetList <- function(ds, algorithm = 'all', ...) {
    if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)

    rbindlist(lapply(ds, function(ds)
      get_RT_overview(ds)))
}

#' @rdname get_overview
#' @export
get_overview.DataSetList <- function(ds, ...) {
    df <- rbindlist(lapply(ds, function(ds)
      get_overview(ds)))
    if (length(get_funcId(ds)) > 1 || length(get_dim(ds)) > 1) {
      p2 <- df[,lapply(.SD, mean, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('mean reached')]
      if (attr(ds, 'maximization')) {
        p1 <- df[,lapply(.SD, max, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('budget', 'best reached')]
        p3 <- df[,lapply(.SD, min, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('worst recorded', 'worst reached')]
      }
      else {
        p1 <- df[,lapply(.SD, min, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('best reached')]
        p3 <- df[,lapply(.SD, max, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('budget', 'worst recorded', 'worst reached')]
      }
      return(merge(merge(p1,p2),p3))
    }
    else {
      return(df)
    }
}

#' @rdname get_FV_sample
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#' @export
#'
get_FV_sample.DataSetList <- function(ds, runtime, algorithm = 'all', ...) {
    if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
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
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#' @export
get_PAR_summary.DataSetList <- function(ds, idxValue, algorithm = 'all', ...) {
  if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
  if (algorithm != 'all')
    ds <- subset(ds, algId == algorithm)

  rbindlist(lapply(ds, function(ds) get_PAR_summary(ds, idxValue, ...)))
}

#' @rdname get_PAR_sample
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider. 
#' @export
get_PAR_sample.DataSetList <- function(ds, idxValue, algorithm = 'all', ...) {
  if (!missing("algorithm")) warning("Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer.")
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
  ll <- unique(unname(unlist(sapply(dsList, function(d) attr(d, 'funcId')))))

  # TODO: what if the function ID is a double value?
  # those are coerced to integers now
  if (is.integer(ll)) return(sort(ll))
  
  lli <- suppressWarnings(as.integer(ll))
  if (any(is.na(lli))) return(sort(ll))
  
  if (all((lli >= 0L) & (lli <= 1000000000L))) return(ll[order(lli)])
  
  # TODO: should this be even allowed?
  sort(lli)
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
    x <- sort(unique(as.numeric(unlist(
      lapply(dsList, function(x)
        as.vector(x$FV))
    ))))
  }
  else
    x <- (sort(unique(as.numeric(unlist(
      lapply(dsList, function(x)
        rownames(x$RT))
    )))))
  x[!is.na(x) & !is.infinite(x)]
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

#' Get all attributes which can be used to subset a DataSetList
#'
#' @param dsl The DataSetList
#'
#' @return The list of available attributes
#' @export
#' @examples
#' get_static_attributes(dsl)
get_static_attributes <- function(dsl) {
  full_names <- unique(unlist(lapply(dsl, function(ds) {names(attributes(ds))})))
  
  reserved_attributes <- c("names", "class", "suite", "maximization", "algInfo", "comment", 
                           "datafile", "maxRT", "finalFV", "format")
  setdiff(full_names, reserved_attributes)
}

#' Get all options for a specific attribute which can be used to subset a DataSetList
#' 
#' This is a more generic version of the existing `get_dim`, `get_funcId` and `get_algId` functions.
#' Note the only attributes returned by `get_static_attributes` are supported in this funcion
#'
#' @param dsl The DataSetList
#' @param attribute the name of the attribute for which to get the available options in dsl
#' @return The list of options for the specified attribute
#' @export
#' @examples
#' get_static_attribute_values(dsl, 'funcId')
get_static_attribute_values <- function(dsl, attribute) {
  unique(unlist(lapply(dsl, function(ds) {attr(ds, attribute)})))
}

#' Filter a DataSetList by some criteria
#'
#' @param x The DataSetList
#' @param ... The conditions to filter on. Can be any expression which assigns True or False
#' to a DataSet object, such as DIM == 625 or funcId == 2. Usage of && and || is only supported on default attributes 
#' (funcId, algId, DIM), not on combinations of with other attributes (e.g. instance). In those cases, & and | should 
#' be used respectively
#'
#' @return The filtered DataSetList
#' @export
#' @examples
#' subset(dsl, funcId == 1)
#' subset(dsl, funcId == 1 && DIM == 16) # Can use && and || for default attributes
#' subset(dsl, instance == 1)
#' subset(dsl, instance == 1 & funcId == 1) # Can use & and | for all attributes
#' subset(dsl, instance == 1, funcId == 1) # Comma-seperated conditions are treated as AND
subset.DataSetList <- function(x, ...) {
  condition_call <- substitute(list(...))
  enclos <- parent.frame()
  
  condition_call <- tryCatch({
    temp <- eval(condition_call, enclos = enclos)
    if (class(temp) == "list") {
      temp <- lapply(unlist(temp), function(x) {
        if (class(x) == 'character') {
          parse(text = x)
        }
        else x
      })
      unlist(temp)
    } else if (class(temp) == "call") {
      list(temp)
    } else {
      NULL
    }
  }, error = function(e) {condition_call[2:length(condition_call)]})
  
  obj <- lapply(x,
                function(ds){
                  mask <- tryCatch(expr = {
                    mask <- NULL
                    for (idx in seq(length(condition_call))) {
                      mask_temp <- unlist(
                        eval(condition_call[[idx]], attributes(ds), enclos = enclos)
                      )
                      if (is.null(mask)) mask <- mask_temp
                      else {
                        if (length(mask_temp) == 1 && !mask_temp) {
                          mask <- F
                        } else if (length(mask_temp) == 1) {
                          mask <- mask
                        } else if (length(mask_temp) == length(mask) || length(mask) == 1) {
                          mask <- mask & mask_temp
                        } else {
                          stop("Error creating mask")
                        }
                        
                      }
                    }
                    mask
                  }, error = function(e) {F})
                  
                  if (length(mask) == 1 && mask) return(ds)
                  else if (length(mask) == 1 || !any(mask)) return(NULL)
                  return(subset(ds, mask))
                })
  
  class(obj) <- c('DataSetList', class(obj))
  obj <- Filter(Negate(is.null), obj)
  
  # also slice the attributes accordingly
  attr(obj, 'suite') <- attr(x, 'suite')
  attr(obj, 'maximization') <- attr(x, 'maximization')
  attr(obj, 'DIM') <- sapply(obj, function(ds) attr(ds, 'DIM'))
  attr(obj, 'funcId') <- sapply(obj, function(ds) attr(ds, 'funcId'))
  attr(obj, 'algId') <- sapply(obj, function(ds) attr(ds, 'algId'))
  
  return(obj)
}

#' Add unique identifiers to each DataSet in the provided DataSetList based on static attributes
#' 
#' Note that this function returns a new DataSetList object, since a split into new datasetlist has to be done to
#' ensure each dataset has exactly one unique identifier.
#' Note that only static attributes (see `get_static_attributes`) can be used to create unique identifiers. 
#'
#' @param dsl The DataSetList
#' @param attrs The list of attributes to combine into a unique identifier
#' @return A new DataSetList object where the split has been done based on the provided attributes, and the unique
#' identifier has been added. 
#' @export
#' @examples
#' add_unique_id(dsl, c('instance'))
add_unique_id <- function(dsl, attrs) {
  if (!all(attrs %in% get_static_attributes(dsl))) stop("Selected attributes are not usable to create unique ids")
  grid <- expand.grid(lapply(attrs, function(x){get_static_attribute_values(dsl, x)}))
  colnames(grid) <- attrs
  
  dsl_new <- DataSetList()
  attr_vals <- c()
  for (x in transpose(grid)) {
    conditions <- unlist(lapply(seq(length(attrs)), function(idx) {
      parse(text = paste0(attrs[[idx]], ' == ', x[[idx]]))
    }))
    dsl_temp <- subset(dsl, conditions)
    if (length(attrs) == 1) 
      attr_val <- x
    else
      attr_val <- paste0(x, collapse = "_")
    
    attr_vals <- c(attr_vals, rep(attr_val, length(dsl_temp)))
    dsl_new <- c(dsl_new, dsl_temp)
  }
  attr(dsl_new, 'identifying_variables') <- attrs
  attr(dsl_new, 'unique_ids') <- attr_vals
  for (idx in seq(length(dsl_temp))) {
    attr(dsl_new[[idx]], 'unique_id') <- attr_vals[[idx]]
  }
  dsl_new
}

#' Get the unique identifiers for each DataSet in the provided DataSetList
#' 
#' If no unique identifier is set (using `add_unique_id`), this function falls back on returning the algorith id
#' (from `get_aldId`)to ensure backwards compatibility
#'
#' @param dsl The DataSetList
#' @return The list of unique identiefiers present in dsl
#' @export
#' @examples
#' get_unique_id(dsl)
get_unique_id <- function(dsl) {
  temp <- attr(dsl, 'unique_id')
  if (is.null(temp)) return(get_algId(dsl))
  return(unique(temp))
}


#' Save DataTable in multiple formats
#' 
#' @param df The DataTable to store
#' @param file String. The name of the figure file, with the extension of the required file-format
#' @param format Optional, string. Overwrites the extension of the `file` parameter. If not specified while
#' file does not have an extension, it defaults to csv
#' 
#' @export
#' @examples 
#' df <- generate_data.Single_Function(subset(dsl, funcId == 1), which = 'by_RT')
#' save_table(df, tempfile(fileext = ".md"))
save_table <- function(df, file, format = NULL) {
  if (is.null(format)) {
    format <- tools::file_ext(file)
  }
  if (format == 'TeX' || format == 'tex') {
    if (requireNamespace('xtable', quietly = T))
      print(xtable::xtable(df), file = file)
    else 
      write(kable(df, format = 'latex'), file)
  } else if (format == 'Markdown' || format == 'md') {
    write(kable(df, format = 'markdown'), file)
  } else if (format == 'html') {
    write(kable(df, format = "html"), file)
  }
  else { #Default to csv
    write.csv(df, file, row.names = F)
  }
}

#' Generation of default ECDF-targets
#' 
#' @param dsList The DataSetList object for which to generate the targets
#' @param type The way to generate the targets. Either 'log-linear', 'linear' or 'bbob' (51 fixed targets,
#' equal for all functions / dimensions)
#' @param number_targets The amount of targets to generate
#' 
#' @return A data.table with 3 columns: funcId, DIM and target
#' @export
#' @examples 
#' get_ECDF_targets(dsl, 'linear', 10)
get_ECDF_targets <- function(dsList, type = "log-linear", number_targets = 10) {
  funcIds <- get_funcId(dsList)
  dims <- get_dim(dsList)
  
  dt <- rbindlist(apply(expand.grid(funcIds, dims), 1, function(x) {
    if (type == 'bbob') {
      fseq <- rev(seq_FV(c(100,1e-8), length.out = 51, scale = 'log'))
    }
    else {
      dsl <- subset(dsList, funcId == x[[1]] && DIM == x[[2]])
      if (length(dsl) == 0) 
        return(NULL)
      fall <- get_funvals(dsl)
      if (length(fall) < 2) 
        return(NULL) 
      
      fseq <- seq_FV(fall, length.out = number_targets, scale = ifelse(type == "log-linear", 'log', 'linear'))
    }
    data.table(funcId = x[[1]], DIM = x[[2]], target = fseq)
  }))
  dt
}

### __________________________ Rewritten data generation functions _______________________ ###
#' Generate dataframe of a single function/dimension pair
#' 
#' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function 
#' 
#' @param dsList The DataSetList object
#' @param start Optional start value (Runtime or target value)
#' @param stop Optional end value (Runtime or target value)
#' @param scale_log Wheterh to use logarithmic scaling or not
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' @param include_opts Whether or not to also include the best value hit by each algorithm to
#' the generated datapoints
#' @param budget Optional; overwrites the budget of each individual algorithm when doing ERT calculations. Only works
#' in fixed_target mode.
#' 
#' @export
#' @examples 
#' generate_data.Single_Function(subset(dsl, funcId == 1), which = 'by_RT')
generate_data.Single_Function <- function(dsList, start = NULL, stop = NULL, 
                                          scale_log = F, which = 'by_RT', include_opts = F, budget = NULL) {
  
  if (length(get_funcId(dsList)) != 1 || length(get_dim(dsList)) != 1 ) {
    #Required because target generation is included in this function, 
    #which needs to be done on a per-function basis
    stop("Multiple functions / dimensions are present in provided DataSetList. 
    Please call this function for each individual function/dimension pair instead.") 
  }
  
  by_rt <- (which == 'by_RT')
  
  if (by_rt)
    all <- get_funvals(dsList)
  else
    all <- get_runtimes(dsList)

  maximization <- attr(dsList, 'maximization')
  
  if (is.null(maximization)) maximization <- T
  if (is.null(start)) start <- min(all)
  if (is.null(stop)) stop <- max(all)
  
  if (by_rt) {
    Xseq <- seq_FV(all, start, stop, length.out = 60,
                   scale = ifelse(scale_log, 'log', 'linear'))
    if (include_opts) {
      for (algid in get_algId(dsList)) {
        if (maximization)
          Xseq <- c(Xseq, max(get_funvals(subset(dsList, algId == algid))))
        else
          Xseq <- c(Xseq, min(get_funvals(subset(dsList, algId == algid))))
      }
      Xseq <- unique(sort(Xseq))
    }
    dt <- get_RT_summary(dsList, ftarget = Xseq, budget = budget)
  }
  else {
    Xseq <- seq_RT(all, start, stop, length.out = 60,
                   scale = ifelse(scale_log, 'log', 'linear'))
    if (include_opts) {
      for (algid in get_algId(dsList)) {
        Xseq <- c(Xseq, max(get_funvals(subset(dsList, algId == algid))))
      }
      Xseq <- unique(sort(Xseq))
    }
    dt <- get_FV_summary(dsList, Xseq)
  }
  
  
  dt[, `:=`(upper = mean + sd, lower = mean - sd)]
  return(dt)
}

#' Generate dataframe of a single function/dimension pair for creating PDF or PMF plots
#' 
#' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function 
#' 
#' @param dsList The DataSetList object
#' @param target The target value (Runtime or target value)
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' 
#' @export
#' @examples 
#' generate_data.PMF(subset(dsl, funcId == 1), target = 15, which = 'by_RT')
generate_data.PMF <- function(dsList, target, which = 'by_RT') {
  if (which == 'by_RT')
    return(get_RT_sample(dsList, target, output = 'long'))
  return(get_FV_sample(dsList, target, output = 'long'))
}

#' Generate dataframe of a single function/dimension pair
#' 
#' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function 
#' 
#' @param dsList The DataSetList object
#' @param target The target value (Runtime or target value)
#' @param use.equal.bins Whether all bins should be equal size for each algorithm or not
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' 
#' @export
#' @examples 
#' generate_data.hist(subset(dsl, funcId == 1), target = 15, which = 'by_RT')
generate_data.hist <- function(dsList, target, use.equal.bins = F, which = 'by_RT') {
  width <- NULL # Set local binding to remove warnings
  
  if (length(get_funcId(dsList)) > 1 || length(get_dim(dsList)) > 1)
    stop('This function is only available a single function/dimension pair at a time.')
  
  if (use.equal.bins) {
    if (which == 'by_RT')
      res1 <- hist(
        get_RT_sample(dsList, target, output = 'long')$RT, 
        breaks = nclass.FD, 
        plot = F
      )
    else
      res1 <- hist(
        get_FV_sample(dsList, target, output = 'long')$`f(x)`, 
        breaks = nclass.FD, 
        plot = F
      )
  }
  
  dt <- as.data.table(
    rbindlist(
      lapply(
        dsList, 
        function(ds) {
          algId <- attr(ds, 'algId')
          
          if (which == 'by_RT') 
            data <- get_RT_sample(ds, target, output = 'long')$RT
          else if (which == 'by_FV') 
            data <- get_FV_sample(ds, target, output = 'long')$`f(x)`
          else 
            stop('Invalid argument for parameter `which`.')
          
          if (sum(!is.na(data)) < 2)
            return(NULL)
          
          if (use.equal.bins) 
            breaks <- res1$breaks
          else 
            breaks <- nclass.FD
          
          res <- hist(data, breaks = breaks, plot = F)
          breaks <- res$breaks
          
          plot_text <- paste0(
            '<b>count</b>: ', res$counts, '<br><b>breaks</b>: [',
            breaks[-length(breaks)], ',', breaks[-1], ']'
          )
          
          plot_data <- data.frame(
            x = res$mids, 
            y = res$counts, 
            algId = algId,
            width = breaks[2] - breaks[1],
            text =  plot_text
          )
        }
      ) %>% {
        `[`(., !vapply(., is.null, logical(1)))
      }
    )
  )
  dt
}

#' Generate dataframe of a single function/dimension pair
#' 
#' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function 
#' 
#' @param dsList The DataSetList object
#' @param targets A list or data.table containing the targets per function / dimension. If this is 
#' a data.table, it needs columns 'target', 'DIM' and 'funcId'
#' @param scale_log Wheterh to use logarithmic scaling or not
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' @param use_full_range Whether or not to use the full range of the x-axis or cut it off as soon as 
#' all algorithms reach 98\% success (+10\% buffer). Only supported in the case of one function and dimension
#' 
#' @export
#' @examples 
#' generate_data.ECDF(subset(dsl, funcId == 1), c(10, 15, 16))
generate_data.ECDF <- function(dsList, targets, scale_log = F, which = 'by_RT', use_full_range = TRUE) {
  V1 <- NULL #Set local binding to remove warnings
  by_rt <- which == 'by_RT'
  if (by_rt) {
    RT <- get_runtimes(dsList)
    if (!use_full_range) {
      if (length(unique(get_funcId(dsList))) > 1 || length(unique(get_dim(dsList))) > 1) {
        warning("The limiting of x-values is only supported in the case of 1 function 1 dimension!")
      }
      else {
        maxRT <- as.integer(max(get_RT_summary(dsList, targets)[,'98%']) * 1.1) #Slight buffer for nicer plotting
        RT <- c(min(RT), maxRT)
      }
    }
    x <- unique(seq_RT(RT, length.out = 50, scale = ifelse(scale_log, 'log', 'linear')))
    
    #TODO: Some scaling by dimension?
    
    if (!is.data.table(targets)) {
      if (length(get_funcId(dsList)) > 1 || length(get_dim(dsList)) > 1 )
        stop("Targets provided are not in data.table format, while multiple functions / dimensions 
             are present in provided DataSetList.")
      targets <- data.table(
        target = targets, 
        funcId = get_funcId(dsList), 
        DIM = get_dim(dsList)
      )
    }
  }
  else {
    FV <- get_funvals(dsList)
    x <- unique(seq_FV(FV, length.out = 50, scale = ifelse(scale_log, 'log', 'linear')))
  }
  
  dt <- as.data.table(rbindlist(lapply(dsList, function(df) {
    algId <- attr(df, 'algId')
    if (by_rt) {
      temp <- targets[DIM == attr(df, 'DIM'), c('target', 'funcId')]
      targets_ <- temp[funcId == attr(df, 'funcId')][['target']]
    }
    else 
      targets_ <- targets
    m <- lapply(targets_, function(target) {
      if (by_rt)
        data <- get_RT_sample(df, target, output = 'long')$RT
      else
        data <- get_FV_sample(df, target, output = 'long')$`f(x)`
      
      if (all(is.na(data)))
        return(rep(0, length(x)))
      fun <- ecdf(data)
      if (is.function(fun)) fun(x) else NA
    }) %>%
      do.call(rbind, .)
    
    data.frame(x = x,
               mean = apply(m, 2, . %>% mean(na.rm = T)),
               sd = apply(m, 2, . %>% sd(na.rm = T))) %>%
      mutate(upper = mean + sd, lower = mean - sd, algId = algId)
  })))
  dt[, mean(mean), by = .(x, algId)][, .(mean = V1, algId = algId, x = x)]
}


#' Generate dataframe containing the AUC for any ECDF-curves
#' 
#' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function 
#' 
#' @param dsList The DataSetList object
#' @param targets A list or data.table containing the targets per function / dimension. If this is 
#' a data.table, it needs columns 'target', 'DIM' and 'funcId'
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' @param scale_log Whether to use logarithmic scaling or not
#' @param dt_ecdf A data table of the ECDF to avoid needless recomputations. Will take preference if it 
#' is provided together with dsList and targets
#' @param multiple_x Boolean, whether to get only the total AUC or get stepwise AUC values
#' 
#' @export
#' @examples 
#' generate_data.AUC(dsl, get_ECDF_targets(dsl))
#' generate_data.AUC(NULL, NULL, dt_ecdf = generate_data.ECDF(dsl, get_ECDF_targets(dsl)))
generate_data.AUC <- function(dsList, targets, scale_log = F, which = 'by_RT', dt_ecdf = NULL,
                              multiple_x = FALSE) {
  idx <- auc_contrib <- mean_pre <- mean_post <- x <- x_pre <- auc <- NULL
  if (is.null(dt_ecdf)) {
    if (length(dsList) == 0 || is.null(targets))
      return(NULL)
    dt_ecdf <- generate_data.ECDF(dsList, targets, scale_log, which)
  }
  max_idx <- nrow(unique(dt_ecdf[,'x']))
  dt_ecdf[, idx := seq(max_idx), by = 'algId']
  dt3 = copy(dt_ecdf)
  dt3[, idx := idx - 1]
  dt_merged = merge(dt_ecdf, dt3, by = c('algId', 'idx'))
  colnames(dt_merged) <- c("algId", "idx", "mean_pre", "x_pre", "mean_post", "x")
  dt_merged[, auc_contrib := ((mean_pre + mean_post)/2)*(x - x_pre)]
  dt_merged[, auc := cumsum(auc_contrib)/x, by = 'algId']
  #TODO: just for max x
  if (multiple_x)
    return(dt_merged[, c('algId','x','auc') ])
  return(dt_merged[idx == (max_idx - 1), c('algId','x','auc') ])
}
  
  
# #' Generate dataframe of a single function/dimension pair
# #' 
# #' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function
# #' 
# #' @param dsList The DataSetList object
# #' @param targets A list of the target value for which to calculate the AUC (Runtime or target value)
# #' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
# #' 
# #' @export
# #' @examples
# #' generate_data.AUC(subset(dsl, funcId == 1), c(12, 16))
# generate_data.AUC <- function(dsList, targets, which = 'by_RT') {
#   if (length(get_funcId(dsList)) > 1 || length(get_dim(dsList)) > 1) {
#     stop("This function is only available a single function/dimension pair at a time.")
#   }
#   by_rt <- which == 'by_RT'
#   
#   if (by_rt)
#     RT.max <- sapply(dsList, function(ds) max(attr(ds, 'maxRT'))) %>% max
#   else {
#     funevals.max <- sapply(dsList, function(ds) max(attr(ds, 'finalFV'))) %>% max
#     funevals.min <- sapply(dsList, function(ds) min(attr(ds, 'finalFV'))) %>% min
#   }
#   
#   as.data.table(rbindlist(lapply(dsList, function(df) {
#     algId <- attr(df, 'algId')
#     if (by_rt)
#       auc <- sapply(targets, function(fv) {
#         ECDF(df, fv) %>% AUC(from = 1, to = RT.max)
#       })
#     else {
#       funs <- lapply(targets, function(r) {
#         get_FV_sample(df, r, output = 'long')$'f(x)' %>% {
#           if (all(is.na(.))) NULL
#           else  {
#             f <- ecdf(.)
#             attr(f, 'min') <- min(.)
#             attr(f, 'max') <- max(.)
#             f
#           }
#         }
#       })
#       
#       auc <- sapply(funs,
#                     function(fun) {
#                       if (is.null(fun)) 0
#                       else{ 
#                         if (attr(df, 'maximization'))
#                           integrate(fun, lower = attr(fun, 'min') - 1, upper = funevals.max,
#                                     subdivisions = 1e3) %>% {'$'(., 'value') / funevals.max}
#                         else 
#                           integrate(fun, lower =  funevals.min, upper = attr(fun, 'max') + 1,
#                                     subdivisions = 1e3) %>% {'$'(., 'value') / (attr(fun, 'max') + 1)}
#                       }
#                     })
#     }
#     data.frame(x = targets, AUC = auc, algId = algId)
#   })))
# }

#' Generate dataframe of a single function/dimension pair
#' 
#' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function 
#' 
#' @param dsList The DataSetList object
#' @param scale_log Wheterh to use logarithmic scaling or not
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' 
#' @export
#' @examples 
#' generate_data.Parameters(subset(dsl, funcId == 1))
generate_data.Parameters <- function(dsList, which = 'by_RT', scale_log = F) {
  if (length(get_funcId(dsList)) > 1 || length(get_dim(dsList)) > 1) {
    stop("This function is only available a single function/dimension pair at a time.")
  }
  if (which == 'by_RT') {
    rtall <- get_runtimes(dsList)
    
    rtseq <- seq_RT(rtall, length.out = 50,  scale = ifelse(scale_log, 'log', 'linear'))
    req(rtseq)
    
    dt <- get_PAR_summary(dsList, rtseq, which = 'by_RT')
  }
  else if (which == 'by_FV') {
    rtall <- get_funvals(dsList)
    
    rtseq <- seq_FV(rtall, length.out = 50,  scale = ifelse(scale_log, 'log', 'linear'))
    req(rtseq)
    
    dt <- get_PAR_summary(dsList, rtseq, which = 'by_FV')
  }
  else stop("Invalid value for parameter `which`")
  dt[, `:=`(upper = mean + sd, lower = mean - sd)]
}

#' Generate dataframe of a single function/dimension pair
#' 
#' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function 
#' 
#' @param dsList The DataSetList object
#' @param aggr_on Which attribute to use for aggregation. Either 'funcId' or 'DIM'
#' @param targets Optional list of target values (Runtime or target value)
#' @param which Whether to use a fixed-target 'by_RT' perspective or fixed-budget 'by_FV'
#' 
#' @export
#' @examples 
#' generate_data.Aggr(dsl)
generate_data.Aggr <- function(dsList, aggr_on = 'funcId', targets = NULL, which = 'by_RT') {
  maximize <- attr(dsList, 'maximization')
  variable <- fid <- value <- NULL #Set local binding to remove warnings
  by_rt <- which == 'by_RT'
  
  if (is.null(targets)) {
    targets <- get_target_dt(dsList, which)
  }
 
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(dsList) else get_dim(dsList)
  N <- length(get_algId(dsList))

  dt <- rbindlist(lapply(aggr_attr, function(agg_val) {
    if (by_rt) {
      if (aggr_on == 'funcId')
        dt <- get_RT_summary(subset(dsList, funcId == agg_val), targets[funcId == agg_val][['target']])
      else
        dt <- get_RT_summary(subset(dsList, DIM == agg_val), targets[DIM == agg_val][['target']])
      dt[, c('algId', value = 'ERT', 'funcId', 'DIM')]
      setnames(dt, 'ERT', 'value')
    }
    else{
      if (aggr_on == 'funcId')
        dt <- get_FV_summary(subset(dsList, funcId == agg_val), targets[funcId == agg_val][['target']])
      else
        dt <- get_FV_summary(subset(dsList, DIM == agg_val), targets[DIM == agg_val][['target']])
      dt[, c('algId', value = 'mean', 'funcId', 'DIM')]
      setnames(dt, 'mean', 'value')
    }
  }))

  if (by_rt) order_sel <- 1
  else order_sel <- -1*(maximize*2 - 1)
  
  dt[, rank := frank(order_sel*value, na.last = T), by = .(DIM, funcId)]
  return(dt)
}



#' Generate dataframe of a the unaggregated values of individual algorithms. Stripped-down version of 
#' 
#' This provides an unaggregated version of the function `generate_data.ECDF`. 
#' 
#' @param dsList The DataSetList object
#' @param targets A list or data.table containing the targets per function / dimension. If this is 
#' a data.table, it needs columns 'target', 'DIM' and 'funcId'
#' @param scale_log Wheterh to use logarithmic scaling or not
#' 
#' @export
#' @examples 
#' generate_data.ECDF_raw(subset(dsl, funcId == 1), c(10, 15, 16))
generate_data.ECDF_raw <- function(dsList, targets, scale_log = F) {
  V1 <- NULL #Set local binding to remove warnings
  
  #Get the x-coordinates at which to calculate the ECDF
  RT <- get_runtimes(dsList)
  x <- unique(seq_RT(RT, length.out = 50, scale = ifelse(scale_log, 'log', 'linear')))
  #TODO: Some scaling by dimension?
  
  #Get targets to use
  if (!is.data.table(targets)) {
    if (length(get_funcId(dsList)) > 1 || length(get_dim(dsList)) > 1 )
      stop("Targets provided are not in data.table format, while multiple functions / dimensions
           are present in provided DataSetList.")
    targets <- data.table(
      target = targets,
      funcId = get_funcId(dsList),
      DIM = get_dim(dsList)
    )
  }
  
  dt <- as.data.table(rbindlist(lapply(dsList, function(df) {
    algId <- attr(df, 'algId')
    temp <- targets[DIM == attr(df, 'DIM'), c('target', 'funcId')]
    targets_ <- temp[funcId == attr(df, 'funcId')][['target']]
    
    m <- lapply(targets_, function(target) {
      data <- fast_RT_samples(df$RT, target, attr(df, 'maximization'))
      if (all(is.na(data)))
        hit <- (rep(0, length(x)))
      else {
        fun <- ecdf(data)
        hit <- if (is.function(fun)) fun(x) else NA
      }
      data.table(hit = hit, rt = x, target = target, funcId = attr(df, 'funcId'), DIM = attr(df, 'DIM'), algId = attr(df, 'algId'))
    }) %>%
      do.call(rbind, .)
    m
  })))
  dt
}