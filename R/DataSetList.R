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
read_dir <-
  function(path,
           verbose = T,
           print_fun = NULL,
           maximization = TRUE,
           format = IOHprofiler,
           subsampling = FALSE) {
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
DataSetList <-
  function(path = NULL,
           verbose = T,
           print_fun = NULL,
           maximization = NULL,
           format = IOHprofiler,
           subsampling = FALSE,
           full_aggregation = TRUE) {
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
            print_fun(sprintf(
              '      %d instances on f%s %dD...\n',
              length(info$instance),
              info$funcId,
              info$DIM
            ))
          }

          copy_flag <- TRUE
          if (ifelse(is.null(info$version), F, (compareVersion(info$version, "0.3.3") >= 0))) {
            tryCatch(
              data <- read_IOH_v1plus(info),
              error = function(e) {
                print(e)
                return(NULL)
              }
            )
          } else {
            data <- DataSet(
              info,
              maximization = maximization,
              format = format,
              subsampling = subsampling
            )
          }
          DIM[i] <- attr(data, 'DIM')
          funcId[i] <-
            attr(data,
                 getOption('IOHanalyzer.function_representation', 'funcId'))
          algId[i] <- attr(data, 'algId')

          # TODO: double-check the following treatment on `instance`!!!
          instance <-
            attr(data, 'instance') #Was instance without index?
          suites[i] <- attr(data, 'suite')
          maximizations[i] <- attr(data, 'maximization')

          if (length(object) != 0) {
            idx <- which(sapply(object, function(obj)
              obj == data))
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
    attr(object, 'ID') <- attr(object, 'algId')
    attr(object, 'ID_attributes') <- c('algId')
    attr(object, 'constrained') <-
      any(unlist(lapply(object, function(x) {
        attr(x, 'constrained')
      })))
    if (full_aggregation)
      clean_DataSetList(object)
    else
      object
  }


#' Clean DataSetList object by concatenating DataSets
#'
#' Concatenates all DataSets with the same ID, algid, function id and dimension
#'
#' @param dsList The DataSetList object to clean
#' @export
#' @examples
#' clean_DataSetList(dsl)
clean_DataSetList <- function(dsList) {
  #To ensure no uninitialized variables are present
  .I <- NULL

  if (is.null(attr(dsList, 'ID'))) {
    dsList <-
      change_id(dsList, getOption("IOHanalyzer.ID_vars", c("algId")))
  }

  cases <- mapply(
    function(...)
      paste0(list(...), collapse = ','),
    attr(dsList, 'funcId'),
    attr(dsList, 'DIM'),
    attr(dsList, 'algId'),
    attr(dsList, 'ID'),
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
    attr(dsList, 'DIM') <- sapply(dsList, function(ds)
      attr(ds, 'DIM'))
    attr(dsList, 'funcId') <-
      sapply(dsList, function(ds)
        attr(ds, 'funcId'))
    attr(dsList, 'algId') <-
      sapply(dsList, function(ds)
        attr(ds, 'algId'))
    attr(dsList, 'ID') <-
      sapply(dsList, function(ds)
        attr(ds, 'ID'))
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

  for (attr_str in c('DIM', 'funcId', 'algId', 'ID')) {
    attr(object, attr_str) <-
      unlist(lapply(dsl, function(x)
        attr(x, attr_str)))
  }

  # Deal with Suites on the datasetlist level
  attr(object, "suite") <- unique(unlist(lapply(dsl, function(x)
    attr(x, "suite"))))

  # These attributes NEED to be the same across the datasetlist
  for (attr_str in c('maximization', 'ID_attributes', 'constrained')) {
    temp <- unique(unlist(lapply(dsl, function(x)
      attr(x, attr_str))))

    if (length(temp) > 1) {
      stop(
        paste0(
          "Attempted to add datasetlists with different ",
          attr_str,
          "-attributes! This will lead to errors when processing
                  this data!"
        )
      )
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
  attr(obj, 'ID') <- attr(x, 'ID')[i]
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
    idx <- c(1:5, '---', (N - 4):N)
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
  as.data.frame(t(sapply(object,
                         function(d) {
                           list(
                             suite = attr(d, 'suite'),
                             funcId = attr(d, 'funcId'),
                             DIM = attr(d, 'DIM'),
                             ID = attr(d, 'ID'),
                             datafile = attr(d, 'datafile'),
                             comment = attr(d, 'comment')
                           )
                         })))
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
arrange <- function(dsl, ...)
  UseMethod('arrange', dsl)

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
      if (v[[1L]] == '-')
        order[[i]] <- -1L
      v <- v[[-1L]]
    }

    v <- as.character(v)
    if (v %in% c('DIM', 'funcId', 'ID'))
      cols[[i]] <- v
    else {
      cols[[i]] <- NULL
      order[[i]] <- NULL
    }
  }

  cols <- unlist(cols, use.names = F)
  order <- unlist(order)
  x <-
    c(list(1:length(dsl)), lapply(cols, function(col)
      attr(dsl, col)))
  names(x) <- c('index', cols)

  DT <- rbindlist(list(x))
  data.table::setorderv(DT, cols, order)
  idx <- DT[[1]]
  dsl <- dsl[idx]

  # TODO: perhaps we do not need those attributes at all...
  for (v in c('DIM', 'funcId', 'ID'))
    attr(dsl, v) <- sapply(dsl, function(d)
      attr(d, v))
  dsl
}

#' @rdname get_ERT
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider.
#' @export
#'
get_ERT.DataSetList <-
  function(ds,
           ftarget,
           budget = NULL,
           algorithm = 'all',
           ...) {
    if (!missing("algorithm"))
      warning(
        "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
      )
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)

    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'),
              attr(ds, 'funcId'),
              get_ERT(ds, ftarget, budget))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }))
  }

#' @rdname get_RT_summary
#' @export
get_RT_summary.DataSetList <-
  function(ds, ftarget, budget = NULL, ...) {
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
get_RT_sample.DataSetList <-
  function(ds, ftarget, algorithm = 'all', ...) {
    if (!missing("algorithm"))
      warning(
        "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
      )
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
  if (!missing("algorithm"))
    warning(
      "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
    )
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
get_FV_summary.DataSetList <-
  function(ds,
           runtime,
           algorithm = 'all',
           include_geom_mean = F,
           ...) {
    if (!missing("algorithm"))
      warning(
        "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
      )
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)

    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(
          attr(ds, 'DIM'),
          attr(ds, 'funcId'),
          get_FV_summary(ds, runtime, include_geom_mean)
        )
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }))
  }

#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider.
#' @export
#' @rdname get_FV_overview
#'
get_FV_overview.DataSetList <-
  function(ds, algorithm = 'all', ...) {
    if (!missing("algorithm"))
      warning(
        "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
      )
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)

    rbindlist(lapply(ds, function(ds)
      get_FV_overview(ds)))

  }

#' @rdname get_RT_overview
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider.
#' @export
get_RT_overview.DataSetList <-
  function(ds, algorithm = 'all', ...) {
    if (!missing("algorithm"))
      warning(
        "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
      )
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
    p2 <-
      df[, lapply(.SD, mean, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('mean reached')]
    if (attr(ds, 'maximization')) {
      p1 <-
        df[, lapply(.SD, max, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('budget', 'best reached')]
      p3 <-
        df[, lapply(.SD, min, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('worst recorded', 'worst reached')]
    }
    else {
      p1 <-
        df[, lapply(.SD, min, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('best reached')]
      p3 <-
        df[, lapply(.SD, max, na.rm = TRUE), by = c('DIM', 'funcId'), .SDcols = c('budget', 'worst recorded', 'worst reached')]
    }
    return(merge(merge(p1, p2), p3))
  }
  else {
    return(df)
  }
}

#' @rdname get_FV_sample
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider.
#' @export
#'
get_FV_sample.DataSetList <-
  function(ds, runtime, algorithm = 'all', ...) {
    if (!missing("algorithm"))
      warning(
        "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
      )
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
get_PAR_summary.DataSetList <-
  function(ds, idxValue, algorithm = 'all', ...) {
    if (!missing("algorithm"))
      warning(
        "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
      )
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)

    rbindlist(lapply(ds, function(ds)
      get_PAR_summary(ds, idxValue, ...)))
  }

#' @rdname get_PAR_sample
#' @param algorithm DEPRECATED, will be removed in next release. Which algorithms in the DataSetList to consider.
#' @export
get_PAR_sample.DataSetList <-
  function(ds, idxValue, algorithm = 'all', ...) {
    if (!missing("algorithm"))
      warning(
        "Argument 'algorithm' is deprecated and will be removed in the next release of IOHanalyzer."
      )
    if (algorithm != 'all')
      ds <- subset(ds, algId == algorithm)

    rbindlist(lapply(ds, function(ds)
      get_PAR_sample(ds, idxValue, ...)), fill = T)
  }

#' @rdname get_id
#' @export
get_id.DataSetList <- function(ds, ...) {
  temp <- attr(ds, 'ID')
  if (is.null(temp)) {
    # warning("No ID attribute set, returning the algId's instead. (from 1.6.0 onwards, ID attributes are always added
    #         to new datasets, see the 'change_id' function.")
    return(get_algId(ds))
  }
  return(unique(temp))
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
  sort(unique(sapply(dsList, function(d)
    attr(d, 'DIM'))))
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
  ll <-
    unique(unname(unlist(sapply(dsList, function(d)
      attr(d, 'funcId')))))

  # TODO: what if the function ID is a double value?
  # those are coerced to integers now
  if (is.integer(ll))
    return(sort(ll))

  lli <- suppressWarnings(as.integer(ll))
  if (any(is.na(lli)))
    return(sort(ll))

  if (all((lli >= 0L) &
          (lli <= 1000000000L)))
    return(ll[order(lli)])

  # TODO: should this be even allowed?
  sort(lli)
}

#' Get all function names present in a DataSetList
#'
#' @param dsList The DataSetLsit
#'
#' @return A list of all unique function names which occur in the DataSetList
#' @export
#' @examples
#' get_funcName(dsl)
get_funcName <- function(dsList) {
  unique(unname(unlist(sapply(dsList, function(d)
    attr(d, 'funcName')))))
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
  unique(sapply(dsList, function(d)
    attr(d, 'algId')))
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
  unique(unlist(lapply(dsList,
                       function(d) {
                         if (which == 'by_FV')
                           names(d$PAR$by_FV)
                         else if (which == 'by_RT')
                           names(d$PAR$by_RT)
                       })))
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
  if (length(get_RT(dsList[[1]])) == 0) {
    x <- sort(unique(as.numeric(unlist(
      lapply(dsList, function(x)
        as.vector(get_FV(x)))
    ))))
  }
  else
    x <- (sort(unique(as.numeric(unlist(
      lapply(dsList, function(x)
        rownames(get_RT(x)))
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
  full_names <-
    unique(unlist(lapply(dsl, function(ds) {
      names(attributes(ds))
    })))

  reserved_attributes <-
    c(
      "names",
      "class",
      "suite",
      "maximization",
      "algInfo",
      "comment",
      "datafile",
      "maxRT",
      "finalFV",
      "format"
    )
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
  unique(unlist(lapply(dsl, function(ds) {
    attr(ds, attribute)
  })))
}

#' Filter a DataSetList by some criteria
#'
#' @param x The DataSetList
#' @param ... The conditions to filter on. Can be any expression which assigns True or False
#' to a DataSet object, such as DIM == 625 or funcId == 2. Usage of && and || is only supported on default attributes
#' (funcId, algId, DIM), not on combinations of with other attributes (e.g. instance). In those cases, & and | should
#' be used respectively. Alternatively, this can be used as a keyword argument named 'text', with the condition as a
#' string to be parsed. This allows exectution of subset commands on arbitrary variables in code.
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
  enclos <- parent.frame()
  if (hasArg('text')) {
    text <- list(...)$text
    condition_call <- parse(text = text)
  } else {
    condition_call <- substitute(list(...))
    condition_call <- condition_call[2:length(condition_call)]
  }

  obj <- lapply(x,
                function(ds) {
                  mask <- tryCatch(
                    expr = {
                      mask <- NULL
                      for (idx in seq(length(condition_call))) {
                        mask_temp <- unlist(eval(condition_call[[idx]], attributes(ds), enclos = enclos))
                        if (is.null(mask))
                          mask <- mask_temp
                        else {
                          if (length(mask_temp) == 1 && !mask_temp) {
                            mask <- F
                          } else if (length(mask_temp) == 1) {
                            mask <- mask
                          } else if (length(mask_temp) == length(mask) ||
                                     length(mask) == 1) {
                            mask <- mask & mask_temp
                          } else {
                            stop("Error creating mask")
                          }

                        }
                      }
                      mask
                    },
                    error = function(e) {
                      F
                    }
                  )

                  if (length(mask) == 1 && mask)
                    return(ds)
                  else if (length(mask) == 1 ||
                           !any(mask))
                    return(NULL)
                  return(subset(ds, mask))
                })

  class(obj) <- c('DataSetList', class(obj))
  obj <- Filter(Negate(is.null), obj)

  # also slice the attributes accordingly
  attr(obj, 'suite') <- attr(x, 'suite')
  attr(obj, 'maximization') <- attr(x, 'maximization')
  attr(obj, 'DIM') <- sapply(obj, function(ds)
    attr(ds, 'DIM'))
  attr(obj, 'funcId') <-
    sapply(obj, function(ds)
      attr(ds, 'funcId'))
  attr(obj, 'algId') <- sapply(obj, function(ds)
    attr(ds, 'algId'))
  unique_ids <-
    unlist(sapply(obj, function(ds)
      attr(ds, 'unique_id')))
  if (!any(is.null(unique_ids))) {
    attr(obj, 'unique_ids') <- unique_ids
  }
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
#' change_id(dsl, c('instance'))
change_id <- function(dsl, attrs) {
  if (length(dsl) == 0)
    return(dsl)
  if (!all(attrs %in% get_static_attributes(dsl)))
    stop("Selected attributes are not usable to create unique ids")
  grid <-
    expand.grid(lapply(attrs, function(x) {
      get_static_attribute_values(dsl, x)
    }))
  colnames(grid) <- attrs

  dsl_new <- DataSetList()
  attr_vals <- c()
  for (x in transpose(grid)) {
    #TODO: on Windows, UTF-8 Characters get converted into <U+XXXX> characters, which then
    #don't interact correctly with the parse used in 'subset' function, leading to empty datasetlist objects.
    #I'm not aware of any way to fix this, so UFT-8 characters should be avoided in ID for now.
    conditions <-
      paste0(unlist(lapply(seq(length(
        attrs
      )), function(idx) {
        paste0(attrs[[idx]], ' == "', x[[idx]], '"')
      })), collapse = " & ")
    dsl_temp <- subset(dsl, text = conditions)
    if (length(attrs) == 1)
      attr_val <- x
    else
      attr_val <- paste0(x, collapse = "_")

    attr_vals <- c(attr_vals, rep(attr_val, length(dsl_temp)))
    dsl_new <- c(dsl_new, dsl_temp)
  }
  attr(dsl_new, 'ID_attributes') <- attrs
  attr(dsl_new, 'ID') <- attr_vals
  for (idx in seq(length(dsl_new))) {
    attr(dsl_new[[idx]], 'ID') <- attr_vals[[idx]]
  }
  class(dsl_new) <- c("DataSetList", "list")
  attr(dsl_new, 'suite') <- attr(dsl, 'suite')
  attr(dsl_new, 'maximization') <- attr(dsl, 'maximization')

  attr(dsl_new, 'DIM') <-
    lapply(dsl_new, function(x)
      attr(x, 'DIM'))
  attr(dsl_new, 'funcId') <-
    lapply(dsl_new, function(x)
      attr(x, 'funcId'))
  attr(dsl_new, 'algId') <-
    lapply(dsl_new, function(x)
      attr(x, 'algId'))
  dsl_new
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
  else {
    #Default to csv
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
get_ECDF_targets <-
  function(dsList,
           type = "log-linear",
           number_targets = 10) {
    funcIds <- get_funcId(dsList)
    dims <- get_dim(dsList)

    dt <- rbindlist(apply(expand.grid(funcIds, dims), 1, function(x) {
      if (type == 'bbob') {
        fseq <- rev(seq_FV(c(100, 1e-8), length.out = 51, scale = 'log'))
      }
      else {
        dsl <- subset(dsList, funcId == x[[1]] && DIM == x[[2]])
        if (length(dsl) == 0)
          return(NULL)
        fall <- get_funvals(dsl)
        if (length(fall) < 2)
          return(NULL)

        fseq <-
          seq_FV(
            fall,
            length.out = number_targets,
            scale = ifelse(type == "log-linear", 'log', 'linear')
          )
      }
      data.table(funcId = x[[1]],
                 DIM = x[[2]],
                 target = fseq)
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
#' @param include_geom_mean Boolean to indicate whether to include the geometric mean.
#' Only works in fixed_budget mode. Negative values cause NaN, zeros cause output to be completely 0. Defaults to False.
#'
#' @export
#' @examples
#' generate_data.Single_Function(subset(dsl, funcId == 1), which = 'by_RT')
generate_data.Single_Function <-
  function(dsList,
           start = NULL,
           stop = NULL,
           scale_log = F,
           which = 'by_RT',
           include_opts = F,
           budget = NULL,
           include_geom_mean = F) {
    if (length(get_funcId(dsList)) != 1 ||
        length(get_dim(dsList)) != 1) {
      #Required because target generation is included in this function,
      #which needs to be done on a per-function basis
      stop(
        "Multiple functions / dimensions are present in provided DataSetList.
    Please call this function for each individual function/dimension pair instead."
      )
    }

    by_rt <- (which == 'by_RT')

    if (by_rt)
      all <- get_funvals(dsList)
    else
      all <- get_runtimes(dsList)

    maximization <- attr(dsList, 'maximization')

    if (is.null(maximization))
      maximization <- T
    if (is.null(start))
      start <- min(all)
    if (is.null(stop))
      stop <- max(all)

    if (by_rt) {
      Xseq <- seq_FV(
        all,
        start,
        stop,
        length.out = 60,
        scale = ifelse(scale_log, 'log', 'linear')
      )
      if (include_opts) {
        for (uid in get_id(dsList)) {
          if (maximization)
            Xseq <-
              c(Xseq, max(get_funvals(subset(
                dsList, ID == uid
              ))))
          else
            Xseq <-
              c(Xseq, min(get_funvals(subset(
                dsList, ID == uid
              ))))
        }
        Xseq <- unique(sort(Xseq))
      }
      dt <- get_RT_summary(dsList, ftarget = Xseq, budget = budget)
    }
    else {
      Xseq <- seq_RT(
        all,
        start,
        stop,
        length.out = 60,
        scale = ifelse(scale_log, 'log', 'linear')
      )
      if (include_opts) {
        for (uid in get_id(dsList)) {
          Xseq <- c(Xseq, max(get_funvals(subset(
            dsList, ID == uid
          ))))
        }
        Xseq <- unique(sort(Xseq))
      }
      dt <-
        get_FV_summary(dsList, Xseq, include_geom_mean = include_geom_mean)
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
generate_data.hist <-
  function(dsList,
           target,
           use.equal.bins = F,
           which = 'by_RT') {
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

    dt <- as.data.table(rbindlist(lapply(dsList,
                                         function(ds) {
                                           ID <- get_id(ds)

                                           if (which == 'by_RT')
                                             data <- get_RT_sample(ds, target, output = 'long')$RT
                                           else if (which == 'by_FV')
                                             data <-
                                               get_FV_sample(ds, target, output = 'long')$`f(x)`
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

                                           plot_text <- paste0('<b>count</b>: ',
                                                               res$counts,
                                                               '<br><b>breaks</b>: [',
                                                               breaks[-length(breaks)],
                                                               ',',
                                                               breaks[-1],
                                                               ']')

                                           plot_data <- data.frame(
                                             x = res$mids,
                                             y = res$counts,
                                             ID = ID,
                                             width = breaks[2] - breaks[1],
                                             text =  plot_text
                                           )
                                         }) %>% {
                                           `[`(., !vapply(., is.null, logical(1)))
                                         }))
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
generate_data.ECDF <-
  function(dsList,
           targets,
           scale_log = F,
           which = 'by_RT',
           use_full_range = TRUE) {
    by_rt <- which == 'by_RT'
    V1 <- frac <- NULL
    if (by_rt) {
      maximization <- attr(dsList, "maximization")
      RT <- get_runtimes(dsList)
      if (!use_full_range) {
        if (length(unique(get_funcId(dsList))) > 1 ||
            length(unique(get_dim(dsList))) > 1) {
          warning("The limiting of x-values is only supported in the case of 1 function 1 dimension!")
        }
        else {
          maxRT <-
            as.integer(max(get_RT_summary(dsList, targets)[, '98%']) * 1.1) #Slight buffer for nicer plotting
          RT <- c(min(RT), maxRT)
        }
      }
      x <-
        unique(seq_RT(
          RT,
          length.out = 50,
          scale = ifelse(scale_log, 'log', 'linear')
        ))

      #TODO: Some scaling by dimension?

      if (!is.data.table(targets)) {
        if (length(get_funcId(dsList)) > 1 || length(get_dim(dsList)) > 1)
          stop(
            "Targets provided are not in data.table format, while multiple functions / dimensions
             are present in provided DataSetList."
          )
        targets <- data.table(
          target = targets,
          funcId = get_funcId(dsList),
          DIM = get_dim(dsList)
        )
      }
    }
    else {
      FV <- get_funvals(dsList)
      x <-
        unique(seq_FV(
          FV,
          length.out = 50,
          scale = ifelse(scale_log, 'log', 'linear')
        ))
    }



    dt <- as.data.table(rbindlist(lapply(dsList, function(df) {
      ID <- get_id(df)
      if (by_rt) {
        temp <- targets[DIM == attr(df, 'DIM'), c('target', 'funcId')]
        targets_ <- temp[funcId == attr(df, 'funcId')][['target']]
      }
      else
        targets_ <- targets


      if (by_rt) {
        data <- get_FV_sample(df, x, output = 'long')
        if (maximization)
          data$frac <-
            unlist(lapply(data$`f(x)`, function(temp)
              mean(temp > targets_)))
        else
          data$frac <-
            unlist(lapply(data$`f(x)`, function(temp)
              mean(temp < targets_)))
        res <-
          data[, .(x = runtime,
                   mean = mean(frac),
                   sd = sd(frac)), by = 'runtime']
      } else {
        data <- get_RT_sample(df, x, output = 'long')
        data[is.na(data)] <- Inf
        data$frac <-
          unlist(lapply(data$RT, function(temp)
            mean(temp < targets_)))
        res <-
          data[, .(x = target,
                   mean = mean(frac),
                   sd = sd(frac)), by = 'target']
      }
      mutate(res,
             upper = mean + sd,
             lower = mean - sd,
             ID = ID)
    })))
    dt[, mean(mean), by = .(x, ID)][, .(mean = V1, ID = ID, x = x)]
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
#' @param normalize Whether to normalize the resulting AUC values to [0,1] or not
#'
#' @export
#' @examples
#' \dontshow{data.table::setDTthreads(1)}
#' generate_data.AUC(dsl, get_ECDF_targets(dsl))
#' generate_data.AUC(NULL, NULL, dt_ecdf = generate_data.ECDF(dsl, get_ECDF_targets(dsl)))
generate_data.AUC <-
  function(dsList,
           targets,
           scale_log = F,
           which = 'by_RT',
           dt_ecdf = NULL,
           multiple_x = FALSE,
           normalize = T) {
    idx <-
      auc_contrib <- mean_pre <- mean_post <- x <- x_pre <- auc <- NULL
    if (is.null(dt_ecdf)) {
      if (length(dsList) == 0 || is.null(targets))
        return(NULL)
      dt_ecdf <- generate_data.ECDF(dsList, targets, scale_log, which)
    }
    dt_ecdf <- dt_ecdf[, c('mean', 'ID', 'x')]
    rt_max <- max(dt_ecdf[, 'x'])
    dt_merged <- rbindlist(lapply(unique(dt_ecdf$ID), function(id) {
      dt_part <- dt_ecdf[ID == id,]
      max_idx <- nrow(unique(dt_part[, 'x']))
      dt_part[, idx := seq(max_idx), by = 'ID']
      dt3 = copy(dt_part)
      dt3[, idx := idx - 1]
      dt_merged_part = merge(dt_part, dt3, by = c('ID', 'idx'))
      colnames(dt_merged_part) <-
        c("ID", "idx", "mean_pre", "x_pre", "mean_post", "x")
      if (max(dt_part[, 'x']) < rt_max) {
        extreme <- data.table(
          "ID" = id,
          "idx" = max_idx + 1,
          "x_pre" = max(dt_part[, 'x']),
          "x" = rt_max,
          "mean_pre" = max(dt_part[, 'mean']),
          "mean_post" = max(dt_part[, 'mean'])
        )
        max_idx <- max_idx + 1
        dt_merged_part <- rbind(dt_merged_part, extreme)
      }
      dt_merged_part[, auc_contrib := ((mean_pre + mean_post) / 2) * (x - x_pre)]
      if (normalize) {
        dt_merged_part[, auc := cumsum(auc_contrib) / x, by = 'ID']
      } else {
        dt_merged_part[, auc := cumsum(auc_contrib), by = 'ID']
      }
      if (multiple_x)
        return(dt_merged_part[, c('ID', 'x', 'auc')])
      return(dt_merged_part[idx == (max_idx - 1), c('ID', 'x', 'auc')])
    }))
    return(dt_merged)
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
generate_data.Parameters <-
  function(dsList,
           which = 'by_RT',
           scale_log = F) {
    if (length(get_funcId(dsList)) > 1 || length(get_dim(dsList)) > 1) {
      stop("This function is only available a single function/dimension pair at a time.")
    }
    if (which == 'by_RT') {
      rtall <- get_runtimes(dsList)

      rtseq <-
        seq_RT(rtall,
               length.out = 50,
               scale = ifelse(scale_log, 'log', 'linear'))
      req(rtseq)

      dt <- get_PAR_summary(dsList, rtseq, which = 'by_RT')
    }
    else if (which == 'by_FV') {
      rtall <- get_funvals(dsList)

      rtseq <-
        seq_FV(rtall,
               length.out = 50,
               scale = ifelse(scale_log, 'log', 'linear'))
      req(rtseq)

      dt <- get_PAR_summary(dsList, rtseq, which = 'by_FV')
    }
    else
      stop("Invalid value for parameter `which`")
    dt[, `:=`(upper = mean + sd, lower = mean - sd)]
  }

#' Generate dataframe of exactly 2 parameters, matched by running time
#'
#' This function generates a dataframe which can be easily plotted using the `plot_general_data`-function
#'
#' @param dsList The DataSetList object
#' @param par1 The first parameter. Either a parameter name or 'f(x)'
#' @param par2 The second parameter. Either a parameter name or 'f(x)'
#'
#' @export
#' @examples
#' generate_data.Parameter_correlation(subset(dsl, funcId == 1), 'f(x)', 'f(x)')
generate_data.Parameter_correlation <-
  function(dsList, par1, par2) {
    dt <- rbindlist(lapply(dsList, function(ds) {
      if (par1 == 'f(x)') {
        if (!isTRUE(attr(ds, 'contains_full_FV'))) {
          return(NULL)
        }
        dt1 <- ds$FV_raw_mat %>% reshape2::melt()
      } else {
        if (!(par1 %in% get_PAR_name(ds, which = 'by_RT')))
          return(NULL)
        dt1 <- ds$PAR$by_RT[[par1]] %>% reshape2::melt()
      }
      if (par2 == 'f(x)') {
        if (!isTRUE(attr(ds, 'contains_full_FV'))) {
          return(NULL)
        }
        dt2 <- ds$FV_raw_mat %>% reshape2::melt()
      } else {
        if (!(par2 %in% get_PAR_name(ds, which = 'by_RT')))
          return(NULL)
        dt2 <- ds$PAR$by_RT[[par2]] %>% reshape2::melt()
      }

      colnames(dt1) <- c('runtime', 'run', par1)
      colnames(dt2) <- c('runtime', 'run', par2)

      dt <- merge(dt1, dt2)
      dt$ID <- get_id(ds)
      dt
    }))
    return(dt)
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
generate_data.Aggr <-
  function(dsList,
           aggr_on = 'funcId',
           targets = NULL,
           which = 'by_RT') {
    maximize <- attr(dsList, 'maximization')
    variable <-
      fid <- value <- NULL #Set local binding to remove warnings
    by_rt <- which == 'by_RT'

    if (is.null(targets)) {
      targets <- get_target_dt(dsList, which)
    }

    aggr_attr <-
      if (aggr_on == 'funcId')
        get_funcId(dsList)
    else
      get_dim(dsList)
    N <- length(get_id(dsList))

    dt <- rbindlist(lapply(aggr_attr, function(agg_val) {
      if (by_rt) {
        if (aggr_on == 'funcId')
          dt <-
            get_RT_summary(subset(dsList, funcId == agg_val), targets[funcId == agg_val][['target']])
        else
          dt <-
            get_RT_summary(subset(dsList, DIM == agg_val), targets[DIM == agg_val][['target']])
        dt[, c('ID', value = 'ERT', 'funcId', 'DIM')]
        setnames(dt, 'ERT', 'value')
      }
      else{
        if (aggr_on == 'funcId')
          dt <-
            get_FV_summary(subset(dsList, funcId == agg_val), targets[funcId == agg_val][['target']])
        else
          dt <-
            get_FV_summary(subset(dsList, DIM == agg_val), targets[DIM == agg_val][['target']])
        dt[, c('ID', value = 'mean', 'funcId', 'DIM')]
        setnames(dt, 'mean', 'value')
      }
    }))

    if (by_rt)
      order_sel <- 1
    else
      order_sel <- -1 * (maximize * 2 - 1)

    dt[, rank := frank(order_sel * value, na.last = T), by = .(DIM, funcId)]
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
#' \dontshow{data.table::setDTthreads(1)}
#' generate_data.ECDF_raw(subset(dsl, funcId == 1), c(10, 15, 16))
generate_data.ECDF_raw <- function(dsList, targets, scale_log = F) {
  V1 <- NULL #Set local binding to remove warnings

  #Get the x-coordinates at which to calculate the ECDF
  RT <- get_runtimes(dsList)
  x <-
    unique(seq_RT(
      RT,
      length.out = 50,
      scale = ifelse(scale_log, 'log', 'linear')
    ))
  #TODO: Some scaling by dimension?

  #Get targets to use
  if (!is.data.table(targets)) {
    if (length(get_funcId(dsList)) > 1 || length(get_dim(dsList)) > 1)
      stop(
        "Targets provided are not in data.table format, while multiple functions / dimensions
           are present in provided DataSetList."
      )
    targets <- data.table(
      target = targets,
      funcId = get_funcId(dsList),
      DIM = get_dim(dsList)
    )
  }

  dt <- as.data.table(rbindlist(lapply(dsList, function(df) {
    ID <- get_id(df)
    temp <- targets[DIM == attr(df, 'DIM'), c('target', 'funcId')]
    targets_ <- temp[funcId == attr(df, 'funcId')][['target']]

    m <- lapply(targets_, function(target) {
      data <- fast_RT_samples(df$RT, target, attr(df, 'maximization'))
      if (all(is.na(data)))
        hit <- (rep(0, length(x)))
      else {
        fun <- ecdf(data)
        hit <- if (is.function(fun))
          fun(x)
        else
          NA
      }
      data.table(
        hit = hit,
        rt = x,
        target = target,
        funcId = attr(df, 'funcId'),
        DIM = attr(df, 'DIM'),
        ID = ID
      )
    }) %>%
      do.call(rbind, .)
    m
  })))
  dt
}

#' Extract the position information from a datasetlist object
#'
#' @param dsList The DataSetList object
#' @param iid the Instance Id from which to get the position history (can be a list)
#'
#' @export
#' @examples
#' get_position_dsl(subset(dsl, funcId == 1), 1)
get_position_dsl <- function(dsList, iid) {
  if (length(get_dim(dsList)) != 1 ||
      length(get_funcId(dsList)) != 1)
    return(NULL)
  dim <- get_dim(dsList)

  dt <- rbindlist(lapply(dsList, function(ds) {
    if (!isTRUE(attr(ds, 'contains_position')))
      return(NULL)
    instance_idxs <- which(attr(ds, 'instance') %in% iid)
    dt_sub <- rbindlist(lapply(instance_idxs, function(idx) {
      temp <- lapply(seq(0, dim - 1), function(x_idx) {
        ds$PAR$by_RT[[paste0('x', x_idx)]][, idx]
      })
      names(temp) <- paste0('x', seq(0, dim - 1))
      if ('generation' %in% get_PAR_name(ds)) {
        temp$generation <- ds$PAR$by_RT[['generation']][, idx]
      }
      dt_subsub <-
        data.table('runtime' = as.integer(rownames(ds$PAR$by_RT$x0)), setDT(temp))
      dt_subsub$FV <- ds$FV[, idx]
      if (length(unique(dt_subsub$runtime)) < max(dt_subsub$runtime)) {
        dt_subsub <- dt_subsub[1:nrow(dt_subsub) - 1, ]
      }
      dt_subsub[, 'run_nr' := idx]
      dt_subsub[, 'iid' := attr(ds, 'instance')[idx]]
      dt_subsub
    }))
    dt_sub[, 'algId' := attr(ds, 'algId')]
    dt_sub
  }))
  if (nrow(dt) == 0)
    return(NULL)
  dt[, runtime := as.numeric(runtime)]
  return(dt)
}

#' Nevergrad-dashboard based algorithm comparison
#'
#' This procedure calculates the fraction of times algorithm A is better than
#' algorithm B according to their mean on each function,dimension,target tuple
#'
#' @param dsList The DataSetList, can contain multiple functions and dimensions, but should have the
#' same algorithms for all of them. For functions/dimensions where this is not the case,
#' all algorithms are considered tied.
#' @param which Whether to use fixed-target ('by_FV') or fixed-budget ('by_RT') perspective
#' @param target_dt Custom data.table target value to use. When NULL, this is selected automatically.
#' @return A matrix containing the pairwise win-ratios.
#'
#' @export
#' @examples
#' generate_data.Heatmaps(dsl)
#' generate_data.Heatmaps(dsl, which = 'by_RT')
generate_data.Heatmaps <-
  function(dsList,
           which = 'by_FV',
           target_dt = NULL) {
    req(length(get_id(dsList)) > 1)

    if (!is.null(target_dt) &&
        !('data.table' %in% class(target_dt))) {
      warning("Provided `target_dt` argument is not a data.table")
      target_dt <- NULL
    }

    if (is.null(target_dt))
      target_dt <- get_target_dt(dsList, which)

    if (!(which %in% c('by_FV', 'by_RT')))
      stop("Invalid argument: 'which' can only be 'by_FV' or 'by_RT'")

    n_ids <- length(get_id(dsList))
    is_max <- attr(dsList, 'maximization')

    res <- lapply(get_funcId(dsList), function(fid) {
      res <- lapply(get_dim(dsList), function(dim) {
        ds_sub <- subset(dsList, funcId == fid & DIM == dim)
        targets <- target_dt[DIM == dim & funcId == fid, 'target']

        mat_temp <- lapply(targets, function(target) {
          if (which == 'by_FV')
            dt <- get_RT_summary(ds_sub, target)
          else
            dt <- get_FV_summary(ds_sub, target)
          temp <- as.numeric(as.matrix(dt[, 'mean']))
          if (which == 'by_RT' & is_max) {
            #Maximization of FV
            temp[is.na(temp)] <- -Inf
            mat <- outer(temp, temp, FUN = "-")
            winrates <- (mat > 0) + 0.5 * (mat == 0)
          }
          else {
            #Minimization (FV or RT)
            temp[is.na(temp)] <- Inf
            mat <- outer(temp, temp, FUN = "-")
            winrates <- (mat < 0) + 0.5 * (mat == 0)
          }
          if (nrow(winrates) != n_ids) {
            warning(
              "The number of algorithm present in a subset is not equal to the overall number, skipping subset"
            )
            return(matrix(0.5, n_ids, n_ids))
          }
          winrates
        })
        res_mat <- simplify2array(mat_temp)
        res_dt <- as.data.frame(apply(res_mat, c(1, 2), mean))
        res_dt[is.na(res_dt)] <- 0.5
        res_dt

      })
    })
    winrates_aggr <-
      array(unlist(res), dim = c(n_ids, n_ids, length(res)))
    winrates_aggr <- apply(winrates_aggr, c(1, 2), mean)
    rownames(winrates_aggr) <- get_id(dsList)
    colnames(winrates_aggr) <- get_id(dsList)
    winrates_aggr
  }

#' Generate data for the cumulative difference plot.
#'
#' This function generates a dataframe that can be used to generate
#' the `cumulative_difference_plot`.
#'
#' @param dsList The DataSetList object.
#' Note that the `cumulative_difference_plot` can only compare two algorithms
#' in a single problem of dimension one.
#' @param runtime_or_target_value The target runtime or the target value
#' @param isFixedBudget Should be TRUE when target runtime is used. False otherwise.
#' @param alpha 1 minus the confidence level of the confidence band.
#' @param EPSILON If abs(x-y) < EPSILON, then we assume that x = y.
#' @param nOfBootstrapSamples The number of bootstrap samples used in the estimation.
#' @return A dataframe with the data to generate the cumulative difference plot.
#'
#' @export
#' @examples
#'
#' dsl_sub <- subset(dsl, funcId == 1)
#' generate_data.CDP(dsl_sub, 15, TRUE, nOfBootstrapSamples = 10)
generate_data.CDP <-
  function(dsList,
           runtime_or_target_value,
           isFixedBudget,
           alpha = 0.05,
           EPSILON = 1e-80,
           nOfBootstrapSamples = 1e3)
  {
    if (!requireNamespace("RVCompare", quietly = TRUE)) {
      stop("Package \"RVCompare\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    if (length(get_dim(dsList)) != 1 ||
        length(get_funcId(dsList)) != 1)
      return(NULL)
    if (length(get_id(dsList)) != 2)
      return(NULL)

    if (isFixedBudget)
    {
      subds <-
        get_FV_sample(dsList, runtime_or_target_value, output = 'long')

      algorithms <- unique(subds$ID)
      X_A <- subds[subds$ID == algorithms[1]]$`f(x)`
      X_B <- subds[subds$ID == algorithms[2]]$`f(x)`
    }
    else
    {
      subds <-
        get_RT_sample(dsList, runtime_or_target_value, output = 'long')

      algorithms <- unique(subds$ID)
      X_A <- subds[subds$ID == algorithms[1]]$`RT`
      X_B <- subds[subds$ID == algorithms[2]]$`RT`
    }

    res <- data.frame(X_A, X_B)
    colnames(res) <- algorithms


    if (isFixedBudget && attr(dsList, 'maximization'))
    {
      X_A <- -X_A
      X_B <- -X_B
    }

    res <- RVCompare::get_Y_AB_bounds_bootstrap(
      X_A,
      X_B,
      ignoreMinimumLengthCheck = TRUE,
      alpha = alpha,
      EPSILON = EPSILON,
      nOfBootstrapSamples =
        nOfBootstrapSamples
    )

    return(data.frame(res))
  }


#' Generate dataframe consisting of the levelsets of the EAF
#'
#' This function generates a dataframe which can be easily plotted using the `plot_eaf_data`-function
#'
#' @param dsList The DataSetList object
#' @param n_sets The number of level sets to calculate
#' @param subsampling Level of subsampling to use for runtime-values (number of runtimes to consider).
#'  Setting to 0 will make the calculations more precise at the cost of potentially much longer exectution times
#' @param scale_xlog Only has effect when `subsampling` is True. The scaling of the subsampled runtimes
#' When true, these are equally spaced in log-space, when false they are linearly spaced.
#' @param xmin Minimum runtime value
#' @param xmax Maximum runtime value
#'
#' @export
#' @examples
#' generate_data.EAF(subset(dsl, funcId == 1))
generate_data.EAF <-
  function(dsList,
           n_sets = 11,
           subsampling = 100,
           scale_xlog = F,
           xmin = "",
           xmax = "") {
    V1 <- NULL #Set local binding to remove warnings

    if (!requireNamespace("eaf", quietly = TRUE)) {
      stop("Package \"eaf\" needed for this function to work. Please install it.",
           call. = FALSE)
    }

    # if (length(get_id(dsList)) != 1 ) {
    #   stop("Multiple IDs are present in provided DataSetList.
    #   Please call this function for each individual ID instead.")
    # }
    ids <- get_id(dsList)
    runtimes <- get_runtimes(dsList)
    xmin <-
      ifelse(xmin == "", min(runtimes, na.rm = T), as.numeric(xmin))
    xmax <-
      ifelse(xmax == "", max(runtimes, na.rm = T), as.numeric(xmax))

    if (subsampling > 0) {
      runtimes <-
        seq_RT(
          c(xmin, xmax),
          from = xmin,
          to = xmax,
          length.out = subsampling,
          scale = ifelse(scale_xlog, 'log', 'linear')
        )
    } else {
      runtimes <- runtimes[runtimes > xmin]
      runtimes <- runtimes[runtimes < xmax]
    }

    temp <- lapply(ids, function(id) {
      dsl <- subset(dsList, ID == id)


      dt <- get_FV_sample(dsl, runtimes, output = 'long')
      max_runs <- max(dt$run)
      dt_temp <-
        dt[!is.na(`f(x)`), .(temp = max_runs * funcId + run, `f(x)`, `runtime`)]

      if (attr(dsl, 'maximization')) {
        quals <- dt_temp[, .(`runtime`, `f(x)` = -1 * `f(x)`)]
      } else {
        quals <- dt_temp[, c('runtime', 'f(x)')]
      }
      runs <- dt_temp$temp
      eaf <-
        eafs(quals, runs, percentiles = seq(0, 100, length.out = n_sets))
      if (attr(dsl, 'maximization')) {
        eaf[, 2] = eaf[, 2] * -1
      }
      eaf_table <- as.data.table(eaf)
      colnames(eaf_table) <- c('runtime', 'f(x)', 'percentage')
      eaf_table$ID <- id
      return(eaf_table)
    })
    dt <- rbindlist(temp)
    return(dt)
  }

#' Generate dataframe consisting of the ECDF-equivalent based on the EAF
#'
#' This function uses EAF-data to calculate a target-independent version of the ECDF
#'
#' @param eaf_table Datatable resulting from the `generate_data.EAF` function
#' @param min_val Minimum value to use for y-space
#' @param max_val Maximum value to use for y-space
#' @param maximization Whether the data resulted from maximization or not
#' @param scale_log Whether to use logarithmic scaling in y-space before calculating the partial integral
#' @param normalize Whether to normalize the resulting integrals to [0,1] (Based on `min_val` and `max_va`)
#' @export
#' @examples
#' \dontshow{data.table::setDTthreads(1)}
#' generate_data.ECDF_From_EAF(generate_data.EAF(subset(dsl, funcId == 1)), 1, 16, maximization = TRUE)
generate_data.ECDF_From_EAF <-
  function(eaf_table,
           min_val,
           max_val,
           maximization = F,
           scale_log = F,
           normalize = T) {
    runtimes <- sort(unique(eaf_table[, `runtime`]))

    ext_func <- ifelse(maximization, max, min)

    fvals <-  sort(unique(eaf_table[, `f(x)`]))
    min_val <-
      ifelse(min_val == "", min(fvals, na.rm = T), as.numeric(min_val))
    max_val <-
      ifelse(max_val == "", max(fvals, na.rm = T), as.numeric(max_val))

    if (scale_log) {
      min_val <- max(min_val, 1e-12)
      max_val <- max(max_val, 1e-12)
      eaf_table <-
        eaf_table[, .(`runtime`, `f(x)` = log10(pmax(min_val, pmin(`f(x)`, max_val))), `percentage`, `ID`)]
      min_val <- log10(min_val)
      max_val <- log10(max_val)
    }

    ecdf_full <-
      rbindlist(lapply(unlist(unique(eaf_table[, 'ID'])), function(id) {
        ecdf <- rbindlist(lapply(runtimes, function(runtime_value) {
          temp <-
            eaf_table[runtime <= runtime_value , .(agg_fval = ext_func(`f(x)`)), by = c('percentage')]
          agg_vals <- pmax(pmin(temp$agg_fval, max_val), min_val)

          if (maximization) {
            partials <-
              rev(c(rev(agg_vals), max_val) - c(min_val, rev(agg_vals)))
          } else {
            partials <- c(agg_vals, max_val) - c(min_val, agg_vals)
          }
          list(runtime_value, sum(partials * c(0, temp$perc / 100)))
        }))
        if (normalize) {
          ecdf[, 2] = ecdf[, 2] / (max_val - min_val)
        }
        ecdf <- as.data.table(ecdf)
        colnames(ecdf) <- c('x', 'mean')
        ecdf$ID <- id
        ecdf
      }))
    return(ecdf_full)
  }


#' Generate differences between two EAFs
#'
#' This function uses the 'eaf' package to calculate eaf differences
#'
#' @param dsList1 The first DataSetList object
#' @param dsList2 The second DataSetList object
#' @export
#' @examples
#' generate_data.EAF_Difference(dsl[1], dsl[3])
generate_data.EAF_Difference <- function(dsList1, dsList2) {
  dt <- get_FV_sample(dsList1, get_runtimes(dsList1), output = 'long')
  max_runs <- max(dt$run)
  dt_temp = dt[, .(temp = max_runs * funcId + run, `f(x)`, `runtime`)]

  quals <- dt_temp[, c('runtime', 'f(x)')]
  runs <- dt_temp$temp - min(dt_temp$temp) + 1 #need to start at 1
  x <- cbind(quals, runs)
  x <- x[!is.na(`f(x)`), .(`runtime`, `f(x)`, `runs`)]

  dt <- get_FV_sample(dsList2, get_runtimes(dsList2), output = 'long')
  max_runs <- max(dt$run)
  dt_temp = dt[, .(temp = max_runs * funcId + run, `f(x)`, `runtime`)]

  quals <- dt_temp[, c('runtime', 'f(x)')]
  runs <- dt_temp$temp - min(dt_temp$temp) + 1 #need to start at 1
  y <- cbind(quals, runs)
  y <- y[!is.na(`f(x)`), .(`runtime`, `f(x)`, `runs`)]

  diff <- eafdiff(x, y, rectangles = T)

  return(diff)
}


#' Generate EAF-differences between each function and the remaining portfolio
#'
#' This is an approximation of ``, since the number of required polygons
#' can quickly become problematic for plotly. This function uses discretized
#' contour matrices instead, which trades off accuracy for scalability.
#'
#' @param dsList The DataSetList object, containing at least 2 IDs
#' @param xmin Minimum runtime to consider
#' @param xmax Maximum runtime to consider
#' @param ymin Minimum f(x) to consider
#' @param ymax Maximum f(x) to consider
#' @param x.log Whether to scale the y-space logarithmically
#' @param y.log Whether to scale the y-space logarithmically
#' @export
#' @examples
#' generate_data.EAF_diff_Approximate(subset(dsl, funcId == 1), 1, 16, 1, 16)
generate_data.EAF_diff_Approximate <-
  function(dsList,
           xmin,
           xmax,
           ymin,
           ymax,
           x.log = T,
           y.log = T) {
    RT <- get_runtimes(dsList)
    xmin <- max(as.numeric(xmin), min(RT))
    xmax <- min(as.numeric(xmax), max(RT))

    x <- unique(seq_RT(c(xmin, xmax), length.out = 50, scale = 'log'))

    FV <- get_funvals(dsList)
    ymin <- max(as.numeric(ymin), min(FV))
    ymax <- min(as.numeric(ymax), max(FV))
    y <-
      rev(unique(seq_FV(
        c(ymin, ymax), length.out = 50, scale = 'log'
      )))

    algs <- get_algId(dsList)

    mats <- lapply(algs, function(alg) {
      ds <- subset(dsList, algId == alg)
      do.call(rbind, lapply(x, function(xval) {
        unlist(get_RT_summary(ds, y, xval)$ps)
      }))
    })

    names(mats) <- algs
    fv_sum = get_FV_summary(dsList, x, include_limits = T)

    matrices_diffs <- lapply(algs, function(alg) {
      mat_max <- Reduce(pmax, mats[algs[algs != alg]])
      diff <- mats[alg][[1]] - mat_max
      # diff[diff<0] <- 0
      rownames(diff) <- x
      colnames(diff) <- y
      return(t(diff))
    })

    names(matrices_diffs) <- algs
    return(matrices_diffs)
  }
