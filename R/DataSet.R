#' Constructor of S3 class 'DataSet'
#'
#' DataSet contains the following attributes
#'  * funId
#'  * DIM
#'  * algId
#'  * datafile
#'  * instance
#'  * maxEvals
#'  * finalFunEvals
#'  * comment
#'  * Additional attributes based on the original format
#'
#' @param info A List. Contains a set of in a *.info file.
#' @param verbose Logical.
#' @param maximization Logical. Whether the underlying optimization algorithm performs a maximization?
#' Set to NULL to determine automatically based on format
#' @param format A character. The format of data source, either 'IOHProfiler', 'COCO' or 'TWO_COL"
#' @param subsampling Logical. Whether *.cdat files are subsampled?
#' @param full_sampling Logical. Whether the raw (unaligned) FV matrix should be stored.
#' Currenlt only useful when a correlation plot between function values and parameters should be made
#'
#' @return A S3 object 'DataSet'
#' @export
#' @examples
#' path <- system.file('extdata', 'ONE_PLUS_LAMDA_EA', package = 'IOHanalyzer')
#' info <- read_index_file(file.path(path, 'IOHprofiler_f1_i1.info'))
#' DataSet(info[[1]])
DataSet <-
  function(info,
           verbose = F,
           maximization = NULL,
           format = IOHprofiler,
           subsampling = FALSE,
           full_sampling = FALSE) {
    if (!is.null(info)) {
      path <- dirname(info$datafile)
      suite <- toupper(info$suite)

      # for an unknown suite, to detect the format
      if (is.null(suite) || length(suite) == 0) {
        if (verbose)
          warning(
            "Suite-name not provided in .info-file, taking best guess based on
                the format of data-files."
          )
        suite <- switch(
          format,
          IOHprofiler = "Unknown",
          COCO = "BBOB",
          BIOBJ_COCO = "biobj-bbob",
          TWO_COL = "Unknown"
        )
      }

      if (is.null(maximization) || maximization == AUTOMATIC) {
        #TODO: Better way to deal with capitalization of attributes
        if (!is.null(info$maximization))
          maximization <- info$maximization
        else if (!is.null(info$Maximization))
          maximization <- info$Maximization
        else if (!is.null(suite)) {
          if (verbose)
            warning(
              "maximization or minimization not specified in .info-file,
                    taking best guess based on the suite-name."
            )
          if (grepl("\\w*bbob\\w*", suite, ignore.case = T) != 0)
            maximization <- FALSE
          else
            maximization <- TRUE
        }
        else {
          warning(
            "Can't detect maximization based on suite-attribute, setting to
                minimization by default"
          )
          maximization <- FALSE # default to minimization
        }
      }

      if (!(isTRUE(maximization) || isFALSE(maximization)))
        warning("unclear whether we should maximize or minimize.")

      datBaseName <-
        sub(pattern = '(.*)\\..*$',
            replacement = '\\1',
            basename(info$datafile))
      datFile <- file.path(path, paste0(datBaseName, '.dat'))
      tdatFile <- file.path(path, paste0(datBaseName, '.tdat'))
      cdatFile <- file.path(path, paste0(datBaseName, '.cdat'))

      # NOTE: preference on data file from coco: dat > tdat > cdat
      if (file.exists(datFile))
        rtFile <- datFile
      else if (file.exists(tdatFile))
        rtFile <- tdatFile
      else if (file.exists(cdatFile))
        # TODO: perhaps turn on `subsampling` here as this would take quite some time
        rtFile <- cdatFile
      else
        stop('No datafiles found, please verify the integrity of the chosen files')

      read_raw <- switch(
        format,
        IOHprofiler = read_dat,
        COCO = read_dat__COCO,
        BIOBJ_COCO = read_dat__BIOBJ_COCO,
        TWO_COL = read_dat  # TODO: perhaps rename `TWO_COL` or to use a better naming
        # scheme for all format names
      )

      RT_raw <- read_raw(rtFile, subsampling)

      if (is.null(maximization)) {
        if (verbose)
          warning(
            "Did not find maximization / minimization, auto-detecting based on
                function value progression"
          )
        # TODO: idxTarget should be set depending on the data format
        idxTarget <- 2
        cond <-
          unique(lapply(RT_raw, function(FV)
            FV[1, idxTarget] >= FV[nrow(FV), idxTarget]))
        if (length(cond) > 1)
          stop('The detected maximization differs in multiple runs')
        maximization <- cond
      }

      RT <-
        align_running_time(RT_raw, format = format, maximization = maximization)
      FV <- align_function_value(RT_raw, format = format)

      PAR <- list('by_FV' = RT[names(RT) != 'RT'],
                  'by_RT' = FV[names(FV) != 'FV'])

      RT <- RT$RT
      mode(RT) <- 'integer'
      FV <- FV$FV

      if (format %in% c(IOHprofiler)) {
        # try to save some memory here...
        FV <- tryCatch({
          .FV <- FV
          mode(.FV) <- 'integer'
          if (all(FV == .FV))
            .FV
          else
            FV
        },
        warning = function(w)
          FV) # in case the type coercion gives a warning
      }

      # TODO: add more data sanity checks
      maxRT <-
        set_names(sapply(RT_raw, function(d)
          d[nrow(d), idxEvals]), NULL)
      # Fix for old-format files which do not store used runtime in .dat-files
      maxRT <- pmax(maxRT, info$maxRT)
      if (any(maxRT != info$maxRT) && verbose)
        warning('Inconsitent maxRT in *.info file and *.cdat file')

      # TODO: clean up these if-statements: Function to set idxTarget and n_data_column?
      # `idxTarget` is a global variable?
      if (format == TWO_COL)
        finalFV <-
        set_names(sapply(RT_raw, function(d)
          d[nrow(d), idxTarget - 1]), NULL)
      else
        finalFV <-
        set_names(sapply(RT_raw, function(d)
          d[nrow(d), idxTarget]), NULL)

      if (any(finalFV != info$finalFV) && verbose)
        warning('Inconsitent finalFvalue in *.info file and *.dat file')

      if (length(info$instance) != length(RT_raw)) {
        if (verbose)
          warning('The number of instances found in the info is inconsistent with the data!')
        info$instance <- seq(length(RT_raw))
      }

      temp <- do.call(function(...)
        structure(list(
          RT = RT, FV = FV, PAR = PAR
        ), class = c('DataSet', 'list'), ...),
        c(
          info,
          list(
            maxRT = maxRT,
            finalFV = finalFV,
            format = format,
            maximization = maximization,
            suite = suite,
            ID = info$algId
          )
        ))
      if (isTRUE(info$constrained) || full_sampling) {
        FV_raw_mat <- matrix(nrow = nrow(FV), ncol = length(RT_raw))
        for (idx in seq(length(RT_raw))) {
          FV_raw_mat[, idx] = RT_raw[[idx]][, 2]
        }
        temp$FV_raw_mat <- FV_raw_mat
        attr(temp, 'contains_full_FV') <- TRUE
      }
      else {
        attr(temp, 'contains_full_FV') <- FALSE
      }
      if (getOption('IOHanalyzer.function_representation', 'funcId') == 'funcName') {
        attr(temp, 'funcId') <- attr(temp, 'funcName')
      }
      return(temp)
    }
    else
      structure(list(), class = c('DataSet', 'list'))
  }

#' S3 concatenation function for DataSet
#'
#' @description Concatenation for DataSets. Combines multiple runs from separate DataSets
#' into a single DataSet object if all provided arguments have the same dimension, function ID and
#' algorithm ID, and each contains only a single run. Currently does not support parameter tracking
#'
#' @param ... The DataSets to concatenate
#' @return A new DataSet
#' @export
#' @examples
#' c(dsl[[1]], dsl[[1]])
c.DataSet <- function(...) {
  dsl <- list(...)

  if (length(dsl) == 1)
    dsl <- dsl[[1]]
  dsl <- dsl[sapply(dsl, length) != 0]

  if (length(dsl) == 0)
    return()
  if (length(dsl) == 1)
    return(dsl[[1]])

  for (ds in dsl) {
    if (!any((class(ds)) == 'DataSet'))
      stop("Operation only possible when all arguments are DataSets")
  }

  fixed_attrs <-
    c('suite', 'maximization', 'DIM', 'funcId', 'algId', 'format')
  info <- list()
  for (attr_str in fixed_attrs) {
    temp  <-  unique(unlist(lapply(dsl, function(x)
      attr(x, attr_str))))
    if (length(temp) > 1) {
      stop(
        paste0(
          "Attempted to add datasets with different ",
          attr_str,
          "-attributes! This is not supported, please keep them as separate DataSets!"
        )
      )
    }
    info <- c(info, temp)
  }
  names(info) <- fixed_attrs

  #Record number of runs to make masks of static attributes
  nr_runs <- sapply(dsl, function(x)
    ncol(x$FV))
  for (attr_str in names(attributes(dsl[[1]]))) {
    if (attr_str %in% fixed_attrs ||
        attr_str %in% c("names", "class"))
      next
    temp  <- unlist(lapply(dsl, function(x)
      attr(x, attr_str)))
    if (length(unique(temp)) == 1)
      temp <- unique(temp)
    else {
      if (length(temp) == length(nr_runs))
        temp <- list(temp_name = rep(temp, nr_runs))
      else
        temp <- list(temp_name = temp)
    }
    names(temp) <- attr_str
    info <- c(info, temp)
  }

  format <- info[['format']] #attr(dsl[[1]], "format")

  RT_raw <- unlist(lapply(dsl, function(ds) {
    lapply(seq_len(ncol(ds$RT)), function(cnr) {
      rt_temp <- as.matrix(ds$RT[, cnr])
      cbind(rt_temp, as.numeric(rownames(ds$RT)))
    })
  }), recursive = F)

  RT <-
    align_running_time(RT_raw,
                       format = "TWO_COL",
                       maximization = info$maximization)$RT
  FV_mat <-
    as.matrix(align_function_value(RT_raw, format = "TWO_COL")$FV)

  if (info$maximization) {
    FV_mat <-
      apply(FV_mat, 2, function(x) {
        x[is.na(x)] <- min(x, na.rm = T)
        x
      })
    FV <-
      do.call(cbind, lapply(seq(ncol(FV_mat)), function(x)
        cummax(FV_mat[, x])))
  }
  else {
    FV_mat <-
      apply(FV_mat, 2, function(x) {
        x[is.na(x)] <- max(x, na.rm = T)
        x
      })
    FV <-
      do.call(cbind, lapply(seq(ncol(FV_mat)), function(x)
        cummin(FV_mat[, x])))
  }

  # TODO: to deal with cases where aligned parameters are present in original DataSets
  PAR <- list('by_FV' = RT[names(RT) != 'RT'],
              'by_RT' = FV[names(FV) != 'FV'])

  # Unaligned parameters
  for (par_name in names(dsl[[1]]$PAR)) {
    if (!par_name %in% c('by_FV', 'by_RT'))
      PAR[[par_name]] <-
        unlist(lapply(dsl, function(x) {
          x$PAR[[par_name]]
        }), recursive = F)
  }

  do.call(function(...)
    structure(list(
      RT = RT, FV = FV, PAR = PAR
    ), class = c('DataSet', 'list'), ...),
    c(info))
}

#' S3 subset function for DataSet
#'
#' @description Subset for DataSets. Based on the provided mask, the relevant data is taken from the given DataSet
#' and turned into a new DataSet object.
#'
#' @param x The DataSet from which to get a subset
#' @param mask The mask (as boolean list) to use when subsetting. The length should be equal to the number of runs
#'  present in the provided dataset object x.
#' @param ... Arguments passed to underlying subset method (not yet supported)
#'
#' @return A new DataSet
#' @export
#' @examples
#' subset(dsl[[1]], c(0,1,1,1,0,0,0,0,0,0,0))
subset.DataSet <- function(x, mask, ...) {
  if (length(mask) != ncol(x$FV))
    stop(
      paste(
        "The input DataSet has",
        ncol(x$FV),
        "runs while the input mask array has length",
        length(mask)
      )
    )

  info <- list()
  for (attr_str in names(attributes(x))) {
    if (attr_str %in% c('names', 'class'))
      next
    temp  <- attr(x, attr_str)
    if (length(unique(temp)) == 1)
      temp <- unique(temp)
    else {
      if (length(temp) == length(mask))
        temp <- list(temp[mask])
      else{
        warning(
          paste0(
            "Attribute detected (",
            attr_str,
            ") with incorrect length for the mask-based subsetting!"
          )
        )
        next
      }
    }
    names(temp) <- attr_str
    info <- c(info, temp)
  }

  format <- info[['format']]

  RT <- as.matrix(x$RT[, mask])
  FV <- as.matrix(x$FV[, mask])

  PAR <- list(
    'by_FV' = ifelse(ncol(x$PAR$by_FV) == length(mask), x$PAR$by_FV[, mask], NULL),
    'by_RT' = ifelse(ncol(x$PAR$by_RT) == length(mask), x$PAR$by_RT[, mask], NULL)
  )

  do.call(function(...)
    structure(list(
      RT = RT, FV = FV, PAR = PAR
    ), class = c('DataSet', 'list'), ...),
    c(info))
}

#' S3 generic print operator for DataSet
#'
#' @param x A DataSet object
#' @param ... Arguments passed to other methods
#'
#' @return A short description of the DataSet
#' @examples
#' print(dsl[[1]])
#' @export
print.DataSet <- function(x, ...) {
  cat(as.character.DataSet(x, ...))
}

#' S3 generic cat operator for DataSet
#'
#' @param x A DataSet object
#'
#' @return A short description of the DataSet
#' @export
#' @examples
#' cat.DataSet(dsl[[1]])
cat.DataSet <- function(x)
  cat(as.character(x))

#' S3 generic as.character operator for DataSet
#'
#' @param x A DataSet object
#' @param verbose Verbose mode, currently not implemented
#' @param ... Arguments passed to other methods
#'
#' @return A short description of the DataSet
#' @export
#' @examples
#' as.character(dsl[[1]])
as.character.DataSet <- function(x, verbose = F, ...) {
  # TODO: implement the verbose mode
  sprintf('DataSet(%s on f%s %dD)',
          attr(x, 'algId'),
          attr(x, 'funcId'),
          attr(x, 'DIM'))
}

#' S3 generic summary operator for DataSet
#'
#' @param object A DataSet object
#' @param ... Arguments passed to other methods
#'
#' @return A summary of the DataSet containing both function-value and runtime based statistics.
#' @examples
#' summary(dsl[[1]])
#' @export
summary.DataSet <- function(object, ...) {
  ds_attr <- attributes(object)
  cat('DataSet Object:\n')
  cat(sprintf('Source: %s\n', ds_attr$src))
  cat(sprintf('Algorithm: %s\n', ds_attr$algId))
  cat(sprintf('Function ID: %s\n', ds_attr$funcId))
  cat(sprintf('Dimension: %dD\n', ds_attr$DIM))

  n_instance <- length(ds_attr$instance)
  if (n_instance >= 15) {
    inst <- paste0(
      paste(ds_attr$instance[1:7], collapse = ','),
      ',...,',
      paste(ds_attr$instance[(n_instance - 7):n_instance], collapse = ',')
    )
    cat(sprintf('%d instance found: %s\n\n', n_instance, inst))
  }
  else
    cat(sprintf(
      '%d instance found: %s\n\n',
      n_instance,
      paste(ds_attr$instance, collapse = ',')
    ))

  cat('runtime summary:\n')
  function_values <- as.numeric(rownames(object$RT))
  RT.summary <- get_RT_summary(object, function_values)
  print(RT.summary)
  cat('\n')

  cat('function value summary:\n')
  runtimes <- as.numeric(rownames(object$FV))
  if (length(runtimes) > 100) {
    runtimes <- runtimes[seq(1, length(runtimes), length.out = 100)]
  }

  FV.summary <- get_FV_summary(object, runtimes)
  print(FV.summary)
  cat('\n')

  cat(paste('Attributes:', paste0(names(ds_attr), collapse = ', ')))
}

#' S3 generic == operator for DataSets
#'
#' @param dsL A `DataSet` object
#' @param dsR A `DataSet` object
#'
#'
#' @return True if the DataSets contain the same function, dimension and algorithm,
#' and have the exact same attributes
#' @examples
#' dsl[[1]] == dsl[[2]]
#' @export
`==.DataSet` <- function(dsL, dsR) {
  if (length(dsL) == 0 || length(dsR) == 0)
    return(FALSE)

  for (attr_str in names(attributes(dsL))) {
    if (any(attr(dsL, attr_str) != attr(dsR, attr_str)))
      return(FALSE)
  }
  return(TRUE)
}

#' Get Expected RunTime
#'
#' @param ds A DataSet or DataSetList object
#' @param budget Optional; overwrites the budget found in ds for ERT-calculation
#' @param ... Arguments passed to other methods
#' @param ftarget The function target(s) for which to get the ERT
#'
#' @return A data.table containing the runtime samples for each provided target
#' function value
#' @examples
#' get_ERT(dsl, 14)
#' get_ERT(dsl[[1]], 14)
#' @export
#'
get_ERT <-
  function(ds, ftarget, budget, ...)
    UseMethod("get_ERT", ds)

#' Get RunTime Sample
#'
#' @param ds A DataSet or DataSetList object
#' @param ftarget A Numerical vector. Function values at which runtime values are consumed
#' @param ... Arguments passed to other methods
#'
#'
#' @return A data.table containing the runtime samples for each provided target
#' function value
#' @examples
#' get_RT_sample(dsl, 14)
#' get_RT_sample(dsl[[1]], 14)
#' @export
get_RT_sample <-
  function(ds, ftarget, ...)
    UseMethod("get_RT_sample", ds)

#' Get RunTime Summary
#'
#' @param ds A DataSet or DataSetList object
#' @param budget Optional; overwrites the budget found in ds for ERT-calculation
#' @param ... Arguments passed to other methods
#' @param ftarget The function target(s) for which to get the runtime summary
#'
#' @return A data.table containing the runtime statistics for each provided target
#' function value
#' @examples
#' get_RT_summary(dsl, 14)
#' get_RT_summary(dsl[[1]], 14)
#' @export
get_RT_summary <-
  function(ds, ftarget, budget, ...)
    UseMethod("get_RT_summary", ds)

#' Get Funtion Value Samples
#'
#' @param ds A DataSet or DataSetList object
#' @param runtime A Numerical vector. Runtimes at which function values are reached
#' @param ... Arguments passed to other methods
#'
#' @return A data.table containing the function value samples for each provided
#' target runtime
#' @examples
#' get_FV_sample(dsl, 100)
#' get_FV_sample(dsl[[1]], 100)
#' @export
get_FV_sample <- function(ds, ...)
  UseMethod("get_FV_sample", ds)

#' Get Function Value Summary
#'
#' @param ds A DataSet or DataSetList object
#' @param runtime A Numerical vector. Runtimes at which function values are reached
#' @param include_geom_mean Boolean to indicate whether to include the geometric mean.
#' Only works in fixed_budget mode. Negative values cause NaN, zeros cause output to be completely 0. Defaults to False.
#' @param ... Arguments passed to other methods
#'
#' @return A data.table containing the function value statistics for each provided
#' target runtime value
#' @examples
#' get_FV_summary(dsl, 100)
#' get_FV_summary(dsl[[1]], 100)
#' @export
get_FV_summary <- function(ds, ...)
  UseMethod("get_FV_summary", ds)

#' Get Parameter Value Samples
#'
#' @param ds A DataSet or DataSetList object
#' @param idxValue A Numerical vector. Index values at which parameter values are observed.
#' The index value can either take its value in the range of running times, or function values.
#' Such a value type is signified by `which` parameter.
#' @param ... Arguments passed to other methods
#'
#' @return A data.table object containing parameter values aligned at each given target value
#' @examples
#' get_PAR_sample(dsl, 14)
#' get_PAR_sample(dsl[[1]], 14)
#' @export
get_PAR_sample <-
  function(ds, idxValue, ...)
    UseMethod("get_PAR_sample", ds)

#' Get Parameter Value Summary
#'
#' @param ds A DataSet or DataSetList object
#' @param idxValue A Numerical vector. Index values at which parameter values are observed.
#' The index value can either take its value in the range of running times, or function values.
#' Such a value type is signified by `which` parameter.
#' @param ... Arguments passed to other methods
#'
#' @return A data.table object containing basic statistics of parameter values aligned at each given target value
#' @examples
#' get_PAR_summary(dsl, 14)
#' get_PAR_summary(dsl[[1]], 14)
#' @export
get_PAR_summary <-
  function(ds, idxValue, ...)
    UseMethod("get_PAR_summary", ds)

#' Get the parameter names of the algorithm
#'
#' @param ds A DataSet object
#' @param which a string takes it value in `c('by_FV', 'by_RT')`, indicating the
#' parameters aligned against the running time (RT) or function value (FV). `'by_FV'`
#' is the default value.
#' @return a character list of paramter names, if recorded in the data set
#' @examples
#' get_PAR_name(dsl[[1]])
#' @export
get_PAR_name <- function(ds, which)
  UseMethod("get_PAR_name", ds)

#' Get Function Value condensed overview
#'
#' @param ds A `DataSet` or `DataSetList` object
#' @param ... Arguments passed to other methods
#'
#' @return A data.table containing the algorithm ID, best, worst and mean reached function
#' values, the number of runs and available budget for the DataSet
#' @examples
#' get_FV_overview(dsl)
#' get_FV_overview(dsl[[1]])
#' get_FV_overview(dsl, algorithm = '(1+1)_greedy_hill_climber_1')
#' @export
get_FV_overview <-
  function(ds, ...)
    UseMethod("get_FV_overview", ds)

#' Get Runtime Value condensed overview
#'
#' @param ds A DataSet or DataSetList object
#' @param ... Arguments passed to other methods
#'
#' @return A data.table containing the algorithm ID, minimum and maximum used evaluations,
#' number of runs and available budget for the DataSet
#' @examples
#' get_RT_overview(dsl)
#' get_RT_overview(dsl[[1]])
#' @export
get_RT_overview <-
  function(ds, ...)
    UseMethod("get_RT_overview", ds)

#' Get condensed overview of datasets
#'
#' @param ds A DataSet or DataSetList object
#' @param ... Arguments passed to other methods
#'
#' @return A data.table containing some basic information about the provided DataSet(List)
#' @examples
#' get_overview(dsl)
#' get_overview(dsl[[1]])
#' @export
get_overview <- function(ds, ...)
  UseMethod("get_overview", ds)

#' Get condensed overview of datasets
#'
#' Get the unique identifiers for each DataSet in the provided DataSetList
#'
#' If no unique identifier is set (using `change_id` or done in DataSet construction from 1.6.0 onwards),
#' this function falls back on returning the algorith id (from `get_aldId`)to ensure backwards compatibility
#'
#' @param ds The DataSetList
#' @param ... Arguments passed to other methods
#'
#' @return The list of unique identiefiers present in dsl
#' @examples
#' get_id(dsl)
#' get_id(dsl[[1]])
#' @export
get_id <- function(ds, ...)
  UseMethod("get_id", ds)

#' Get function value matrix of the used dataset.
#'
#' To be used instead of accessing ds$FV directly, since in the case of constrained
#' problems, the violation handling should be applied before using the function values
#' Constraint penalty function should be set in global options, as IOHanalyzer.Violation_Function
#'
#'
#' @param ds The DataSet
#' @param ... Arguments passed to other methods
#'
#' @return The matrix of FV values in the dataset, penalized if applicable.
#' @examples
#' get_FV(dsl[[1]])
#' @export
get_FV <- function(ds, ...)
  UseMethod("get_FV", ds)

#' Get runtime matrix of the used dataset.
#'
#' To be used instead of accessing ds$RT directly, since in the case of constrained
#' problems, the violation handling should be applied before using the function values
#' Constraint penalty function should be set in global options, as IOHanalyzer.Violation_Function
#'
#'
#' @param ds The DataSet
#' @param ... Arguments passed to other methods
#'
#' @return The matrix of FV values in the dataset, penalized if applicable.
#' @examples
#' get_RT(dsl[[1]])
#' @export
get_RT <- function(ds, ...)
  UseMethod("get_RT", ds)


#' @rdname get_FV_overview
#' @export
get_FV_overview.DataSet <- function(ds, ...) {
  data <- get_FV(ds)
  runs <- ncol(data)
  last_row <- data[nrow(data),]
  budget <- max(attr(ds, 'maxRT'))
  maximization <- attr(ds, 'maximization')

  op <- ifelse(maximization, max, min)
  op_inv <- ifelse(maximization, min, max)

  best_fv <- op(last_row, na.rm = T)
  worst_recorded_fv <- op_inv(data, na.rm = T)
  worst_fv <- op_inv(last_row, na.rm = T)
  mean_fv <- mean(last_row, na.rm = T)
  median_fv <- median(last_row, na.rm = T)
  runs_reached <- sum(last_row == best_fv)

  data.table(
    ID = get_id(ds),
    DIM = attr(ds, 'DIM'),
    funcId = attr(ds, 'funcId'),
    `worst recorded` = worst_recorded_fv,
    `worst reached` = worst_fv,
    `best reached` = best_fv,
    `mean reached` = mean_fv,
    `median reached` = median_fv,
    runs = runs,
    `succ` = runs_reached,
    budget = budget
  )
}

#' @rdname get_RT_overview
#' @export
#'
get_RT_overview.DataSet <- function(ds, ...) {
  if (!is.null(attr(ds, "format")) &&
      attr(ds, "format") == NEVERGRAD) {
    data <- get_FV(ds)
    budget <- max(attr(ds, 'maxRT'))
    runs <- ncol(data)
    min_rt <- rownames(data) %>% as.integer %>% min
    max_rt <- budget
  }

  else{
    data <- get_RT(ds)
    runs <- ncol(data)
    budget <- max(attr(ds, 'maxRT'))
    min_rt <- min(data, na.rm = T)
    max_rt <- max(data, na.rm = T)
  }

  data.table(
    ID = get_id(ds),
    DIM = attr(ds, 'DIM'),
    funcId = attr(ds, 'funcId'),
    `miminal runtime` = min_rt,
    `maximal runtime` = max_rt,
    `runs` = runs,
    `Budget` = budget
  )
}

#' @rdname get_overview
#' @export
#'
get_overview.DataSet <- function(ds, ...) {
  data <- get_FV(ds)
  runs <- ncol(data)

  budget <- max(attr(ds, 'maxRT'))
  if (!is.null(get_RT(ds)) && length(get_RT(ds)) > 0) {
    max_rt <- max(get_RT(ds), na.rm = T)
    budget <- max(budget, max_rt)
  }
  else
    max_rt <- budget

  last_row <- data[nrow(data),]
  maximization <- attr(ds, 'maximization')

  op <- ifelse(maximization, max, min)
  op_inv <- ifelse(maximization, min, max)

  best_fv <- op(last_row, na.rm = T)
  worst_recorded_fv <- op_inv(data, na.rm = T)
  worst_fv <- op_inv(last_row, na.rm = T)
  mean_fv <- mean(last_row, na.rm = T)
  median_fv <- median(last_row, na.rm = T)
  runs_reached <- sum(last_row == best_fv)

  data.table(
    ID = get_id(ds),
    `DIM` = attr(ds, 'DIM'),
    `funcId` = attr(ds, 'funcId'),
    `runs` = runs,
    `best reached` = best_fv,
    `succ` = runs_reached,
    `budget` = budget,
    `max evals used` = max_rt,
    `worst recorded` = worst_recorded_fv,
    `worst reached` = worst_fv,
    `mean reached` = mean_fv,
    `median reached` = median_fv
  )

}

#' @rdname get_ERT
#' @export
#'
get_ERT.DataSet <- function(ds, ftarget, budget = NULL, ...) {
  data <- get_RT(ds)
  if (is.null(budget) || is.na(budget))
    maxRT <- attr(ds, 'maxRT')
  else
    maxRT <- as.numeric(budget)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')

  ftarget <-
    sort(as.double(unique(c(ftarget))), decreasing = !maximization)
  FValues <- as.numeric(rownames(data))
  idx <- seq_along(FValues)
  op <- ifelse(maximization, `>=`, `<=`)

  matched <- sapply(ftarget, function(f)
    idx[`op`(FValues, f)][1])

  if (is.list(matched))
    return(data.table())

  data <- data[matched, , drop = FALSE]
  dt <-
    as.data.table(cbind(get_id(ds), ftarget, SP(data, maxRT)$ERT))
  colnames(dt) <- c('ID', 'target', 'ERT')
  dt
}

#' @rdname get_RT_summary
#' @export
#'
get_RT_summary.DataSet <-
  function(ds, ftarget, budget = NULL, ...) {
    data <- get_RT(ds)
    if (is.null(budget) ||
        is.na(budget))
      maxRT <- max(attr(ds, 'maxRT'))
    else
      maxRT <- as.numeric(budget)
    ID <- get_id(ds)
    maximization <- attr(ds, 'maximization')

    ftarget <-
      sort(as.double(unique(c(ftarget))), decreasing = !maximization)
    FValues <- as.numeric(rownames(data))
    idx <- seq_along(FValues)
    op <- ifelse(maximization, `>=`, `<=`)

    matched <- sapply(ftarget,
                      function(f) {
                        idx[`op`(FValues, f)][1]
                      })

    if (is.list(matched)) {
      return(data.table())
    }

    data <- data[matched, , drop = FALSE]
    pen_data <- data
    pen_data[is.na(pen_data)] = maxRT * getOption("IOHanalyzer.PAR_penalty", 1)
    pen_data[pen_data > maxRT] = maxRT * getOption("IOHanalyzer.PAR_penalty", 1)
    dt_temp <- apply(data, 1, IOHanalyzer_env$D_quantile) %>%
      t %>%
      as.data.table %>%
      cbind(as.data.table(SP(data, maxRT))) %>%
      cbind(
        ID,
        ftarget,
        apply(data, 1, .mean),
        apply(data, 1, .median),
        apply(data, 1, .sd),
        apply(pen_data, 1, .mean),
        .
      ) %>%
      set_colnames(c(
        'ID',
        'target',
        'mean',
        'median',
        'sd',
        paste0('PAR-', getOption("IOHanalyzer.PAR_penalty", 1)),
        paste0(getOption("IOHanalyzer.quantiles") * 100, '%'),
        'ERT',
        'runs',
        'ps'
      ))
    dt_temp
  }

#' Get the maximal running time
#'
#' @param ds A DataSet or DataSetList object
#' @param ... Arguments passed to other methods
#'
#' @return A data.table object containing the algorithm ID and the running time
#' when the algorithm terminates in each run
#' @examples
#' get_maxRT(dsl)
#' get_maxRT(dsl[[1]])
#' @export
get_maxRT <- function(ds, ...)
  UseMethod("get_maxRT", ds)

#' @rdname get_maxRT
#' @param output The format of the outputted table: 'wide' or 'long'
#' @export
#'
get_maxRT.DataSet <- function(ds, output = 'wide', ...) {
  ID <- get_id(ds)
  N <- ncol(get_RT(ds))
  maxRT <- attr(ds, 'maxRT')
  if (length(maxRT) < N) {
    maxRT <- rep(maxRT, N)
  }
  res <- t(c(ID, maxRT)) %>%
    as.data.table %>%
    set_colnames(c('ID', paste0('run.', seq(N))))

  if (output == 'long') {
    res <-
      melt(res,
           id = 'ID',
           variable.name = 'run',
           value.name = 'maxRT')
    res[, run := as.numeric(gsub('run.', '', run)) %>% as.integer][, maxRT := as.integer(maxRT)][order(run)]
  }
  res
}

#' @rdname get_RT_sample
#' @param output A character determining the format of output data.table: 'wide' or 'long'
#' @export
get_RT_sample.DataSet <-
  function(ds, ftarget, output = 'wide', ...) {
    data <- get_RT(ds)
    N <- ncol(data)
    ID <- get_id(ds)
    maximization <- attr(ds, 'maximization')

    ftarget <-
      sort(as.double(unique(c(ftarget))), decreasing = !maximization)
    FValues <- as.double(rownames(data))
    idx <- seq_along(FValues)
    op <- ifelse(maximization, `>=`, `<=`)

    matched <- sapply(ftarget,
                      function(f) {
                        idx[`op`(FValues, f)][1]
                      })

    res <-
      cbind(ID, ftarget, as.data.table(data[matched, , drop = FALSE])) %>%
      set_colnames(c('ID', 'target', paste0('run.', seq(N))))

    if (output == 'long') {
      # TODO: option to not add run etc to speed up performance of ECDF calculation?
      res <-
        melt(
          res,
          id = c('ID', 'target'),
          variable.name = 'run',
          value.name = 'RT'
        )
      res[, run := as.integer(as.numeric(gsub('run.', '', run)))][, RT := as.integer(RT)][order(target, run)]
    }
    res
  }

#' Function to get just the RT samples needed, without any formatting to improve speed
#' @param RT_mat A matrix containing the RT-values of a dataset
#' @param target Which target-value to use
#' @param maximization Whether maximization is needed or not
#' @export
fast_RT_samples <- function(RT_mat, target, maximization = F) {
  if (maximization)
    idxs <-
      seq_along(rownames(RT_mat))[as.double(rownames(RT_mat)) >= target]
  else
    idxs <-
      seq_along(rownames(RT_mat))[as.double(rownames(RT_mat)) <= target]
  if (length(idxs) > 0) {
    return(RT_mat[idxs[[1]],])
  }
  return(rep(NA, 15))
}

#' @rdname get_FV_summary
#' @export
#'
get_FV_summary.DataSet <-
  function(ds, runtime, include_geom_mean = F, ...) {
    data <- get_FV(ds)
    NC <- ncol(data)
    NR <- nrow(data)
    ID <- get_id(ds)
    maximization <- attr(ds, 'maximization')

    runtime <- sort(as.numeric(unique(c(runtime))))
    RT <- as.numeric(rownames(data))
    idx <- seq_along(RT)

    if (max(RT) < max(runtime)) {
      #Avoid forgetting stopped runs
      data2 <- rbind(data, data[nrow(data), ])
      rownames(data2) <- c(rownames(data), max(c(runtime, RT)) + 1)
      data <- data2
    }

    data <-
      apply(data, 2, function(x) {
        #Remove NA to preserve monotonicity of mean
        temp <- x
        temp[is.na(temp)] <- min(x, na.rm = T)
        temp
      })

    matched <- sapply(runtime, function(r)
      rev(idx[r >= RT])[1])
    data <- data[matched, , drop = FALSE]

    dt <- cbind(
      ID,
      runtime,
      NC,
      apply(data, 1, .mean),
      apply(data, 1, .median),
      apply(data, 1, .sd),
      as.data.table(t(
        apply(data, 1, IOHanalyzer_env$C_quantile)
      ))
    ) %>%
      set_colnames(c(
        'ID',
        'runtime',
        'runs',
        'mean',
        'median',
        'sd',
        paste0(getOption("IOHanalyzer.quantiles") * 100, '%')
      ))

    if (include_geom_mean) {
      dt <- cbind(dt, apply(data, 1, function(x) {
        exp(mean(log(x)))
      }))
      setnames(dt, 'V2', 'geometric mean')
    }
    return(dt)
  }

#' @rdname get_FV_sample
#' @param output A String. The format of the output data: 'wide' or 'long'
#'
#' @export
#'
get_FV_sample.DataSet <-
  function(ds, runtime, output = 'wide', ...) {
    data <- get_FV(ds)
    N <- ncol(data)
    n_row <- nrow(data)
    ID <- get_id(ds)
    maximization <- attr(ds, 'maximization')

    runtime <- sort(as.numeric(unique(c(runtime))))
    RT <- as.numeric(rownames(data))
    idx <- seq_along(RT)

    matched <- sapply(runtime, function(r)
      rev(idx[r >= RT])[1])
    res <-
      cbind(ID, runtime, as.data.table(data[matched, , drop = FALSE])) %>%
      set_colnames(c('ID', 'runtime', paste0('run.', seq(N))))

    if (output == 'long') {
      res <-
        melt(
          res,
          id = c('ID', 'runtime'),
          variable.name = 'run',
          value.name = 'f(x)'
        )
      res[, run := as.integer(as.numeric(gsub('run.', '', run)))][order(runtime, run)]
    }
    res
  }

#' @rdname get_PAR_name
#' @export
#'
get_PAR_name.DataSet <- function(ds, which = 'by_FV') {
  names(ds$PAR[[which]])
}

#' @rdname get_PAR_summary
#' @param parId A character vector. Either 'all' or the name of parameters to be retrieved
#' @param which A string takes values in `c('by_FV', 'by_RT')`, indicating the parameters to be
#' retrieved are aligned against the running time (RT) or function value (FV). `'by_FV'`
#' is the default value.
#' @export
get_PAR_summary.DataSet <-
  function(ds,
           idxValue,
           parId = 'all',
           which = 'by_FV',
           ...) {
    if (which == 'by_FV') {
      RefValues <- as.numeric(rownames(get_RT(ds)))
      ds_par <- ds$PAR$by_FV
      idx_name <- 'target'
    }
    else if (which == 'by_RT') {
      RefValues <- as.numeric(rownames(get_FV(ds)))
      ds_par <- ds$PAR$by_RT
      idx_name <- 'runtime'
    }

    idx <- seq_along(RefValues)
    ID <- get_id(ds)
    par_name <- get_PAR_name(ds, which = which)

    if (parId != 'all')
      par_name <- intersect(par_name, parId)
    if (length(par_name) == 0)
      return(NULL)

    maximization <- attr(ds, 'maximization')
    cond <- maximization || which == 'by_RT'
    op <- ifelse(cond, `>=`, `<=`)
    idxValue <- sort(as.numeric(c(idxValue)), decreasing = !cond)

    matched <- sapply(idxValue,
                      function(f) {
                        idx[`op`(RefValues, f)][1]
                      })

    lapply(par_name,
           function(par) {
             data <- ds_par[[par]][matched, , drop = FALSE]
             df <- cbind(
               ID,
               par,
               idxValue,
               apply(data, 1, function(x)
                 length(x[!is.na(x)])),
               apply(data, 1, .mean),
               apply(data, 1, .median),
               apply(data, 1, .sd),
               as.data.table(t(
                 apply(data, 1, IOHanalyzer_env$C_quantile)
               ))
             )
             colnames(df) <-
               c(
                 'ID',
                 'parId',
                 idx_name,
                 'runs',
                 'mean',
                 'median',
                 'sd',
                 paste0(getOption("IOHanalyzer.quantiles") * 100, '%')
               )
             df
           }) %>%
      rbindlist
  }

#' @rdname get_PAR_sample
#' @param parId A character vector. Either 'all' or the name of parameters to be retrieved
#' @param which A string takes values in `c('by_FV', 'by_RT')`, indicating the parameters to be
#' retrieved are aligned against the running time (RT) or function value (FV). `'by_FV'`
#' is the default value.
#' @param output A character. The format of the output data: 'wide' or 'long'
#' @export
get_PAR_sample.DataSet <-
  function(ds,
           idxValue,
           parId = 'all',
           which = 'by_FV',
           output = 'wide',
           ...) {
    N <- length(attr(ds, 'instance'))
    if (which == 'by_FV') {
      RefValues <- as.numeric(rownames(get_RT(ds)))
      ds_par <- ds$PAR$by_FV
      idx_name <- 'target'
    }
    else if (which == 'by_RT') {
      RefValues <- as.numeric(rownames(get_FV(ds)))
      ds_par <- ds$PAR$by_RT
      idx_name <- 'runtime'
    }

    idx <- seq_along(RefValues)
    ID <- get_id(ds)
    par_name <- get_PAR_name(ds, which = which)

    if (parId != 'all')
      par_name <- intersect(par_name, parId)
    if (length(par_name) == 0)
      return(NULL)

    maximization <- attr(ds, 'maximization')
    cond <- maximization || which == 'by_RT'
    op <- ifelse(cond, `>=`, `<=`)
    idxValue <- sort(as.numeric(c(idxValue)), decreasing = !cond)
    matched <-
      sapply(idxValue, function(f)
        idx[`op`(RefValues, f)][1])

    res <- lapply(par_name,
                  function(parId) {
                    data <- ds_par[[parId]]
                    data <-
                      as.data.table(data[matched, , drop = FALSE])
                    data <- cbind(ID, parId, idxValue, data)
                    colnames(data) <-
                      c('ID', 'parId', idx_name, paste0('run.', seq(N)))
                    data
                  })
    res <- rbindlist(res)

    if (output == 'long') {
      res <-
        melt(
          res,
          id = c('ID', 'parId', idx_name),
          variable.name = 'run',
          value.name = 'value'
        )
      res[, run := as.integer(as.numeric(gsub('run.', '', run)))]
      if (which == 'by_FV')
        res[order(target, run)]
      else if (which == 'by_RT')
        res[order(runtime, run)]
    }
    res
  }

#' @rdname get_id
#' @export
get_id.DataSet <- function(ds, ...) {
  temp <- attr(ds, 'ID')
  if (is.null(temp)) {
    # warning("No ID attribute set, returning the algId's instead. (from 1.6.0 onwards, ID attributes are always added
    #         to new datasets, see the 'change_id' function.")
    return(attr(ds, 'algId'))
  }
  return(unique(temp))
}

#' @rdname get_FV
#' @export
get_FV.DataSet <- function(ds, ...) {
  if (isTRUE(attr(ds, 'constrained')) &&
      !is.null(getOption("IOHanalyzer.Violation_Function"))) {
    FV <- getOption(
      "IOHanalyzer.Violation_Function",
      default = function(x, y) {
        x
      }
    )(ds$FV_raw_mat, ds$PAR$by_RT$violation)
    if (attr(ds, 'maximization'))
      return(do.call(cbind, lapply(seq(ncol(
        FV
      )), function(x)
        cummax(FV[, x]))))
    return(do.call(cbind, lapply(seq(ncol(
      FV
    )), function(x)
      cummin(FV[, x]))))
  } else
    return(ds$FV)
}

#' @rdname get_RT
#' @export
get_RT.DataSet <- function(ds, ...) {
  if (isTRUE(attr(ds, 'constrained')) &&
      !is.null(getOption("IOHanalyzer.Violation_Function"))) {
    data <- getOption(
      "IOHanalyzer.Violation_Function",
      default = function(x, y) {
        x
      }
    )(ds$FV_raw_mat, ds$PAR$by_RT$violation)
    FV <- unique(sort(data, decreasing = !attr(ds, 'maximization')))
    index <- as.numeric(rownames(data))
    RT <-
      c_align_running_time_matrix(data, FV, as.numeric(index), attr(ds, 'maximization'))
    rownames(RT) <- FV
    return(RT)
  } else
    return(ds$RT)
}
