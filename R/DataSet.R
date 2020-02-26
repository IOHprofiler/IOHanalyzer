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
#'
#' @return A S3 object 'DataSet'
#' @export
#' @examples
#' path <- system.file('extdata', 'ONE_PLUS_LAMDA_EA', package = 'IOHanalyzer')
#' info <- read_index_file(file.path(path, 'IOHprofiler_f1_i1.info'))
#' DataSet(info[[1]])
DataSet <- function(info, verbose = F, maximization = NULL, format = IOHprofiler,
                    subsampling = FALSE) {
  if (!is.null(info)) {
    path <- dirname(info$datafile)
    suite <- toupper(info$suite)

    # for an unknown suite, to detect the format
    if (is.null(suite) || length(suite) == 0) {
      if (verbose)
        warning("Suite-name not provided in .info-file, taking best guess based on
                the format of data-files.")
      suite <- switch(format,
                      IOHprofiler = "PBO",
                      COCO = "BBOB",
                      BIOBJ_COCO = "biobj-bbob",
                      TWO_COL = NULL)
    }

    if (is.null(maximization)) {
      maximization <- info$maximization
      if (is.null(maximization) && !is.null(suite)) {
        if (verbose)
          warning("maximization or minimization not specified in .info-file,
                  taking best guess based on the suite-name.")
        if (grepl("\\w*bbob\\w*", suite, ignore.case = T) != 0)
          maximization <- F
        else
          maximization <- T
      }
    }

    datBaseName <- strsplit(basename(info$datafile), '\\.')[[1]][1]
    datFile <- file.path(path, paste0(datBaseName, '.dat'))
    tdatFile <- file.path(path, paste0(datBaseName, '.tdat'))
    cdatFile <- file.path(path, paste0(datBaseName, '.cdat'))

    # NOTE: preference on data file for the alignment by RT: cdat > tdat > dat
    if (file.exists(cdatFile))
      fvFile <- cdatFile
    else if (file.exists(tdatFile))
      fvFile <- tdatFile
    else if (file.exists(datFile))
      fvFile <- datFile
    else
      stop('No datafiles found, please verify the integrity of the chosen files')

    # NOTE: preference on data file for the alignment by FV: dat > tdat > cdat
    if (file.exists(datFile))
      rtFile <- datFile
    else if (file.exists(tdatFile))
      rtFile <- tdatFile
    else if (file.exists(cdatFile))
      # TODO: perhaps turn on `subsampling` here as this would take quite some time
      rtFile <- cdatFile

    read_raw <- switch(
      format,
      IOHprofiler = read_dat,
      COCO = read_dat__COCO,
      BIOBJ_COCO = read_dat__BIOBJ_COCO,
      TWO_COL = read_dat  # TODO: perhaps rename `TWO_COL` or to use a better naming
                          # scheme for all format names
    )

    RT_raw <- read_raw(rtFile, subsampling)
    FV_raw <- read_raw(fvFile, subsampling)

    if (is.null(maximization)) {
      if (verbose)
        warning("Did not find maximization / minimization, auto-detecting based on
                function value progression")
      # TODO: idxTarget should be set depending on the data format
      idxTarget <- 2
      cond <- unique(lapply(FV_raw, function(FV) FV[1, idxTarget] >= FV[nrow(FV), idxTarget]))
      if (length(cond) > 1)
        stop('The detected maximization differs in multiple runs')
      maximization <- cond
    }

    RT <- align_running_time(RT_raw, format = format, maximization = maximization)
    FV <- align_function_value(FV_raw, format = format)

    PAR <- list(
      'by_FV' = RT[names(RT) != 'RT'],
      'by_RT' = FV[names(FV) != 'FV']
    )

    RT <- RT$RT
    mode(RT) <- 'integer'
    FV <- FV$FV
    
    if (format %in% c(IOHprofiler)) {
      # try to save some memory here...
      FV <- tryCatch({
        .FV <- FV
        mode(.FV) <- 'integer'
        if (all(FV == .FV)) .FV
        else FV
      },
      warning = function(w) FV) # in case the type coercion gives a warning 
    }
   
    # TODO: add more data sanity checks
    maxRT <- set_names(sapply(RT_raw, function(d) d[nrow(d), idxEvals]), NULL)
    # Fix for old-format files which do not store used runtime in .dat-files
    maxRT <- pmax(maxRT, info$maxRT)
    if (any(maxRT != info$maxRT) && verbose)
      warning('Inconsitent maxRT in *.info file and *.cdat file')

    # TODO: clean up these if-statements: Function to set idxTarget and n_data_column?
    # `idxTarget` is a global variable?
    if (format == TWO_COL)
      finalFV <- set_names(sapply(FV_raw, function(d) d[nrow(d), idxTarget - 1]), NULL)
    else
      finalFV <- set_names(sapply(FV_raw, function(d) d[nrow(d), idxTarget]), NULL)

    if (any(finalFV != info$finalFV) && verbose)
      warning('Inconsitent finalFvalue in *.info file and *.dat file')

    if (length(info$instance) != length(RT_raw)) {
      if (verbose)
        warning('The number of instances found in the info is inconsistent with the data!')
      info$instance <- seq(length(RT_raw))
    }

    do.call(
      function(...)
        structure(list(RT = RT, FV = FV, PAR = PAR), class = c('DataSet', 'list'), ...),
      c(info, list(maxRT = maxRT, finalFV = finalFV, format = format,
                   maximization = maximization, suite = suite))
    )
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
  if (length(dsl) == 1) dsl <- dsl[[1]]
  dsl <- dsl[sapply(dsl, length) != 0]
  if (length(dsl) == 0)
    return()
  
  for (ds in dsl) {
    if (!any((class(ds)) == 'DataSet'))
      stop("Operation only possible when all arguments are DataSets")
  }
  
  fixed_attrs <- c('suite', 'maximization', 'DIM', 'funcId', 'algId', 'format')
  
  info <- list()
  for (attr_str in fixed_attrs) {
    temp  <-  unique(unlist(lapply(dsl, function(x) attr(x, attr_str))))
    if (length(temp) > 1) {
      stop(paste0("Attempted to add datasets with different ", attr_str, 
                  "-attributes! Tis is not allowed, please keep them as separate DataSets!"))
    }
    info <- c(info,temp)
  }
  names(info) <- fixed_attrs

  for (attr_str in names(attributes(dsl[[1]]))) {
    if (attr_str %in% fixed_attrs || attr_str %in% c("names", "class")) next
    temp  <- unlist(lapply(dsl, function(x) attr(x, attr_str)))
    if (length(unique(temp)) == 1) 
      temp <- unique(temp)
    else 
      temp <- list(temp_name = temp)
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
  
  
  RT <- align_running_time(RT_raw, format = "TWO_COL", maximization = info$maximization)$RT
  FV <- align_function_value(RT_raw, format = "TWO_COL")$FV

  #TODO: Deal with cases where aligned parameters are present in original DataSets
  PAR <- list(
    'by_FV' = RT[names(RT) != 'RT'],
    'by_RT' = FV[names(FV) != 'FV']
  )
  
  #Unaligned parameters
  for (par_name in names(dsl[[1]]$PAR)) {
    if (!par_name %in% c('by_FV', 'by_RT'))
      PAR[[par_name]] <- unlist(lapply(dsl, function(x) {x$PAR[[par_name]]}), recursive = F)
  }

  do.call(
    function(...)
      structure(list(RT = RT, FV = FV, PAR = PAR), class = c('DataSet', 'list'), ...),
    c(info)
  )
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
cat.DataSet <- function(x) cat(as.character(x))

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
  sprintf('DataSet(%s on f%s %dD)', attr(x, 'algId'), attr(x, 'funcId'),
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
    if (attr(dsL, attr_str) != attr(dsR, attr_str)) return(FALSE)
  }
  return(TRUE)
}

#' Get Expected RunTime
#'
#' @param ds A DataSet or DataSetList object
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
get_ERT <- function(ds, ftarget, ...) UseMethod("get_ERT", ds)

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
get_RT_sample <- function(ds, ftarget, ...) UseMethod("get_RT_sample", ds)

#' Get RunTime Summary
#'
#' @param ds A DataSet or DataSetList object
#' @param ... Arguments passed to other methods
#' @param ftarget The function target(s) for which to get the runtime summary
#'
#' @return A data.table containing the runtime statistics for each provided target
#' function value
#' @examples
#' get_RT_summary(dsl, 14)
#' get_RT_summary(dsl[[1]], 14)
#' @export
get_RT_summary <- function(ds, ftarget, ...) UseMethod("get_RT_summary", ds)

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
get_FV_sample <- function(ds, ...) UseMethod("get_FV_sample", ds)

#' Get Function Value Summary
#'
#' @param ds A DataSet or DataSetList object
#' @param runtime A Numerical vector. Runtimes at which function values are reached
#' @param ... Arguments passed to other methods
#'
#' @return A data.table containing the function value statistics for each provided
#' target runtime value
#' @examples
#' get_FV_summary(dsl, 100)
#' get_FV_summary(dsl[[1]], 100)
#' @export
get_FV_summary <- function(ds, ...) UseMethod("get_FV_summary", ds)

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
get_PAR_sample <- function(ds, idxValue, ...) UseMethod("get_PAR_sample", ds)

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
get_PAR_summary <- function(ds, idxValue, ...) UseMethod("get_PAR_summary", ds)

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
get_PAR_name <- function(ds, which) UseMethod("get_PAR_name", ds)

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
get_FV_overview <- function(ds, ...) UseMethod("get_FV_overview", ds)

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
get_RT_overview <- function(ds, ...) UseMethod("get_RT_overview", ds)

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
get_overview <- function(ds, ...) UseMethod("get_overview", ds)

#' @rdname get_FV_overview
#' @export
get_FV_overview.DataSet <- function(ds, ...) {
  data <- ds$FV
  runs <- ncol(data)
  last_row <- data[nrow(data), ]
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

  data.table(algId = attr(ds, 'algId'),
             DIM = attr(ds, 'DIM'),
             funcId = attr(ds, 'funcId'),
             `worst recorded` = worst_recorded_fv,
             `worst reached` = worst_fv,
             `best reached` = best_fv,
             `mean reached` = mean_fv,
             `median reached` = median_fv,
             runs = runs,
             `succ` = runs_reached,
             budget = budget)
}

#' @rdname get_RT_overview
#' @export
#'
get_RT_overview.DataSet <- function(ds, ...) {

  if (!is.null(attr(ds, "format")) && attr(ds, "format") == NEVERGRAD) {
    data <- ds$FV
    budget <- max(attr(ds, 'maxRT'))
    runs <- ncol(data)
    min_rt <- rownames(data) %>% as.integer %>% min
    max_rt <- budget
  }

  else{
    data <- ds$RT
    runs <- ncol(data)
    budget <- max(attr(ds, 'maxRT'))
    min_rt <- min(data, na.rm = T)
    max_rt <- max(data, na.rm = T)
  }

  data.table(algId = attr(ds, 'algId'),
             DIM = attr(ds, 'DIM'),
             funcId = attr(ds, 'funcId'),
             `miminal runtime` = min_rt,
             `maximal runtime` = max_rt,
             `runs` = runs,
             `Budget` = budget)
}

#' @rdname get_overview
#' @export
#'
get_overview.DataSet <- function(ds, ...) {
  data <- ds$FV
  runs <- ncol(data)

  budget <- max(attr(ds, 'maxRT'))
  if (!is.null(ds$RT) && length(ds$RT) > 0) {
    max_rt <- max(ds$RT, na.rm = T)
    budget <- max(budget, max_rt)
  }
  else max_rt <- budget

  last_row <- data[nrow(data), ]
  maximization <- attr(ds, 'maximization')

  op <- ifelse(maximization, max, min)
  op_inv <- ifelse(maximization, min, max)

  best_fv <- op(last_row, na.rm = T)
  worst_recorded_fv <- op_inv(data, na.rm = T)
  worst_fv <- op_inv(last_row, na.rm = T)
  mean_fv <- mean(last_row, na.rm = T)
  median_fv <- median(last_row, na.rm = T)
  runs_reached <- sum(last_row == best_fv)

  data.table(`algId` = attr(ds, 'algId'),
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
get_ERT.DataSet <- function(ds, ftarget, ...) {
  data <- ds$RT
  maxRT <- attr(ds, 'maxRT')
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')

  ftarget <- sort(as.double(unique(c(ftarget))), decreasing = !maximization)
  FValues <- as.numeric(rownames(data))
  idx <- seq_along(FValues)
  op <- ifelse(maximization, `>=`, `<=`)

  matched <- sapply(ftarget, function(f) idx[`op`(FValues, f)][1])

  if (is.list(matched))
    return(data.table())

  data <- data[matched, , drop = FALSE]
  dt <- as.data.table(cbind(algId, ftarget, SP(data, maxRT)$ERT))
  colnames(dt) <- c('algId', 'target', 'ERT')
  dt
}

#' @rdname get_RT_summary
#' @export
#'
get_RT_summary.DataSet <- function(ds, ftarget, ...) {
  data <- ds$RT
  maxRT <- attr(ds, 'maxRT')
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')

  ftarget <- sort(as.double(unique(c(ftarget))), decreasing = !maximization)
  FValues <- as.numeric(rownames(data))
  idx <- seq_along(FValues)
  op <- ifelse(maximization, `>=`, `<=`)

  matched <- sapply(
    ftarget,
    function(f) {
      idx[`op`(FValues, f)][1]
    }
  )

  if (is.list(matched)) {
    return(data.table())
  }

  if (1 < 2) {
    data <- data[matched, , drop = FALSE]
    apply(data, 1, IOHanalyzer_env$D_quantile) %>%
      t %>%
      as.data.table %>%
      cbind(as.data.table(SP(data, maxRT))) %>%
      cbind(algId, ftarget,
            apply(data, 1, .mean),
            apply(data, 1, .median),
            apply(data, 1, .sd), .) %>%
      set_colnames(c('algId', 'target', 'mean', 'median',
                     'sd', paste0(getOption("IOHanalyzer.quantiles") * 100, '%'),
                     'ERT', 'runs', 'ps'))
  } else {# TODO: remove this case, deprecated...
    NAs <- is.na(matched)
    if (any(NAs)) {
      rbindlist(
        list(
          data[matched[!NAs], ],
          data.table(cbind(t(t(ftarget[NAs])), matrix(NA, sum(NAs), ncol(data) - 1)))
        )
      ) %>% cbind(algId, .)
    } else {
      data[matched, -c('target')] %>% cbind(algId, ftarget, .)
    }
  }
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
get_maxRT <- function(ds, ...) UseMethod("get_maxRT", ds)

#' @rdname get_maxRT
#' @param output The format of the outputted table: 'wide' or 'long'
#' @export
#'
get_maxRT.DataSet <- function(ds, output = 'wide', ...) {
  algId <- attr(ds, 'algId')
  N <- ncol(ds$RT)
  res <- t(c(algId, attr(ds, 'maxRT'))) %>%
    as.data.table %>%
    set_colnames(c('algId', paste0('run.', seq(N))))

  if (output == 'long') {
    res <- melt(res, id = 'algId', variable.name = 'run', value.name = 'maxRT')
    res[, run := as.numeric(gsub('run.', '', run)) %>% as.integer
        ][, maxRT := as.integer(maxRT)
          ][order(run)]
  }
  res
}

#' @rdname get_RT_sample
#' @param output A character determining the format of output data.table: 'wide' or 'long'
#' @export
get_RT_sample.DataSet <- function(ds, ftarget, output = 'wide', ...) {
  data <- ds$RT
  N <- ncol(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')

  ftarget <- sort(as.double(unique(c(ftarget))), decreasing = !maximization)
  FValues <- as.double(rownames(data))
  idx <- seq_along(FValues)
  op <- ifelse(maximization, `>=`, `<=`)

  matched <- sapply(
    ftarget,
    function(f) {
      idx[`op`(FValues, f)][1]
    }
  )

  res <- cbind(algId, ftarget, as.data.table(data[matched, , drop = FALSE])) %>%
    set_colnames(c('algId', 'target', paste0('run.', seq(N))))

  if (output == 'long') {
    #TODO: option to not add runnr etc to speed up performance of ECDF calculation?
    res <- melt(res, id = c('algId', 'target'), variable.name = 'run', value.name = 'RT')
    res[, run := as.integer(as.numeric(gsub('run.', '', run)))
        ][, RT := as.integer(RT)
          ][order(target, run)]
  }
  res
}

#' @rdname get_FV_summary
#' @export
#'
get_FV_summary.DataSet <- function(ds, runtime, ...) {
  data <- ds$FV
  NC <- ncol(data)
  NR <- nrow(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')

  runtime <- sort(as.numeric(unique(c(runtime))))
  RT <- as.numeric(rownames(data))
  idx <- seq_along(RT)

  matched <- sapply(runtime, function(r) {
    res <- idx[RT >= r][1]
    ifelse(is.na(res), NR, res)
  })

  data <- data[matched, , drop = FALSE]
  df <- cbind(algId, runtime, NC,
              apply(data, 1, .mean),
              apply(data, 1, .median),
              apply(data, 1, .sd),
              as.data.table(t(apply(data, 1, IOHanalyzer_env$C_quantile)))
  )
  colnames(df) <- c('algId', 'runtime', 'runs', 'mean', 'median', 'sd',
                    paste0(getOption("IOHanalyzer.quantiles") * 100, '%'))
  df
}

#' @rdname get_FV_sample
#' @param output A String. The format of the output data: 'wide' or 'long'
#'
#' @export
#'
get_FV_sample.DataSet <- function(ds, runtime, output = 'wide', ...) {
  data <- ds$FV
  N <- ncol(data)
  n_row <- nrow(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')

  runtime <- sort(as.numeric(unique(c(runtime))))
  RT <- as.numeric(rownames(data))
  idx <- seq_along(RT)

  matched <- sapply(runtime, function(r) {
    res <- idx[RT >= r][1]
    ifelse(is.na(res), n_row, res)
  })

  res <- cbind(algId, runtime, as.data.table(data[matched, , drop = FALSE])) %>%
    set_colnames(c('algId', 'runtime', paste0('run.', seq(N))))

  if (output == 'long') {
    res <- melt(res, id = c('algId', 'runtime'), variable.name = 'run', value.name = 'f(x)')
    res[, run := as.integer(as.numeric(gsub('run.', '', run)))
        ][order(runtime, run)]
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
get_PAR_summary.DataSet <- function(ds, idxValue, parId = 'all', which = 'by_FV', ...) {
  if (which == 'by_FV') {
    RefValues <- as.numeric(rownames(ds$RT))
    ds_par <- ds$PAR$by_FV
    idx_name <- 'target'
  }
  else if (which == 'by_RT') {
    RefValues <- as.numeric(rownames(ds$FV))
    ds_par <- ds$PAR$by_RT
    idx_name <- 'runtime'
  }

  idx <- seq_along(RefValues)
  algId <- attr(ds, 'algId')
  par_name <- get_PAR_name(ds, which = which)

  if (parId != 'all')
    par_name <- intersect(par_name, parId)
  if (length(par_name) == 0)
    return(NULL)

  maximization <- attr(ds, 'maximization')
  cond <- maximization || which == 'by_RT'
  op <- ifelse(cond, `>=`, `<=`)
  idxValue <- sort(as.numeric(c(idxValue)), decreasing = !cond)

  matched <- sapply(
    idxValue,
    function(f) {
      idx[`op`(RefValues, f)][1]
    }
  )

  lapply(par_name,
         function(par) {
           data <- ds_par[[par]][matched, , drop = FALSE]
           df <- cbind(
             algId, par, idxValue,
             apply(data, 1, function(x) length(x[!is.na(x)])),
             apply(data, 1, .mean),
             apply(data, 1, .median),
             apply(data, 1, .sd),
             as.data.table(t(apply(data, 1, IOHanalyzer_env$C_quantile)))
           )
           colnames(df) <- c('algId', 'parId', idx_name, 'runs', 'mean', 'median', 'sd',
                             paste0(getOption("IOHanalyzer.quantiles") * 100, '%'))
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
get_PAR_sample.DataSet <- function(ds, idxValue, parId = 'all', which = 'by_FV',
                                   output = 'wide', ...) {
  N <- length(attr(ds, 'instance'))
  if (which == 'by_FV') {
    RefValues <- as.numeric(rownames(ds$RT))
    ds_par <- ds$PAR$by_FV
    idx_name <- 'target'
  }
  else if (which == 'by_RT') {
    RefValues <- as.numeric(rownames(ds$FV))
    ds_par <- ds$PAR$by_RT
    idx_name <- 'runtime'
  }

  idx <- seq_along(RefValues)
  algId <- attr(ds, 'algId')
  par_name <- get_PAR_name(ds, which = which)

  if (parId != 'all')
    par_name <- intersect(par_name, parId)
  if (length(par_name) == 0)
    return(NULL)

  maximization <- attr(ds, 'maximization')
  cond <- maximization || which == 'by_RT'
  op <- ifelse(cond, `>=`, `<=`)
  idxValue <- sort(as.numeric(c(idxValue)), decreasing = !cond)
  matched <- sapply(idxValue, function(f) idx[`op`(RefValues, f)][1])

  res <- lapply(par_name,
                function(parId) {
                  data <- ds_par[[parId]]
                  data <- as.data.table(data[matched, , drop = FALSE])
                  data <- cbind(algId, parId, idxValue, data)
                  colnames(data) <- c('algId', 'parId', idx_name, paste0('run.', seq(N)))
                  data
                })
  res <- rbindlist(res)

  if (output == 'long') {
    res <- melt(res, id = c('algId', 'parId', idx_name), variable.name = 'run', value.name = 'value')
    res[, run := as.integer(as.numeric(gsub('run.', '', run)))]
    if (which == 'by_FV')
      res[order(target, run)]
    else if (which == 'by_RT')
      res[order(runtime, run)]
  }
  res
}
