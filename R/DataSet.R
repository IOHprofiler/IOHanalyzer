# This file contains some functions for reading, aligning, analyzing the raw data
# from the pseudo-boolean benchmarking
#
# Author: Hao Wang
# Email: wangronin@gmail.com

# TODO: do we need to import those here?
# suppressMessages(library(magrittr))
# suppressMessages(library(reshape2))
# suppressMessages(library(data.table))

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
#' @param format A character. The format of data source, either 'IOHProfiler', 'COCO' or 'TWO_COL"
#' @param subsampling Logical. Whether *.cdat files are subsampled?
#' @param include_param Whether to include the recorded parameters in the alignment
#' @return A S3 object 'DataSet'
#' @export
#' @examples 
#' path <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package="IOHanalyzer")
#' info <- read_IndexFile(file.path(path,"IOHprofiler_f1_i1.info"))
#' DataSet(info[[1]])
DataSet <- function(info, verbose = F, maximization = TRUE, format = IOHprofiler,
                    subsampling = FALSE, include_param = TRUE) {
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
    } else if (format %in% c(COCO, BIBOJ_COCO)) {
      datFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.dat'))
      tdatFile <- file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.tdat'))
    } else if (format == TWO_COL) {
      datFile <-  file.path(path, paste0(strsplit(filename, '\\.')[[1]][1], '.dat'))
    }

    # TODO: whether to keep the raw data set list?
    if (format == IOHprofiler) {
      dat <- read_dat(datFile, subsampling)         # read the dat file
      cdat <- read_dat(cdatFile, subsampling)       # read the cdat file
    } else if (format == COCO) {
      dat <- read_COCO_dat2(datFile, DIM=info$DIM, subsampling)    # read the dat file
      cdat <- read_COCO_dat2(tdatFile, DIM=info$DIM, subsampling)   # read the tdat file
    } else if (format == BIBOJ_COCO) {
      dat <- read_BIOBJ_COCO_dat(datFile, subsampling)    # read the dat file
      cdat <- read_BIOBJ_COCO_dat(tdatFile, subsampling)   # read the tdat file
    } else if (format == TWO_COL) {
      dat <- read_dat(datFile, subsampling)
    }

    # check if the number of instances does not match
    if (format != TWO_COL)
      stopifnot(length(dat) == length(cdat))

    if (format == IOHprofiler || format == TWO_COL)
      maximization <- TRUE
    else if (format == COCO)
      maximization <- FALSE

    # RT <- align_by_target(dat, maximization = maximization, format = format) # runtime
    RT <- align_runtime(dat, format = format, include_param = include_param)

    #TODO: check if this works correctly without CDAT-file
    if (format == TWO_COL)
      FV <- align_function_value(dat, format = format, include_param = include_param)  # function value
    else
      FV <- align_function_value(cdat, format = format, include_param = include_param)  # function value

    # TODO: remove this and incorporate the parameters aligned by runtimes
    FV[names(FV) != 'FV'] <- NULL

    # TODO: add more data sanity checks

    # TODO: add the same sanity checks for TWO_COL format
    if (format != TWO_COL) {
      maxRT <- sapply(cdat, function(d) d[nrow(d), idxEvals]) %>% set_names(NULL)
      if (any(maxRT != info$maxRT))
        warning('Inconsitent maxRT in *.info file and *.cdat file')
    }
    else{
      maxRT <- info$maxRT
    }

    #TODO: Clean up these if-statements: Function to set idxTarget and n_data_column?
    if (format == TWO_COL)
      finalFV <- sapply(dat, function(d) d[nrow(d), idxTarget - 1]) %>% set_names(NULL)
    else
      finalFV <- sapply(dat, function(d) d[nrow(d), idxTarget]) %>% set_names(NULL)

    if (any(finalFV != info$finalFV))
      warning('Inconsitent finalFvalue in *.info file and *.dat file')

    if (length(info$instance) != length(dat)) {
      warning('The number of instances found in the info is inconsistent with the data!')
      info$instance <- seq(length(dat))
    }

    # TODO: maybe the RT summary should not be always pre-computed
    AUX <- list()
    # data <- RT$RT
    # n_instance <- length(info$instance)
    # RT.summary <- data.table(target = rownames(data) %>% as.double,
    #                          mean = apply(data, 1, .mean),
    #                          median = apply(data, 1, .median),
    #                          sd = apply(data, 1, .sd))
    #
    # names <- paste0(probs * 100, '%')
    # RT.summary[, (names) := t(apply(data, 1, D_quantile)) %>% split(c(col(.)))]
    # RT.summary[, c('ERT', 'runs', 'ps') := SP(data, maxRT)]
    # AUX$RT.summary <- RT.summary

    do.call(function(...) structure(c(RT, FV, AUX), class = c('DataSet', 'list'), ...),
            c(info, list(maxRT = maxRT, finalFV = finalFV, format = format,
                         maximization = maximization)))

  } else {
    structure(list(), class = c('DataSet'))
  }
}

#' S3 generic print operator for DataSet
#'
#' @param x A DataSet object
#' @param verbose Verbose mode, currently not implemented
#' @param ... Arguments passed to other methods
#'
#' @return A short description of the DataSet
#' @examples 
#' print(dsl[[1]])
#' @export
print.DataSet <- function(x, verbose = F, ...) {
  # TODO: implement the verbose mode
  cat(as.character.DataSet(x, ...))
}

#' S3 generic cat operator for DataSet
#'
#' @param x A DataSet object
#'
#' @return A short description of the DataSet
#' @export
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
  function_values <- rownames(object$RT) %>% as.numeric
  RT.summary <- get_RT_summary(object, function_values)
  print(RT.summary)
  cat('\n')

  cat('function value summary:\n')
  runtimes <- rownames(object$FV) %>% as.numeric
  if (length(runtimes) > 100) {
    runtimes <- runtimes[seq(1, length(runtimes), length.out = 100)]
  }

  FV.summary <- get_FV_summary(object, runtimes)
  print(FV.summary)
  cat('\n')

  cat(paste('Attributes:', paste0(names(ds_attr), collapse = ', ')))
}


# # TODO: implement the 'save' option
# plot.DataSet <- function(ds, ask = TRUE, save = FALSE) {
#   dt <- data.table(ds$RT)
#   NC <- ncol(dt)
#   colnames(dt) <- as.character(seq(ncol(dt)))
#   dt[, target := as.numeric(rownames(ds$RT))]
#   dt_mean <- data.table(target = dt$target, mean = rowMeans(dt[, -c('target')], na.rm = T))
# 
#   target <- dt[, target]
#   N <- length(target)
#   if (N >= 30) # limit the number of point to plot
#     target <- as.numeric(target[seq(1, N, by = ceiling(N / 30))])
# 
#   # plot runtime curves
#   p <- melt(dt, id.vars = 'target', variable.name = 'instance', value.name = 'runtime') %>%
#     ggplot(aes(target, runtime, colour = as.factor(instance))) +
#     geom_line(aes(group = instance), alpha = 0.8) +
#     geom_line(data = dt_mean, aes(target, mean), colour = 'black', size = 1.5, alpha = 0.8) +
#     scale_colour_manual(values = colorspace::rainbow_hcl(NC)) +
#     scale_x_continuous(breaks = target) +
#     guides(colour = FALSE)
# 
#   print(p)
# 
#   if (ask) x <- readline("show data aligned by runtime?")
# 
#   df <- ds$FV
#   if (nrow(df) > 500) {
#     idx <- c(1, seq(1, nrow(df), length.out = 500), nrow(df)) %>% unique
#     df <- df[idx, ]
#   }
# 
#   dt <- data.table(df)
#   colnames(dt) <- as.character(seq(ncol(dt)))
#   dt[, budget := as.numeric(rownames(df))]
#   dt_mean <- data.table(budget = dt$budget, mean = rowMeans(dt[, -c('budget')], na.rm = T))
# 
#   budget <- dt[, budget]
#   N <- length(budget)
#   if (N >= 30) {
#     N.log10 <- log10(N)
#     index <- floor(10 ^ seq(0, N.log10, by = N.log10 / 30))
#     budget <- as.numeric(budget[index])
#   }
# 
#   # plot function value curves
#   p <- melt(dt, id.vars = 'budget', variable.name = 'instance', value.name = 'Fvalue') %>%
#     ggplot(aes(budget, Fvalue, colour = instance)) +
#     geom_line(aes(group = instance), alpha = 0.8) +
#     geom_line(data = dt_mean, aes(budget, mean), colour = 'black', size = 1.5, alpha = 0.8) +
#     scale_colour_manual(values = colorspace::rainbow_hcl(NC)) +
#     # scale_x_continuous(breaks = budget) +
#     scale_x_log10() +
#     guides(colour = FALSE)
# 
#   print(p)
# }

#' S3 generic == operator for DataSets
#'
#' @param dsL A DataSet object
#' @param dsR A DataSet object
#'
#'
#' @return True if the DataSets contain the same function, dimension and algorithm,
#' and have equal precision and comments; False otherwise
#' @examples 
#' dsl[[1]] == dsl[[2]]
#' @export
`==.DataSet` <- function(dsL, dsR) {
  if (length(dsL) == 0 || length(dsR) == 0)
    return(FALSE)

  attr(dsL, 'funcId') == attr(dsR, 'funcId') &&
    attr(dsL, 'DIM') == attr(dsR, 'DIM') &&
    attr(dsL, 'Precision') == attr(dsR, 'Precision') &&
    attr(dsL, 'algId') == attr(dsR, 'algId') &&
    attr(dsL, 'comment') == attr(dsR, 'comment')
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
#' @param ftarget A Numerical vector. Function values at which parameter values are observed
#' @param ... Arguments passed to other methods
#' 
#' @return A data.table object containing parameter values aligned at each given target value
#' @examples 
#' get_PAR_sample(dsl, 14)
#' get_PAR_sample(dsl[[1]], 14)
#' @export
get_PAR_sample <- function(ds, ftarget, ...) UseMethod("get_PAR_sample", ds)
#' Get Parameter Value Summary
#'
#' @param ds A DataSet or DataSetList object
#' @param ftarget A Numerical vector. Function values at which parameter values are observed
#' @param ... Arguments passed to other methods
#'
#' @return A data.table object containing basic statistics of parameter values aligned at each given target value
#' @examples 
#' get_PAR_summary(dsl, 14)
#' get_PAR_summary(dsl[[1]], 14)
#' @export
get_PAR_summary <- function(ds, ftarget, ...) UseMethod("get_PAR_summary", ds)

#' Get the parameter names of the algorithm
#'
#' @param ds A DataSet object
#'
#' @return a character list of paramter names, if recorded in the data set
#' @examples 
#' get_PAR_name(dsl[[1]])
#' @export
get_PAR_name <- function(ds) UseMethod("get_PAR_name", ds)
#' Get Function Value condensed overview
#'
#' @param ds A DataSet or DataSetList object
#' @param ... Arguments passed to other methods
#'
#' @return A data.table containing the algorithm ID, best, worst and mean reached function
#' values, the number of runs and available budget for the DataSet
#' @examples 
#' get_FV_overview(dsl)
#' get_FV_overview(dsl[[1]])
#' get_FV_overview(dsl, algorithm = "(1+1)_greedy_hill_climber_1" )
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

  data.table(Algorithm = attr(ds, 'algId'),
             DIM = attr(ds, 'DIM'),
             fID = attr(ds, 'funcId'),
             `Worst recorded f(x)` = worst_recorded_fv,
             `Worst reached f(x)` = worst_fv,
             `Best reached f(x)` = best_fv,
             `mean reached f(x)` = mean_fv,
             `median reached f(x)` = median_fv,
             runs = runs,
             `runs reached` = runs_reached,
             Budget = budget)
}

#' @rdname get_RT_overview
#' @export
#'
get_RT_overview.DataSet <- function(ds, ...) {
  data <- ds$RT
  runs <- ncol(data)
  budget <- max(attr(ds, 'maxRT'))

  min_rt <- min(data, na.rm = T)
  max_rt <- max(data, na.rm = T)

  data.table(Algorithm = attr(ds, 'algId'),
             DIM = attr(ds, 'DIM'),
             fID = attr(ds, 'funcId'),
             `miminal runtime` = min_rt,
             `maximal runtime` = max_rt,
             'runs' = runs,
             Budget = budget)
}

#' @rdname get_ERT
#' @export
#'
get_ERT.DataSet <- function(ds, ftarget, ...) {
  data <- ds$RT
  maxRT <- attr(ds, 'maxRT')
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')

  ftarget <- c(ftarget) %>% as.double %>% sort(decreasing = !maximization)
  FValues <- rownames(data) %>% as.numeric
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

  data <- data[matched, , drop = FALSE]

  SP(data, maxRT)$ERT %>%
  cbind(algId, ftarget, .) %>%
    as.data.table %>%
    set_colnames(c('algId', 'target', 'ERT'))
}

#' @rdname get_RT_summary
#' @export
#'
get_RT_summary.DataSet <- function(ds, ftarget, ...) {
  data <- ds$RT
  maxRT <- attr(ds, 'maxRT')
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')

  ftarget <- c(ftarget) %>% as.double %>% sort(decreasing = !maximization)
  FValues <- rownames(data) %>% as.numeric
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

  # func <- lapply(func,
  #   function(f)
  #     switch(f,
  #       mean = .mean, median = .median, sd = .sd,
  #       quantile = D_quantile, ERT = SP
  #     )
  # )

  if (1 < 2) {
    data <- data[matched, , drop = FALSE]
    apply(data, 1, D_quantile) %>%
      t %>%
      as.data.table %>%
      cbind(as.data.table(SP(data, maxRT))) %>%
      cbind(algId, ftarget,
            apply(data, 1, .mean),
            apply(data, 1, .median),
            apply(data, 1, .sd), .) %>%
      set_colnames(c('algId', 'target', 'mean', 'median',
                     'sd', paste0(probs * 100, '%'), 'ERT', 'runs', 'ps'))
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

#' @rdname get_RT_sample
#' @param output A character determining the format of output data.table: 'wide' or 'long'
#' @export
get_RT_sample.DataSet <- function(ds, ftarget, output = 'wide', ...) {
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

#' @rdname get_FV_summary
#' @export
#'
get_FV_summary.DataSet <- function(ds, runtime, ...) {
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

#' @rdname get_PAR_name
#' @export
#'
get_PAR_name.DataSet <- function(ds) {
  name <- names(ds)
  name[!(name %in% c('RT', 'RT.summary', 'FV'))]
}

#' @rdname get_PAR_summary
#' @param parId A character vector. Either 'all' or the name of parameters to be retrieved
#' @export
get_PAR_summary.DataSet <- function(ds, ftarget, parId = 'all', ...) {
  FValues <- rownames(ds$RT) %>% as.numeric
  idx <- seq_along(FValues)

  algId <- attr(ds, 'algId')
  par_name <- get_PAR_name(ds)
  if (parId != 'all')
    par_name <- intersect(par_name, parId)
  if (length(par_name) == 0)
    return(NULL)

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

#' @rdname get_PAR_sample
#' @param parId A character vector. Either 'all' or the name of parameters to be retrieved
#' @param output A character. The format of the output data: 'wide' or 'long'
#' @export
get_PAR_sample.DataSet <- function(ds, ftarget, parId = 'all', output = 'wide', ...) {
  N <- length(attr(ds, 'instance'))
  FValues <- rownames(ds$RT) %>% as.numeric
  idx <- seq_along(FValues)

  algId <- attr(ds, 'algId')
  par_name <- get_PAR_name(ds)
  if (parId != 'all')
    par_name <- intersect(par_name, parId)
  if (length(par_name) == 0)
    return(NULL)

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
