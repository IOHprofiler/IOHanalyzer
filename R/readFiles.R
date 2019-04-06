#' reduce the size of the data set by evenly subsampling the records
#'
#' @param df The data to subsample
#' @param n The amount of samples
#' @return A smaller data.frame
#' @export
limit.data <- function(df, n) {
  N <- nrow(df)
  if (N > n) {
    idx <- c(1, seq(1, N, length.out = n), N) %>% unique
    df[idx, ]
  } else
    df
}

#' Scan *.info files for IOHProfiler or COCO
#'
#' @param folder The folder containing the .info files
#' @return The paths to all found .info-files
#' @export
scan_IndexFile <- function(folder) {
  folder <- trimws(folder)
  file.path(folder, list.files(folder, pattern = '.info', recursive = T))
}


#' Read .info files and extract information
#'
#' @param fname The path to the .info file
#' @return The data contained in the .info file
#' @export
#'
read_IndexFile <- function(fname) {
  tryCatch(read_IndexFile_IOH(fname),
           warning = function(e) read_IndexFile_BIOBJ_COCO(fname),
           error = function(e) read_IndexFile_BIOBJ_COCO(fname),
           finally = function(e) stop(paste0('Error in reading .info files ', e)))
}


#' Read IOHprofiler-based .info files and extract information
#'
#' @param fname The path to the .info file
#' @return The data contained in the .info file
#' @export
#'
read_IndexFile_IOH <- function(fname) {
  f <- file(fname, 'r')
  path <- dirname(fname)
  data <- list()
  i <- 1

  while (TRUE) {
    # TODO: remove suppressWarnings later
    lines <- suppressWarnings(readLines(f, n = 3))
    if (length(lines) == 0)
      break

    # TODO: make this quote symbol ' or " as the configurable parameter
    # or simply fix it
    name_value <- read.csv(text = lines[1], header = F, quote = "'") %>%
      as.list %>%
      unlist %>%
      as.vector

    header <- name_value %>%
      trimws %>% {
        regmatches(., regexpr("=", .), invert = T) # match the first appearance of '='
      } %>%
      unlist %>%
      trimws %>%
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

    # TODO: this must also be removed...
    if (record[2] == "") {
      warning(sprintf('File %s is incomplete!', fname))
      res <- NULL
      info <- NULL
    } else {
      res <- strsplit(record[-1], ':') %>% unlist %>% matrix(nrow = 2)
      info <- strsplit(res[2, ], '\\|') %>% unlist %>% matrix(nrow = 2)
    }

    record[1] <-  gsub("\\\\", "/", record[1])
    datafile <- file.path(path, record[1])

    # TODO: check the name of the attributes and fix them!
    data[[i]] <- c(
      header,
      list(
        comment = lines[2],
        datafile = datafile,
        instance = as.numeric(res[1, ]),
        maxRT = as.numeric(info[1, ]),
        finalFV = as.numeric(info[2, ])
      )
    )
    i <- i + 1
  }
  close(f)
  data
}

#' Read Biobjective COCO-based .info files and extract information
#'
#' @param fname The path to the .info file
#' @return The data contained in the .info file
#' @export
#'
read_IndexFile_BIOBJ_COCO <- function(fname) {
  f <- file(fname, 'r')
  path <- dirname(fname)
  data <- list()
  i <- 1

  lines <- suppressWarnings(readLines(f, n = 2))  # read header and comments
  comment <- lines[2]
  name_value <- read.csv(text = lines[1], header = F, quote = "'") %>%
    as.list %>% unlist %>% as.vector

  header <- name_value %>%
    trimws %>% {
      regmatches(., regexpr("=", .), invert = T) # match the first appearance of '='
    } %>%
    unlist %>%
    trimws %>%
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

  names(header) <- gsub('algorithm', 'algId', names(header))

  while (TRUE) {
    # TODO: remove suppressWarnings later
    lines <- suppressWarnings(readLines(f, n = 1))
    if (length(lines) == 0)
      break

    record <- strsplit(lines[1], ',')[[1]] %>% trimws

    # TODO: this must also be removed...
    if (record[4] == "") {
      warning(sprintf('File %s is incomplete!', fname))
      res <- NULL
      info <- NULL
    } else {
      res <- strsplit(record[-c(1, 2, 3)], ':') %>% unlist %>% matrix(nrow = 2)
      info <- strsplit(res[2, ], '\\|') %>% unlist %>% as.numeric %>% matrix(nrow = 2)
    }

    record[3] <-  gsub("\\\\", "/", record[3])
    if ('folder' %in% names(header))
      datafile <- file.path(path, header$folder, record[3])
    else
      datafile <- file.path(path, record[3])

    funcId <- strsplit(record[1], '=')[[1]][2] %>% trimws %>% as.numeric
    DIM <- strsplit(record[2], '=')[[1]][2] %>% trimws %>% as.numeric

    # TODO: check the name of the attributes and fix them!
    data[[i]] <- c(
      header,
      list(
        comment = comment,
        funcId = funcId,
        DIM = DIM,
        datafile = datafile,
        instance = as.numeric(res[1, ]),
        maxRT = info[1, ],
        finalFV = info[2, ]
      )
    )
    i <- i + 1
  }
  close(f)
  data
}

#' Check the format of data
#'
#' Throws a warning when multiple formats are found in the same folder.
#'
#' @param path The path to the folder to check
#' @return The format of the data in the given folder. Either 'COCO' or 'IOHprofiler'.
#' @export
#'
check_format <- function(path) {
  index_files <- scan_IndexFile(path)
  info <- lapply(index_files, read_IndexFile) %>% unlist(recursive = F)
  datafile <- sapply(info, function(item) item$datafile)

  format <- lapply(datafile, function(file) {
    first_line <- scan(file, what = 'character', sep = '\n', n = 1, quiet = T)
    if (startsWith(first_line, '% function'))
      COCO
    else if (startsWith(first_line, '\"function'))
      IOHprofiler
    else if (first_line == '%')  # Bi-objective COCO format...
      BIBOJ_COCO
  }) %>%
    unlist %>%
    unique

  if (length(format) > 1) {
    stop(
      paste(
        path,
        'contains multiple data formats. This is not allowed for data processing.
        Please check the returned dataframe for more information.'
      )
      )
  } else
    format
  }

#' Read IOHProfiler *.dat files
#'
#' @param fname The path to the .dat file
#' @param subsampling Whether to subsample the data or not
#' @return A list of data.frames
#' @export
#'
read_dat <- function(fname, subsampling = FALSE) {
  # TODO: use the same data loading method as in read_COCO_dat
  df <- fread(fname, header = FALSE, sep = ' ', colClasses = 'character', fill = T)
  idx <- which(!grepl('\\d+', df[[1]], perl = T))

  # check for data consistence
  header_len <- apply(df[idx, ] != "", 1, sum) %>% min
  idx %<>% c(nrow(df) + 1)
  df <- df[, 1:header_len]

  # turn off the warnings of the data coersion below
  options(warn = -1)
  columns <- colnames(df) <- as.matrix(df[1, ])
  # TOOD: this opeartor is the bottelneck
  df %<>% sapply(function(c) {class(c) <- 'numeric'; c})
  # df <- df[, lapply(.SD, as.numeric)]
  # df %<>% mutate_all(funs(as.numeric(.)))
  options(warn = 0)

  res <- lapply(seq(length(idx) - 1), function(i) {
    i1 <- idx[i] + 1
    i2 <- idx[i + 1] - 1
    ans <- df[i1:i2, ]
    if (i1 == i2)
      ans <- t(ans) %>% as.matrix

    # TODO: determine the number of record in the 'efficient mode'
    if (subsampling)
      ans <- limit.data(ans, n = 500)
    else
      ans
  })
  res
}

# TODO: maybe not subsampling for COCO data
#' read COCO '.dat'-like file
#'
#' @param fname The path to the .dat file
#' @param subsampling Whether to subsample the data or not
#' @return A list of data.frames
#' @export
#'
read_COCO_dat <- function(fname, subsampling = FALSE) {
  c_read_dat(path.expand(fname), 7, '%')
}

read_COCO_dat2 <- function(fname, subsampling = FALSE) {
  select <- seq(5)
  # read the file as a character vector (one string per row)
  X <- fread(fname, header = FALSE, sep = '\n', colClasses = 'character')[[1]]
  idx <- which(startsWith(X, '%'))
  X <- gsub('\\s+|\\t', ' ', X, perl = T)

  header <- gsub(' \\| ', '|', X[1], perl = T) %>%
    gsub('\\.\\.\\.|% ', '', ., perl = T) %>% {
      strsplit(., split = '\\|')[[1]][select]
    }

  df <- fread(text = X[-idx], header = F, sep = ' ', select = select, fill = T)
  idx <- c((idx + 1) - seq_along(idx), nrow(df))

  lapply(seq(length(idx) - 1), function(i) {
    i1 <- idx[i]
    i2 <- idx[i + 1] - 1
    as.matrix(df[i1:i2, ])
  })
}

read_BIOBJ_COCO_dat <- function(fname, subsampling = FALSE) {
  if (endsWith(fname, '.dat'))
    select <- seq(3)
  else if (endsWith(fname, '.tdat'))
    select <- seq(2)

  # read the file as a character vector (one string per row)
  X <- fread(fname, header = FALSE, sep = '\n', colClasses = 'character')[[1]]
  idx <- which(startsWith(X, '%'))
  X <- gsub('\\s+|\\t', ' ', X, perl = T)

  header <- gsub(' \\| ', '|', X[4], perl = T) %>%
    gsub('\\.\\.\\.|% ', '', ., perl = T) %>% {
      strsplit(., split = '\\|')[[1]][select]
    }

  df <- fread(text = X[-idx], header = F, sep = ' ', select = select, fill = T)

  idx <- which(startsWith(X, '% function'))
  idx <- c((idx + 1) - seq_along(idx) * 4, nrow(df))

  lapply(seq(length(idx) - 1), function(i) {
    i1 <- idx[i]
    i2 <- idx[i + 1] - 1
    as.matrix(df[i1:i2, ])
  })
}

# TODO: double check the index of the target column
# global variables for the alignment
idxEvals <- 1
idxTarget <- 3
n_data_column <- 5

#' Align data by runtimes
#' @param data The data to align
#' @param format Whether the data is form IOHprofiler or COCO.
#' @return Data aligned by runtime
#' @export
#'
align_runtime <- function(data, format = IOHprofiler) {
  if (format == IOHprofiler) {
    maximization <- TRUE
    idxTarget <- 3
  } else if (format == COCO) {
    maximization <- FALSE
    idxTarget <- 3
  } else if (format == BIBOJ_COCO) {
    maximization <- FALSE
    n_data_column <- 3
    idxTarget <- 2
  } else if (format == TWO_COL) {
    maximization <- TRUE
    n_data_column <- 2
    idxTarget <- 2
  }

  FV <- lapply(data, function(x) x[, idxTarget]) %>%
    unlist %>%
    unique %>%
    sort(decreasing = !maximization)

  n_rows <- sapply(data, nrow)
  n_column <- sapply(data, ncol) %>% unique

  if (format == COCO) {
    n_param <- 0
    idxValue <- idxEvals
    param_names <- NULL
  } else if (format == IOHprofiler) {
    n_param <- n_column - n_data_column
    if (n_param > 0) {
      param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
      idxValue <- c(idxEvals, (n_data_column + 1):n_column)
    } else {
      param_names <- NULL
      idxValue <- idxEvals
    }
  } else {
    param_names <- NULL
    idxValue <- idxEvals
  }

  c_align_runtime(data, FV, idxValue - 1, maximization, idxTarget - 1) %>%
    set_names(c('RT', param_names))
}


check_contiguous <- function(data) {
  sapply(data,
         function(d) {
           v <- d[, idxEvals]
           N <- length(v)
           v[1] == 1 && v[N] == N
         }) %>%
    all
}

align_contiguous <- function(data, idx, rownames) {
  N <- length(data)
  nrow <- length(rownames)
  lapply(data,
         function(d) {
           v <- d[, idx]
           r <- nrow - length(v)
           if (r > 0) {
             v <- c(v, rep(v[length(v)], r))
           }
           v
         }) %>%
    unlist %>%
    matrix(nrow = nrow, ncol = N) %>%
    set_rownames(rownames)
}

align_non_contiguous <- function(data, idx, rownames) {
  N <- length(data)
  nrow <- length(rownames)
  lapply(data,
         function(d) {
           c_impute(d[, idx], d[, idxEvals], rownames)
         }) %>%
    unlist %>%
    matrix(nrow = nrow, ncol = N) %>%
    set_rownames(rownames)
}


#' Align data by function values
#' @param data The data to align
#' @param format Whether the data is form IOHprofiler or COCO.
#' @param include_param Whether to include the recorded parameters in the alignment
#' @return Data aligned by function value
#' @export
#'
align_function_value <- function(data, include_param = TRUE, format = IOHprofiler) {
  N <- length(data)
  n_column <- sapply(data, ncol) %>% unique
  stopifnot(length(n_column) == 1)

  if (format == COCO) {
    maximization <- FALSE
    idxTarget <- 3
    n_param <- 0
  } else if (format == IOHprofiler) {
    maximization <- TRUE
    idxTarget <- 3
    n_param <- n_column - n_data_column
  } else if (format == BIBOJ_COCO) {  # new bi-objective COCO format
    maximization <- FALSE
    idxTarget <- 2
    n_data_column <- 2
    n_param <- 0                      # no parameter is allowed in this case
  } else if (format == TWO_COL) {
    maximization <- TRUE
    idxTarget <- 2
    n_param <- 0
  }

  include_param <- include_param && (n_param > 0)

  if (check_contiguous(data)) {
    nrow <- sapply(data, nrow) %>% max
    runtime <- seq(nrow)
    align_func <- align_contiguous
  } else {
    runtime <- lapply(data, function(x) x[, idxEvals]) %>% unlist %>% unique %>% sort
    nrow <- length(runtime)
    align_func <- align_non_contiguous
  }

  FV <- align_func(data, idxTarget, runtime)

  if (include_param) {
    param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
    param <- list()
    for (i in seq(n_param)) {
      name <- param_names[i]
      param[[name]] <- align_func(data, i + n_data_column, runtime)
    }
  }

  if (include_param) {
    c(list(FV = FV), param)
  } else {
    list(FV = FV)
  }
}
