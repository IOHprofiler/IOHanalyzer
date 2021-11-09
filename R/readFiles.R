# sourceCpp('src/align.cc')
# sourceCpp('src/read.cc')

#' Reduce the size of the data set by evenly subsampling the records
#'
#' @param df The data to subsample
#' @param n The amount of samples
#' @return A smaller data.frame
limit.data <- function(df, n) {
  N <- nrow(df)
  if (N > n) {
    idx <- unique(c(1, seq(1, N, length.out = n), N))
    df[idx, ]
  } else
    df
}

#' Scan *.info files for IOHProfiler or COCO
#'
#' @param folder The folder containing the .info files
#' @return The paths to all found .info-files
#' @export
#' @note This automatically filetrs our files of size 0
#' @examples
#' path <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package="IOHanalyzer")
#' scan_index_file(path)
scan_index_file <- function(folder) {
  folder <- trimws(folder)
  files <- list.files(folder, pattern = '.info$', recursive = T, full.names = T)
  files[file.size(files) > 0]
}

#' Read .info files and extract information
#'
#' @param fname The path to the .info file
#' @return The data contained in the .info file
#' @export
#' @examples
#' path <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package="IOHanalyzer")
#' info <- read_index_file(file.path(path,"IOHprofiler_f1_i1.info"))
read_index_file <- function(fname) {
  tryCatch(
    read_index_file__IOH(fname),
    warning = function(e) read_index_file__COCO(fname),
    error = function(e) read_index_file__COCO(fname),
    finally = function(e) stop(paste0('Error in reading .info files ', e))
  )
}

#' Read IOHprofiler-based .info files and extract information
#'
#' @param fname The path to the .info file
#' @return The data contained in the .info file
#' @noRd
read_index_file__IOH <- function(fname) {
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
    # TODO: Fix this
    name_value <- read.csv(text = lines[1], header = F, quote = c("\"","'")) %>%
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
          
          #Conversion to logical / numeric values when applicable (1 / 0 are seen as numeric)
          val_orig <- value
          value <- suppressWarnings(as.numeric(val_orig))
          if (is.na(value)) value <- as.logical(val_orig)
          if (!is.na(value)) ans[[name]] <- value
          
          # if (name == 'maximization')
          #   value <- as.logical(value)
          # else
          #   value <- suppressWarnings(as.numeric(value)) # convert quoted numeric values to numeric
          # 
          # if (!is.na(value)) ans[[name]] <- value
        }
        ans
      }

    record <- trimws(strsplit(lines[3], ',')[[1]])

    # TODO: this must also be removed...
    if (record[2] == "") {
      warning(sprintf('File %s is incomplete!', fname))
      finalFVs <- NULL
      instances <- NULL
      maxRTs <- NULL
    } else {
      res <- matrix(unlist(strsplit(record[-1], ':')), nrow = 2)
      info <- matrix(unlist(strsplit(res[2, ], '\\|')), nrow = 2)  
      #Check for incorrect usages of reset_problem and remove them
      maxRTs <- as.numeric(info[1,])
      idx_correct <- which(maxRTs > 0)
      finalFVs <- as.numeric(info[2,])[idx_correct]
      instances <- as.numeric(res[1,])[idx_correct]
      maxRTs <- maxRTs[idx_correct]
    }
    
    record[1] <- gsub("\\\\", "/", record[1])
    datafile <- file.path(path, record[1])

    # TODO: check the name of the attributes and fix them!
    data[[i]] <- c(
      header,
      list(
        comment = lines[2],
        datafile = datafile,
        instance = instances,
        maxRT = maxRTs,
        finalFV = finalFVs
      )
    )
    i <- i + 1
  }
  close(f)
  
  datafiles <- unlist(lapply(data, function(x) x$datafile))
  if (length(datafiles) > length(unique(datafiles)))
    return(merge_indexinfo(data))
  else 
    return(data)
}

#' Process IOHprofiler-based .info files if they contain multiple references
#' to a single data-file
#'
#' This is needed to assure that the meta-information is concatenated properly
#' and no datafile is processed more often than nessecary
#'
#' @param indexInfo The info-list to reduce
#' @return a reduced version of the provided indexInfo, preserving original order
#' @noRd
merge_indexinfo <- function(indexInfo) {
  datafiles <- unlist(lapply(indexInfo, function(x) x$datafile))
  lapply(unique(datafiles), function(dfile) {
    new_info <- list()
    idxs <- datafiles == dfile
    infos <- indexInfo[idxs]
    nr_runs <- length(unlist(lapply(infos, function(x) x$instance)))
    for (a in attributes(infos[[1]])$names) {
      temp <- unlist(lapply(infos, function(x) x[[a]]))
      if (length(temp) == nr_runs)
        new_info[[a]] <- temp
      else
        new_info[[a]] <- unique(temp)
    }
    new_info
  })
}

#' Read single-objective COCO-based .info files and extract information
#'
#' @param fname The path to the .info file
#' @return The data contained in the .info file
#' @noRd
read_index_file__COCO <- function(fname) {
  f <- file(fname, 'r')
  path <- dirname(fname)
  data <- list()
  i <- 1
  while (TRUE) {
    
    lines <- suppressWarnings(readLines(f, n = 3))  # read header and comments
    if (length(lines) < 3) {
      break
    }
    comment <- lines[2]
    name_value <- as.vector(unlist(as.list(read.csv(text = lines[1], header = F, quote = "'"))))
    
    header <- trimws(name_value) %>% {
      regmatches(., regexpr("=", .), invert = T)  # match the first appearance of '='
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
    
    record <- strsplit(lines[3], ',')[[1]] %>% trimws
    
    if (length(record) < 2) {
      warning(sprintf('File %s is incomplete!', fname))
      res <- NULL
      info <- NULL
    } else {
      res <- matrix(unlist(strsplit(record[-c(1)], ':')), nrow = 2)
      info <- matrix(as.numeric(unlist(strsplit(res[2, ], '\\|'))), nrow = 2)
    }
    
    record[1] <-  gsub("\\\\", "/", record[1])
    if ('folder' %in% names(header))
      datafile <- file.path(path, header$folder, record[1])
    else
      datafile <- file.path(path, record[1])
    
    
    # TODO: check the name of the attributes and fix them!
    data[[i]] <- c(
      header,
      list(
        comment = comment,
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

#' Read bi-objective COCO-based .info files and extract information
#'
#' @param fname The path to the .info file
#' @return The data contained in the .info file
#' @noRd
read_index_file__BIOBJ_COCO <- function(fname) {
  f <- file(fname, 'r')
  path <- dirname(fname)
  data <- list()
  i <- 1

  lines <- suppressWarnings(readLines(f, n = 2))  # read header and comments
  comment <- lines[2]
  name_value <- as.vector(unlist(as.list(read.csv(text = lines[1], header = F, quote = "'"))))

  header <- trimws(name_value) %>% {
      regmatches(., regexpr("=", .), invert = T)  # match the first appearance of '='
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
      res <- matrix(unlist(strsplit(record[-c(1, 2, 3)], ':')), nrow = 2)
      info <- matrix(as.numeric(unlist(strsplit(res[2, ], '\\|'))), nrow = 2)
    }

    record[3] <-  gsub("\\\\", "/", record[3])
    if ('folder' %in% names(header))
      datafile <- file.path(path, header$folder, record[3])
    else
      datafile <- file.path(path, record[3])

    funcId <- trimws(strsplit(record[1], '=')[[1]][2])
    funcId.int <- suppressWarnings(as.integer(funcId.int))
    if(!any(is.na(funcId.int))) {
      if(all((funcId.int >= 0L) & (funcId.int <= 1000000000L))) {
        funcId <- funcId.int
      }
    }

    DIM <- as.numeric(trimws(strsplit(record[2], '=')[[1]][2]))

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
#' @return The format of the data in the given folder. Either 'COCO', 'IOHprofiler',
#' 'NEVERGRAD' or 'SOS'.
#' @export
#' @examples
#' path <- system.file("extdata", "ONE_PLUS_LAMDA_EA", package = "IOHanalyzer")
#' check_format(path)
check_format <- function(path) {
  if (sub('[^\\.]*\\.', '', basename(path), perl = T) == "csv")
    return(NEVERGRAD)
  
  if (sub('[^\\.]*\\.', '', basename(path), perl = T) == "rds")
    return("RDS")
  
  index_files <- scan_index_file(path)
  if (length(index_files) == 0) 
    return(SOS)
  
  info <- unlist(lapply(index_files, read_index_file), recursive = F)
  datafile <- sapply(info, function(item) item$datafile)

  format <- lapply(datafile, function(file) {
    tryCatch({
      if (!file.exists(file)) {
        cdatfile <- stri_replace(file, ".cdat", fixed = ".dat")
        tdatfile <- stri_replace(file, ".tdat", fixed = ".dat")
        if (file.exists(cdatfile)) file <- cdatfile
        else file <- tdatfile
      }
      first_line <- scan(file, what = 'character', sep = '\n', n = 1, quiet = T)
    }, error = function(e) {
      stop("Error detecting data files specified in .info, please verify the
             integrity of the provided files.")
    })

    if (startsWith(first_line, '% function') || startsWith(first_line, '% f evaluations'))
      COCO
    else if (startsWith(first_line, '\"function')) {
      n_col <- ncol(fread(file, header = FALSE, sep = ' ',
                         colClasses = 'character', fill = T, nrows = 1))
      if (n_col == 2)
        TWO_COL
      else
        IOHprofiler
    }
    else if (first_line == '%')  # Bi-objective COCO format...
      BIBOJ_COCO
    else {
      stop("Error detecting file format of file ", file, "; Please verify
           the integrity of this file.")
    }
  }) %>%
    unlist %>%
    unique

  csv_files <- file.path(path, list.files(path, pattern = '.csv', recursive = T))
  if (length(csv_files) > 0)
    format <- c(format, NEVERGRAD)
  
  txt_files <- file.path(path, list.files(path, pattern = '.txt', recursive = T))
  if (length(txt_files) > 0)
    format <- c(format, SOS)

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
#' @noRd
#' @return A list of data.frames
read_dat <- function(fname, subsampling = FALSE) {
  # TODO: use the same data loading method as in read_dat__COCO
  df <- fread(fname, header = FALSE, sep = ' ', colClasses = 'character', fill = T)
  colnames(df) <- as.character(df[1, ])
  idx <- which(!grepl('\\d+', df[[1]], perl = T))

  # check for data consistence
  header_len <- min(apply(df[idx, ] != "", 1, sum))
  idx <- c(idx, nrow(df) + 1)
  df <- df[, 1:header_len]

  # turn off the warnings of the data coersion below
  options(warn = -1)
  # TOOD: this opeartor is the bottelneck
  df <- sapply(df, function(c) {class(c) <- 'numeric'; c})
  options(warn = 0)

  res <- lapply(seq(length(idx) - 1), function(i) {
    i1 <- idx[i] + 1
    i2 <- idx[i + 1] - 1
    ans <- df[i1:i2, ]
    if (i1 == i2)
      ans <- as.matrix(t(ans))

    # TODO: determine the number of record in the 'efficient mode'
    if (subsampling)
      ans <- limit.data(ans, n = 500)
    else
      ans
  })
  res
}

# TODO: this method is deprecated. Remove it later
# TODO: maybe not subsampling for COCO data
#' read COCO '.dat'-like file
#'
#' @param fname The path to the .dat file
#' @param subsampling Whether to subsample the data or not
#' @noRd
#' @return A list of data.frames
read_dat__COCO_ <- function(fname, subsampling = FALSE) {
  c_read_dat(path.expand(fname), 7, '%')
}

#' read COCO '.dat'-like file directly in R
#'
#' @param fname The path to the .dat file
#' @param subsampling Whether to subsample the data or not
#' @noRd
#' @return A list of data.frames
read_dat__COCO <- function(fname, subsampling = FALSE) {
  select <- seq(5)
  # read the file as a character vector (one string per row)
  X <- fread(fname, header = FALSE, sep = '\n', colClasses = 'character')[[1]]
  idx <- which(startsWith(X, '%'))
  X <- gsub('\\s+|\\t', ' ', X, perl = T)

  df <- fread(text = X[-idx], header = F, sep = ' ', select = select, fill = T)
  idx <- c((idx + 1) - seq_along(idx), nrow(df))

  lapply(seq(length(idx) - 1),
         function(i) {
           i1 <- idx[i]
           i2 <- idx[i + 1] - 1
           as.matrix(df[i1:i2, ])
         })
}

read_dat__BIOBJ_COCO <- function(fname, subsampling = FALSE) {
  if (endsWith(fname, '.dat'))
    select <- seq(3)
  else if (endsWith(fname, '.tdat'))
    select <- seq(2)

  # read the file as a character vector (one string per row)
  X <- fread(fname, header = FALSE, sep = '\n', colClasses = 'character')[[1]]
  idx <- which(startsWith(X, '%'))
  X <- gsub('\\s+|\\t', ' ', X, perl = T)

  df <- fread(text = X[-idx], header = F, sep = ' ', select = select, fill = T)
  idx <- which(startsWith(X, '% function'))
  idx <- c((idx + 1) - seq_along(idx) * 4, nrow(df))

  lapply(seq(length(idx) - 1), function(i) {
    i1 <- idx[i]
    i2 <- idx[i + 1] - 1
    as.matrix(df[i1:i2, ])
  })
}

# global variables for the alignment functions
idxEvals <- 1
idxTarget <- 3
n_data_column <- 5

# TODO: add docs to the following three functions
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

#' Align data by runtimes
#' @param data The data to align
#' @param format Whether the data is form IOHprofiler or COCO
#' @param include_param Whether to include the recorded parameters in the alignment
#' @noRd
#' @return Data aligned by the running time
align_running_time <- function(data, format = IOHprofiler, include_param = TRUE,
                               maximization = TRUE) {
  if (format == IOHprofiler)
    idxTarget <- 3
  else if (format == COCO)
    idxTarget <- 3
  else if (format == BIBOJ_COCO) {
    n_data_column <- 3
    idxTarget <- 2
  }
  else if (format == TWO_COL) {
    n_data_column <- 2
    idxTarget <- 2
  }

  FV <- sort(unique(unlist(lapply(data, function(x) x[, idxTarget]))),
             decreasing = !maximization)
  n_column <- unique(sapply(data, ncol))

  if (format == COCO) {
    n_param <- 0
    idxValue <- idxEvals
    param_names <- NULL
  }
  else if (format == IOHprofiler) {
    n_param <- n_column - n_data_column
    if (include_param && n_param > 0) {
      param_names <- colnames(data[[1]])[(n_data_column + 1):n_column]
      idxValue <- c(idxEvals, (n_data_column + 1):n_column)
    }
    else {
      param_names <- NULL
      idxValue <- idxEvals
    }
  }
  else {
    param_names <- NULL
    idxValue <- idxEvals
  }

  res <- c_align_running_time(data, FV, idxValue - 1, maximization, idxTarget - 1)
  names(res) <- c('RT', param_names)
  res
}

#' Align data by function values
#' @param data The data to align
#' @param format Whether the data is form IOHprofiler or COCO.
#' @param include_param Whether to include the recorded parameters in the alignment
#' @noRd
#' @return Data aligned by the function value
align_function_value <- function(data, include_param = TRUE, format = IOHprofiler) {
  n_column <- unique(sapply(data, ncol))
  stopifnot(length(n_column) == 1)

  if (format == COCO) {
    maximization <- FALSE
    idxTarget <- 3
    n_param <- 0
  }
  else if (format == IOHprofiler) {
    maximization <- TRUE
    idxTarget <- 3
    n_param <- n_column - n_data_column
  }
  else if (format == BIBOJ_COCO) {  # bi-objective COCO format
    maximization <- FALSE
    idxTarget <- 2
    n_data_column <- 2
    n_param <- 0                   # no parameter is allowed in this case
  }
  else if (format == TWO_COL) {
    maximization <- TRUE
    idxTarget <- 2
    n_param <- 0
  }

  if (check_contiguous(data)) {
    nrow <- sapply(data, nrow) %>% max
    runtime <- seq(nrow)
    align_func <- align_contiguous
  } else {
    runtime <- sort(unique(unlist(lapply(data, function(x) x[, idxEvals]))))
    nrow <- length(runtime)
    align_func <- align_non_contiguous
  }

  FV <- align_func(data, idxTarget, runtime)
  include_param <- include_param && (n_param > 0)

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


#' Read Nevergrad data
#'
#' Read .csv files in nevergrad format and extract information as a DataSetList
#'
#' @param fname The path to the .csv file
#' @return The DataSetList extracted from the .csv file provided
#' @noRd
read_nevergrad <- function(path){
  dt <- fread(path)

  if (!'name' %in% colnames(dt)) {
    dt[, name := function_class]
  }
  
  triplets <- unique(dt[, .(optimizer_name, dimension, name)])
  algIds <- unique(triplets$optimizer_name)
  DIMs <- unique(triplets$dimension)
  funcIds <- unique(triplets$name)

  res <- list()

  idx <- 1

  for (i in seq(nrow(triplets))) {
    algId <- triplets$optimizer_name[i]
    DIM <- triplets$dimension[i]
    funcId <- triplets$name[i]

    rescale_name <- 'rescale'
    if ( !('rescale' %in% colnames(dt))) {
      if ( 'transform' %in% colnames(dt))
        colnames(dt)[colnames(dt) == "transform"] <- "rescale"
      else{
        dt$rescale <- NA
      }
    }

    data <- dt[optimizer_name == algId & dimension == DIM & name == funcId,
               .(budget, loss, rescale)]

    for (scaled in unique(data$rescale)) {
      if (!is.na(scaled)) {
        data_reduced <- data[rescale == scaled, .(budget, loss)]
      }
      else {
        data_reduced <- data[is.na(rescale), .(budget, loss)]
      }

      if (!is.na(scaled) && scaled) {
        funcId_name <- paste0(funcId, '_rescaled')
      }
      else {
        funcId_name <- funcId
      }

      rows <- unique(data_reduced$budget) %>% sort
      FV <- lapply(rows,
             function(b) {
               data_reduced[budget == b, loss]
             }
          ) %>%
        do.call(rbind, .) %>%
        set_rownames(rows)

      RT <- list()

      ds <-  structure(list(RT = RT, FV = FV),
                       class = c('DataSet', 'list'),
                       maxRT = max(rows),
                       finalFV = min(FV),
                       format = 'NEVERGRAD',
                       maximization = FALSE,
                       algId = algId,
                       funcId = funcId_name,
                       DIM = DIM)
      res[[idx]] <- ds
      idx <- idx + 1
    }
  }
  class(res) %<>% c('DataSetList')
  attr(res, 'DIM') <- DIMs
  attr(res, 'funcId') <- funcIds
  attr(res, 'algId') <- algIds
  attr(res, 'suite') <- 'NEVERGRAD'
  attr(res, 'maximization') <- F
  res

}

#' Read single DataSet of SOS-based data
#' 
#' Read single .txt files in SOS format and extract information as a DataSet
#'
#' @param file The path to the .txt file
#' @return The DataSet extracted from the .txt file provided
#' @noRd
read_single_file_SOS <- function(file) {
  V1 <- NULL #Local binding to remove CRAN warnings
  
  algId <- substr(basename(file), 1,  stringi::stri_locate_last(basename(file), fixed = 'D')[[1]] - 1)
  
  dt <- fread(file, header = F)
  header <- scan(file, what = 'character', sep = '\n', n = 1, quiet = T)
  splitted <- header %>% trimws %>% strsplit("\\s+") %>% .[[1]] %>% .[2:length(.)]
  info <- list(algId = algId)
  for (i in seq_len(length(splitted) / 2)) {
    temp <- splitted[[2*i]]
    name <- splitted[[2*i - 1]]
    if (name == 'function') name <- 'funcId'
    if (name == 'dim') name <- 'DIM'
    names(temp) <- name
    info <- c(info, temp)
  }
  
  dim <- as.numeric(info$DIM)
  
  
  RT_raw <- dt[[colnames(dt)[[ncol(dt) - dim - 1]]]]
  names(RT_raw) <- dt[[colnames(dt)[[ncol(dt) - dim - 2]]]]
  RT <- as.matrix(RT_raw)
  mode(RT) <- 'integer'
  
  FV_raw <- dt[[colnames(dt)[[ncol(dt) - dim - 2]]]]
  names(FV_raw) <- dt[[colnames(dt)[[ncol(dt) - dim - 1]]]]
  FV <- as.matrix(FV_raw)
  
  
  pos <- dt[, (ncol(dt) - dim + 1):ncol(dt)]
  colnames(pos) <- as.character(seq_len(dim))
  
  maxRT <- max(RT)
  finalFV <- min(FV)
  
  if (sum(FV == finalFV) > 1) {
    #Reconstruct population to determine which best solution is final position
    ids_min <- dt[FV_raw == finalFV, V1]
    replaced_idxs <- dt[[colnames(dt)[[ncol(dt) - dim]]]]
    #If none, take the last one added
    pos_idx <- max(ids_min)
    for (i in ids_min) {
      if (all(replaced_idxs != i)) {
        #If multiple, take the first one added
        pos_idx <- i
        break
      }
    }
    final_pos <- as.numeric(pos[pos_idx, ])
  }
  else {
    final_pos <- as.numeric(pos[which.min(FV), ])
  }
  
  PAR <- list(
    # 'position' = list(pos),
    'final_position' = list(final_pos),
    'by_FV' = NULL,
    'by_RT' = NULL
  )
  
  
  
  object <- list()
  class(object) <- c('DataSet', class(object))
  object$RT <- RT
  object$FV <- FV
  object$PAR <- PAR
  attr(object, 'maxRT') <- maxRT
  attr(object, 'finalFV') <- finalFV
  attr(object, 'format') <- "SOS"
  attr(object, 'maximization') <- F
  attr(object, 'suite') <- "SOS"
  for (i in seq_along(info)) {
    attr(object, names(info)[[i]]) <- type.convert(info[[i]], as.is = T)
  }
  object
}


#' Read DataSetList of SOS-based data
#' 
#' Read directory containing .txt files in SOS format and extract information as a DataSetList
#'
#' @param dir The path to the directory file
#' @param corrections_file A file containing boundary-correction ratios for the files in `dir`
#' @return The DataSetList extracted from the directory provided
#' @noRd
read_datasetlist_SOS <- function(dir, corrections_files = NULL) {
  V1 <- V3 <- V4 <- NULL #Local binding to remove CRAN warnings
  res <- list()
  dims <- list()
  funcIds <- list()
  algIds <- list()
  suites <- list()
  maximizations <- list()
  
  idx <- 1
  
  corrs <- as.data.table(rbindlist(lapply(corrections_files, fread)))
  
  for (f in list.files(dir, recursive = T, pattern = "*.txt", full.names = T)) {
    if (f %in% corrections_files) next
    ds <- read_single_file_SOS(f)
    
    dims[[idx]] <- attr(ds, 'DIM')
    funcIds[[idx]] <- attr(ds, 'funcId')
    algIds[[idx]] <- attr(ds, 'algId')
    suites[[idx]] <- attr(ds, 'suite')
    maximizations[[idx]] <- attr(ds, 'maximization')
    
    if (nrow(corrs) > 0) {
      fn <- substr(basename(f), 1, nchar(basename(f)) - 4)
      corr_opts <- corrs[V1 == fn, ]
      if (stri_detect_fixed(fn, "DE")) {
        corr <- corr_opts[V3 == attr(ds, 'F'), ][V4 == attr(ds, 'CR'), 'V2'][['V2']]
      }
      else if (stri_detect_fixed(fn, "RIS")) {
        corr <- corr_opts[['V2']]
      }
      else {
        warning("Unknown algorithm, so skipping lookup of boundary corrections ratio")
        corr <- NULL
      }
      if (length(corr) == 1)
        ds$PAR$'corrections' <- corr[[1]]
      else
        warning(paste0("No boundary corrections ratio found for ", fn))
    }
    
    res[[idx]] <- ds
    idx <- idx + 1
  }
  class(res) %<>% c('DataSetList')
  attr(res, 'DIM') <- dims
  attr(res, 'funcId') <- funcIds
  attr(res, 'algId') <- algIds
  
  suite <- unique(suites)
  maximization <- unique(maximizations)
  if (length(suite) != 1 || length(maximization) != 1) {
    warning("Multipe different suites detected!")
  }
  
  attr(res, 'suite') <- suite
  attr(res, 'maximization') <- maximization
  res
  clean_DataSetList(res)
}

#' Find corrections-files in SOS-based folder
#' 
#' Read directory containing .txt files in SOS format and extract the corrections-files
#'
#' @param path The path to the directory file
#' @return The relative paths to the corection files
#' @noRd
locate_corrections_files <- function(path) {
  files <- list.files(path, recursive = T, pattern = "*.txt", full.names = T)
  files[stri_detect_fixed(files, 'corrections')]
}
