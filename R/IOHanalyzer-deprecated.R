##IOHanalyzer-deprecated.R
#' @title Deprecated function in package \pkg{IOHanalyzer}
#' @description The functions listed below are deprecated and will be defunct in
#'   the near future. When possible, alternative functions with similar
#'   functionality are also mentioned.
#' @name IOHanlyzer-deprecated
#' @keywords internal
NULL

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
    .Deprecated("generate_data.Aggr", msg = "This function will be deprecated in the next release of IOHanalyzer.
              Please use the suggested function instead.")
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
    .Deprecated("generate_data.Aggr", msg = "This function will be deprecated in the next release of IOHanalyzer.
              Please use the suggested function instead.")
    
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

# TODO: review / re-write this function 
#TODO: inconsistent use of format_func gives slightly different results between
#generated and uploaded targets
#' Generate ECDF targets for a DataSetList
#'
#' @param data A DataSetList
#' @param format_func function to format the targets
#'
#' @return a vector of targets
#' @export
#' @examples 
#' get_default_ECDF_targets(dsl)
get_default_ECDF_targets <- function(data, format_func = as.integer) {
  .Deprecated("get_ECDF_targets", msg = "This function will be deprecated in the next release of IOHanalyzer.
              Please use the suggested function instead.")
  
  funcIds <- get_funcId(data)
  dims <- get_dim(data)
  
  targets <- list()
  names <- list()
  for (i in seq_along(funcIds)) {
    Id <- funcIds[[i]]
    data_sub <- subset(data, funcId == Id)
    
    for (j in seq_along(dims)) {
      dim <- dims[[j]]
      data_subsub <- subset(data_sub, DIM == dim)
      if (length(data_subsub) == 0) break
      fall <- get_funvals(data_subsub)
      if (length(fall) < 2) break #TODO: double check why this can happen in nevergrad?
      #TODO: Account for minimization / maximization
      fmin <- min(fall)
      fmax <- max(fall)
      
      fseq <- seq_FV(fall, fmin, fmax, length.out = 10) %>% 
        sapply(format_func)
      targets <- append(targets, list(fseq))
      
      if (length(funcIds) == 1) {
        names <- append(names, dim)
      } else if (length(dims) == 1) {
        names <- append(names, Id)
      } else
        names <- append(names, paste0(Id, ";", dim))
    }
  }
  targets %>% set_names(names)
}