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

generate_ECDF_targets <- function(data, format_func = as.integer){

  funcs <- unique(attr(data,'funcId'))
  dims <- unique(attr(data,'DIM'))
  
  targets <- list()
  for(i in seq_along(funcs)){
    func <- funcs[[i]]
    data_sub <- subset(data,funcId == func)
    for(j in seq_along(dims)){
      dim <- dims
      data_subsub <- subset(data_sub,DIM == dim)
      
      fall <- get_Funvals(data_subsub)
      #TODO: Account for minimization / maximization
      fmin <- min(fall)
      fmax <- max(fall)
      
      fseq <- seq_FV(fall, fmin, fmax, length.out = 10) %>% format_func
      targets <- append(targets,list(fseq))
    }
  }
  
  targets
}

calc_ECDF_MULTI <- function(data){

  funcs <- unique(attr(data,'funcId'))
  algs <- unique(attr(data,'algId'))
  dims <- unique(attr(data,'DIM'))
  
  targets <- generate_ECDF_targets(data, function(x) x)
  nr_targets = length(targets)
  cdfs_list <- list()
  
  if(length(algs) != 1)
    return(NULL)
  
  
  algID <- algs[[1]]

  cdfs <- list()
  for( j in seq_along(funcs)){
    fid <- funcs[[j]]
    for( k in seq_along(dims)){
      dim <- dims[[k]]
      target_idx = (length(dims) * j-1) + k
      fseq <- targets[[target_idx]]
      
      # TODO: think of a better way to determine the datapoints
      x <- seq(min(fseq),max(fseq),length.out = 50)
      
      data_subset <- subset(data, funcId == fid, DIM = dim)
      if(length(data_subset) != 1)
        next
      ds <- data_subset[[1]]
      
      cdf <- lapply(fseq, function(f) {
        rt <- get_RT_sample(ds, f, output = 'long')$RT
        if (all(is.na(rt)))
          return(rep(0, length(x)))
        fun <- ecdf(rt)
        fun(x)
      }) %>% 
        do.call(rbind, .)
      cdfs <- append(cdfs,list(cdf))
    }
  
  cdfs_list <- append(cdfs_list, cdfs)
  }
  
  m <- cdfs_list %>% simplify2array %>%
    apply(.,c(1,2),mean)
  
  df_plot <- data.frame(x = seq(50), 
                        mean = apply(m, 2, . %>% mean(na.rm = T)),
                        sd = apply(m, 2, . %>% sd(na.rm = T))) %>% 
    mutate(upper = mean + sd, lower = mean - sd)
  df_plot
}