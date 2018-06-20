align.target <- function(data, targets = NULL) {
  N <- length(data)
  data.iter <- lapply(data, function(d) ihasNext(iter(d, by = 'row')))
  
  if (is.null(target))
    values <- lapply(data, function(x) unique(x$BestF)) %>% unlist %>% sort %>% unique
  
  res <- matrix(NA, length(values), N)
  current_eval <- rep(NA, N)
  for (i in seq_along(values)) {
    target <- values[i]
    for (k in seq_along(data)) {
      d <- data.iter[[k]]
      while (hasNext(d)) {
        current_eval[k] <- nextElem(d)$fct_eval
        if (current_eval[k] >= target) 
          break
      }
    }
    res[i, ] <- current_eval
  }
  row.names(res) <- values
  res
}