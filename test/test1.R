source('pproc.R')

path <- '~/Dropbox/data/LO_adap_mutationrate/data_f2/IOHProfilerexp_f2_DIM100_i1.cdat'
N <- 1e5

idxEvals <- 1
idx <- 3
cdat <- read_dat(path, subsampling = T)

.min <- lapply(cdat, function(x) min(x[, idx])) %>% unlist %>% min
.max <- lapply(cdat, function(x) max(x[, idx])) %>% unlist %>% max
runtimes <- lapply(cdat, function(x) x[, idxEvals]) %>% unlist %>% unique %>% sort

for (i in seq(N)) {
  cat('.')
  res <- align_non_contiguous(cdat, idx, runtimes)
  
  if (any(res > .max) || any(res < .min) || any(!res %% 1 == 0))
    cat('F')
}



