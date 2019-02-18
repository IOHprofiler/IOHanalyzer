library(stringr)

tmp <- list()
i <- 1
exdir <- 'data_to_test/tmp'

unlink(file.path(exdir, '*'), recursive = T)

for (file in list.files('data_to_test/data/2019gecco-zip/ins1-11runs/', 
                        pattern = '.zip', recursive = T, full.names = T)) {
  print(file)

  unzip(file, list = FALSE, exdir = exdir)
  path <- str_match(basename(file), "(.*)\\.zip")[2]
  
  tmp[[i]] <- read_dir(file.path(exdir, path), verbose = F)
  i <- i + 1
}

res1 <- do.call(c.DataSetList, tmp)

attr(res1, 'algId') <- paste0(attr(res1, 'algId'), '_1')
for (i in seq_along(res1)) {
  attr(res1[[i]], 'algId') <- paste0(attr(res1[[i]], 'algId'), '_1')
}

saveRDS(res1, file = "data_to_test/data/2019gecco-ins1-11run.rds")

tmp <- list()
i <- 1
unlink(file.path(exdir, '*'), recursive = T)

for (file in list.files('data_to_test/data/2019gecco-zip/ins11-1run/', 
                        pattern = '.zip', recursive = T, full.names = T)) {
  print(file)
  
  unzip(file, list = FALSE, exdir = exdir)
  path <- str_match(basename(file), "(.*)\\.zip")[2]
  
  tmp[[i]] <- read_dir(file.path(exdir, path), verbose = F)
  i <- i + 1
}

res2 <- do.call(c.DataSetList, tmp)

attr(res2, 'algId') <- paste0(attr(res2, 'algId'), '_11')
for (i in seq_along(res2)) {
  attr(res2[[i]], 'algId') <- paste0(attr(res2[[i]], 'algId'), '_11')
}

saveRDS(res2, file = "data_to_test/data/2019gecco-ins11-1run.rds")

res <- c(res1, res2)
saveRDS(res, file = "data_to_test/data/2019gecco.rds")
