library(data.table)
library(magrittr)
library(data.table)

dt <- fread('D:/IOH_data/NEVERGRAD/mlda_Wed_Jan_30_18_24_34_PST_2019.csv')

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

  data <- dt[optimizer_name == algId & dimension == DIM & name == funcId,
             .(budget, loss, rescale)]

  for (scaled in unique(data$rescale)){
    if (!is.na(scaled)){
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


