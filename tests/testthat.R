library(testthat)
library(IOHanalyzer)
library(data.table)

threads <- getDTthreads()
setDTthreads(1)
test_check("IOHanalyzer")
setDTthreads(threads)
