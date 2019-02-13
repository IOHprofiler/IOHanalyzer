# This file contains some functions for reading and writing to the repository
#
# Author: Diederick Vermetten
# Email: d.vermetten@gmail.com

library(DBI)

#TODO: open and close the connection when needed
con <- dbConnect(RMariaDB::MariaDB(), user='IOHProfiler', dbname="iohprofiler",
                      password="IOHProfiler", host='localhost')

rds_location <- file.path(Sys.getenv('HOME'), 'repository')

prodecure_filename <- "CALL `iohprofiler`.`get_filenames`("
prodecure_upload <- "CALL `iohprofiler`.`insert_item`("

upload_dataSetList <- function(dsList){
  # to_upload <- DataSetList()
  should_upload <- F
  for (ds in dsList){
    # if (verify_upload_dataSet(ds)) to_upload = c(to_upload,ds)
    should_upload <- should_upload | verify_upload_dataSet(ds)
  }
  if(!should_upload) return(0)
  
  filename <- basename(tempfile(pattern = "rds_file",tmpdir = rds_location))
  
  # result <- do.call(c.DataSetList, to_upload)
  saveRDS(dsList, file = file.path(rds_location, paste0(filename,".rds")))
  
  succes <- 0
  for (ds in dsList){
    succes <- succes + upload_dataSet(ds, filename)
  }
  return(succes)
}

upload_dataSet <- function(ds, filename){
  algid <- attr(ds,'algId')
  funcid <- attr(ds,'funcId')
  dim <- attr(ds,'DIM')
  suite <- attr(ds,'src')
  statement <- paste0(prodecure_upload,  funcid,",'", suite,"',", dim,",'", algid, "','" , filename , "');")
  return(dbExecute(con,statement))
}

verify_upload_dataSet <- function(ds){
  algid <- attr(ds,'algId')
  funcid <- attr(ds,'funcId')
  dim <- attr(ds,'DIM')
  suite <- attr(ds,'src')
  return(length(get_repository_filename(suite,algid,funcid,dim)) == 0)
}


get_repository_filename <- function(suite, algid = "NULL", funcid = "-1", dim = "-1"){
  statement <- paste0(prodecure_filename,  funcid,",'", suite,"',", dim,",'", algid, "');")
  response <- dbSendQuery(con, statement)
  ans <- dbFetch(response, n=-1)
  dbClearResult(response)
  ans[[1]]
}

