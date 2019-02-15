# This file contains some functions for reading and writing to the repository
#
# Author: Diederick Vermetten
# Email: d.vermetten@gmail.com

library(DBI)

con <- NULL

rds_location <- file.path(Sys.getenv('HOME'), 'repository')

#TODO: cleaner solution?
prodecure_filename <- "CALL `iohprofiler`.`get_filenames`("
prodecure_upload <- "CALL `iohprofiler`.`insert_item`("
prodecure_funcids <- "CALL `iohprofiler`.`get_fids`("
prodecure_algids <- "CALL `iohprofiler`.`get_algs`("
prodecure_dims <- "CALL `iohprofiler`.`get_dims`("

open_connection <- function(){
  if(!is.null(con)) return(T)
  succes <- T
  tryCatch({
    con <<- dbConnect(RMariaDB::MariaDB(), user='IOHProfiler', dbname="iohprofiler",
                     password="IOHProfiler", host='localhost')
  },
  error = function(cond){
    # message(cond)
    # message("Setting succes to F")
    succes <<- F
    })
  return(succes)
}

close_connection <- function(){
  if(!is.null(con)) dbDisconnect(con)
}

upload_dataSetList <- function(dsList){
  #Only upload new data
  dsList <- subset(dsList, lapply(dsList, verify_upload_dataSet))
  if(length(dsList) == 0){
    return(0)
  }
  filename <- basename(tempfile(pattern = "rds_file", tmpdir = rds_location))
    saveRDS(dsList, file = file.path(rds_location, paste0(filename, ".rds")))
  
  succes <- 0
  for (ds in dsList){
    succes <- succes + upload_dataSet(ds, filename)
  }
  return(succes)
}

upload_dataSet <- function(ds, filename){
  algid <- attr(ds, 'algId')
  funcid <- attr(ds, 'funcId')
  dim <- attr(ds, 'DIM')
  suite <- attr(ds, 'src')
  statement <- paste0(prodecure_upload,  funcid, ",'", suite, "',", dim,",'", 
                      algid, "','" , filename , "');")
  return(dbExecute(con, statement))
}

verify_upload_dataSet <- function(ds){
  algid <- attr(ds, 'algId')
  funcid <- attr(ds, 'funcId')
  dim <- attr(ds, 'DIM')
  suite <- attr(ds, 'src')
  return(length(get_repository_filename(suite, algid, funcid, dim)) == 0)
}


get_repository_filename <- function(suite, algid = "NULL", funcid = "-1", dim = "-1"){
  statement <- paste0(prodecure_filename,  funcid, ",'", suite, "',", dim, ",'", algid, "');")
  response <- dbSendQuery(con, statement)
  ans <- dbFetch(response, n=-1)
  dbClearResult(response)
  ans[[1]]
}

load_from_repository <- function(suite, algid = "all", funcid = "all", dim = "all"){
  #Ensure inputs are valid for calling the stored procedures
  if (algid == "all") algid <- "NULL"
  if (funcid == "all") funcid <- "-1"
  if (dim == "all") dim <- "-1"
  
  filenames <- get_repository_filename(suite, algid, funcid, dim)
  dsList <- DataSetList()
  for (filename in filenames){
    file_location <- file.path(rds_location, paste0(filename,".rds"))
    dslist <- readRDS(file_location)
    dsList <- c(dsList, dslist)
  }
  if(funcid != "-1")
    dsList <- subset(dsList, funcId==funcid)
  if(dim != "-1")
    dsList <- subset(dsList, DIM==dim)
  if(algid != "NULL")
    dsList <- subset(dsList, algId==algid)
  return(dsList)
}

get_available_funcs <- function(suite, algid = "all", dim = "all" ){
  #Ensure inputs are valid for calling the stored procedures
  if (algid == "all") algid <- "NULL"
  if (dim == "all") dim <- "-1"
  
  statement <-  paste0(prodecure_funcids, "'", suite,"','", algid, "',", dim, ");")
  response <- dbSendQuery(con, statement)
  ans <- dbFetch(response, n=-1)
  dbClearResult(response)
  ans[[1]]
}

get_available_dims <- function(suite, funcid = "all", algid = "all" ){
  #Ensure inputs are valid for calling the stored procedures
  if (algid == "all") algid <- "NULL"
  if (funcid == "all") funcid <- "-1"
  
  statement <-  paste0(prodecure_dims, "'", suite, "',", funcid,",'", algid, "');")
  response <- dbSendQuery(con, statement)
  ans <- dbFetch(response, n=-1)
  dbClearResult(response)
  ans[[1]]
}

get_available_algs <- function(suite, funcid = "all", dim = "all" ){
  #Ensure inputs are valid for calling the stored procedures
  if (funcid == "all") funcid <- "-1"
  if (dim == "all") dim <- "-1"
  
  statement <-  paste0(prodecure_algids, "'", suite, "',", funcid,",", dim, ");")
  response <- dbSendQuery(con, statement)
  ans <- dbFetch(response, n=-1)
  dbClearResult(response)
  ans[[1]]
}




