# This file contains some functions for reading and writing to the repository
# The repository itself is a single directory, containing .rds files
# A database (MySQL) keeps track of which data is contained in which file
#
# Author: Diederick Vermetten
# Email: d.vermetten@gmail.com

suppressMessages(library(DBI))
suppressMessages(library(dbplyr))
suppressMessages(library(dplyr))

con <- NULL

#Location where rds-files are stored
rds_location <- get_repo_location(F)

#TODO: cleaner solution?
prodecure_upload <- "CALL `iohprofiler`.`insert_item`("


open_connection <- function(){
  if(!is.null(con)) return(T)
  succes <- T
  tryCatch({
    con <<- dbConnect(RMariaDB::MariaDB(), user='IOHProfiler', dbname="iohprofiler",
                     password="IOHProfiler", host='localhost')
  },
  error = function(cond){
    succes <<- F
    })
  return(succes)
}

close_connection <- function(){
  if(!is.null(con)) dbDisconnect(con)
  con <<- NULL
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

get_repository_filename <- function(suiteIn, algidIn = "all", funcIn = "all", dimIn = "all"){
  ans <- tbl(con,"repository_index") %>%
    filter((dim == dimIn || dimIn == 'all')
           && (func == funcIn || funcIn =='all')
           && (algid == algidIn || algidIn == 'all')
           && suite == suiteIn ) %>%
    select(filename) %>%
    collect()%>%
    .[["filename"]] %>%
    unique
  return(ans)
}

load_from_repository <- function(suite, algid = "all", funcid = "all", dim = "all"){
  filenames <- get_repository_filename(suite, algid, funcid, dim)
  dsList <- DataSetList()
  for (filename in filenames){
    file_location <- file.path(rds_location, paste0(filename, ".rds"))
    dslist <- readRDS(file_location)
    dsList <- c(dsList, dslist)
  }
  if(funcid != "all")
    dsList <- subset(dsList, funcId==funcid)
  if(dim != "all")
    dsList <- subset(dsList, DIM==dim)
  if(algid != "all")
    dsList <- subset(dsList, algId==algid)
  return(dsList)
}

get_available_funcs <- function(suiteIn, algidIn = "all", dimIn = "all" ){
  ans <- tbl(con,"repository_index") %>%
    filter((dim == dimIn || dimIn == 'all')
           && (algid == algidIn || algidIn == 'all')
           && suite == suiteIn ) %>%
    select(func) %>%
    collect()%>%
    .[["func"]] %>%
    unique
  return(ans)
}

get_available_dims <- function(suiteIn, funcIn = "all", algidIn = "all" ){
  ans <- tbl(con,"repository_index") %>%
    filter((func == funcIn || funcIn =='all')
           && (algid == algidIn || algidIn == 'all')
           && suite == suiteIn ) %>%
    select(dim) %>%
    collect()%>%
    .[["dim"]] %>%
    unique
  return(ans)
}

get_available_algs <- function(suiteIn, funcIn = "all", dimIn = "all" ){
  ans <- tbl(con,"repository_index") %>%
    filter((dim == dimIn || dimIn == 'all')
           && (func == funcIn || funcIn =='all')
           && suite == suiteIn ) %>%
    select(algid) %>%
    collect() %>%
    .[["algid"]] %>%
    unique
  return(ans)
}




