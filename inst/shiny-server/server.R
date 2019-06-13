format <- NULL         # the unique format of the data set
sub_sampling <- TRUE   # perform sub-sampling of the data set?
repo_dir <- ''         # repository directory
repo_data <- NULL      # repository data
has_rendered_ERT_per_fct <- FALSE
# Formatter for function values
format_FV <- function(v) format(v, digits = 2, nsmall = 2)
format_RT <- function(v) as.integer(v)

# directory where data are extracted from the zip file
exdir <- file.path(tempdir(), 'data')

setTextInput <- function(session, id, name, alternative) {
  v <- REG[[id]]
  if (name %in% names(v)) {
    updateTextInput(session, id, value = v[[name]])
  } else
    updateTextInput(session, id, value = alternative)
}

#TODO: this function could be made more clear
set_format_func <- function(format){
  format_FV <<- if (format == COCO || format == BIBOJ_COCO) format(v, format = 'e', digits = 3, nsmall = 2)
                else  function(v) format(v, digits = 2, nsmall = 2)
}

# register previous text inputs, which is used to restore those values
REG <- lapply(widget_id, function(x) list())

# TODO: maybe give this function a better name
# get the current 'id' of the selected data: funcID + DIM
get_data_id <- function(dsList) {
  if (is.null(dsList) | length(dsList) == 0)
    return(NULL)

  paste(get_funcId(dsList), get_dim(dsList), sep = '-')
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # clean up the temporarsy files on server when exiting
  session$onSessionEnded(function() {
    # close_connection()
    unlink(exdir, recursive = T)
  })

  for (f in list.files('server', pattern = '.R', full.names = T)) {
    source(f, local = TRUE)
  }
})
