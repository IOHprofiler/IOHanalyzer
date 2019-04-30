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
  format_FV <<- ifelse((format == COCO || format == BIBOJ_COCO),
                       function(v) format(v, format = 'e', digits = 5, nsmall = 2),
                       function(v) format(v, digits = 2, nsmall = 2))
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

  # Data summary for Fixed-Target Runtime (ERT)  --------------
  runtime_summary <- reactive({
    req(input$RTSummary.Statistics.Min,
        input$RTSummary.Statistics.Max,
        input$RTSummary.Statistics.Step)

    fstart <- format_FV(input$RTSummary.Statistics.Min) %>% as.numeric
    fstop <- format_FV(input$RTSummary.Statistics.Max) %>% as.numeric
    fstep <- format_FV(input$RTSummary.Statistics.Step) %>% as.numeric
    data <- DATA()

    req(fstart <= fstop, fstep <= fstop - fstart, data)
    fall <- get_funvals(data)

    if (input$RTSummary.Statistics.Single)
      fstop <- fstart

    fseq <- seq_FV(fall, fstart, fstop, fstep)
    req(fseq)

    df <- get_RT_summary(data, fseq, algorithm = input$RTSummary.Statistics.Algid)
    df[, c('DIM', 'funcId') := NULL]
  })

  output$table_RT_summary <- renderTable({
    df <- runtime_summary()
    df$runs %<>% as.integer
    df$median %<>% as.integer
    df$target <- format_FV(df$target)

    # format the integers
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% as.integer
    }
    df
  })

})
