#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com
format <- NULL         # the unique format of the data set
sub_sampling <- TRUE   # perform sub-sampling of the data set?
repo_dir <- ''         # repository directory
repo_data <- NULL      # repository data


#TODO: duplicated from plotDataSetList; should maybe be exported in future version?
get_legends <- function(dsList) {
  N <- length(dsList)
  legends <- sapply(dsList, function(d) attr(d, 'algId'))

  if (length(unique(legends)) < N) {
    funcId <- sapply(dsList, function(d) attr(d, 'funcId'))
    if (length(unique(funcId)) > 1)
      legends <- paste0(legends, '-F', funcId)
  }

  if (length(unique(legends)) < N) {
    DIM <- sapply(dsList, function(d) attr(d, 'DIM'))
    if (length(unique(DIM)) > 1)
      legends <- paste0(legends, '-', DIM, 'D')
  }
  legends
}
#TODO: duplicated from plot; should maybe be exported in future version?
Set3 <- function(n) colorspace::sequential_hcl(n, c(-88, 59), c. = c(60, 75, 55), l = c(40, 90),
                                               power = c(0.1, 1.2), gamma = NULL,
                                               fixup = TRUE, alpha = 1)
color_palettes <- function(ncolor) {
  if (ncolor < 5) return(Set3(ncolor)) #Was set2, which gave NAFF as color?

  brewer <- function(n) {
    colors <- RColorBrewer::brewer.pal(n, 'Spectral')
    colors[colors == "#FFFFBF"] <- "#B2B285"
    colors
  }

  color_fcts <- c(colorRamps::primary.colors, Set3)

  n <- min(11, ncolor)
  colors <- brewer(n)
  ncolor <- ncolor - n

  i <- 1
  while (ncolor > 0) {
    n <- min(8, ncolor)
    if (i > length(color_fcts)) {
      colors <- c(colors, colorRamps::primary.colors(ncolor))
      break
    } else {
      colors <- c(colors, color_fcts[[i]](n))
      ncolor <- ncolor - n
    }
    i <- i + 1
  }
  colors
}

# Formatter for function values
format_FV <- function(v) format(v, digits = 2, nsmall = 2)
format_RT <- function(v) as.integer(v)

# directory where data are extracted from the zip file
exdir <- file.path(tempdir(check = T), 'data')

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

  # shared reactive variables
  folderList <- reactiveValues(data = list())
  DataList <- reactiveValues(data = DataSetList())

  # set up the global variable
  observe({
    sub_sampling <<- input$upload.subsampling
  })

  # set up list of datasets (scan the repository, looking for .rds files)
  observe({
    repo_dir <<- get_repo_location()
    rds_files <- list.files(repo_dir, pattern = '.rds') %>% sub('\\.rds$', '', .)

    if (length(rds_files) != 0) {
      updateSelectInput(session, 'repository.dataset', choices = rds_files, selected = NULL)
    } else { # TODO: the alert msg should be updated
      shinyjs::alert("No repository file found. To make use of the IOHProfiler-repository,
                      please create a folder called 'repository' in your home directory
                      and make sure it contains the '2019gecco.rds'-file
                      provided on the IOHProfiler github-page.")
    }
  })

  # load repository that is selected
  observeEvent(input$repository.dataset, {
    req(input$repository.dataset)

    rds_file <- file.path(repo_dir, paste0(input$repository.dataset, ".rds"))
    repo_data <<- readRDS(rds_file)

    algIds <- c(get_algId(repo_data), 'all')
    dims <- c(get_dim(repo_data), 'all')
    funcIds <- c(get_funcId(repo_data), 'all')

    updateSelectInput(session, 'repository.algId', choices = algIds, selected = 'all')
    updateSelectInput(session, 'repository.dim', choices = dims, selected = 'all')
    updateSelectInput(session, 'repository.funcId', choices = funcIds, selected = 'all')
    shinyjs::enable('repository.load_button')
  })

  # add the data from repository
  observeEvent(input$repository.load_button, {
    data <- repo_data
    if (input$repository.funcId != 'all')
      data <- subset(data, funcId == input$repository.funcId)
    if (input$repository.dim != 'all')
      data <- subset(data, DIM == input$repository.dim)
    if (input$repository.algId != 'all')
      data <- subset(data, algId == input$repository.algId)

    DataList$data <- c(DataList$data, data)
    format <<- attr(data[[1]], 'format')
  })

  # upload the compressed the data file and uncompress them
  selected_folders <- reactive({
    if (!is.null(input$upload.add_zip)) {
      datapath <- input$upload.add_zip$datapath
      folders <- rep('', length(datapath))

      for (i in seq(datapath)) {
        filetype <- sub('[^\\.]*\\.', '', basename(datapath[i]), perl = T)

        if (filetype == 'zip')
          unzip_fct <- unzip
        else if (filetype %in% c('bz2', 'bz', 'gz', 'tar', 'tgz', 'tar.gz', 'xz'))
          unzip_fct <- untar

        if (filetype == 'zip')
          files <- unzip_fct(datapath[i], list = T)$Name
        else
          files <- unzip_fct(datapath[i], list = T)

        idx <- grep('*.info', files)[1]
        info <- files[idx]

        if (is.null(info)) return(NULL)

        if (basename(info) == info) {
          folder <- Sys.time()  # generate a folder name here
          .exdir <- file.path(exdir, folder)
          unzip_fct(datapath[i], list = FALSE, exdir = .exdir)
          folders[i] <- .exdir
        } else {
          folder <- dirname(info)
          unzip_fct(datapath[i], list = FALSE, exdir = exdir)
          folders[i] <- file.path(exdir, folder)
        }
      }
      folders
    } else
      NULL
  })

  # load, process the data folders and update DataSetList
  observeEvent(selected_folders(), {
    folders <- selected_folders()
    format_selected <- input$upload.data_format
    maximization <- input$upload.maximization

    req(length(folders) != 0)

    if (length(folderList$data) == 0)
      folder_new <- folders
    else
      folder_new <- setdiff(folders, intersect(folderList$data, folders))

    req(length(folder_new) != 0)
    format_detected <- lapply(folder_new, check_format) %>% unique

    if (length(format_detected) != 1)
      print_html(paste('<p style="color:red;">more than one format: <br>',
                       format_detected,
                       'is detected in the uploaded data set... skip the uploaded data'))

    else
      format <<- format_detected   # set the global data format

    for (folder in folder_new) {
      indexFiles <- scan_IndexFile(folder)

      if (length(indexFiles) == 0)
        print_html(paste('<p style="color:red;">format', format_selected,
                         'is selected, however', format_detected,
                         'is detected...<br>using the detected one...</p>'))
      else {
        folderList$data <- c(folderList$data, folder)

        if (format_selected == AUTOMATIC) {
          set_format_func(format)
        } else if (format_detected != format) {
          print_html(paste('<p style="color:red;">format', format_selected,
                           'is selected, however', format,
                           'is detected...<br>using the detected one...</p>'))
        }

        if (maximization == AUTOMATIC)
          maximization <- ifelse((format == COCO || format == BIBOJ_COCO), FALSE, TRUE)
        else
          maximization <- ifelse((maximization == "MAXIMIZE"), TRUE, FALSE)

        # read the data set and handle potential errors
        new_data <- tryCatch(
          DataSetList(folder, print_fun = print_html,
                   maximization = maximization,
                   format = format,
                   subsampling = sub_sampling),
          error = function(e) {
            print_html(paste('<p style="color:red;">The following error happened
                            when processing the data set:</p>'))
            print_html(paste('<p style="color:red;">', e, '</p>'))
            DataSetList()
          }
        )

        DataList$data <- c(DataList$data, new_data)
        shinyjs::html("upload_data_promt",
                      sprintf('%d: %s\n', length(folderList$data), folder),
                      add = TRUE)
      }
    }
  })

  # remove all uploaded data set
  observeEvent(input$upload.remove_data, {
    if (length(DataList$data) != 0) {
      DataList$data <- DataSetList() # must be a 'DataSetList'
      folderList$data <- list()
      print_html('<p style="color:red;">all data are removed!</p>')
      print_html('', 'upload_data_promt')
    }
  })

  # show the detailed information on DataSetList
  output$data_info <- renderDataTable({
    datasets <- DataList$data
    if (length(datasets) != 0)
      summary(datasets)
    else
      data.frame()
  }, options = list(pageLength = 10, scrollX = F, autoWidth = TRUE,
                    columnDefs = list(list(width = '20px', targets = c(0, 1)))))

  # update the list of dimensionality, funcId and algId and parameter list
  observe({
    data <- DataList$data
    if (length(data) == 0)
      return()

    # TODO: create reactive values for them
    algIds_ <- get_algId(data)
    algIds <- c(algIds_, 'all')
    parIds <- c(get_parId(data), 'all')
    funcIds <- get_funcId(data)
    DIMs <- get_dim(data)

    updateSelectInput(session, 'Overall.Dim', choices = DIMs, selected = DIMs[1])
    updateSelectInput(session, 'Overall.Funcid', choices = funcIds, selected = funcIds[1])
    updateSelectInput(session, 'RTSummary.Statistics.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'RTSummary.Overview.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'FCESummary.Overview.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'RTSummary.Sample.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'PAR.Plot.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'FCESummary.Statistics.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'FCESummary.Sample.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'PAR.Summary.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'PAR.Sample.Algid', choices = algIds, selected = 'all')
    updateSelectInput(session, 'ERTPlot.Multi.Algs', choices = algIds_, selected = algIds_[1])
    updateSelectInput(session, 'FCEPlot.Multi.Algs', choices = algIds_, selected = algIds_[1])
    updateSelectInput(session, 'PAR.Summary.Param', choices = parIds, selected = 'all')
    updateSelectInput(session, 'PAR.Sample.Param', choices = parIds, selected = 'all')
  })

  # update (filter) according to users selection DataSets
  DATA <- reactive({
    dim <- input$Overall.Dim
    id <- input$Overall.Funcid

    if (length(DataList$data) == 0) return(NULL)

    subset(DataList$data, DIM == dim, funcId == id)
  })

  # TODO: give a different name for DATA and DATA_RAW
  DATA_RAW <- reactive({
    DataList$data
  })

  MAX_ERTS_FUNC <- reactive({
    dim <- input$Overall.Dim
    data <- subset(DataList$data, DIM == dim)
    max_ERTs(data, aggr_on = 'funcId', maximize = !(format == COCO || format == BIBOJ_COCO))
  })

  MAX_ERTS_DIM <- reactive({
    func <- input$Overall.Funcid
    data <- subset(DataList$data, funcId == func)
    max_ERTs(data, aggr_on = 'DIM', maximize = !(format == COCO || format == BIBOJ_COCO))
  })

  MEAN_FVALS_FUNC <- reactive({
    dim <- input$Overall.Dim
    data <- subset(DataList$data, DIM == dim)
    mean_FVs(data, aggr_on = 'funcId')
  })

  MEAN_FVALS_DIM <- reactive({
    func <- input$Overall.Funcid
    data <- subset(DataList$data, funcId == func)
    mean_FVs(data, aggr_on = 'DIM')
  })

  # TODO: make this urgely snippet look better...
  # register the TextInput and restore them when switching funcID and DIM
  observeEvent(eval(eventExpr), {
    data <- DATA()
    name <- get_data_id(data)

    if (is.null(name))
      return()

    for (id in widget_id) {
      REG[[id]][[name]] <<- input[[id]]
    }
  })

  # update the values for the grid of target values
  observe({
    data <- DATA()
    v <- get_funvals(data)
    name <- get_data_id(data)
    req(v)

    # choose proper scaling for the function value
    # v <- trans_funeval(v)    # Do the scaling in seq_FV instead
    q <- quantile(v, probs = c(.25, .5, .75), names = F)

    # TODO: we need to fix this for the general case!!!
    length.out <- 10
    fseq <- seq_FV(v, length.out = length.out)
    start <- fseq[1]
    stop <- fseq[length.out]
    # s <- ((stop - start) * 0.1 + start)
    # e <- ((stop - start) * 0.9 + start)

    #TODO: Make more general
    if (abs(2 * fseq[3] - fseq[2] - fseq[4]) < 1e-12) #arbitrary precision
      step <- fseq[3] - fseq[2]
    else
      step <- log10(fseq[3]) - log10(fseq[2])

    setTextInput(session, 'RTSummary.Statistics.Min', name, alternative = format_FV(start))
    setTextInput(session, 'RTSummary.Statistics.Max', name, alternative = format_FV(stop))
    setTextInput(session, 'RTSummary.Statistics.Step', name, alternative = format_FV(step))
    setTextInput(session, 'RTSummary.Sample.Min', name, alternative = format_FV(start))
    setTextInput(session, 'RTSummary.Sample.Max', name, alternative = format_FV(stop))
    setTextInput(session, 'RTSummary.Sample.Step', name, alternative = format_FV(step))
    setTextInput(session, 'RTECDF.Multi.Min', name, alternative = format_FV(start))
    setTextInput(session, 'RTECDF.Multi.Max', name, alternative = format_FV(stop))
    setTextInput(session, 'RTECDF.Multi.Step', name, alternative = format_FV(step))
    setTextInput(session, 'RTPMF.Bar.Target', name, alternative = format_FV(median(v)))
    setTextInput(session, 'RTPMF.Hist.Target', name, alternative = format_FV(median(v)))
    setTextInput(session, 'ERTPlot.Min', name, alternative = format_FV(start))
    setTextInput(session, 'ERTPlot.Max', name, alternative = format_FV(stop))
    setTextInput(session, 'ERTPlot.Aggr.Targets', name, alternative = "")
    setTextInput(session, 'FCEPlot.Aggr.Targets', name, alternative = "")
    setTextInput(session, 'RTECDF.Single.Target', name, alternative = format_FV(q[2]))
    setTextInput(session, 'RTECDF.AUC.Min', name, alternative = format_FV(start))
    setTextInput(session, 'RTECDF.AUC.Max', name, alternative = format_FV(stop))
    setTextInput(session, 'RTECDF.AUC.Step', name, alternative = format_FV(step))
    setTextInput(session, 'PAR.Plot.Min', name, alternative = format_FV(start))
    setTextInput(session, 'PAR.Plot.Max', name, alternative = format_FV(stop))
    setTextInput(session, 'PAR.Summary.Min', name, alternative = format_FV(start))
    setTextInput(session, 'PAR.Summary.Max', name, alternative = format_FV(stop))
    setTextInput(session, 'PAR.Summary.Step', name, alternative = format_FV(step))
    setTextInput(session, 'PAR.Sample.Min', name, alternative = format_FV(start))
    setTextInput(session, 'PAR.Sample.Max', name, alternative = format_FV(stop))
    setTextInput(session, 'PAR.Sample.Step', name, alternative = format_FV(step))
  })

  # update the values for the grid of running times
  observe({
    data <- DATA()
    v <- get_runtimes(data)
    name <- get_data_id(data)
    # s <- ((max(v) - min(v)) * 0.05 + min(v)) %>% as.integer
    # e <- ((max(v) - min(v)) * 0.95 + min(v)) %>% as.integer

    if (length(v) == 0) return()

    # TODO: this part should be made generic!!!
    v <- as.integer(v)
    q <- quantile(v, probs = c(.25, .5, .75), names = F, type = 3)

    grid <- seq(min(v), max(v), length.out = 10) %>% as.integer
    step <- max(1, min(grid[-1] - grid[-length(grid)]))

    start <- min(v)
    stop <- max(v)

    setTextInput(session, 'FCESummary.Statistics.Min', name, alternative = min(v))
    setTextInput(session, 'FCESummary.Statistics.Max', name, alternative = max(v))
    setTextInput(session, 'FCESummary.Statistics.Step', name, alternative = step)
    setTextInput(session, 'FCESummary.Sample.Min', name, alternative = min(v))
    setTextInput(session, 'FCESummary.Sample.Max', name, alternative = max(v))
    setTextInput(session, 'FCESummary.Sample.Step', name, alternative = step)
    setTextInput(session, 'FCEPDF.Hist.Runtime', name, alternative = median(v))
    setTextInput(session, 'FCEPDF.Bar.Runtime', name, alternative = median(v))
    setTextInput(session, 'FCEPlot.Min', name, alternative = start)
    setTextInput(session, 'FCEPlot.Max', name, alternative = stop)
    setTextInput(session, 'FCEECDF.Mult.Min', name, alternative = min(v))
    setTextInput(session, 'FCEECDF.Mult.Max', name, alternative = max(v))
    setTextInput(session, 'FCEECDF.Mult.Step', name, alternative = step)
    setTextInput(session, 'FCEECDF.AUC.Min', name, alternative = min(v))
    setTextInput(session, 'FCEECDF.AUC.Max', name, alternative = max(v))
    setTextInput(session, 'FCEECDF.AUC.Step', name, alternative = step)

    #TODO: remove q and replace by single number
    setTextInput(session, 'FCEECDF.Single.Target', name, alternative = q[2])
  })

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

  # Data summary for Fixed-Target Runtime (ERT)  --------------
  runtime_summary_condensed <- reactive({
    data <- DATA()
    req(data)
    fall <- get_funvals(data)
    get_FV_overview(data, algorithm = input$RTSummary.Overview.Algid)
  })

  output$table_RT_overview <- renderTable({
    req(input$RTSummary.Overview.Algid)
    df <- runtime_summary_condensed()

    df$"Budget" %<>% as.integer
    df$"runs" %<>% as.integer
    df$"runs reached" %<>% as.integer
    df$"Worst recorded f(x)" <- format_FV(df$"Worst recorded f(x)")
    df$"Worst reached f(x)" <- format_FV(df$"Worst reached f(x)")
    df$"mean reached f(x)" <- format_FV(df$"mean reached f(x)")
    df$"median reached f(x)" <- format_FV(df$"median reached f(x)")
    df$"Best reached f(x)" <- format_FV(df$"Best reached f(x)")
    df
  })

  output$RTSummary.Statistics.Download <- downloadHandler(
    filename = {
      fstart <- format_FV(input$RTSummary.Statistics.Min)
      fstop <- format_FV(input$RTSummary.Statistics.Max)
      fstep <- format_FV(input$RTSummary.Statistics.Step)
      eval(RT_csv_name)
    },
    content = function(file) {
      write.csv(runtime_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )

  get_RT <- reactive({
    req(input$RTSummary.Sample.Min,
        input$RTSummary.Sample.Max,
        input$RTSummary.Sample.Step)

    fstart <- format_FV(input$RTSummary.Sample.Min) %>% as.numeric
    fstop <- format_FV(input$RTSummary.Sample.Max) %>% as.numeric
    fstep <- format_FV(input$RTSummary.Sample.Step) %>% as.numeric
    data <- DATA()

    req(fstart <= fstop, fstep <= fstop - fstart, data)
    fall <- get_funvals(data)

    if (input$RTSummary.Sample.Single)
      fstop <- fstart

    fseq <- seq_FV(fall, fstart, fstop, fstep)
    req(fseq)

    get_RT_sample(data, ftarget = fseq, algorithm = input$RTSummary.Sample.Algid,
                  output = input$RTSummary.Sample.DownloadFormat)
  })

  output$RTSummary.Sample.Download <- downloadHandler(
    filename = {
      fstart <- input$RTSummary.Sample.Min %>% format_FV
      fstop <- input$RTSummary.Sample.Max %>% format_FV
      fstep <- input$RTSummary.Sample.Step %>% format_FV
      eval(RTSample_csv_name)
    },
    content = function(file) {
      write.csv(get_RT(), file, row.names = F)
    },
    contentType = "text/csv"
  )

  output$table_RT_sample <- renderDataTable({
    df <- get_RT()
    df[is.na(df)] <- 'NA'
    df
  }, options = list(pageLength = 20, scrollX = T))

  # The expected runtime plot ---------------------
  # TODO: wrapp this as a separate function for DataSet class
  output$ERT_PER_FUN <- renderPlotly({
    render_ert_per_fct()
  })

  output$ERTPlot.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_ERT_PER_FUN)
    },
    content = function(file) {
      save_plotly(render_ert_per_fct(), file,
                  format = input$ERTPlot.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$ERTPlot.Format)
  )

  update_ert_per_fct <- observe({
    plotlyProxy("ERT_PER_FUN", session) %>%
      plotlyProxyInvoke("relayout", list(xaxis = list(type = ifelse(input$ERTPlot.semilogx, 'log', 'linear')),
                                         yaxis = list(type = ifelse(input$ERTPlot.semilogy, 'log', 'linear'))))
  })

  ert_per_fct_data <- reactive({
    req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
    dsList <- DATA()
    Fall <- get_funvals(dsList)

    Fseq <- seq_FV(Fall, as.numeric(input$ERTPlot.Min), as.numeric(input$ERTPlot.Max), length.out = 60,
                   scale = ifelse(isolate(input$ERTPlot.semilogx), 'log', 'linear'))

    dt <- get_RT_summary(dsList, ftarget = Fseq)
    dt[, `:=`(upper = mean + sd, lower = mean - sd)]
    dt
  })

  ERTPlot_traces <- reactive({
    input$ERTPlot_Traces
  })

  #TODO: Think of a better method to add / remove traces. Keep an eye on https://github.com/ropensci/plotly/issues/1248 for possible fix
  #TODO: Figure out how to get visibility of current traces to set the 'visible' property correctely
  update_ert_per_fct_ERT <- observe({
    req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
    x <- session$sendCustomMessage("set_trace_input","#FF0073")
    visible_traces <- ERTPlot_traces()
    shinyjs::alert(visible_traces)
    dsList <- DATA()
    N <- length(dsList)
    if (input$ERTPlot.show.ERT){
      nr_other_active = isolate(1+input$ERTPlot.show.mean + input$ERTPlot.show.median)
      legends <- get_legends(dsList)
      colors <- color_palettes(N)
      dt <- ert_per_fct_data()

      for (i in seq_along(dsList)) {
        rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')

        legend <- legends[i]
        ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
                       funcId == attr(dsList[[i]], 'funcId') &
                       DIM == attr(dsList[[i]], 'DIM')]
        plotlyProxy("ERT_PER_FUN", session) %>%
          plotlyProxyInvoke("addTraces", list(x = ds_ERT$target, y = ds_ERT$ERT, type = 'scatter',
                          name = paste0(legend, '.ERT'), mode = 'lines+markers',
                          marker = list(color = rgb_str), legendgroup = legend,
                          line = list(color = rgb_str)),(i-1)*nr_other_active)
      }
    }
    else{
      nr_other_active = isolate(1+input$ERTPlot.show.mean + input$ERTPlot.show.median)
      plotlyProxy("ERT_PER_FUN", session) %>%
        plotlyProxyInvoke("deleteTraces", nr_other_active*0:(N-1))
    }
  })

  update_ert_per_fct_mean <- observe({
    req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
    x <- session$sendCustomMessage("set_trace_input","#FF0073")

    dsList <- DATA()
    N <- length(dsList)
    if (input$ERTPlot.show.mean){
      nr_other_active = isolate(1+input$ERTPlot.show.ERT + input$ERTPlot.show.median)
      legends <- get_legends(dsList)
      colors <- color_palettes(N)
      dt <- ert_per_fct_data()

      for (i in seq_along(dsList)) {
        rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')

        legend <- legends[i]
        ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
                       funcId == attr(dsList[[i]], 'funcId') &
                       DIM == attr(dsList[[i]], 'DIM')]
        plotlyProxy("ERT_PER_FUN", session) %>%
          plotlyProxyInvoke("addTraces", list(x = ds_ERT$target, y = ds_ERT$mean, type = 'scatter',
                                              name = paste0(legend, '.mean'), mode = 'lines+markers',
                                              marker = list(color = rgb_str), legendgroup = legend,
                                              line = list(color = rgb_str)),1+(i-1)*nr_other_active)
      }
    }
    else{
      nr_other_active = isolate(1+input$ERTPlot.show.ERT + input$ERTPlot.show.median)
      plotlyProxy("ERT_PER_FUN", session) %>%
        plotlyProxyInvoke("deleteTraces", 1+(nr_other_active*0:(N-1)))
    }
  })

  update_ert_per_fct_median <- observe({
    req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
    x <- session$sendCustomMessage("set_trace_input","#FF0073")

    dsList <- DATA()
    N <- length(dsList)
    if (input$ERTPlot.show.median){
      nr_other_active = isolate(input$ERTPlot.show.mean + input$ERTPlot.show.ERT)
      legends <- get_legends(dsList)
      colors <- color_palettes(N)
      dt <- ert_per_fct_data()

      for (i in seq_along(dsList)) {
        rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')

        legend <- legends[i]
        ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
                       funcId == attr(dsList[[i]], 'funcId') &
                       DIM == attr(dsList[[i]], 'DIM')]
        plotlyProxy("ERT_PER_FUN", session) %>%
          plotlyProxyInvoke("addTraces", list(x = ds_ERT$target, y = ds_ERT$median, type = 'scatter',
                                              name = paste0(legend, '.median'), mode = 'lines+markers',
                                              marker = list(color = rgb_str), legendgroup = legend,
                                              line = list(color = rgb_str)),nr_other_active+(i-1)*(nr_other_active+1))
      }
    }
    else{
      nr_other_active = isolate(input$ERTPlot.show.mean + input$ERTPlot.show.ERT)
      seq_del = nr_other_active+((1+nr_other_active)*0:(N-1))
      plotlyProxy("ERT_PER_FUN", session) %>%
        plotlyProxyInvoke("deleteTraces", seq_del)
    }
  })

  update_ert_per_fct_CI <- observe({
    req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())

    dsList <- DATA()
    N <- length(dsList)
    if (input$ERTPlot.show.CI){
      nr_other_active = isolate(input$ERTPlot.show.mean + input$ERTPlot.show.ERT + input$ERTPlot.show.median)
      legends <- get_legends(dsList)
      colors <- color_palettes(N)
      dt <- ert_per_fct_data()

      for (i in seq_along(dsList)) {
        rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')

        legend <- legends[i]
        ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
                       funcId == attr(dsList[[i]], 'funcId') &
                       DIM == attr(dsList[[i]], 'DIM')]
        plotlyProxy("ERT_PER_FUN", session) %>%
          plotlyProxyInvoke("addTraces",
                            list(x = ds_ERT$target, y = ds_ERT$upper, type = 'scatter', mode = 'lines',
                                 line = list(color = rgba_str, width = 0), legendgroup = legend,
                                 showlegend = F, name = 'mean +/- sd'),
                            nr_other_active+(i-1)*(nr_other_active+1)) %>%
          plotlyProxyInvoke("addTraces",
                            list(x = ds_ERT$target, y = ds_ERT$lower, type = 'scatter', mode = 'lines',
                                 fill = 'tonexty',  line = list(color = 'transparent'),
                                 legendgroup = legend,
                                 fillcolor = rgba_str, showlegend = F, name = 'mean +/- sd'),
                            nr_other_active+1+(i-1)*(nr_other_active+1))
      }
    }
    else{
      nr_other_active = isolate(input$ERTPlot.show.mean + input$ERTPlot.show.ERT + input$ERTPlot.show.median)
      seq_del = nr_other_active+((2+nr_other_active)*0:(N-1))
      seq_del2 = nr_other_active+((1+nr_other_active)*0:(N-1))

      plotlyProxy("ERT_PER_FUN", session) %>%
        plotlyProxyInvoke("deleteTraces", seq_del) %>%
        plotlyProxyInvoke("deleteTraces", seq_del2)
    }
  })

  render_ert_per_fct <- reactive({
    req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
    fstart <- input$ERTPlot.Min %>% as.numeric
    fstop <- input$ERTPlot.Max %>% as.numeric
    plot_RT_single_fct(DATA(), Fstart = fstart, Fstop = fstop,
                 show.CI = isolate(input$ERTPlot.show.CI),
                 show.density = input$ERTPlot.show.density,
                 show.runs = input$ERTPlot.show_all,
                 show.optimal = input$ERTPlot.show.best_of_all,
                 show.pareto = input$ERTPlot.show.pareto_optima,
                 show.ERT = isolate(input$ERTPlot.show.ERT),
                 show.mean = isolate(input$ERTPlot.show.mean),
                 show.median = isolate(input$ERTPlot.show.median),
                 scale.xlog = isolate(input$ERTPlot.semilogx),
                 scale.ylog = isolate(input$ERTPlot.semilogy),
                 show.grad = input$ERTPlot.show.grad,
                 show.intensity = input$ERTPlot.show.intensity,
                 scale.reverse = (format == COCO || format == BIBOJ_COCO))
  })

  output$ERTPlot.Multi.Plot <- renderPlotly(
    render_ERTPlot_multi_plot()
  )

  render_ERTPlot_multi_plot <- eventReactive(input$ERTPlot.Multi.PlotButton, {
    req(input$ERTPlot.Multi.Algs)
    data <- subset(DATA_RAW(),
                   algId %in% input$ERTPlot.Multi.Algs,
                   DIM == input$Overall.Dim)
    req(data)

    plot_RT_all_fcts(data,
                     scale.xlog = input$ERTPlot.Multi.Logx,
                     scale.ylog = input$ERTPlot.Multi.Logy,
                     scale.reverse = (format == COCO || format == BIBOJ_COCO))
  })

  output$ERTPlot.Multi.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_ERT_PER_FUN_MULTI)
    },
    content = function(file) {
      save_plotly(render_ERTPlot_multi_plot(), file,
                  format = input$ERTPlot.Multi.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$ERTPlot.Multi.Format)
  )

  output$ERTPlot.Aggr.Plot <- renderPlotly(
    render_ERTPlot_aggr_plot()
  )

   get_max_targets <- function(data, aggr_on, maximize){
    targets <- c()
    aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_DIM(data)

    for (j in seq_along(aggr_attr)) {
      dsList_filetered <- if (aggr_on == 'funcId') subset(data,funcId == aggr_attr[[j]])
      else subset(data, DIM == aggr_attr[[j]])

      Fall <- get_funvals(dsList_filetered)
      Fval <- ifelse(maximize, max(Fall), min(Fall))
      targets <- c(targets,Fval)
    }
    targets
  }

  render_ERTPlot_aggr_plot <- reactive({
    #TODO: figure out how to avoid plotting again when default targets are written to input
    data <- DATA_RAW()
    if (length(data) == 0) return(NULL)
    data <- subset(data, DIM == input$Overall.Dim)
    erts <- MAX_ERTS_FUNC()
    aggr_on = 'funcId'
    aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_DIM(data)
    targets <- get_max_targets(data, aggr_on, maximize = !(format == COCO || format == BIBOJ_COCO))
    erts <- max_ERTs(data, aggr_on, targets, maximize = !(format == COCO || format == BIBOJ_COCO))

    plot_ERT_AGGR(data, plot_mode = input$ERTPlot.Aggr.Mode, targets = targets,
                  scale.ylog = input$ERTPlot.Aggr.Logy,
                  maximize = !(format == COCO || format == BIBOJ_COCO),
                  use_rank = input$ERTPlot.Aggr.Ranking,
                  aggr_on = aggr_on, erts = erts)
  })

  output$ERTPlot.Aggr.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_ERT_AGGR)
    },
    content = function(file) {
      save_plotly(render_ERTPlot_aggr_plot(), file,
                  format = input$ERTPlot.Aggr.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$ERTPlot.Aggr.Format)
  )

  # empirical p.m.f. of the runtime
  output$RT_PMF <- renderPlotly({
    render_RT_PMF()
  })

  output$RTPMF.Bar.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_PMF)
    },
    content = function(file) {
      save_plotly(render_RT_PMF(), file,
                  format = input$RTPMF.Bar.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$RTPMF.Bar.Format)
  )

  render_RT_PMF <- reactive({
    ftarget <- input$RTPMF.Bar.Target %>% as.numeric
    plot_RT_PMF(DATA(), ftarget, show.sample = input$RTPMF.Bar.Sample,
                scale.ylog = input$RTPMF.Bar.Logy)
  })

  # historgram of the running time
  output$RT_HIST <- renderPlotly({
    req(input$RTPMF.Bar.Target)
    render_RT_HIST()
  })

  output$RTPMF.Hist.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_HIST)
    },
    content = function(file) {
      save_plotly(render_RT_HIST(), file,
                  format = input$RTPMF.Hist.Format,
                  width = fig_width2, height = fig_height2)
    },
    contentType = paste0('image/', input$RTPMF.Hist.Format)
  )

  render_RT_HIST <- reactive({
    req(input$RTPMF.Hist.Target)
    ftarget <- format_FV(input$RTPMF.Hist.Target) %>% as.numeric
    plot_mode <- input$RTPMF.Hist.Mode

    # TODO: remove 'DataSetList' in the future
    plot_RT_HIST(DATA(), ftarget, plot_mode = plot_mode)
  })

  output$RT_ECDF_MULT <- renderPlotly({
    req(length(DATA_RAW()) > 0)
    render_RT_ECDF_MULT()
  })

  render_RT_ECDF_MULT <- reactive({

    dsList <- subset(DATA_RAW(), DIM == input$Overall.Dim)
    targets <- uploaded_RT_ECDF_targets()

    plot_RT_ECDF_MULTI(dsList, targets = targets)
  })

  output$RTECDF.Aggr.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_ECDF_MULT)
    },
    content = function(file) {
      save_plotly(render_RT_ECDF_MULT(), file,
                  format = input$RTECDF.Aggr.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$RTECDF.Aggr.Format)
  )

  RT_ECDF_MULTI_TABLE <- reactive({
    targets <- uploaded_RT_ECDF_targets()
    funcId <- names(targets)

    if (is.null(targets)) {
      data <- subset(DATA_RAW(), DIM == input$Overall.Dim)
      targets <- get_default_ECDF_targets(data)
      funcId <- unique(attr(data, 'funcId')) %>% sort
    }

    targets <- lapply(targets, function(t) {
      paste0(as.character(t), collapse = ',')
    })

    data.frame(funcId = funcId, target = unlist(targets))
  })

  output$RT_GRID_GENERATED <- renderTable({
    req(length(DATA_RAW()) > 0)
    df <- RT_ECDF_MULTI_TABLE()
    df$funcId <- as.integer(df$funcId)
    df
  })

  uploaded_RT_ECDF_targets <- reactive({
    if (!is.null(input$RTECDF.Aggr.Table.Upload)) {
      df <- read.csv(input$RTECDF.Aggr.Table.Upload$datapath, header = T, sep = ';')
      value <- as.character(df$target)

      lapply(value,
             function(v) {
               unlist(strsplit(v, '[,]')) %>%
                 as.numeric
             }) %>%
        set_names(df$funcId)
    } else
      NULL
  })

  output$RTECDF.Aggr.Table.Download <- downloadHandler(
    filename = 'Example_ECDF_TARGETS.csv',
    content = function(file) {
      write.table(RT_ECDF_MULTI_TABLE(), file, row.names = F,
                  col.names = T, sep = ';')
    },
    contentType = "text/csv"
  )

  # The ECDF plots for the runtime ----------------
  output$RT_ECDF <- renderPlotly({
    req(input$RTECDF.Single.Target)
    ftargets <- as.numeric(format_FV(input$RTECDF.Single.Target))

    plot_RT_ECDF(DATA(), ftargets, scale.xlog = input$RTECDF.Single.Logx)

  })

  output$RT_GRID <- renderPrint({
    req(input$RTECDF.Multi.Min, input$RTECDF.Multi.Max, input$RTECDF.Multi.Step)

    fstart <- format_FV(input$RTECDF.Multi.Min) %>% as.numeric
    fstop <- format_FV(input$RTECDF.Multi.Max) %>% as.numeric
    fstep <- format_FV(input$RTECDF.Multi.Step) %>% as.numeric

    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- get_funvals(data)

    seq_FV(fall, fstart, fstop, by = fstep) %>% cat
  })

  output$RT_ECDF_AGGR <- renderPlotly({
    render_RT_ECDF_AGGR()
  })

  output$RTECDF.Multi.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_ECDF_AGGR)
    },
    content = function(file) {
      save_plotly(render_RT_ECDF_AGGR(), file,
                  format = input$RTECDF.Multi.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$RTECDF.Multi.Format)
  )

  render_RT_ECDF_AGGR <- reactive({
    req(input$RTECDF.Multi.Min, input$RTECDF.Multi.Max, input$RTECDF.Multi.Step)

    fstart <- format_FV(input$RTECDF.Multi.Min) %>% as.numeric
    fstop <- format_FV(input$RTECDF.Multi.Max) %>% as.numeric
    fstep <- format_FV(input$RTECDF.Multi.Step) %>% as.numeric

    plot_RT_ECDF_AGGR(
      DATA(), fstart, fstop, fstep,
      show.per_target = input$RTECDF.Multi.Targets,
      scale.xlog = input$RTECDF.Multi.Logx
    )
  })

  # evaluation rake of all courses
  output$RT_AUC <- renderPlotly({
    render_RT_AUC()
  })

  output$RTECDF.AUC.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_RT_AUC)
    },
    content = function(file) {
      save_plotly(render_RT_AUC(), file,
                  format = input$RTECDF.AUC.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$RTECDF.AUC.Format)
  )

  render_RT_AUC <- reactive({
    req(input$RTECDF.AUC.Min, input$RTECDF.AUC.Max, input$RTECDF.AUC.Step)

    fstart <- format_FV(input$RTECDF.AUC.Min) %>% as.numeric
    fstop <- format_FV(input$RTECDF.AUC.Max) %>% as.numeric
    fstep <- format_FV(input$RTECDF.AUC.Step) %>% as.numeric

    plot_RT_AUC(
      DATA(), fstart, fstop, fstep, fval_formatter = format_FV
    )
  })

  # TODO: rename 'FCE'...
  # Data summary for Fixed-Budget target (FCE)  --------------
  FCE_runtime_summary_condensed <- reactive({
    data <- DATA()
    fall <- get_funvals(data)
    get_RT_overview(data, algorithm = input$FCESummary.Overview.Algid)
  })

  output$table_FV_overview <- renderTable({
    req(input$FCESummary.Overview.Algid)
    df <- FCE_runtime_summary_condensed()

    df$"runs" %<>% as.integer
    df$"Budget" %<>% as.integer
    df$"miminal runtime" %<>% as.integer
    df$"maximal runtime" %<>% as.integer

    df
  })

  get_FCE_summary <- reactive({
    req(input$FCESummary.Statistics.Min, input$FCESummary.Statistics.Max, input$FCESummary.Statistics.Step)

    rt_min <- input$FCESummary.Statistics.Min %>% as.integer
    rt_max <- input$FCESummary.Statistics.Max %>% as.integer
    rt_step <- input$FCESummary.Statistics.Step %>% as.integer

    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- get_runtimes(data)

    if (input$FCESummary.Statistics.Single)
      rt_max <- rt_min

    rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
    req(rt_seq)

    get_FV_summary(data, rt_seq, algorithm = input$FCESummary.Statistics.Algid)[
      , c('DIM', 'funcId') := NULL
      ]
  })

  output$FCE_SUMMARY <- renderTable({
    df <- get_FCE_summary()
    df$runs %<>% as.integer
    # df$runs.MaxFunvals %<>% as.integer
    df$median %<>% format(format = 'e', digits = 3)
    df$mean %<>% format(format = 'e', digits = 3)
    df$runtime %<>% as.integer

    # TODO: make probs as a global option
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.

    # format the integers
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% format(format = 'e', digits = 3)
    }
    df
  })

  output$FCESummary.Statistics.Download <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(get_algId(data), collapse = ';')
      rt_min <- input$FCESummary.Statistics.Min %>% as.integer %>% as.character
      rt_max <- input$FCESummary.Statistics.Max %>% as.integer %>% as.character
      rt_step <- input$FCESummary.Statistics.Step %>% as.integer %>% as.character
      eval(FV_csv_name)
    },
    content = function(file) {
      write.csv(get_FCE_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )

  get_FCE <- reactive({
    req(input$FCESummary.Sample.Min, input$FCESummary.Sample.Max, input$FCESummary.Sample.Step)
    rt_min <- input$FCESummary.Sample.Min %>% as.integer
    rt_max <- input$FCESummary.Sample.Max %>% as.integer
    rt_step <- input$FCESummary.Sample.Step %>% as.integer

    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- get_runtimes(data)

    if (input$FCESummary.Sample.Single)
      rt_max <- rt_min

    rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
    req(rt_seq)

    get_FV_sample(data, rt_seq, algorithm = input$FCESummary.Sample.Algid,
                  output = input$FCESummary.Sample.Format)

    # res <- list()
    # n_runs_max <- sapply(data, function(x) length(attr(x, 'instance'))) %>% max
    #
    # for (i in seq_along(data)) {
    #   ds <- data[[i]]
    #   algId <- attr(ds, 'algId')
    #   if (input$FCESummary.Sample.Algid != 'all' && algId != input$FCESummary.Sample.Algid)
    #     next
    #
    #   rt <- get_FV_sample(ds, rt_seq, output = input$FCESummary.Sample.Format)
    #   if (input$FCESummary.Sample.Format == 'wide') {
    #     # impute the missing records
    #     n <- ncol(rt) - 2
    #     if (n < n_runs_max)
    #       rt %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
    #   }
    #   res[[i]] <- rt
    # }
    # do.call(rbind, res)
  })

  output$FCESummary.Sample.Download <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(get_algId(data), collapse = ';')
      rt_min <- input$FCESummary.Statistics.Min %>% as.integer %>% as.character
      rt_max <- input$RT_MAX %>% as.integer %>% as.character
      rt_step <- input$RT_STEP %>% as.integer %>% as.character
      eval(FVSample_csv_name)
    },
    content = function(file) {
      write.csv(get_FCE(), file, row.names = F)
    },
    contentType = "text/csv"
  )

  output$FCE_SAMPLE <- renderDataTable({
    df <- get_FCE()
    df[is.na(df)] <- 'NA'
    df}, options = list(pageLength = 20, scrollX = T))

  # Expected Target Value Convergence
  output$FCE_PER_FUN <- renderPlotly({
    req(input$FCEPlot.Min, input$FCEPlot.Max)
    render_FV_PER_FUN()
  })

  output$FCEPlot.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_PER_FUN)
    },
    content = function(file) {
      save_plotly(render_FV_PER_FUN(), file,
                  format = input$FCEPlot.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FCEPlot.Format)
  )

  render_FV_PER_FUN <- reactive({
    rt_min <- input$FCEPlot.Min %>% as.integer
    rt_max <- input$FCEPlot.Max %>% as.integer
    plot_FV_line(DATA(), RTstart = rt_min, RTstop = rt_max, show.CI = input$FCEPlot.show.CI,
                 show.grad = input$FCEPlot.show.grad, show.intensity = input$FCEPlot.show.intensity,
                 show.density = input$FCEPlot.show.density, show.pareto = input$FCEPlot.show.pareto_optima,
                 show.runs = input$FCEPlot.show.all, show.optimal = input$FCEPlot.show.best_of_all,
                 show.mean = input$FCEPlot.show.mean, show.median = input$FCEPlot.show.median,
                 scale.xlog = input$FCEPlot.semilogx, scale.ylog = input$FCEPlot.semilogy)
  })


  output$FCEPlot.Multi.Plot <- renderPlotly(
    render_FCEPlot_multi_plot()
  )

  render_FCEPlot_multi_plot <- eventReactive(input$FCEPlot.Multi.PlotButton, {
    req(input$FCEPlot.Multi.Algs)
    data <- subset(DATA_RAW(),
                   algId %in% input$FCEPlot.Multi.Algs,
                   DIM == input$Overall.Dim)
    req(data)

    plot_FV_all_fcts(data,
                     scale.xlog = input$FCEPlot.Multi.Logx,
                     scale.ylog = input$FCEPlot.Multi.Logy)
  })

  output$FCEPlot.Multi.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_PER_FUN_MULTI)
    },
    content = function(file) {
      save_plotly(render_FCEPlot_multi_plot(), file,
                  format = input$FCEPlot.Multi.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FCEPlot.Multi.Format)
  )

  output$FCEPlot.Aggr.Plot <- renderPlotly(
    render_FCEPlot_aggr_plot()
  )

  get_max_runtimes <- function(data, aggr_on){
    runtimes <- c()
    aggr_attr <- if(aggr_on == 'funcId') get_funcId(data) else get_dim(data)

    for (j in seq_along(aggr_attr)) {
      dsList_filetered <- if(aggr_on == 'funcId') subset(data,funcId==aggr_attr[[j]])
      else subset(data, DIM==aggr_attr[[j]])

      RTall <- get_runtimes(dsList_filetered)
      RTval <- max(RTall)
      runtimes <- c(runtimes,RTval)
    }
    runtimes
  }

  render_FCEPlot_aggr_plot <- reactive({
    #TODO: figure out how to avoid plotting again when default targets are written to input
    data <- DATA_RAW()
    if(length(data) == 0) return(NULL)
    if(input$FCEPlot.Aggr.Aggregator == 'Functions'){
      data <- subset(data, DIM==input$Overall.Dim)
      fvs <- MEAN_FVALS_FUNC()
    }
    else{
      data <- subset(data, funcId==input$Overall.Funcid)
      fvs <- MEAN_FVALS_DIM()
    }
    aggr_on = ifelse(input$FCEPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
    aggr_attr <- if(aggr_on == 'funcId') get_funcId(data) else get_dim(data)
    update_targets <- F
    update_data <- T
    if(input$FCEPlot.Aggr.Targets == ""){
      update_targets <- T
    }
    else{
      runtimes <- as.numeric(unlist(strsplit(input$FCEPlot.Aggr.Targets,",")))
      runtimes2 <- get_max_runtimes(data, aggr_on)
      if(runtimes == runtimes2)
        update_data <- F
      if(length(runtimes) != length(aggr_attr)){
        update_targets <- T
      }
    }
    if(update_targets){
      runtimes <- get_max_runtimes(data, aggr_on)
      updateTextInput(session, 'FCEPlot.Aggr.Targets', value = runtimes %>% toString)
      return(NULL)
    }
    if(update_data)
      fvs <- mean_FVs(data, aggr_on, runtimes)
    plot_FCE_AGGR(data, plot_mode = input$FCEPlot.Aggr.Mode, runtimes = runtimes,
                  scale.ylog = input$FCEPlot.Aggr.Logy,
                  use_rank = input$FCEPlot.Aggr.Ranking,
                  aggr_on = aggr_on, fvs = fvs)

  })

  output$FCEPlot.Aggr.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_AGGR)
    },
    content = function(file) {
      save_plotly(render_FCEPlot_aggr_plot(), file,
                  format = input$ERTPlot.Aggr.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FCEPlot.Aggr.Format)
  )


  # empirical p.d.f. of the target value
  render_FV_PDF <- reactive({
    req(input$FCEPDF.Bar.Runtime)
    runtime <- input$FCEPDF.Bar.Runtime %>% as.integer
    plot_FV_PDF(DATA(), runtime, show.sample = input$FCEPDF.Bar.Samples,
                scale.ylog = input$FCEPDF.Bar.Logy )
  })

  output$FCEPDF.Bar.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_PDF)
    },
    content = function(file) {
      save_plotly(render_FV_PDF(), file,
                  format = input$FCEPDF.Bar.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FCEPDF.Bar.Format)
  )

  output$FCE_PDF <- renderPlotly({
    render_FV_PDF()
  })

  # historgram of the target values -----------
  render_FV_HIST <- reactive({
    req(input$FCEPDF.Hist.Runtime != "")   # require non-empty input
    runtime <- input$FCEPDF.Hist.Runtime %>% as.integer
    plot_FV_HIST(DATA(), runtime, plot_mode = input$FCEPDF.Hist.Mode)
  })

  output$FCEPDF.Hist.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_HIST)
    },
    content = function(file) {
      save_plotly(render_FV_HIST(), file,
                  format = input$FCEPDF.Hist.Format,
                  width = fig_width2, height = fig_height2)
    },
    contentType = paste0('image/', input$FCEPDF.Hist.Format)
  )

  output$FCE_HIST <- renderPlotly({
    render_FV_HIST()
  })

  # The ECDF plots for the target value ----------------
  output$FCE_ECDF_PER_TARGET <- renderPlotly({
    req(input$FCEECDF.Single.Target)
    runtimes <- as.integer(input$FCEECDF.Single.Target)
    plot_FCE_ECDF_PER_TARGET(DATA(),runtimes, scale.xlog = input$FCEECDF.Single.Logx)
  })

  output$FCE_RT_GRID <- renderPrint({
    req(input$FCEECDF.Mult.Min, input$FCEECDF.Mult.Max, input$FCEECDF.Mult.Step)

    rt_min <- input$FCEECDF.Mult.Min %>% as.integer
    rt_max <- input$FCEECDF.Mult.Max %>% as.integer
    rt_step <- input$FCEECDF.Mult.Step %>% as.integer

    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- get_runtimes(data)

    seq_RT(rt, from = rt_min, to = rt_max, by = rt_step) %>% cat
  })

  render_FV_ECDF_AGGR <- reactive({
    req(input$FCEECDF.Mult.Min, input$FCEECDF.Mult.Max, input$FCEECDF.Mult.Step)

    rt_min <- input$FCEECDF.Mult.Min %>% as.integer
    rt_max <- input$FCEECDF.Mult.Max %>% as.integer
    rt_step <- input$FCEECDF.Mult.Step %>% as.integer

    plot_FV_ECDF_AGGR(DATA(),rt_min = rt_min,
                      rt_max = rt_max, rt_step = rt_step,
                      scale.xlog = input$FCEECDF.Mult.Logx,
                      show.per_target = input$FCEECDF.Mult.Targets)
  })

  output$FCEECDF.Mult.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_ECDF_AGGR)
    },
    content = function(file) {
      save_plotly(render_FV_ECDF_AGGR(), file,
                  format = input$FCEECDF.Mult.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FCEECDF.Mult.Format)
  )

  output$FCE_ECDF_AGGR <- renderPlotly({
    render_FV_ECDF_AGGR()
  })

  # evaluation rake of all courses
  render_FV_AUC <- reactive({
    req(input$FCEECDF.AUC.Min, input$FCEECDF.AUC.Max, input$FCEECDF.AUC.Step)

    rt_min <- input$FCEECDF.AUC.Min %>% as.integer
    rt_max <- input$FCEECDF.AUC.Max %>% as.integer
    rt_step <- input$FCEECDF.AUC.Step %>% as.integer
    plot_FV_AUC(DATA(), rt_min = rt_min,
                rt_max = rt_max, rt_step = rt_step)

  })

  output$FCEECDF.AUC.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_FV_AUC)
    },
    content = function(file) {
      save_plotly(render_FV_AUC(), file,
                  format = input$FCEECDF.AUC.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$FCEECDF.AUC.Format)
  )

  output$FCE_AUC <- renderPlotly({
    render_FV_AUC()
  })

  # Expected Evolution of parameters in the algorithm
  render_PAR_PER_FUN <- reactive({
    req(input$PAR.Plot.Min, input$PAR.Plot.Max)

    f_min <- format_FV(input$PAR.Plot.Min) %>% as.numeric
    f_max <- format_FV(input$PAR.Plot.Max) %>% as.numeric
    tryCatch(
    plot_PAR_Line(DATA(),f_min,f_max,algids = input$PAR.Plot.Algid,
                  show.mean = (input$PAR.Plot.show.mean == 'mean'),
                  show.median = (input$PAR.Plot.show.mean == 'median'),
                  scale.xlog = input$PAR.Plot.Logx,
                  scale.ylog = input$PAR.Plot.Logy),
      error = function(e) {
        #TODO: more robust error handling; don't assume this causes the error
        shinyjs::alert("Not all algorithms contain the same parameters. Please select a single algorithm to plot instead.")
      }
    )
  })

  output$PAR.Plot.Download <- downloadHandler(
    filename = function() {
      eval(FIG_NAME_PAR_PER_FUN)
    },
    content = function(file) {
      save_plotly(render_PAR_PER_FUN(), file,
                  format = input$PAR.Plot.Format,
                  width = fig_width2, height = fig_height)
    },
    contentType = paste0('image/', input$PAR.Plot.Format)
  )

  output$PAR_PER_FUN <- renderPlotly({
    render_PAR_PER_FUN()
  })

  # TODO: add ks test for ECDF later
  # output$ks <- renderPrint({
  #   target <- input$target %>% as.numeric
  #   df.aligneds <- aligned()
  #
  #   running_time <- list()
  #   for (i in seq_along(df.aligneds)) {
  #     df <- df.aligneds[[i]]
  #     v <- rownames(df) %>% as.numeric
  #     idx <- order(abs(target - v))[1]
  #     running_time[[i]] <- df[idx, ] %>% as.vector
  #   }
  #   algorithm1 <- running_time[[1]]
  #   algorithm2 <- running_time[[2]]
  #   a <- ks.test(algorithm1, algorithm2, alternative = 'less')
  #   print(a)
  # })
  #

  parameter_summary <- reactive({
    req(input$PAR.Summary.Min, input$PAR.Summary.Max, input$PAR.Summary.Step)

    fstart <- format_FV(input$PAR.Summary.Min) %>% as.numeric
    fstop <- format_FV(input$PAR.Summary.Max) %>% as.numeric
    fstep <- format_FV(input$PAR.Summary.Step) %>% as.numeric

    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- get_funvals(data)

    if (input$PAR.Summary.Single)
      fstop <- fstart

    fseq <- seq_FV(fall, fstart, fstop, by = fstep)
    req(fseq)

    get_PAR_summary(data, fseq, input$PAR.Summary.Algid, input$PAR.Summary.Param)
  })

  parameter_sample <- reactive({
    req(input$PAR.Sample.Algid, input$PAR.Sample.Max,
        input$PAR.Sample.Step, input$PAR.Sample.Min,
        input$PAR.Sample.Param)

    fstart <- format_FV(input$PAR.Sample.Min) %>% as.numeric
    fstop <- format_FV(input$PAR.Sample.Max) %>% as.numeric
    fstep <- format_FV(input$PAR.Sample.Step) %>% as.numeric

    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- get_funvals(data)

    if (input$PAR.Sample.Single)
      fstop <- fstart

    fseq <- seq_FV(fall, fstart, fstop, by = fstep)
    req(fseq)

    get_PAR_sample(data, ftarget = fseq,
                   algorithm = input$PAR.Sample.Algid,
                   parId = input$PAR.Sample.Param,
                   output = input$PAR.Sample.Format)
  })

  output$table_PAR_SAMPLE <- renderDataTable({
    dt <- parameter_sample()
    req(length(dt) != 0)
    dt[is.na(dt)] <- 'NA'
    dt}, options = list(pageLength = 20, scrollX = T))

  output$table_PAR_summary <- renderTable({
    dt <- parameter_summary()
    req(length(dt) != 0)
    dt$runs %<>% as.integer
    dt$mean %<>% format(digits = 2, nsmall = 2)
    dt$median %<>% format(digits = 2, nsmall = 2)

    # TODO: make probs as a global option
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.

    # format the integers
    # for (p in paste0(probs * 100, '%')) {
    #   df[[p]] %<>% format(digits = 2, nsmall = 2)
    # }
    dt
  })

  output$PAR.Sample.Download <- downloadHandler(
    filename = {
      fstart <- format_FV(input$PAR.Sample.Min)
      fstop <- format_FV(input$PAR.Sample.Max)
      fstep <- format_FV(input$PAR.Sample.Step)
      eval(PARSample_csv_name)
    },
    content = function(file) {
      write.csv(parameter_sample(), file, row.names = F)
    },
    contentType = "text/csv"
  )

  output$PAR.Summary.Download <- downloadHandler(
    filename = {
      fstart <- format_FV(input$PAR.Summary.Min)
      fstop <- format_FV(input$PAR.Summary.Max)
      fstep <- format_FV(input$PAR.Summary.Step)
      eval(PAR_csv_name)
    },
    content = function(file) {
      write.csv(parameter_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
})
