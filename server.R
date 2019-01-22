#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com
# 
# TODO:
#   * add 'shiny::req' to all the functions when the input might be insufficient
#   * rename most of the control widgets in a uniform and understandable scheme
suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(reshape2))
suppressMessages(library(magrittr))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))

for (f in list.files('pproc', pattern = '.R', full.names = T)) {
  source(f)
}

source('plot.R')

options(width = 80)
options(shiny.maxRequestSize = 200 * 1024 ^ 2)   # maximal number of requests, this is too many...

symbols <- c("circle-open", "diamond-open", "square-open", "cross-open",
             "triangle-up-open", "triangle-down-open")

# TODO: put it as an option such that the user can select
maximization <- "MAXIMIZE"
src_format <- AUTOMATIC # TODO: this shoule be taken from the data set
sub_sampling <- TRUE

# Inserts values from "from_data" to "to_data" that are better than were
insert_best_parts <- function(from_data, to_data, best_is_min) {
  if (all(is.na(from_data)))
    to_data
  else
    if (best_is_min)
      pmin(from_data, to_data)
  else
    pmax(from_data, to_data)
}

# Formatter for function values
format_FV <- function(v) format(v, digits = 2, nsmall = 2)
format_RT <- function(v) as.integer(v)

# transformations applied on the function value
# trans_funeval <- `log10`
# reverse_trans_funeval <- function(x) 10 ^ x

# transformations applied on runtime values

# directory where data are extracted from the zip file
exdir <- file.path(Sys.getenv('HOME'), 'data')

setTextInput <- function(session, id, name, alternative) {
  v <- REG[[id]]
  if (name %in% names(v)) {
    updateTextInput(session, id, value = v[[name]])
  } else
    updateTextInput(session, id, value = alternative)
}

#TODO: this function could be made more clear
set_format <- function(format){
  format_FV <<- ifelse((format == COCO),
                       function(v) format(v, format = "e", digits = 5, nsmall = 2),
                       function(v) format(v, digits = 2, nsmall = 2))
}

# register previous text inputs, which is used to restore those values
REG <- lapply(widget_id, function(x) list())

# TODO: maybe give this function a better name
# get the current 'id' of the selected data: funcID + DIM 
get_data_id <- function(dsList) {
  if (is.null(dsList) | length(dsList) == 0)
    return(NULL)
  
  paste(getfuncId(dsList), getDIM(dsList), sep = '-')
}

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  # clean up the temporarsy files on server when exiting  
  session$onSessionEnded(function() {
    unlink(exdir, recursive = T)
  })
  
  # shared reactive variables
  folderList <- reactiveValues(data = list())
  DataList <- reactiveValues(data = DataSetList())
  
  
  # update maximization indication, trans_funeval according to src_format 
  observe({
    selected_format <<- input$DATA_SRC_FORMAT
    maximization <<- input$DATA_SRC_MINMAX
  })
  
  # should subsamping be turned on?
  observe({
    sub_sampling <<- input$SUBSAMPLING  
  })
  
  # IMPORTANT: this only works locally, keep it for the local version
  # links to users file systems
  # volumes <- getVolumes()
  # shinyDirChoose(input, 'directory', roots = volumes, session = session)
  
  # browse data directory, upload data and process data -------
  
  # the directory selected by the user
  # selected_folders <- reactive({
  #   # a <- list(root = "Macintosh HD", path = list('', "Users", "wanghao", "(1+1)-Cholesky-CMA"))
  #   # parseDirPath(volumes, a)
  #   parseDirPath(volumes, input$directory)
  # })
  
  # the folder where the uploaded zip file is uncompressed
  selected_folders <- reactive({
    if (!is.null(input$ZIP)) {
      datapath <- input$ZIP$datapath
      filename <- input$ZIP$name
      folders <- rep('', length(datapath))
      
      for (i in seq(datapath)) {
        unzip(datapath[i], list = FALSE, exdir = exdir)
        name <- strsplit(filename[i], '\\.')[[1]][1]
        folders[i] <- file.path(exdir, name)
      }
      return(folders)
    } else
      return(NULL)
  })
  
  # TODO: this part might not be
  # observe({
  #   if (input$singleF) {
  #     fstart <- input$fstart
  #     # updateTextInput(session, 'fstart', value = format_FV(start))
  #     updateTextInput(session, 'fstop', value = format_FV(fstart))
  #     # updateTextInput(session, 'fstep', value = format_FV(step))
  #   }
  # })
  # 
  # # print the folderList
  # output$upload_data_promt <- renderPrint({
  #   folders <- folderList$data
  #   for (i in seq_along(folders)) {
  #     cat(sprintf('%d: %s\n', i, folders[[i]]))
  #   }
  # })
  
  # load, process the data folders and update DataSetList
  observeEvent(selected_folders(), {
    folders <- selected_folders()
    req(length(folders) != 0)
    
    if (length(folderList$data) == 0)
      folder_new <- folders
    else
      folder_new <- setdiff(folders, intersect(folderList$data, folders))
    
    req(length(folder_new) != 0)
    
    for (folder in folder_new) {
      indexFiles <- scan_indexFile(folder)
      if (length(indexFiles) == 0) 
        shinyjs::html("process_data_promt", 
                      paste0('<p style="color:red;">No index file (.info) is found in folder ', 
                             folder, '... skip</p>'), add = TRUE)
      else {
        folderList$data <- c(folderList$data, folder)
        
        # check if the newly loaded data contradicts the selected format
        found_format <- check_format(folder)
        
        if(selected_format == AUTOMATIC){
          set_format(found_format)
          format <- found_format
        }
        else if (found_format != selected_format && (selected_format != TWO_COL || found_format == COCO)){
          shinyjs::html("process_data_promt", 
                        paste0('<p style="color:red;">Format specified does not match format found (', 
                                found_format, ')... skip</p>'), add = TRUE)
          break
        }
        else{
          format <- selected_format
        }
        
        if(maximization == AUTOMATIC){
          minmax <- ifelse((found_format == COCO),FALSE,TRUE)
        }
        else{
          minmax <- ifelse((maximization == "MAXIMIZE"),TRUE,FALSE)
        }
        print_fun <- function(s) shinyjs::html("process_data_promt", s, add = TRUE)
        DataList$data <- c(DataList$data, read_dir(folder, print_fun = print_fun,
                                                   maximization = minmax,
                                                   format = format,
                                                   subsampling = sub_sampling))
        src_format <<- format
        shinyjs::html("upload_data_promt", 
                      sprintf('%d: %s\n', length(folderList$data), folder), add = TRUE)
      }
    } 
  })
  
  # remove all uploaded data set
  observeEvent(input$RM_DATA, {
    if (length(DataList$data) != 0) {
      DataList$data <- DataSetList() # must be a 'DataSetList'
      folderList$data <- list() 
      shinyjs::html("process_data_promt", '<p style="color:red;">all data are removed!</p>', add = FALSE)
      shinyjs::html("upload_data_promt", "", add = FALSE)
    }
  })
  
  # show the detailed information on DataSetList
  output$DATASETLIST_INFO <- renderDataTable({
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
    
    dim <- getDIM(data)
    updateSelectInput(session, 'DIM_INPUT', choices = dim, selected = dim[1])
    
    funcID <- getfuncId(data)
    updateSelectInput(session, 'FUNCID_INPUT', choices = funcID, selected = funcID[1])
    
    algId <- c(getAlgId(data), 'all')
    updateSelectInput(session, 'ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'ALGID_RAW_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'PAR_ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'FCE_ALGID_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'FCE_ALGID_RAW_INPUT', choices = algId, selected = 'all')
    updateSelectInput(session, 'PAR_ALGID_INPUT_SUMMARY', choices = algId, selected = 'all')
    updateSelectInput(session, 'PAR_ALGID_INPUT_SAMPLE', choices = algId, selected = 'all')
    
    parId <- c(getParId(data), 'all')
    updateSelectInput(session, 'PAR_INPUT', choices = parId, selected = 'all')
    updateSelectInput(session, 'PAR_INPUT_SAMPLE', choices = parId, selected = 'all')
  })
  
  # update (filter) according to users selection DataSets 
  DATA <- reactive({
    dim <- input$DIM_INPUT
    id <- input$FUNCID_INPUT
    
    if (length(DataList$data) == 0)
      return(NULL)
    
    subset(DataList$data, DIM == dim, funcId == id)
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
    v <- getFunvals(data)
    req(v)
    
    # choose proper scaling for the function value
    # v <- trans_funeval(v)    # Do the scaling in seq_FV instead
    q <- quantile(v, probs = c(.25, .5, .75), names = F)
    
    # TODO: we need to fix this for the general case!!!
    length.out <- 10
    fseq <- seq_FV(v, length.out = length.out)
    start <- fseq[1]
    stop <- fseq[length.out]

    #TODO: Make more general
    if(abs(2*fseq[3]-fseq[2] - fseq[4]) < 1e-12) #arbitrary precision
        step <- fseq[3]-fseq[2]
    else
      step <- log10(fseq[3]) - log10(fseq[2])
    
    name <- get_data_id(data)
    setTextInput(session, 'fstart', name, alternative = format_FV(start))
    setTextInput(session, 'fstop', name, alternative = format_FV(stop))
    setTextInput(session, 'fstep', name, alternative = format_FV(step))
    
    setTextInput(session, 'F_MIN_SAMPLE', name, alternative = format_FV(start))
    setTextInput(session, 'F_MAX_SAMPLE', name, alternative = format_FV(stop))
    setTextInput(session, 'F_STEP_SAMPLE', name, alternative = format_FV(step))
    
    setTextInput(session, 'RT_fstart', name, alternative = format_FV(start))
    setTextInput(session, 'RT_fstop', name, alternative = format_FV(stop))
    setTextInput(session, 'RT_fstep', name, alternative = format_FV(step))
    setTextInput(session, 'RT_fselect', name, alternative = format_FV(median(v)))
    
    setTextInput(session, 'RT_PMF_FTARGET', name, alternative = format_FV(median(v)))
    setTextInput(session, 'RT_PMF_HIST_FTARGET', name, alternative = format_FV(median(v)))
    
    # s <- ((stop - start) * 0.1 + start)
    # e <- ((stop - start) * 0.9 + start)
    setTextInput(session, 'ERT_FSTART', name, alternative = format_FV(start))
    setTextInput(session, 'ERT_FSTOP', name, alternative = format_FV(stop))
    
    setTextInput(session, 'RT_ECDF_FTARGET1', name, alternative = format_FV(q[1]))
    setTextInput(session, 'RT_ECDF_FTARGET2', name, alternative = format_FV(q[2]))
    setTextInput(session, 'RT_ECDF_FTARGET3', name, alternative = format_FV(q[3]))
    
    setTextInput(session, 'RT_AUC_FSTART', name, alternative = format_FV(start))
    setTextInput(session, 'RT_AUC_FSTOP', name, alternative = format_FV(stop))
    setTextInput(session, 'RT_AUC_FSTEP', name, alternative = format_FV(step))
    
    setTextInput(session, 'PAR_F_MIN', name, alternative = format_FV(start))
    setTextInput(session, 'PAR_F_MAX', name, alternative = format_FV(stop))
    
    setTextInput(session, 'PAR_F_MIN_SUMMARY', name, alternative = format_FV(start))
    setTextInput(session, 'PAR_F_MAX_SUMMARY', name, alternative = format_FV(stop))
    setTextInput(session, 'PAR_F_STEP_SUMMARY', name, alternative = format_FV(step))
    
    setTextInput(session, 'PAR_F_MIN_SAMPLE', name, alternative = format_FV(start))
    setTextInput(session, 'PAR_F_MAX_SAMPLE', name, alternative = format_FV(stop))
    setTextInput(session, 'PAR_F_STEP_SAMPLE', name, alternative = format_FV(step))
    
  })
  
  # update the values for the grid of running times
  observe({
    data <- DATA()
    v <- getRuntimes(data)
    
    if (length(v) == 0)
      return()
    
    # TODO: this part should be made generic!!!
    v <- v  %>% as.integer  
    q <- quantile(v, probs = c(.25, .5, .75), names = F, type = 3)
    
    grid <- seq(min(v), max(v), length.out = 10) %>% as.integer
    step <- max(1, min(grid[-1] - grid[-length(grid)])) 
    
    start <- min(v)
    stop <- max(v)
    
    name <- get_data_id(data)
    setTextInput(session, 'RT_MIN', name, alternative = min(v))
    setTextInput(session, 'RT_MAX', name, alternative = max(v))
    setTextInput(session, 'RT_STEP', name, alternative = step)
    
    setTextInput(session, 'RT_MIN_SAMPLE', name, alternative = min(v))
    setTextInput(session, 'RT_MAX_SAMPLE', name, alternative = max(v))
    setTextInput(session, 'RT_STEP_SAMPLE', name, alternative = step)
    
    setTextInput(session, 'FCE_HIST_RUNTIME', name, alternative = median(v))
    setTextInput(session, 'FCE_PDF_RUNTIME', name, alternative = median(v))
    
    # s <- ((max(v) - min(v)) * 0.05 + min(v)) %>% as.integer
    # e <- ((max(v) - min(v)) * 0.95 + min(v)) %>% as.integer
    setTextInput(session, 'FCE_RT_MIN', name, alternative = start)
    setTextInput(session, 'FCE_RT_MAX', name, alternative = stop)
    
    setTextInput(session, 'FCE_ECDF_RT_MIN', name, alternative = min(v))
    setTextInput(session, 'FCE_ECDF_RT_MAX', name, alternative = max(v))
    setTextInput(session, 'FCE_ECDF_RT_STEP', name, alternative = step)
    
    setTextInput(session, 'FCE_AUC_RT_MIN', name, alternative = min(v))
    setTextInput(session, 'FCE_AUC_RT_MAX', name, alternative = max(v))
    setTextInput(session, 'FCE_AUC_RT_STEP', name, alternative = step)
    
    setTextInput(session, 'FCE_ECDF_RT1', name, alternative = q[1])
    setTextInput(session, 'FCE_ECDF_RT2', name, alternative = q[2])
    setTextInput(session, 'FCE_ECDF_RT3', name, alternative = q[3])
  })
  
  # Data summary for Fixed-Target Runtime (ERT)  --------------
  runtime_summary <- reactive({
    req(input$fstart, input$fstop, input$fstep)
    
    fstart <- format_FV(input$fstart) %>% as.numeric
    fstop <- format_FV(input$fstop) %>% as.numeric
    fstep <- format_FV(input$fstep) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    
    data <- DATA()
    fall <- getFunvals(data)
    
    if (input$singleF)
      fstop <- fstart
    
    fseq <- seq_FV(fall, fstart, fstop, fstep)
    req(fseq)
    
    get_RT_summary(data, fseq, algorithm = input$ALGID_INPUT)
  })
  
  output$table_RT_summary <- renderTable({
    df <- runtime_summary()
    df$runs %<>% as.integer
    df$median %<>% as.integer
    df$target <- format_FV(df$target)
    
    # format the integers
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% as.integer
    }
    df
  })
  
  output$downloadData <- downloadHandler(
    filename = {
      data <- DATA()
      fstart <- format_FV(input$fstart)
      fstop <- format_FV(input$fstop)
      fstep <- format_FV(input$fstep) 
      eval(RT_csv_name)
      # sprintf('runtime_summary_(%s,%s,%s).csv',
      #         fstart, fstop, fstep)
    }, 
    content = function(file) {
      write.csv(runtime_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  get_RT <- reactive({
    req(input$fstart, input$fstop, input$fstep)
    
    fstart <- format_FV(input$F_MIN_SAMPLE) %>% as.numeric
    fstop <- format_FV(input$F_MAX_SAMPLE) %>% as.numeric
    fstep <- format_FV(input$F_STEP_SAMPLE) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    
    # we have to remove this part from the dependency of this reactive expression
    isolate({
      data <- DATA()
      fall <- getFunvals(data)
    })
    
    if (input$F_SAMPLE_SINGLE)
      fstop <- fstart
    
    fseq <- seq_FV(fall, fstart, fstop, fstep)
    req(fseq)
    # res <- list()
    # n_runs_max <- sapply(data, function(ds) length(attr(ds, 'instance'))) %>% max
    
    get_RT_sample(data, ftarget = fseq, algorithm = input$ALGID_RAW_INPUT, 
                  output = input$RT_download_format)
    
    # for (i in seq_along(data)) {
    #   ds <- data[[i]]
    #   algId <- attr(ds, 'algId')
    #   if (input$ALGID_RAW_INPUT != 'all' && algId != input$ALGID_RAW_INPUT)
    #     next
    #   
    #   rt <- get_RT_sample(ds, fseq, output = input$RT_download_format)
    #   if (input$RT_download_format == 'wide') {
    #     n <- ncol(rt) - 2
    #     if (n < n_runs_max) 
    #       rt %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
    #   }
    #   res[[i]] <- rt
    # }
    # do.call(rbind, res) 
  })
  
  output$download_runtime <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(getAlgId(data), collapse = ';')
      fstart <- input$F_MIN_SAMPLE %>% format_FV
      fstop <- input$F_MAX_SAMPLE %>% format_FV
      fstep <- input$F_STEP_SAMPLE %>% format_FV
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
    df}, options = list(pageLength = 20, scrollX = T))
  
  # The expected runtime plot ---------------------
  # TODO: wrapp this as a separate function for DataSet class
  output$ERT_PER_FUN <- renderPlotly({
    req(input$ERT_FSTART, input$ERT_FSTOP)
    
    Fmin <- format_FV(input$ERT_FSTART) %>% as.numeric 
    Fmax <- format_FV(input$ERT_FSTOP) %>% as.numeric 
    
    req(Fmin <= Fmax)
    
    data <- DATA()
    fall <- getFunvals(data)
    
    if (input$singleF)
      fstop <- fstart
    
    fseq <- seq_FV(fall, Fmin, Fmax, length.out = 60)
    req(fseq)
    
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    dt <- get_RT_summary(data, fseq)
    dt[, `:=`(upper = ERT + sd, lower = ERT - sd)]
    
    dr <- get_RT_sample(data, fseq)
    
    p <- plot_ly_default(x.title = "best-so-far f(x)-value", 
                         y.title = "function evaluations")
    
    for (i in seq_along(data)) {
      algId <- attr(data[[i]], 'algId')
      ds_ERT <- dt[algId == attr(data[[i]], 'algId')]
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
      
      p %<>% 
        add_trace(data = ds_ERT, x = ~target, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0),  
                  showlegend = F, name = 'mean +/- sd') %>% 
        add_trace(x = ~target, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd')
      
      if (input$show.ERT)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~ERT, type = 'scatter',
                         name = paste0(algId, '.ERT'), mode = 'lines+markers', 
                         marker = list(color = rgb_str),  
                         line = list(color = rgb_str))
      
      if (input$show.mean)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~mean, type = 'scatter', 
                         mode = 'lines+markers', name = paste0(algId, '.mean'), 
                         marker = list(color = rgb_str), 
                         line = list(color = rgb_str, dash = 'dash'))
      
      if (input$show.median)
        p %<>% add_trace(data = ds_ERT, x = ~target, y = ~median, type = 'scatter',
                         name = paste0(algId, '.median'), mode = 'lines+markers', 
                         marker = list(color = rgb_str),  
                         line = list(color = rgb_str, dash = 'dot'))
      
      if (input$show_all) {
        #TODO: Fix this for the case where algorithms do not have the same number of runs
        line_width <- 0.5
        
        dr_ERT <- dr[algId == attr(data[[i]], 'algId')]
        names_to_show = sample(colnames(dr_ERT))
        names_to_show <-
          names_to_show[!names_to_show %in% c('algId', 'target')]
        
        dens <- input$show.density
        counter <- as.integer(length(names_to_show) * dens / 100)
        names_to_show <- head(names_to_show, counter)
        best_parts <- NA
        mentioned <- FALSE
        
        for (run_v in names_to_show) {
          p %<>% add_trace(
            data = dr_ERT,
            x = ~ target,
            y = dr_ERT[[run_v]],
            type = 'scatter',
            mode = 'lines',
            line = list(color = rgb_str, width = line_width),
            #legendgroup = paste("runs of ", algId),
            #legend = list(traceorder = "grouped"),
            text = paste(run_v),
            hoverinfo='none',
            # hoverinfo = "x+y+text",
            # hoverlabel = list(font=list(size = 8)),
            showlegend = !mentioned,
            name = paste("runs of ", algId)
          )
          mentioned <- TRUE
          best_parts <-
            insert_best_parts(best_parts, dr_ERT[[run_v]], TRUE)
        }
        
        if (input$show.best_of_all) {
          mentioned <- FALSE
          check_value <- tail(best_parts, 1)
          for (run_v in names_to_show)
            if (check_value == tail(dr_ERT[[run_v]], 1)) {
              p %<>% add_trace(
                data = dr_ERT,
                x = ~ target,
                y = dr_ERT[[run_v]],
                type = 'scatter',
                mode = 'lines',
                line = list(color = rgb_str, width = line_width *
                              3),
                showlegend = !mentioned,
                name = paste("best.", algId)
              )
              mentioned <- TRUE
            }
        }
        if (input$show.pareto_optima) {
          p %<>% add_trace(
            x = dr_ERT[['target']],
            y = best_parts,
            type = 'scatter',
            mode = 'lines',
            line = list(color = rgb_str, width = line_width *
                          5, dash = 'dot'),
            showlegend = T,
            name = paste("pareto_optima.", algId)
          )
        }
        
      }
    }
    p %<>%
      layout(xaxis = list(type = ifelse(input$semilogx, 'log', 'linear')),
             yaxis = list(type = ifelse(input$semilogy, 'log', 'linear')))
    
    # minimization for COCO
    if (src_format == 'COCO')
      p %<>% layout(xaxis = list(autorange = "reversed"))
    p
  })
  
  # empirical p.m.f. of the runtime
  output$RT_PMF <- renderPlotly({
    req(input$RT_PMF_FTARGET) 
    ftarget <- format_FV(input$RT_PMF_FTARGET) %>% as.numeric
    points <- ifelse(input$RT_SHOW_SAMPLE, 'all', FALSE)
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(x.title = "algorithms",
                         y.title = "runtime / function evaluations")
  
    for (i in seq_along(data)) {
      ds <- data[[i]]
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.5)')
      
      p %<>%
        add_trace(data = get_RT_sample(ds, ftarget, output = 'long'),
                  x = ~algId, y = ~RT, split = ~algId, type = 'violin',
                  hoveron = "points+kde",
                  box = list(visible = T),
                  points = points,
                  pointpos = 1,
                  jitter = 0.1,
                  scalemode = 'count',
                  meanline = list(visible = T),
                  line = list(color = rgb_str, width = 1),
                  marker = list(color = rgb_str))
      
      # rt <- RT(ds, ftarget, format = 'long') %>% 
      #   mutate(label = i)
      
      # kernel estimation of p.m.f.
      # res <- kernel_PMF(as.numeric(rt$RT))
      # 
      # x <- res$x
      # y <- res$y
      # 
      # x <- x[y != 0]
      # y <- y[y != 0] 
      # idx <- seq(1, length(x), length.out = 50)
      # x <- x[idx]
      # y <- y[idx]
      # y <- y / max(y) * 0.8
      
      # p %<>% 
      #   add_trace(x = x, y = y + i, type = 'scatter',
      #             hoveron = "points", showlegend = F,
      #             mode = 'markers', legendgroup = paste0(i),
      #             marker = list(color = 'rgba(9,56,125,0.45)'), 
      #             line = list(color = 'rgb(9,56,125)', width = 0)) %>%
      #   add_trace(data = rt, x = ~RT, y = ~as.character(label), type = 'box',
      #             line = list(color = rgb_str, width = 0.8), legendgroup = paste0(i),
      #             marker = list(color = rgb_str), fillcolor = rgba_str, name = attr(df, 'algorithm'))
      # 
      # for (k in seq_along(x)) {
      #   p %<>% 
      #     add_segments(x = x[k], xend = x[k], y = i, yend = y[k] + i, 
      #                  line = list(color = 'rgba(9,56,125,0.4)'),
      #                  showlegend = F)
      # }
    }
    p %<>%
      layout(yaxis = list(type = ifelse(input$RT_PMF_LOGY, 'log', 'linear')))
  })
  
  # historgram of the running time
  output$RT_HIST <- renderPlotly({
    req(input$RT_PMF_HIST_FTARGET)
    ftarget <- format_FV(input$RT_PMF_HIST_FTARGET) %>% as.numeric
    plot_mode <- input$ERT_illu_mode
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    nrows <- ceiling(n_algorithm / 2.) # keep to columns for the histograms
    
    if (plot_mode == 'overlay') {
      p <- plot_ly_default(x.title = "function evaluations", y.title = "runs")
    } else if (plot_mode == 'subplot') {
      p <- lapply(seq(n_algorithm), function(x) {
        plot_ly_default(x.title = "function evaluations", y.title = "runs")
      })
    }
    
    for (i in seq_along(data)) {
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.35)')
      
      df <- data[[i]]
      algId <- attr(df, 'algId')
      rt <- get_RT_sample(df, ftarget, output = 'long')
      
      # skip if all runtime samples are NA
      if (sum(!is.na(rt$RT)) < 2)
        next
      
      res <- hist(rt$RT, breaks = nclass.FD, plot = F)
      breaks <- res$breaks
      plot_data <- data.frame(x = res$mids, y = res$counts, width = breaks[2] - breaks[1],
                         text = paste0('<b>count</b>: ', res$counts, '<br><b>breaks</b>: [', 
                                       breaks[-length(breaks)], ',', breaks[-1], ']')) 
      
      if (plot_mode == 'overlay') {
        p %<>%
          add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = algId, text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      } else if (plot_mode == 'subplot') {
        p[[i]] %<>% 
          add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = algId, text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      }
    }
    
    if (plot_mode == 'subplot') {
      p <- subplot(p, nrows = nrows, titleX = F, titleY = F, margin = 0.05)
    }
    p
  })
  
  # The ECDF plots for the runtime ----------------
  output$RT_ECDF <- renderPlotly({
    req(input$RT_ECDF_FTARGET1, input$RT_ECDF_FTARGET2, input$RT_ECDF_FTARGET3)
    ftargets <- c(
      format_FV(input$RT_ECDF_FTARGET1),
      format_FV(input$RT_ECDF_FTARGET2),
      format_FV(input$RT_ECDF_FTARGET3)) %>% 
      as.numeric 
      
    
    ftargets <- ftargets[!is.na(ftargets)]
    req(length(ftargets) != 0)
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(title = NULL,
                         x.title = "function evaluations",
                         y.title = "Proportion of runs")
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.35)')
      
      for (i in seq_along(ftargets)) {
        rt <- get_RT_sample(df, ftargets[i], output = 'long')$RT %>% sort
        if (all(is.na(rt)))
          next
        
        # TODO: ECDF computation should be put in pproc/stats.R
        ecdf <- CDF_discrete(rt)
        
        # position of the markers
        x <- quantile(rt, probs = c(0.25, 0.5, 0.75), names = F, type = 3)
        y <- sapply(x, function(x) ecdf[rt == x][1])
        
        p %<>%
          add_trace(data = NULL, x = rt, y = ecdf, type = 'scatter',
                    mode = 'lines', name = algId, showlegend = F,
                    legendgroup = paste0(k),
                    line = list(color = rgb_str, width = 3)) %>% 
          add_trace(data = NULL, x = x, y = y, type = 'scatter',
                    mode = 'markers',  legendgroup = paste0(k),
                    name = sprintf('(%s, %.2e)', algId, ftargets[i]),
                    marker = list(color = rgb_str, symbol = symbols[i], size = 13))
      }
    }
    
    p %<>%
      layout(xaxis = list(type = ifelse(input$RT_ECDF_semilogx, 
                                        'log', 'linear')))
  })
  
  output$RT_GRID <- renderPrint({
    req(input$RT_fstart, input$RT_fstop, input$RT_fstep)
    
    fstart <- format_FV(input$RT_fstart) %>% as.numeric
    fstop <- format_FV(input$RT_fstop) %>% as.numeric 
    fstep <- format_FV(input$RT_fstep) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- getFunvals(data)
    
    seq_FV(fall, fstart, fstop, by = fstep) %>% cat
  })
  
  output$RT_ECDF_AGGR <- renderPlotly({
    req(input$RT_fstart, input$RT_fstop, input$RT_fstep)
    
    fstart <- format_FV(input$RT_fstart) %>% as.numeric
    fstop <- format_FV(input$RT_fstop) %>% as.numeric 
    fstep <- format_FV(input$RT_fstep) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- getFunvals(data)
    
    fseq <- seq_FV(fall, fstart, fstop, fstep)
    req(fseq)
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    RT.max <- sapply(data, function(ds) max(ds$RT, na.rm = T)) %>% max
    RT.min <- sapply(data, function(ds) min(ds$RT, na.rm = T)) %>% min
    x <- seq(RT.min, RT.max, length.out = 50)
    p <- plot_ly_default(x.title = "function evaluations",
                         y.title = "Proportion of (run, target) pairs")
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.15)')
      rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.8)')
      
      m <- lapply(fseq, function(f) {
        rt <- get_RT_sample(df, f, output = 'long')$RT
        if (all(is.na(rt)))
          return(rep(0, length(x)))
        fun <- ecdf(rt)
        fun(x)
      }) %>% 
        do.call(rbind, .)
      
      df_plot <- data.frame(x = x, 
                            mean = apply(m, 2, . %>% mean(na.rm = T)),
                            sd = apply(m, 2, . %>% sd(na.rm = T))) %>% 
        mutate(upper = mean + sd, lower = mean - sd)
                              
      p %<>%
        # TODO: maybe not showing the std. shade at all!
        # add_trace(data = df_plot, x = ~x, y = ~upper, type = 'scatter', mode = 'lines',
        #           line = list(color = rgba_str, width = 0),
        #           showlegend = F, name = 'mean +/- sd') %>%
        # add_trace(x = ~x, y = ~lower, type = 'scatter', mode = 'lines',
        #           fill = 'tonexty',  line = list(color = 'transparent'),
        #           fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd') %>%
        add_trace(data = df_plot, x = ~x, y = ~mean, type = 'scatter',
                  mode = 'lines+markers', name = sprintf('%s', algId), 
                  showlegend = T, legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 4.5),
                  marker = list(color = rgb_str, size = 11))
      
      if (input$RT_ECDF_per_target) {
        for (f in fseq) {
          rt <- get_RT_sample(df, f, output = 'long') %>% '$'('RT') %>% sort
          # TODO: plot the unsuccessful ECDF
          if (all(is.na(rt)))
            next
          else 
            v <- CDF_discrete(rt)
          
          p %<>%
            add_trace(x = rt, y = v, type = 'scatter',
                      mode = 'lines', name = algId, showlegend = F,
                      line = list(color = rgba_str2, width = 1, dash = 'dot'))
        }
      }
    }
    
    p %<>%
      layout(xaxis = list(type = ifelse(input$RT_ECDF_AGGR_semilogx, 
                                        'log', 'linear')))
  })
  
  # evaluation rake of all courses 
  output$RT_AUC <- renderPlotly({
    req(input$RT_AUC_FSTART, input$RT_AUC_FSTOP, input$RT_AUC_FSTEP)
    
    fstart <- format_FV(input$RT_AUC_FSTART) %>% as.numeric
    fstop <- format_FV(input$RT_AUC_FSTOP) %>% as.numeric 
    fstep <- format_FV(input$RT_AUC_FSTEP) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- getFunvals(data)
    
    fseq <- seq_FV(fall, fstart, fstop, fstep)
    req(fseq)

    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    RT.max <- sapply(data, function(ds) max(attr(ds, 'maxRT'))) %>% max
    p <- plot_ly_default()
    
    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.2)')
      
      # calculate ECDFs on user specified targets
      funs <- lapply(fseq, function(f) {
        get_RT_sample(df, f, output = 'long')$RT %>% {
          if (all(is.na(.))) NULL
          else  RT.ECDF(.)
        }
      })
      
      auc <- sapply(funs,
                    function(fun) {
                      if (is.null(fun)) 0
                      else integrate(fun, lower = attr(fun, 'min') - 1, upper = RT.max, 
                                     subdivisions = 5e3) %>% {'$'(., 'value') / RT.max}
                    })
      
      p %<>% 
        add_trace(type = 'scatterpolar', r = auc, 
                  theta = paste0('f:', format_FV(fseq)), 
                  fill = 'toself', fillcolor = rgba_str,
                  marker = list(color = rgb_str), hoverinfo = 'text',
                  text = paste0('area: ', format(auc, digits = 2, nsmall = 2)),
                  name = algId) 
    }
    
    p %<>%
      layout(polar = list(radialaxis = list(visible = T)),
             yaxis = list(type = 'log'),
             autosize = T, hovermode = 'compare',
             paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)')
  })
  
  # TODO: rename 'FCE'...
  # Data summary for Fixed-Budget target (FCE)  --------------
  get_FCE_summary <- reactive({
    req(input$RT_MIN, input$RT_MAX, input$RT_STEP)
    
    rt_min <- input$RT_MIN %>% as.integer
    rt_max <- input$RT_MAX %>% as.integer
    rt_step <- input$RT_STEP %>% as.integer
    
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- getRuntimes(data)
    
    if (input$RT_SINGLE)
      rt_max <- rt_min
    
    rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
    req(rt_seq)
    
    get_FV_summary(data, rt_seq, algorithm = input$FCE_ALGID_INPUT)
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
  
  output$FCE_SUMMARY_download <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(getAlgId(data), collapse = ';')
      rt_min <- input$RT_MIN %>% as.integer %>% as.character
      rt_max <- input$RT_MAX %>% as.integer %>% as.character
      rt_step <- input$RT_STEP %>% as.integer %>% as.character
      eval(FV_csv_name)
    },
    content = function(file) {
      write.csv(get_FCE_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  get_FCE <- reactive({
    req(input$RT_MIN_SAMPLE, input$RT_MAX_SAMPLE, input$RT_STEP_SAMPLE)
    rt_min <- input$RT_MIN_SAMPLE %>% as.integer
    rt_max <- input$RT_MAX_SAMPLE %>% as.integer
    rt_step <- input$RT_STEP_SAMPLE %>% as.integer
    
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- getRuntimes(data)
    
    if (input$RT_SINGLE_SAMPLE)
      rt_max <- rt_min
    
    rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
    req(rt_seq)
    
    get_FV_sample(data, rt_seq, algorithm = input$FCE_ALGID_RAW_INPUT,
                  output = input$download_format_FCE)
    
    # res <- list()
    # n_runs_max <- sapply(data, function(x) length(attr(x, 'instance'))) %>% max
    # 
    # for (i in seq_along(data)) {
    #   ds <- data[[i]]
    #   algId <- attr(ds, 'algId') 
    #   if (input$FCE_ALGID_RAW_INPUT != 'all' && algId != input$FCE_ALGID_RAW_INPUT)
    #     next
    #   
    #   rt <- get_FV_sample(ds, rt_seq, output = input$download_format_FCE)
    #   if (input$download_format_FCE == 'wide') {
    #     # impute the missing records
    #     n <- ncol(rt) - 2
    #     if (n < n_runs_max) 
    #       rt %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
    #   }
    #   res[[i]] <- rt
    # }
    # do.call(rbind, res) 
  })
  
  output$FCE_SAMPLE_download <- downloadHandler(
    filename = {
      data <- DATA()
      algId <- paste0(getAlgId(data), collapse = ';')
      rt_min <- input$RT_MIN %>% as.integer %>% as.character
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
    req(input$FCE_RT_MIN, input$FCE_RT_MAX)
    
    rt_min <- input$FCE_RT_MIN %>% as.integer
    rt_max <- input$FCE_RT_MAX %>% as.integer
    req(rt_min <= rt_max)
    
    data <- DATA()
    rt <- getRuntimes(data)
    rt_seq <- seq_RT(rt, rt_min, rt_max, length.out = 60, 
                     scale = ifelse(input$FCE_semilogx, 'log', 'linear'))
    req(rt_seq)
    
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    fce <- get_FV_summary(data, rt_seq)
    fce[, `:=`(upper = mean + sd, lower = mean - sd)]
    
    fce_runs <- get_FV_sample(data, rt_seq)
    
    p <- plot_ly_default(y.title = "best-so-far f(x)-value", x.title = "runtime")
    
    for (i in seq_along(data)) {
      algId <- attr(data[[i]], 'algId')
      dt_plot <- fce[algId == attr(data[[i]], 'algId')]
      
      if (nrow(dt_plot) == 0)
        next
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
      
      p %<>% 
        add_trace(data = dt_plot, x = ~runtime, y = ~upper, type = 'scatter', mode = 'lines',
                  line = list(color = rgba_str, width = 0), 
                  showlegend = F, name = 'mean +/- sd') %>% 
        add_trace(x = ~runtime, y = ~lower, type = 'scatter', mode = 'lines',
                  fill = 'tonexty',  line = list(color = 'transparent'),
                  fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd')
      
      if (input$FCE_show.mean)
        p %<>% add_trace(data = dt_plot, x = ~runtime, y = ~mean, type = 'scatter', 
                         mode = 'lines+markers', name = sprintf("%s.mean", algId), 
                         marker = list(color = rgb_str), 
                         line = list(color = rgb_str))
      
      if (input$FCE_show.median)
        p %<>% add_trace(data = dt_plot, x = ~runtime, y = ~median, type = 'scatter',
                         name = sprintf("%s.median", algId), mode = 'lines+markers', 
                         marker = list(color = rgb_str),
                         line = list(color = rgb_str, dash = 'dash'))
      
      if (input$FCE_show_all) {
        #TODO: Fix this for the case were algorithms do not have the same number of runs
        min_is_best <- FALSE
        line_width <- 0.5
        
        fce_runs_ERT <- fce_runs[algId == attr(data[[i]], 'algId')]
        names_to_show <- sample(colnames(fce_runs_ERT))
        names_to_show <-
          names_to_show[!names_to_show %in% c('algId', 'runtime')]
        
        dens <- input$FCE_show.density
        counter <- as.integer(length(names_to_show) * dens / 100)
        
        names_to_show <- head(names_to_show, counter)
        best_parts <- NA
        mentioned <- FALSE
        
        for (run_v in names_to_show) {
          p %<>% add_trace(
            data = fce_runs_ERT,
            x = ~ runtime,
            y = fce_runs_ERT[[run_v]],
            type = 'scatter',
            mode = 'lines',
            line = list(color = rgb_str, width = line_width),
            #legendgroup = paste("runs of ", algId),
            #legend = list(traceorder = "grouped"),
            text = paste(run_v),
            hoverinfo = 'none',
            # hoverinfo = "x+y+text",
            # hoverlabel = list(font=list(size = 8)),
            showlegend = !mentioned,
            name = paste("runs of ", algId)
          )
          mentioned <- TRUE
          best_parts <-
            insert_best_parts(best_parts, fce_runs_ERT[[run_v]], min_is_best)
        }
        
        if (input$FCE_show.best_of_all) {
          mentioned <- FALSE
          check_value <- tail(best_parts, 1)
          for (run_v in names_to_show)
            if (check_value == tail(fce_runs_ERT[[run_v]], 1)) {
              p %<>% add_trace(
                data = fce_runs_ERT,
                x = ~ runtime,
                y = fce_runs_ERT[[run_v]],
                type = 'scatter',
                mode = 'lines',
                line = list(color = rgb_str, width = line_width *
                              3),
                showlegend = !mentioned,
                name = paste("best.", algId)
              )
              mentioned = TRUE
            }
        }
        if (input$FCE_show.pareto_optima) {
          p %<>% add_trace(
            x = fce_runs_ERT[['runtime']],
            y = best_parts,
            type = 'scatter',
            mode = 'lines',
            line = list(color = rgb_str, width = line_width *
                          5, dash = 'dot'),
            showlegend = T,
            name = paste("pareto_optima.", algId)
          )
        }
        
      }
    }
    p %<>%
      layout(xaxis = list(type = ifelse(input$FCE_semilogx, 'log', 'linear')),
             yaxis = list(type = ifelse(input$FCE_semilogy, 'log', 'linear')))
  })
  
  # empirical p.d.f. of the target value
  output$FCE_PDF <- renderPlotly({
    req(input$FCE_PDF_RUNTIME)  
    runtime <- input$FCE_PDF_RUNTIME %>% as.integer 
    points <- ifelse(input$FCE_SHOW_SAMPLE, 'all', FALSE)
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(x.title = "algorithms",
                         y.title = "Target value")
    
    for (i in seq_along(data)) {
      ds <- data[[i]]
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.5)')
      
      p %<>%
        add_trace(data = get_FV_sample(ds, runtime, output = 'long'),
                  x = ~algId, y = ~`f(x)`, split = ~algId, type = 'violin',
                  hoveron = "points+kde",
                  box = list(visible = T),
                  points = points,
                  pointpos = 1,
                  jitter = 0.1,
                  scalemode = 'count',
                  meanline = list(visible = T),
                  line = list(color = rgb_str, width = 1),
                  marker = list(color = rgb_str))
    }
    p %<>%
      layout(yaxis = list(type = ifelse(input$FCE_LOGY, 'log', 'linear')))
  })
  
  # historgram of the target values -----------
  output$FCE_HIST <- renderPlotly({
    req(input$FCE_HIST_RUNTIME != "")   # require non-empty input
    runtime <- input$FCE_HIST_RUNTIME %>% as.integer 
    plot_mode <- input$FCE_illu_mode
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    nrows <- ceiling(n_algorithm / 2.) # keep to columns for the histograms
    
    if (plot_mode == 'overlay') {
      p <- plot_ly_default(x.title = "target values", y.title = "runs")
      
    } else if (plot_mode == 'subplot') {
      p <- lapply(seq(n_algorithm), function(x) {
        plot_ly_default(x.title = "target values", y.title = "runs")
      })
    }
    
    for (i in seq_along(data)) {
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.35)')
      
      ds <- data[[i]]
      algId <- attr(ds, 'algId')
      fce <- get_FV_sample(ds, runtime, output = 'long')
      # skip if all target samples are NA
      if (sum(!is.na(fce$`f(x)`)) < 2)
        next
      
      res <- hist(fce$`f(x)`, breaks = nclass.FD, plot = F)
      breaks <- res$breaks
      plot_data <- data.frame(x = res$mids, y = res$counts, width = breaks[2] - breaks[1],
                              text = paste0('<b>count</b>: ', res$counts, 
                                            '<br><b>breaks</b>: [', 
                                            breaks[-length(breaks)], ',', breaks[-1], ']')) 
      
      if (plot_mode == 'overlay') {
        p %<>%
          add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = algId, text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      } else if (plot_mode == 'subplot') {
        p[[i]] %<>% 
          add_trace(data = plot_data, x = ~x, y = ~y, width = ~width, type = 'bar',
                    name = algId, text = ~text, hoverinfo = 'text',
                    marker = list(color = rgba_str,
                                  line = list(color = 'rgb(8,48,107)', width = 1.5)))
      }
    }
    
    if (plot_mode == 'subplot') 
      p <- subplot(p, nrows = nrows, titleX = F, titleY = F, margin = 0.03)
    
    p
  })
  
  # The ECDF plots for the target value ----------------
  output$FCE_ECDF_PER_TARGET <- renderPlotly({
    req(input$FCE_ECDF_RT1, input$FCE_ECDF_RT2, input$FCE_ECDF_RT3)
    runtimes <- c(
      as.integer(input$FCE_ECDF_RT1),
      as.integer(input$FCE_ECDF_RT2),
      as.integer(input$FCE_ECDF_RT3))
      
    
    runtimes <- runtimes[!is.na(runtimes)]
    req(length(runtimes) != 0)
    
    data <- DATA()
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    p <- plot_ly_default(title = NULL,
                         x.title = "target value",
                         y.title = "Proportion of runs")
    
    for (k in seq_along(data)) {
      ds <- data[[k]]
      algId <- attr(ds, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.35)')
      
      for (i in seq_along(runtimes)) {
        funvals <- get_FV_sample(ds, runtimes[i], output = 'long')$`f(x)` %>% sort
        
        if (all(is.na(funvals)))
          next
        
        tmp <- ecdf(funvals) 
        density <- tmp(funvals)
        
        # position of the markers
        x <- quantile(funvals, probs = c(0.25, 0.5, 0.75), names = F, type = 3)
        y <- sapply(x, function(xx) density[funvals == xx])
        
        p %<>%
          add_trace(data = NULL, x = funvals, y = density, type = 'scatter',
                    mode = 'lines', name = algId, showlegend = F,
                    legendgroup = paste0(k),
                    line = list(color = rgb_str, width = 3)) %>% 
          add_trace(data = NULL, x = x, y = y, type = 'scatter',
                    mode = 'markers',  legendgroup = paste0(k),
                    name = sprintf('%s, %.2e', algId, runtimes[i]),
                    marker = list(color = rgb_str, symbol = symbols[i], size = 13))
      }
    }
    
    p %<>%
      layout(xaxis = list(type = ifelse(input$FCE_ECDF_semilogx, 
                                        'log', 'linear')))
  })
  
  output$FCE_RT_GRID <- renderPrint({
    req(input$FCE_ECDF_RT_MIN, input$FCE_ECDF_RT_MAX, input$FCE_ECDF_RT_STEP)
    
    rt_min <- input$FCE_ECDF_RT_MIN %>% as.integer
    rt_max <- input$FCE_ECDF_RT_MAX %>% as.integer
    rt_step <- input$FCE_ECDF_RT_STEP %>% as.integer
    
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- getRuntimes(data)
    
    seq_RT(rt, from = rt_min, to = rt_max, by = rt_step) %>% cat
  })
  
  output$FCE_ECDF_AGGR <- renderPlotly({
    req(input$FCE_ECDF_RT_MIN, input$FCE_ECDF_RT_MAX, input$FCE_ECDF_RT_STEP)
    
    rt_min <- input$FCE_ECDF_RT_MIN %>% as.integer
    rt_max <- input$FCE_ECDF_RT_MAX %>% as.integer
    rt_step <- input$FCE_ECDF_RT_STEP %>% as.integer
    
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    
    data <- DATA()
    rt <- getRuntimes(data)
    rt_seq <- seq_RT(rt, from = rt_min, to = rt_max, by = rt_step)
    req(rt_seq)
    
    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)
    
    funevals.max <- sapply(data, function(ds) max(ds$FV, na.rm = T)) %>% max
    funevals.min <- sapply(data, function(ds) min(ds$FV, na.rm = T)) %>% min
    
    x <- seq(funevals.min, funevals.max, length.out = 40)
    p <- plot_ly_default(x.title = "target value",
                         y.title = "Proportion of (run, budget) pairs")
    
    for (k in seq_along(data)) {
      ds <- data[[k]]
      algId <- attr(ds, 'algId')
      
      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.15)')
      rgba_str2 <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.8)')
      
      fun <- get_FV_sample(ds, rt_seq, output = 'long')$`f(x)` %>% ecdf
      m <- fun(x)
      # m <- lapply(rt_seq, function(r) {
      #   ce <- get_FV_sample(df, r, format = 'long',  maximization = maximization) %>% '$'(`f(x)`)
      #   if (all(is.na(ce)))
      #     return(rep(0, length(x)))
      #   fun <- ecdf(ce)
      #   fun(x)
      # }) %>% 
        # do.call(rbind, .)
      
      df_plot <- data.frame(x = x, mean = m)
      # df_plot <- data.frame(x = x, 
      #                       mean = apply(m, 2, . %>% mean(na.rm = T)),
      #                       sd = apply(m, 2, . %>% sd(na.rm = T))) %>% 
      #   mutate(upper = mean + sd, lower = mean - sd)
      
      p %<>%
        # add_trace(data = df_plot, x = ~x, y = ~upper, type = 'scatter', mode = 'lines',
        #           line = list(color = rgba_str, width = 0), 
        #           showlegend = F, name = 'mean +/- sd') %>% 
        # add_trace(x = ~x, y = ~lower, type = 'scatter', mode = 'lines',
        #           fill = 'tonexty',  line = list(color = 'transparent'),
        #           fillcolor = rgba_str, showlegend = T, name = 'mean +/- sd') %>% 
        add_trace(data = df_plot, x = ~x, y = ~mean, type = 'scatter',
                  mode = 'lines+markers', name = sprintf('%s', algId), 
                  showlegend = T, legendgroup = paste0(k),
                  line = list(color = rgb_str, width = 4.5),
                  marker = list(color = rgb_str, size = 11))
      
      if (input$FCE_ECDF_per_target) {
        for (r in rt_seq) {
          ce <- get_FV_sample(ds, r, output = 'long') %>% '$'(`f(x)`) %>% sort
          if (all(is.na(ce)))
            next
          else {
            fun <- ecdf(ce)
            v <- fun(ce)
          }
          
          p %<>%
            add_trace(x = ce, y = v, type = 'scatter',
                      mode = 'lines', name = algId, showlegend = F,
                      line = list(color = rgba_str2, width = 1))
        }
      }
    }
    
    p %<>%
      layout(xaxis = list(type = ifelse(input$FCE_ECDF_AGGR_semilogx, 
                                        'log', 'linear')))
  })
  
  # evaluation rake of all courses 
  output$FCE_AUC <- renderPlotly({
    req(input$FCE_AUC_RT_MIN, input$FCE_AUC_RT_MAX, input$FCE_AUC_RT_STEP)
    
    rt_min <- input$FCE_AUC_RT_MIN %>% as.integer
    rt_max <- input$FCE_AUC_RT_MAX %>% as.integer
    rt_step <- input$FCE_AUC_RT_STEP %>% as.integer
    
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    data <- DATA()
    rt <- getRuntimes(data)

    rt_seq <- seq_RT(rt, from = rt_min, to = rt_max, by = rt_step)
    req(rt_seq)

    n_algorithm <- length(data)
    colors <- colorspace::rainbow_hcl(n_algorithm)

    funevals.max <- sapply(data, function(ds) max(attr(ds, 'finalFV'))) %>% max
    p <- plot_ly_default()

    for (k in seq_along(data)) {
      df <- data[[k]]
      algId <- attr(df, 'algId')

      rgb_str <- paste0('rgb(', paste0(col2rgb(colors[k]), collapse = ','), ')')
      rgba_str <- paste0('rgba(', paste0(col2rgb(colors[k]), collapse = ','), ',0.2)')

      # calculate ECDFs on user specified targets
      funs <- lapply(rt_seq, function(r) {
        get_FV_sample(df, r, output = 'long')$`f(x)` %>% {
            if (all(is.na(.))) NULL
            else  {
              f <- ecdf(.)
              attr(f, 'min') <- min(.)
              f
            }
          }
      })

      auc <- sapply(funs,
                    function(fun) {
                      if (is.null(fun)) 0
                      else integrate(fun, lower = attr(fun, 'min') - 1, upper = funevals.max,
                                     subdivisions = 1e3) %>% {'$'(., 'value') / funevals.max}
                    })

      p %<>%
        add_trace(type = 'scatterpolar', r = auc,
                  theta = paste0('B:', as.integer(rt_seq)),
                  fill = 'toself', fillcolor = rgba_str,
                  marker = list(color = rgb_str), hoverinfo = 'text',
                  text = paste0('area: ', format(auc, digits = 2, nsmall = 2)),
                  name = algId)
    }

    p %<>%
      layout(polar = list(radialaxis = list(visible = T)),
             yaxis = list(type = 'log'),
             autosize = T, hovermode = 'compare',
             paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)')
  })
  
  # Expected Evolution of parameters in the algorithm
  output$PAR_PER_FUN <- renderPlotly({
    req(input$PAR_F_MIN, input$PAR_F_MAX)
    
    f_min <- format_FV(input$PAR_F_MIN) %>% as.numeric
    f_max <- format_FV(input$PAR_F_MAX) %>% as.numeric  
    
    req(f_min <= f_max)
    
    data <- DATA()
    fall <- getFunvals(data)
    
    fseq <- seq_FV(fall, f_min, f_max, length.out = 50)
    req(fseq)
    
    dt <- get_PAR_summary(data, fseq, input$PAR_ALGID_INPUT)
    req(length(dt) != 0)
    dt[, `:=`(upper = mean + sd, lower = mean - sd)]
    
    par_name <- dt[, parId] %>% unique
    n_param <- length(par_name)
    
    algorithms <- dt[, algId] %>% unique
    n_alg <- length(algorithms)
    colors <- colorspace::rainbow_hcl(n_alg)
    
    nrows <- ceiling(n_param / 2) # two columns
    # styles <- lapply(seq(n_alg),
    #                  function(i) {
    #                    list(target = algorithms[i],
    #                         value = list(name = algorithms[i],
    #                                      marker = list(color = paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')),
    #                                      line = list(color = paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')),
    #                                      showlegend = function(i){if (i == 1)
    #                                        T
    #                                      else 
    #                                        F
    #                                      }))
    #                  })
    
    # TODO: improve the efficiency of plotting here
    p <- lapply(seq(n_param), 
                  function(i) {
                    plot_ly_default(y.title = par_name[i]) %>% 
                      layout(xaxis = list(type = ifelse(input$PAR_semilogx, 'log', 'linear')),
                             yaxis = list(type = ifelse(input$PAR_semilogy, 'log', 'linear')))
                  })
    
    for (i in seq(n_alg)) {
    alg <- algorithms[i]
      
      for (j in seq(n_param)) {
        if (j == 1)
          showlegend <- T
        else 
          showlegend <- F
        
        name <- par_name[j]
        dt_plot <- dt[parId == name & algId == alg]
        rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
        rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
        
        p[[j]] %<>% 
          add_trace(data = dt_plot, x = ~target, y = ~upper, type = 'scatter', mode = 'lines',
                    line = list(color = rgba_str, width = 0),  
                    showlegend = F, legendgroup = ~algId, name = 'mean +/- sd') %>% 
          add_trace(x = ~target, y = ~lower, type = 'scatter', mode = 'lines',
                    fill = 'tonexty',  line = list(color = 'transparent'),
                    fillcolor = rgba_str, showlegend = F, legendgroup = ~algId, 
                    name = 'mean +/- sd')
        
        if (input$PAR_show.mean == 'mean')
          p[[j]] %<>% add_trace(data = dt_plot, x = ~target, y = ~mean, 
                                type = 'scatter', 
                                mode = 'lines+markers', 
                                marker = list(color = rgb_str),
                                line = list(color = rgb_str),
                                name = alg,
                                showlegend = showlegend,
                                legendgroup = ~algId)
                                # transforms = list(
                                #   list(type = 'groupby',
                                #        groups = ~algId,
                                #        styles = styles)
                                #   )
                                # )
                                # 
                                
        else if (input$PAR_show.mean == 'median')
          p[[j]] %<>% add_trace(data = dt_plot, x = ~target, y = ~median,
                                type = 'scatter',
                                mode = 'lines+markers',
                                marker = list(color = rgb_str),
                                line = list(color = rgb_str, dash = 'dash'),
                                name = alg,
                                legendgroup = ~algId,
                                showlegend = showlegend)
      }
    }
        # p %<>%
        #   add_trace(data = dt_plot, x = ~target, y = ~upper, type = 'scatter', mode = 'lines',
        #             line = list(color = rgba_str, width = 0),
        #             showlegend = F, name = paste0(name, '.mean +/- sd')) %>%
        #   add_trace(x = ~target, y = ~lower, type = 'scatter', mode = 'lines',
        #             fill = 'tonexty',  line = list(color = 'transparent'),
        #             fillcolor = rgba_str, showlegend = T, paste0(name, '.mean +/- sd'))
        
      # rgba_str <- paste0('rgba(', paste0(col2rgb(colors[i]), collapse = ','), ',0.3)')
    subplot(p, nrows = nrows, titleX = F, titleY = T, margin = 0.05) %>% 
      add_annotations(x = 0.5 , y = -0.18, text = "best-so-far f(x)-value", 
                      showarrow = F, xref = 'paper', yref = 'paper',
                      font = list(size = 22, family = 'sans-serif'))
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
    req(input$PAR_F_MIN_SUMMARY, input$PAR_F_MAX_SUMMARY, input$PAR_F_STEP_SUMMARY)
    
    fstart <- format_FV(input$PAR_F_MIN_SUMMARY) %>% as.numeric
    fstop <- format_FV(input$PAR_F_MAX_SUMMARY) %>% as.numeric
    fstep <- format_FV(input$PAR_F_STEP_SUMMARY) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- getFunvals(data)
    
    if (input$PAR_F_SINGLE)
      fstop <- fstart
      
    fseq <- seq_FV(fall, fstart, fstop, by = fstep)
    req(fseq)
    
    get_PAR_summary(data, fseq, input$PAR_ALGID_INPUT_SUMMARY, input$PAR_INPUT)
  })
  
  parameter_sample <- reactive({
    req(input$PAR_ALGID_INPUT_SAMPLE, input$PAR_F_MAX_SAMPLE, 
        input$PAR_F_STEP_SAMPLE, input$PAR_F_MIN_SAMPLE,
        input$PAR_INPUT_SAMPLE)
    
    fstart <- format_FV(input$PAR_F_MIN_SAMPLE) %>% as.numeric
    fstop <- format_FV(input$PAR_F_MAX_SAMPLE) %>% as.numeric
    fstep <- format_FV(input$PAR_F_STEP_SAMPLE) %>% as.numeric
    
    req(fstart <= fstop, fstep <= fstop - fstart)
    data <- DATA()
    fall <- getFunvals(data)
    
    if (input$PAR_SAMPLE_F_SINGLE)
      fstop <- fstart
    
    fseq <- seq_FV(fall, fstart, fstop, by = fstep)
    req(fseq)
    
    get_PAR_sample(data, ftarget = fseq, 
                   algorithm = input$PAR_ALGID_INPUT_SAMPLE, 
                   parId = input$PAR_INPUT_SAMPLE,
                   output = input$PAR_download_format)
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
  
  output$PAR_SAMPLE_downloadData <- downloadHandler(
    filename = {
      fstart <- format_FV(input$PAR_F_MIN_SAMPLE)
      fstop <- format_FV(input$PAR_F_MAX_SAMPLE)
      fstep <- format_FV(input$PAR_F_STEP_SAMPLE)
      eval(PARSample_csv_name)
    },
    content = function(file) {
      write.csv(parameter_sample(), file, row.names = F)
    },
    contentType = "text/csv"
  )
  
  output$PAR_downloadData <- downloadHandler(
    filename = {
      fstart <- format_FV(input$PAR_F_MIN_SUMMARY)
      fstop <- format_FV(input$PAR_F_MAX_SUMMARY)
      fstep <- format_FV(input$PAR_F_STEP_SUMMARY) 
      eval(PAR_csv_name)
    }, 
    content = function(file) {
      write.csv(parameter_summary(), file, row.names = F)
    },
    contentType = "text/csv"
  )
})
