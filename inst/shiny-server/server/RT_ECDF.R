output$RT_ECDF_MULT <- renderPlotly({
  req(length(DATA_RAW()) > 0)
  render_RT_ECDF_MULT()
})

render_RT_ECDF_MULT <- reactive({
  req(input$RTECDF.Aggr.Func || input$RTECDF.Aggr.Dim)
  input$RTECDF.Aggr.Refresh
  
  withProgress({
    dsList <- subset(DATA_RAW(), algId %in% input$RTECDF.Aggr.Algs)
    
    if (!input$RTECDF.Aggr.Func) 
      dsList <- subset(dsList, funcId == input$Overall.Funcid)
    
    if (!input$RTECDF.Aggr.Dim) 
      dsList <- subset(dsList, DIM == input$Overall.Dim)
    
    if (length(dsList) <= 1) {
      shinyjs::alert("This is an invalid configuration for this plot. \n
                     Please ensure that the dataset contains multiple functions / 
                     dimensions to aggregate over.")
      return(NULL)
    }
    
    isolate({
      targets <- RT_ECDF_MULTI_TABLE()
    })
  
    Plot.RT.ECDF_Multi_Func(dsList, 
                            targets = targets, 
                            scale.xlog = input$RTECDF.Aggr.Logx)
  },
  message = "Creating plot")
})

output$RTECDF.Aggr.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_RT_ECDF_MULT)
  },
  content = function(file) {
    save_plotly(render_RT_ECDF_MULT(), file)
  },
  contentType = paste0('image/', input$RTECDF.Aggr.Format)
)

RT_ECDF_MULTI_TABLE <- reactiveVal(NULL)
trigger_renderDT <- reactiveVal(NULL)
proxy <- dataTableProxy('RT_GRID_GENERATED')

observe({
  req(length(DATA_RAW()) > 0)
  req(input$RTECDF.Aggr.Algs)
  req(input$Overall.Funcid)
  req(input$Overall.Dim)
  
  dsList <- subset(DATA_RAW(), algId %in% input$RTECDF.Aggr.Algs)
  req(length(dsList) > 0)
  if (!input$RTECDF.Aggr.Func) 
    dsList <- subset(dsList, funcId == input$Overall.Funcid)
  
  if (!input$RTECDF.Aggr.Dim) 
    dsList <- subset(dsList, DIM == input$Overall.Dim)
  
  if (length(dsList) == 0) return(NULL)
  
  targets <- get_default_ECDF_targets(dsList, format_FV)
  
  df <- t(data.frame(targets))
  rownames(df) <- names(targets)
  colnames(df) <- paste0("target.", seq(10))
  dt <- as.data.table(df, keep.rownames = T)
  
  if (!input$RTECDF.Aggr.Func)
    colnames(dt)[[1]] <- "Dim"
  else if (!input$RTECDF.Aggr.Dim)
    colnames(dt)[[1]] <- "Func"
  else
    colnames(dt)[[1]] <- "Func; Dim"
  
  RT_ECDF_MULTI_TABLE(dt)
  trigger_renderDT(rnorm(1))
})

output$RT_GRID_GENERATED <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  trigger_renderDT()
  isolate({RT_ECDF_MULTI_TABLE()})
  }, 
  editable = TRUE, 
  rownames = FALSE,
  options = list(
    pageLength = 5, 
    lengthMenu = c(5, 10, 25, -1), 
    scrollX = T, 
    server = T,
    columnDefs = list(
      list(
        className = 'dt-right', targets = "_all"
      )
    )
  )
)

observeEvent(input$RT_GRID_GENERATED_cell_edit, {
  info <- input$RT_GRID_GENERATED_cell_edit
  i <- info$row
  j <- info$col + 1
  v <- info$value
  
  suppressWarnings({
    df <- RT_ECDF_MULTI_TABLE()
    set(df, i, j, coerceValue(v, as.numeric(df[i, ..j])))
    RT_ECDF_MULTI_TABLE(df)
  })
  replaceData(proxy, RT_ECDF_MULTI_TABLE(), 
              resetPaging = FALSE, rownames = FALSE)
})

observeEvent(input$RTECDF.Aggr.Table.Upload, {
  if (!is.null(input$RTECDF.Aggr.Table.Upload)) {
    df <- read.csv(input$RTECDF.Aggr.Table.Upload$datapath, 
                   sep = ',', header = F) %>%
      set_colnames(c('Func', paste0('target.', seq(ncol(.) - 1)))) %>% 
      as.data.table
    
    if (ncol(df) == ncol(RT_ECDF_MULTI_TABLE())) {
      RT_ECDF_MULTI_TABLE(df)
      replaceData(proxy, RT_ECDF_MULTI_TABLE(), 
                  resetPaging = FALSE, 
                  rownames = FALSE)
    } else {
      RT_ECDF_MULTI_TABLE(df)
      trigger_renderDT(rnorm(1))
    }
  } else
    NULL
})

output$RTECDF.Aggr.Table.Download <- downloadHandler(
  filename = 'Example_ECDF_TARGETS.csv',
  content = function(file) {
    write.table(RT_ECDF_MULTI_TABLE(), file, 
                sep = ',', col.names = F, row.names = F)
  },
  contentType = "text/csv"
)

# The ECDF plots for the runtime ----------------
output$RT_ECDF <- renderPlotly({
  req(input$RTECDF.Single.Target)
  ftargets <- as.numeric(format_FV(input$RTECDF.Single.Target))
  data <- subset(DATA(), algId %in% input$RTECDF.Single.Algs)
  Plot.RT.ECDF_Per_Target(data, ftargets, scale.xlog = input$RTECDF.Single.Logx)
})

output$RT_GRID <- renderPrint({
  req(input$RTECDF.Multi.Min, input$RTECDF.Multi.Max, input$RTECDF.Multi.Step)

  fstart <- format_FV(input$RTECDF.Multi.Min) %>% as.numeric
  fstop <- format_FV(input$RTECDF.Multi.Max) %>% as.numeric
  fstep <- format_FV(input$RTECDF.Multi.Step) %>% as.numeric

  req(fstart <= fstop, fstep <= fstop - fstart)
  data <- DATA()
  fall <- get_funvals(data)

  cat(seq_FV(fall, fstart, fstop, by = fstep))
})

output$RT_ECDF_AGGR <- renderPlotly({
  render_RT_ECDF_AGGR()
})

output$RTECDF.Multi.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_RT_ECDF_AGGR)
  },
  content = function(file) {
    save_plotly(render_RT_ECDF_AGGR(), file)
  },
  contentType = paste0('image/', input$RTECDF.Multi.Format)
)

render_RT_ECDF_AGGR <- reactive({
  req(input$RTECDF.Multi.Min, input$RTECDF.Multi.Max, input$RTECDF.Multi.Step)
  withProgress({
  fstart <- format_FV(input$RTECDF.Multi.Min) %>% as.numeric
  fstop <- format_FV(input$RTECDF.Multi.Max) %>% as.numeric
  fstep <- format_FV(input$RTECDF.Multi.Step) %>% as.numeric
  data <- subset(DATA(), algId %in% input$RTECDF.Multi.Algs)
  
  Plot.RT.ECDF_Single_Func(
    data, fstart, fstop, fstep,
    scale.xlog = input$RTECDF.Multi.Logx
  )
  },
  message = "Creating plot")
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
    save_plotly(render_RT_AUC(), file)
  },
  contentType = paste0('image/', input$RTECDF.AUC.Format)
)

render_RT_AUC <- reactive({
  req(input$RTECDF.AUC.Min, input$RTECDF.AUC.Max, input$RTECDF.AUC.Step)

  fstart <- format_FV(input$RTECDF.AUC.Min) %>% as.numeric
  fstop <- format_FV(input$RTECDF.AUC.Max) %>% as.numeric
  fstep <- format_FV(input$RTECDF.AUC.Step) %>% as.numeric
  data <- subset(DATA(), algId %in% input$RTECDF.AUC.Algs)
  
  Plot.RT.ECDF_AUC(
    data, fstart, fstop, fstep, fval_formatter = format_FV
  )
})
