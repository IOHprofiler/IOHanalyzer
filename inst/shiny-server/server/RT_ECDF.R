output$RT_ECDF_MULT <- renderPlotly({
  req(length(DATA_RAW()) > 0)
  render_RT_ECDF_MULT()
})

render_RT_ECDF_MULT <- reactive({
  req(input$RTECDF.Aggr.Func || input$RTECDF.Aggr.Dim)
  withProgress({
    dsList <- subset(DATA_RAW(), algId %in% input$RTECDF.Aggr.Algs)
    if (!input$RTECDF.Aggr.Func){
      dsList <- subset(dsList, funcId == input$Overall.Funcid)
    }
    if (!input$RTECDF.Aggr.Dim){
      dsList <- subset(dsList, DIM == input$Overall.Dim)
    }
    if (length(dsList) <= 1){
      shinyjs::alert("This is an invalid configuration for this plot. \n
                     Please ensure that the dataset contains multiple functions / dimensions to aggregate over.")
      return(NULL)
    }
    targets <- uploaded_RT_ECDF_targets()
  
    Plot.RT.ECDF_Multi_Func(dsList, targets = targets, scale.xlog = input$RTECDF.Aggr.Logx)
  },
  message = "Creating plot")
})

output$RTECDF.Aggr.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_RT_ECDF_MULT)
  },
  content = function(file) {
    save_plotly(render_RT_ECDF_MULT(), file,
                format = input$RTECDF.Aggr.Format)
  },
  contentType = paste0('image/', input$RTECDF.Aggr.Format)
)

RT_ECDF_MULTI_TABLE <- reactive({
  withProgress({
  targets <- uploaded_RT_ECDF_targets()
  funcId <- names(targets) %>% as.numeric

  if (is.null(targets)) {
    dsList <- subset(DATA_RAW(), algId %in% input$RTECDF.Aggr.Algs)
    if (!input$RTECDF.Aggr.Func){
      dsList <- subset(dsList, funcId == input$Overall.Funcid)
    }
    if (!input$RTECDF.Aggr.Dim){
      dsList <- subset(dsList, DIM == input$Overall.Dim)
    }    
    targets <- get_default_ECDF_targets(dsList)
  }

  targets <- lapply(targets, function(t) {
    paste0(as.character(t), collapse = ',')
  })

  data.frame(names = names(targets), target = unlist(targets))
  },
  message = "Creating plot")
})

output$RT_GRID_GENERATED <- renderDataTable({
  req(length(DATA_RAW()) > 0)
  df <- RT_ECDF_MULTI_TABLE()
  # df$funcId <- as.integer(df$funcId)
  df
}, options = list(pageLength = 5, lengthMenu = c(5, 10, 25)))

uploaded_RT_ECDF_targets <- reactive({
  if (!is.null(input$RTECDF.Aggr.Table.Upload)) {
    df <- read.csv(input$RTECDF.Aggr.Table.Upload$datapath, header = T, sep = ',')
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
                col.names = T, sep = ',')
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
                format = input$RTECDF.Multi.Format)
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
    show.per_target = input$RTECDF.Multi.Targets,
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
    save_plotly(render_RT_AUC(), file,
                format = input$RTECDF.AUC.Format)
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
