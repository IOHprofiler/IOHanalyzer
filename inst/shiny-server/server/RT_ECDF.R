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
