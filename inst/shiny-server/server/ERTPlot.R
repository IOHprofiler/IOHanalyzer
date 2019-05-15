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

update_ert_per_fct_axis <- observe({
  plotlyProxy("ERT_PER_FUN", session) %>%
    plotlyProxyInvoke("relayout", list(xaxis = list(title = 'best-so-far-f(x)-value', type = ifelse(input$ERTPlot.semilogx, 'log', 'linear')),
                                       yaxis = list(title = 'function evaluations', type = ifelse(input$ERTPlot.semilogy, 'log', 'linear'))))
})


render_ert_per_fct <- reactive({
  withProgress({
  req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
  selected_algs <- input$ERTPlot.Algs
  data <- subset(DATA(), algId %in% input$ERTPlot.Algs)
  fstart <- input$ERTPlot.Min %>% as.numeric
  fstop <- input$ERTPlot.Max %>% as.numeric
  Plot.RT.Single_Func(data, Fstart = fstart, Fstop = fstop,
                     show.CI = input$ERTPlot.show.CI,
                     show.ERT = input$ERTPlot.show.ERT,
                     show.mean = input$ERTPlot.show.mean,
                     show.median = input$ERTPlot.show.median,
                     scale.xlog = isolate(input$ERTPlot.semilogx),
                     scale.ylog = isolate(input$ERTPlot.semilogy),
                     scale.reverse = !attr(data[[1]],'maximization'))
  },
  message = "Creating plot"
  )
})

output$ERTPlot.Multi.Plot <- renderPlotly(
  render_ERTPlot_multi_plot()
)

render_ERTPlot_multi_plot <- eventReactive(input$ERTPlot.Multi.PlotButton, {
  req(input$ERTPlot.Multi.Algs)
  withProgress({
  data <- subset(DATA_RAW(),
                 algId %in% input$ERTPlot.Multi.Algs,
                 DIM == input$Overall.Dim)
  req(data)
  if (length(unique(get_funcId(data))) == 1){
    shinyjs::alert("This plot is only available when the dataset contains multiple functions  for the selected dimension.")
    return(NULL)
  }
  Plot.RT.Multi_Func(data,
                   scale.xlog = input$ERTPlot.Multi.Logx,
                   scale.ylog = input$ERTPlot.Multi.Logy,
                   scale.reverse = !attr(data[[1]],'maximization'))
  },
  message = "Creating plot")
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
  withProgress({
  #TODO: figure out how to avoid plotting again when default targets are written to input
  data <- DATA_RAW()
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM == input$Overall.Dim)
  if (length(unique(get_funcId(data))) == 1){
    shinyjs::alert("This plot is only available when the dataset contains multiple functions for the selected dimension.")
    return(NULL)
  }
  if (length(unique(get_algId(data))) == 1){
    shinyjs::alert("This plot is only available when the dataset contains multiple algorithms for the selected dimension.")
    return(NULL)
  }
  erts <- MAX_ERTS_FUNC()
  aggr_on = 'funcId'
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_DIM(data)
  targets <- get_max_targets(data, aggr_on, maximize = !(format == COCO || format == BIBOJ_COCO))
  erts <- max_ERTs(data, aggr_on, targets, maximize = !(format == COCO || format == BIBOJ_COCO))

  Plot.RT.Aggregated(data, plot_mode = input$ERTPlot.Aggr.Mode, targets = targets,
                scale.ylog = input$ERTPlot.Aggr.Logy,
                maximize = attr(data[[1]],'maximization'),
                use_rank = input$ERTPlot.Aggr.Ranking,
                aggr_on = aggr_on, erts = erts)
  },
  message = "Creating plot")
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
