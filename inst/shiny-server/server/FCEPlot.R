# Expected Target Value Convergence
output$FCE_PER_FUN <- renderPlotly({
  req(input$FCEPlot.Min, input$FCEPlot.Max, length(DATA()) > 0)
  render_FV_PER_FUN()
})

output$FCEPlot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_FV_PER_FUN(), file,
                format = input$FCEPlot.Format)
  },
  contentType = paste0('image/', input$FCEPlot.Format)
)


update_fv_per_fct_axis <- observe({
  plotlyProxy("FCE_PER_FUN", session) %>%
    plotlyProxyInvoke("relayout", list(xaxis = list(title = 'runtime', type = ifelse(input$FCEPlot.semilogx, 'log', 'linear')),
                                       yaxis = list(title = 'best-so-far-f(x)-value', type = ifelse(input$FCEPlot.semilogy, 'log', 'linear'))))
})


render_FV_PER_FUN <- reactive({
  withProgress({
  rt_min <- input$FCEPlot.Min %>% as.integer
  rt_max <- input$FCEPlot.Max %>% as.integer
  data <- subset(DATA(), algId %in% input$FCEPlot.Algs)
  Plot.FV.Single_Func(data, RTstart = rt_min, RTstop = rt_max, show.CI = input$FCEPlot.show.CI,
               show.mean = input$FCEPlot.show.mean, show.median = input$FCEPlot.show.median,
               scale.xlog = isolate(input$FCEPlot.semilogx), scale.ylog = isolate(input$FCEPlot.semilogy))
  },
  message = "Creating plot")
})


output$FCEPlot.Multi.Plot <- renderPlotly(
  render_FCEPlot_multi_plot()
)

render_FCEPlot_multi_plot <- reactive({
  req(isolate(input$FCEPlot.Multi.Algs))
  input$FCEPlot.Multi.PlotButton
  withProgress({
  data <- subset(DATA_RAW(),
                 algId %in% isolate(input$FCEPlot.Multi.Algs),
                 DIM == input$Overall.Dim)
  req(length(data) > 0)
  if (length(unique(get_funcId(data))) == 1){
    shinyjs::alert("This plot is only available when the dataset contains multiple functions for the selected dimension.")
    return(NULL)
  }
  Plot.FV.Multi_Func(data,
                   scale.xlog = input$FCEPlot.Multi.Logx,
                   scale.ylog = input$FCEPlot.Multi.Logy)
  },
  message = "Creating plot")
})

output$FCEPlot.Multi.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PER_FUN_MULTI)
  },
  content = function(file) {
    save_plotly(render_FCEPlot_multi_plot(), file,
                format = input$FCEPlot.Multi.Format)
  },
  contentType = paste0('image/', input$FCEPlot.Multi.Format)
)

output$FCEPlot.Aggr.Plot <- renderPlotly(
  render_FCEPlot_aggr_plot()
)

get_max_runtimes <- function(data, aggr_on){
  runtimes <- c()
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)

  for (j in seq_along(aggr_attr)) {
    dsList_filetered <- if (aggr_on == 'funcId') subset(data,funcId==aggr_attr[[j]])
    else subset(data, DIM==aggr_attr[[j]])

    RTall <- get_runtimes(dsList_filetered)
    RTval <- max(RTall)
    runtimes <- c(runtimes,RTval)
  }
  names(runtimes) <- aggr_attr
  runtimes
}

render_FCEPlot_aggr_plot <- reactive({
  input$FCEPlot.Aggr.Refresh
  withProgress({
  #TODO: figure out how to avoid plotting again when default targets are written to input
  req(length(DATA_RAW()) > 0)
  data <- FCEPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  aggr_on <- ifelse(input$FCEPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)

  runtimes <- FCEPlot.Aggr.Targets_obj
  names(runtimes) <- NULL
  fvs <- mean_FVs(data, aggr_on, runtimes)
  Plot.FV.Aggregated(data, plot_mode = input$FCEPlot.Aggr.Mode, runtimes = runtimes,
                scale.ylog = input$FCEPlot.Aggr.Logy,
                use_rank = input$FCEPlot.Aggr.Ranking,
                aggr_on = aggr_on, fvs = fvs)
  },
  message = "Creating plot")
})

FCEPlot.Aggr.data <- function() {
  data <- subset(DATA_RAW(), algId %in% isolate(input$FCEPlot.Aggr.Algs))
  if (length(data) == 0) return(NULL)
  if (input$FCEPlot.Aggr.Aggregator == 'Functions'){
    data <- subset(data, DIM==input$Overall.Dim)
    if (length(unique(get_funcId(data))) == 1){
      shinyjs::alert("This plot is only available when the dataset contains multiple functions for the selected dimension.")
      return(NULL)
    }
    fvs <- MEAN_FVALS_FUNC()
  }
  else{
    data <- subset(data, funcId==input$Overall.Funcid)
    if (length(unique(get_dim(data))) == 1){
      shinyjs::alert("This plot is only available when the dataset contains multiple dimensions for the selected function")
      return(NULL)
    }
    fvs <- MEAN_FVALS_DIM()
  }
  if (length(unique(get_algId(data))) == 1){
    shinyjs::alert("This plot is only available when the dataset contains multiple algorithms for the selected dimension.")
    return(NULL)
  }
  data
}

default_runtimes_table <- reactive({
  data <- FCEPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  aggr_on <- ifelse(input$FCEPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
  get_max_runtimes(data, aggr_on) %>% t %>% as.data.table(keep.rownames = F)
})

FCEPlot.Aggr.Targets_obj <- NULL

proxy_FCEPlot.Aggr.Targets <- dataTableProxy('FCEPlot.Aggr.Targets')

output$FCEPlot.Aggr.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  FCEPlot.Aggr.Targets_obj <<- default_runtimes_table()
  FCEPlot.Aggr.Targets_obj
}, editable = TRUE, rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))

output$FCEPlot.Aggr.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_AGGR)
  },
  content = function(file) {
    save_plotly(render_FCEPlot_aggr_plot(), file,
                format = input$ERTPlot.Aggr.Format)
  },
  contentType = paste0('image/', input$FCEPlot.Aggr.Format)
)

observeEvent(input$FCEPlot.Aggr.Targets_cell_edit, {
  info <- input$FCEPlot.Aggr.Targets_cell_edit
  i <- info$row
  j <- info$col + 1
  v <- info$value
  data <- FCEPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  aggr_on <- ifelse(input$FCEPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)
  suppressWarnings(FCEPlot.Aggr.Targets_obj[i, paste0(aggr_attr[[j]])] <<- DT::coerceValue(v, FCEPlot.Aggr.Targets_obj[i, paste0(aggr_attr[[j]])]))
  replaceData(proxy, FCEPlot.Aggr.Targets_obj, resetPaging = FALSE, rownames = FALSE)
})
