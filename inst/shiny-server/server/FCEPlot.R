# Expected Target Value Convergence
output$FCE_PER_FUN <- renderPlotly({
  req(input$FCEPlot.Min, input$FCEPlot.Max, (length(DATA())>0))
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
  withProgress({
  rt_min <- input$FCEPlot.Min %>% as.integer
  rt_max <- input$FCEPlot.Max %>% as.integer
  Plot.FV.Single_Func(DATA(), RTstart = rt_min, RTstop = rt_max, show.CI = input$FCEPlot.show.CI,
               show.mean = input$FCEPlot.show.mean, show.median = input$FCEPlot.show.median,
               scale.xlog = input$FCEPlot.semilogx, scale.ylog = input$FCEPlot.semilogy)
  },
  message = "Creating plot")
})


output$FCEPlot.Multi.Plot <- renderPlotly(
  render_FCEPlot_multi_plot()
)

render_FCEPlot_multi_plot <- eventReactive(input$FCEPlot.Multi.PlotButton, {
  req(input$FCEPlot.Multi.Algs)
  withProgress({
  data <- subset(DATA_RAW(),
                 algId %in% input$FCEPlot.Multi.Algs,
                 DIM == input$Overall.Dim)
  req(data)

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
  withProgress({
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
  Plot.FV.Aggregated(data, plot_mode = input$FCEPlot.Aggr.Mode, runtimes = runtimes,
                scale.ylog = input$FCEPlot.Aggr.Logy,
                use_rank = input$FCEPlot.Aggr.Ranking,
                aggr_on = aggr_on, fvs = fvs)
  },
  message = "Creating plot")
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
