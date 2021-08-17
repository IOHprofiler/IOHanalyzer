# The ECDF plots for the target value ----------------
output$FCE_ECDF_PER_TARGET <- renderPlotly({
  render_ecdf_per_target()
})

get_data_FV_ECDF_Single <- reactive({
  req(input$FCEECDF.Single.Target)
  ftargets <- as.numeric(format_FV(input$FCEECDF.Single.Target))
  data <- subset(DATA(), ID %in% input$FCEECDF.Single.Algs)
  generate_data.ECDF(data, ftargets, input$FCEECDF.Single.Logx, which = 'by_FV')
})

render_ecdf_per_target <- reactive({
  withProgress({
    plot_general_data(get_data_FV_ECDF_Single(), 'x', 'mean', 'line', 
                      x_title = "Target Value",
                      y_title = "Proportion of runs", scale.xlog = input$FCEECDF.Single.Logx, 
                      show.legend = T,
                      scale.reverse = !attr(DATA()[[1]], 'maximization'))
  },
  message = "Creating plot")
})

output$FCE_RT_GRID <- renderPrint({
  req(input$FCEECDF.Mult.Min, input$FCEECDF.Mult.Max, input$FCEECDF.Mult.Step, length(DATA()) > 0)

  rt_min <- input$FCEECDF.Mult.Min %>% as.numeric
  rt_max <- input$FCEECDF.Mult.Max %>% as.numeric
  rt_step <- input$FCEECDF.Mult.Step %>% as.numeric

  req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
  data <- DATA()
  rt <- get_runtimes(data)

  seq_RT(rt, from = rt_min, to = rt_max, by = rt_step) %>% cat
})

get_data_FV_ECDF_AGGR <- reactive({
  req(input$FCEECDF.Mult.Min, input$FCEECDF.Mult.Max, input$FCEECDF.Mult.Step, length(DATA()) > 0)
  fstart <- format_FV(input$FCEECDF.Mult.Min) %>% as.numeric
  fstop <- format_FV(input$FCEECDF.Mult.Max) %>% as.numeric
  fstep <- format_FV(input$FCEECDF.Mult.Step) %>% as.numeric
  data <- subset(DATA(), ID %in% input$FCEECDF.Mult.Algs)
  targets <- seq_RT(get_funvals(data), fstart, fstop, fstep)
  generate_data.ECDF(data, targets, input$FCEECDF.Mult.Logx, which = 'by_FV')
})

render_FV_ECDF_AGGR <- reactive({
  withProgress({
  # rt_min <- input$FCEECDF.Mult.Min %>% as.numeric
  # rt_max <- input$FCEECDF.Mult.Max %>% as.numeric
  # rt_step <- input$FCEECDF.Mult.Step %>% as.numeric
  # data <- subset(DATA(), algId %in% input$FCEECDF.Mult.Algs)
  # 
  # Plot.FV.ECDF_Single_Func(data,rt_min = rt_min,
  #                   rt_max = rt_max, rt_step = rt_step,
  #                   scale.xlog = input$FCEECDF.Mult.Logx,
  #                   # show.per_target = input$FCEECDF.Mult.Targets,
  #                   scale.reverse = !attr(DATA()[[1]],'maximization'))
    plot_general_data(get_data_FV_ECDF_AGGR(), 'x', 'mean', 'line', 
                      x_title = "Target Value",
                      y_title = "Proportion of (run, target) pairs", 
                      scale.xlog = input$FCEECDF.Mult.Logx, 
                      scale.reverse = !attr(DATA()[[1]], 'maximization'), show.legend = T)
  },
  message = "Creating plot")
})

output$FCEECDF.Mult.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_ECDF_AGGR)
  },
  content = function(file) {
    save_plotly(render_FV_ECDF_AGGR(), file)
  },
  contentType = paste0('image/', input$FCEECDF.Mult.Format)
)

output$FCE_ECDF_AGGR <- renderPlotly({
  render_FV_ECDF_AGGR()
})

get_data_FV_AUC <- reactive({
  req(input$FCEECDF.AUC.Min, input$FCEECDF.AUC.Max, input$FCEECDF.AUC.Step, length(DATA()) > 0)
  
  rt_min <- input$FCEECDF.AUC.Min %>% as.numeric
  rt_max <- input$FCEECDF.AUC.Max %>% as.numeric
  rt_step <- input$FCEECDF.AUC.Step %>% as.numeric
  data <- subset(DATA(), ID %in% input$FCEECDF.AUC.Algs)
  targets <- seq_RT(get_runtimes(data), rt_min, rt_max, rt_step, length.out = 10)
  generate_data.AUC(data, targets, which = 'by_FV')
})


render_FV_AUC <- reactive({
  withProgress({
  # rt_min <- input$FCEECDF.AUC.Min %>% as.numeric
  # rt_max <- input$FCEECDF.AUC.Max %>% as.numeric
  # rt_step <- input$FCEECDF.AUC.Step %>% as.numeric
  # data <- subset(DATA(), algId %in% input$FCEECDF.AUC.Algs)
  # 
  # Plot.FV.ECDF_AUC(data, rt_min = rt_min,
  #             rt_max = rt_max, rt_step = rt_step)
  plot_general_data(get_data_FV_AUC(), 'x', 'AUC', 'radar')
  },
  message = "Creating plot")
})

output$FCEECDF.AUC.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_AUC)
  },
  content = function(file) {
    save_plotly(render_FV_AUC(), file)
  },
  contentType = paste0('image/', input$FCEECDF.AUC.Format)
)

output$FCE_AUC <- renderPlotly({
  render_FV_AUC()
})
