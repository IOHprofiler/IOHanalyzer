# The ECDF plots for the target value ----------------
output$FCE_ECDF_PER_TARGET <- renderPlotly({
  withProgress({
  req(input$FCEECDF.Single.Target)
  runtimes <- as.integer(input$FCEECDF.Single.Target)
  Plot.FV.ECDF_Per_Target(DATA(),runtimes, scale.xlog = input$FCEECDF.Single.Logx,
                          scale.reverse = !attr(DATA()[[1]],'maximization'))
  },
  message = "Creating plot")
})

output$FCE_RT_GRID <- renderPrint({
  req(input$FCEECDF.Mult.Min, input$FCEECDF.Mult.Max, input$FCEECDF.Mult.Step, DATA())

  rt_min <- input$FCEECDF.Mult.Min %>% as.integer
  rt_max <- input$FCEECDF.Mult.Max %>% as.integer
  rt_step <- input$FCEECDF.Mult.Step %>% as.integer

  req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
  data <- DATA()
  rt <- get_runtimes(data)

  seq_RT(rt, from = rt_min, to = rt_max, by = rt_step) %>% cat
})

render_FV_ECDF_AGGR <- reactive({
  req(input$FCEECDF.Mult.Min, input$FCEECDF.Mult.Max, input$FCEECDF.Mult.Step, DATA())
  withProgress({
  rt_min <- input$FCEECDF.Mult.Min %>% as.integer
  rt_max <- input$FCEECDF.Mult.Max %>% as.integer
  rt_step <- input$FCEECDF.Mult.Step %>% as.integer

  Plot.FV.ECDF_Single_Func(DATA(),rt_min = rt_min,
                    rt_max = rt_max, rt_step = rt_step,
                    scale.xlog = input$FCEECDF.Mult.Logx,
                    show.per_target = input$FCEECDF.Mult.Targets,
                    scale.reverse = !attr(DATA()[[1]],'maximization'))
  },
  message = "Creating plot")
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
  req(input$FCEECDF.AUC.Min, input$FCEECDF.AUC.Max, input$FCEECDF.AUC.Step, DATA())
  withProgress({
  rt_min <- input$FCEECDF.AUC.Min %>% as.integer
  rt_max <- input$FCEECDF.AUC.Max %>% as.integer
  rt_step <- input$FCEECDF.AUC.Step %>% as.integer
  Plot.FV.ECDF_AUC(DATA(), rt_min = rt_min,
              rt_max = rt_max, rt_step = rt_step)
  },
  message = "Creating plot")
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
