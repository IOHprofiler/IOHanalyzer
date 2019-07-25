# The ECDF plots for the target value ----------------
output$FCE_ECDF_PER_TARGET <- renderPlotly({
  render_ecdf_per_target()
})

render_ecdf_per_target <- reactive({
  req(input$FCEECDF.Single.Target)
  withProgress({
    runtimes <- as.numeric(input$FCEECDF.Single.Target)
    data <- subset(DATA(), algId %in% input$FCEECDF.Single.Algs)
    
    Plot.FV.ECDF_Per_Target(data,runtimes, scale.xlog = input$FCEECDF.Single.Logx,
                            scale.reverse = !attr(DATA()[[1]],'maximization'))
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

render_FV_ECDF_AGGR <- reactive({
  req(input$FCEECDF.Mult.Min, input$FCEECDF.Mult.Max, input$FCEECDF.Mult.Step, length(DATA()) > 0)
  withProgress({
  rt_min <- input$FCEECDF.Mult.Min %>% as.numeric
  rt_max <- input$FCEECDF.Mult.Max %>% as.numeric
  rt_step <- input$FCEECDF.Mult.Step %>% as.numeric
  data <- subset(DATA(), algId %in% input$FCEECDF.Mult.Algs)
  
  Plot.FV.ECDF_Single_Func(data,rt_min = rt_min,
                    rt_max = rt_max, rt_step = rt_step,
                    scale.xlog = input$FCEECDF.Mult.Logx,
                    # show.per_target = input$FCEECDF.Mult.Targets,
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
                format = input$FCEECDF.Mult.Format)
  },
  contentType = paste0('image/', input$FCEECDF.Mult.Format)
)

output$FCE_ECDF_AGGR <- renderPlotly({
  render_FV_ECDF_AGGR()
})

# evaluation rake of all courses
render_FV_AUC <- reactive({
  req(input$FCEECDF.AUC.Min, input$FCEECDF.AUC.Max, input$FCEECDF.AUC.Step, length(DATA()) > 0)
  withProgress({
  rt_min <- input$FCEECDF.AUC.Min %>% as.numeric
  rt_max <- input$FCEECDF.AUC.Max %>% as.numeric
  rt_step <- input$FCEECDF.AUC.Step %>% as.numeric
  data <- subset(DATA(), algId %in% input$FCEECDF.AUC.Algs)
  
  Plot.FV.ECDF_AUC(data, rt_min = rt_min,
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
                format = input$FCEECDF.AUC.Format)
  },
  contentType = paste0('image/', input$FCEECDF.AUC.Format)
)

output$FCE_AUC <- renderPlotly({
  render_FV_AUC()
})
