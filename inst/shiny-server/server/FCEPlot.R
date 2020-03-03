# Expected Target Value Convergence
output$FCE_PER_FUN <- renderPlotly({
  req(input$FCEPlot.Min, input$FCEPlot.Max, length(DATA()) > 0)
  render_FV_PER_FUN()
})

get_data_FCE_PER_FUN <- reactive({
  req(input$FCEPlot.Min, input$FCEPlot.Max, length(DATA()) > 0)
  data <- subset(DATA(), algId %in% input$FCEPlot.Algs)
  fstart <- input$FCEPlot.Min %>% as.numeric
  fstop <- input$FCEPlot.Max %>% as.numeric
  generate_data.Single_Function(data, fstart, fstop, input$FCEPlot.semilogx, 
                                'by_FV')
})

render_FV_PER_FUN <- reactive({
  withProgress({
    y_attrs <- c()
    if (input$FCEPlot.show.mean) y_attrs <- c(y_attrs, 'mean')
    if (input$FCEPlot.show.median) y_attrs <- c(y_attrs, 'median')
    if (length(y_attrs) > 0)
      p <- plot_general_data(get_data_FCE_PER_FUN(), x_attr = 'runtime', y_attr = y_attrs, 
                             type = 'line', legend_attr = 'algId', show.legend = T, 
                             scale.ylog = isolate(input$FCEPlot.semilogy),
                             scale.xlog = input$FCEPlot.semilogx, x_title = "Runtime",
                             y_title = "Best-so-far f(x)-value")
    else
      p <- NULL
    if (input$FCEPlot.show.CI)
      p <- plot_general_data(get_data_FCE_PER_FUN(), x_attr = 'runtime', y_attr = 'mean', 
                             type = 'ribbon', legend_attr = 'algId', lower_attr = 'lower', 
                             upper_attr = 'upper', p = p)
    p
  },
  message = "Creating plot"
  )
})

output$FCEPlot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_FV_PER_FUN(), file)
  },
  contentType = paste0('image/', input$FCEPlot.Format)
)


update_fv_per_fct_axis <- observe({
  plotlyProxy("FCE_PER_FUN", session) %>%
    plotlyProxyInvoke("relayout", list(yaxis = list(title = 'best-so-far-f(x)-value', type = ifelse(input$FCEPlot.semilogy, 'log', 'linear'))))
})


output$FCEPlot.Multi.Plot <- renderPlotly(
  render_FCEPlot_multi_plot()
)

get_data_FCE_multi_func_bulk <- reactive({
  data <- subset(DATA_RAW(),
                 DIM == input$Overall.Dim)
  if (length(get_algId(data)) < 20) { #Arbitrary limit for the time being
    rbindlist(lapply(get_funcId(data), function(fid) {
      generate_data.Single_Function(subset(data, funcId == fid), scale_log = input$FCEPlot.Multi.Logx, 
                                    which = 'by_FV')
    }))
  }
  else
    NULL
})

get_data_FCEPlot_multi <- reactive({
  req(isolate(input$FCEPlot.Multi.Algs))
  input$FCEPlot.Multi.PlotButton
  if (length(get_algId(data)) < 20) {
    get_data_FCE_multi_func_bulk()[algId %in% isolate(input$FCEPlot.Multi.Algs), ]
  }
  else {
    data <- subset(DATA_RAW(),
                   algId %in% isolate(input$FCEPlot.Multi.Algs),
                   DIM == input$Overall.Dim)
    rbindlist(lapply(get_funcId(data), function(fid) {
      generate_data.Single_Function(subset(data, funcId == fid), scale_log = input$FCEPlot.Multi.Logx, 
                                    which = 'by_FV')
    }))
  }
})

render_FCEPlot_multi_plot <- reactive({
  withProgress({
  plot_general_data(get_data_FCEPlot_multi(), x_attr = 'runtime', y_attr = 'mean', 
                    subplot_attr = 'funcId', type = 'line', scale.xlog = input$FCEPlot.Multi.Logx, 
                    scale.ylog = input$FCEPlot.Multi.Logy, x_title = 'Runtime', 
                    y_title = 'Best-so-far f(x)', show.legend = T)
  },
  message = "Creating plot")
})

output$FCEPlot.Multi.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PER_FUN_MULTI)
  },
  content = function(file) {
    save_plotly(render_FCEPlot_multi_plot(), file)
  },
  contentType = paste0('image/', input$FCEPlot.Multi.Format)
)

