# empirical p.m.f. of the runtime
output$RT_PMF <- renderPlotly({
  render_RT_PMF()
})

output$RTPMF.Bar.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_RT_PMF)
  },
  content = function(file) {
    save_plotly(render_RT_PMF(), file)
  },
  contentType = paste0('image/', input$RTPMF.Bar.Format)
)

get_data_RT_PMF <- reactive({
  ftarget <- input$RTPMF.Bar.Target %>% as.numeric
  data <- subset(DATA(), ID %in% input$RTPMF.Bar.Algs)
  generate_data.PMF(data, ftarget, 'by_RT')
})

render_RT_PMF <- reactive({
  withProgress({
    plot_general_data(get_data_RT_PMF(), 'ID', 'RT', scale.ylog = input$RTPMF.Bar.Logy,
                      x_title = "Algorithm", y_title = "Function Evaluations")
  # ftarget <- input$RTPMF.Bar.Target %>% as.numeric
  # data <- subset(DATA(), algId %in% input$RTPMF.Bar.Algs)
  # Plot.RT.PMF(data, ftarget, show.sample = input$RTPMF.Bar.Sample,
  #             scale.ylog = input$RTPMF.Bar.Logy)
  },
  message = "Creating plot")
})

# historgram of the running time
output$RT_HIST <- renderPlotly({
  req(input$RTPMF.Bar.Target)
  render_RT_HIST()
})

output$RTPMF.Hist.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_RT_HIST)
  },
  content = function(file) {
    save_plotly(render_RT_HIST(), file)
  },
  contentType = paste0('image/', input$RTPMF.Hist.Format)
)

get_data_RT_HIST <- reactive({
  ftarget <- format_FV(input$RTPMF.Hist.Target) %>% as.numeric
  data <- subset(DATA(), ID %in% input$RTPMF.Hist.Algs)
  generate_data.hist(data, ftarget, input$RTPMF.Hist.Equal, 'by_RT')
})

render_RT_HIST <- reactive({
  req(input$RTPMF.Hist.Target)
  
  withProgress({
    # ftarget <- format_FV(input$RTPMF.Hist.Target) %>% as.numeric
    # 
    # # TODO: remove 'DataSetList' in the future
    # data <- subset(DATA(), algId %in% input$RTPMF.Hist.Algs)
    # 
    # Plot.RT.Histogram(data, ftarget, plot_mode = input$RTPMF.Hist.Mode, 
    #                   use.equal.bins = input$RTPMF.Hist.Equal)
    subplot_attr <- if (input$RTPMF.Hist.Mode == 'subplot') 'ID' else NULL
    plot_general_data(get_data_RT_HIST(), 'x', 'y', width = 'width', type = 'hist', 
                      subplot_attr = subplot_attr, x_title = "Function Evaluations",
                      y_title = "Runs")
  },
  message = "Creating plot")
})
