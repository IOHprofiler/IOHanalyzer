# empirical p.d.f. of the target value
get_data_FV_PMF <- reactive({
  ftarget <- input$FCEPDF.Bar.Runtime %>% as.numeric
  data <- subset(DATA(), ID %in% input$FCEPDF.Bar.Algs)
  generate_data.PMF(data, ftarget, 'by_FV')
})

render_FV_PDF <- reactive({
  withProgress({
    plot_general_data(get_data_FV_PMF(), 'ID', 'f(x)', scale.ylog = input$FCEPDF.Bar.Logy,
                      x_title = "Algorithm", y_title = "Target Value",
                      violin.showpoints = input$FCEPDF.Bar.Points)
    # ftarget <- input$RTPMF.Bar.Target %>% as.numeric
    # data <- subset(DATA(), algId %in% input$RTPMF.Bar.Algs)
    # Plot.RT.PMF(data, ftarget, show.sample = input$RTPMF.Bar.Sample,
    #             scale.ylog = input$RTPMF.Bar.Logy)
  },
  message = "Creating plot")
})

output$FCEPDF.Bar.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PDF)
  },
  content = function(file) {
    save_plotly(render_FV_PDF(), file)
  },
  contentType = paste0('image/', input$FCEPDF.Bar.Format)
)

output$FCE_PDF <- renderPlotly({
  render_FV_PDF()
})

# historgram of the target values -----------


get_data_FV_HIST <- reactive({
  ftarget <- input$FCEPDF.Hist.Runtime %>% as.numeric
  data <- subset(DATA(), ID %in% input$FCEPDF.Hist.Algs)
  generate_data.hist(data, ftarget, input$FCEPDF.Hist.Equal, 'by_FV')
})


render_FV_HIST <- reactive({
  req(input$FCEPDF.Hist.Runtime != "", length(DATA()) > 0)   # require non-empty input
  withProgress({
    subplot_attr <- if (input$FCEPDF.Hist.Mode == 'subplot') 'ID' else NULL
    plot_general_data(get_data_FV_HIST(), 'x', 'y', width = 'width', type = 'hist',
                      subplot_attr = subplot_attr, x_title = "Target Values",
                      y_title = "Runs")
  # runtime <- input$FCEPDF.Hist.Runtime %>% as.numeric
  # data <- subset(DATA(), algId %in% input$FCEPDF.Hist.Algs)
  # Plot.FV.Histogram(data, runtime, plot_mode = input$FCEPDF.Hist.Mode, use.equal.bins = input$FCEPDF.Hist.Equal)
  },
  message = "Creating plot")
})

output$FCEPDF.Hist.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_HIST)
  },
  content = function(file) {
    save_plotly(render_FV_HIST(), file)
  },
  contentType = paste0('image/', input$FCEPDF.Hist.Format)
)

output$FCE_HIST <- renderPlotly({
  render_FV_HIST()
})

# --------------------------- Cumulative difference plot



get_data_FV_CDP <- reactive({
  runtime <- input$FCEPDF.CDP.Runtime %>% as.numeric
  data <- subset(DATA(), ID %in% input$FCEPDF.CDP.Algs)
  generate_data.CDP(data, runtime, isMinimizationProblem = TRUE)
})


render_FV_CDP <- reactive({
  req(input$FCEPDF.CDP.Runtime != "", length(DATA()) > 0)   # require non-empty input
  withProgress({
    subplot_attr <- if (input$FCEPDF.CDP.Mode == 'subplot') 'ID' else NULL
    runtime <- input$FCEPDF.CDP.Runtime %>% as.numeric
    data <- subset(DATA(), ID %in% input$FCEPDF.CDP.Algs)
    Plot.cumulative_difference_plot(data, 0, isMinimizationProblem=TRUE, dataAlreadyComputed = TRUE, precomputedData = get_data_FV_CDP())
    # runtime <- input$FCEPDF.Hist.Runtime %>% as.numeric
    # data <- subset(DATA(), algId %in% input$FCEPDF.Hist.Algs)
    # Plot.FV.Histogram(data, runtime, plot_mode = input$FCEPDF.Hist.Mode, use.equal.bins = input$FCEPDF.Hist.Equal)
  },
  message = "Creating plot")
})

output$FCEPDF.CDP.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_CDP)
  },
  content = function(file) {
    save_plotly(render_FV_CDP(), file)
  },
  contentType = paste0('image/', input$FCEPDF.CDP.Format)
)

output$FCE_CDP <- renderPlotly({
  render_FV_CDP()
})

