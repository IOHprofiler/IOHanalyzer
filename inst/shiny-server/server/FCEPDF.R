# empirical p.d.f. of the target value
render_FV_PDF <- reactive({
  req(input$FCEPDF.Bar.Runtime)
  withProgress({
  runtime <- input$FCEPDF.Bar.Runtime %>% as.integer
  Plot.FV.PDF(DATA(), runtime, show.sample = input$FCEPDF.Bar.Samples,
              scale.ylog = input$FCEPDF.Bar.Logy )
  },
  message = "Creating plot")
})

output$FCEPDF.Bar.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PDF)
  },
  content = function(file) {
    save_plotly(render_FV_PDF(), file,
                format = input$FCEPDF.Bar.Format,
                width = fig_width2, height = fig_height)
  },
  contentType = paste0('image/', input$FCEPDF.Bar.Format)
)

output$FCE_PDF <- renderPlotly({
  render_FV_PDF()
})

# historgram of the target values -----------
render_FV_HIST <- reactive({
  req(input$FCEPDF.Hist.Runtime != "")   # require non-empty input
  withProgress({
  runtime <- input$FCEPDF.Hist.Runtime %>% as.integer
  Plot.FV.Histogram(DATA(), runtime, plot_mode = input$FCEPDF.Hist.Mode)
  },
  message = "Creating plot")
})

output$FCEPDF.Hist.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_HIST)
  },
  content = function(file) {
    save_plotly(render_FV_HIST(), file,
                format = input$FCEPDF.Hist.Format,
                width = fig_width2, height = fig_height2)
  },
  contentType = paste0('image/', input$FCEPDF.Hist.Format)
)

output$FCE_HIST <- renderPlotly({
  render_FV_HIST()
})
