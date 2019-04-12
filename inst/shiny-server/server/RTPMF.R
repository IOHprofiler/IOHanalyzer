# empirical p.m.f. of the runtime
output$RT_PMF <- renderPlotly({
  render_RT_PMF()
})

output$RTPMF.Bar.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_RT_PMF)
  },
  content = function(file) {
    save_plotly(render_RT_PMF(), file,
                format = input$RTPMF.Bar.Format,
                width = fig_width2, height = fig_height)
  },
  contentType = paste0('image/', input$RTPMF.Bar.Format)
)

render_RT_PMF <- reactive({
  ftarget <- input$RTPMF.Bar.Target %>% as.numeric
  plot_RT_PMF(DATA(), ftarget, show.sample = input$RTPMF.Bar.Sample,
              scale.ylog = input$RTPMF.Bar.Logy)
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
    save_plotly(render_RT_HIST(), file,
                format = input$RTPMF.Hist.Format,
                width = fig_width2, height = fig_height2)
  },
  contentType = paste0('image/', input$RTPMF.Hist.Format)
)

render_RT_HIST <- reactive({
  req(input$RTPMF.Hist.Target)
  ftarget <- format_FV(input$RTPMF.Hist.Target) %>% as.numeric
  plot_mode <- input$RTPMF.Hist.Mode

  # TODO: remove 'DataSetList' in the future
  plot_RT_HIST(DATA(), ftarget, plot_mode = plot_mode)
})
