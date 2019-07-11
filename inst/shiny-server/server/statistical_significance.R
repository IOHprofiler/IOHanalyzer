render_heatmap <- reactive({
  req(length(DATA()) > 0)
  withProgress({
    target <- as.numeric(input$Stats.Overview.Target)
    data <- subset(DATA(), algId %in% input$Stats.Overview.Algid)
    Plot.Stats.Significance_Heatmap(data, target, alpha = as.numeric(input$Stats.Overview.Alpha),
                                    bootstrap.size = input$Stats.Overview.Samples)
  },
  message = "Creating plot")
})


output$Stats.Overview.Heatmap <- renderPlotly(
  render_heatmap()
)

output$Stats.Overview.Pmatrix <- DT::renderDataTable({
  req(length(DATA()) > 0)
  data <- subset(DATA(), algId %in% input$Stats.Overview.Algid)
  target <- as.numeric(input$Stats.Overview.Target)
  df <- pairwise.test(data, target, alpha = as.numeric(input$Stats.Overview.Alpha),
                      bootstrap.size = input$Stats.Overview.Samples)
  df[is.na(df)] <- 'NA'
  df
}, options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))
