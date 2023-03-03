output$Biobj.Diff.Plot <- renderPlotly({
  req(length(DATA()) > 0)
  render_biobj_diff()
})

get_data_biobj_diff <- reactive({
  req(input$Biobj.Diff.Algs, length(DATA()) > 0)
  data <- subset(DATA(), ID %in% input$Biobj.Diff.Algs)

})

render_biobj_diff <- reactive({
  withProgress({

    data <- get_data_biobj_diff()

    xmin <- isolate(input$Biobj.Diff.xMin %>% as.numeric)
    xmax <- isolate(input$Biobj.Diff.xMax %>% as.numeric)
    ymin <- isolate(input$Biobj.Diff.yMin %>% as.numeric)
    ymax <- isolate(input$Biobj.Diff.yMax %>% as.numeric)

    Plot.BiObj.Difference(data, xmin, xmax, ymin, ymax,
                          scale.xlog = input$Biobj.Diff.semilogx,
                          scale.ylog = input$Biobj.Diff.semilogy,
                          show.min = input$Biobj.Diff.show.min,
                          show.max = input$Biobj.Diff.show.max
    )
  },
  message = "Creating plot"
  )
})

output$Biobj.Diff.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_biobj_diff)
  },
  content = function(file) {
    save_plotly(render_biobj_diff(), file)
  },
  contentType = paste0('image/', input$Biobj.Diff.Format)
)
