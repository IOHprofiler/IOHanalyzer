render_parallel_coord <- reactive({
  data <- DATA()
  req(attr(data, 'suite') == "SOS")
  data <- subset(data, algId == input$ParCoordPlot.Algs)
  withProgress({
    p <- IOH_plot_ly_default("Parallel coordinate plot", "Coordinate", "Value")
    
    pos <- rbindlist(lapply(data, function(ds) {
      final_pos_dt <- rbindlist(lapply(ds$PAR$final_position, function(pos)
      {dt <- as.data.table(t(pos))}))
      colnames(final_pos_dt) <- as.character(seq_len(ncol(final_pos_dt)))
      final_pos_dt[, algId := attr(ds, 'algId')]
      final_pos_dt[, funcId := attr(ds, 'funcId')]
      final_pos_dt[, fval_log := log10(attr(ds, 'finalFV'))]
    }))
    
    pos2 <- pos %>% melt(id.vars =  c('fval_log', 'funcId', 'algId'))
    p %>% add_trace(data = pos2, x = ~variable, y = ~value, type = 'scatter',
                    mode = 'markers', marker = list(color = ~fval_log, colorscale = 'Viridis',
                                                    colorbar = list(title = 'Log best f(x)'),
                                                    symbol = 'cross'),
                    legendgroup = ~algId, opacity = 0.9, showlegend = F) %>%
      layout(yaxis = list(range = c(-0.02,1.02)), xaxis = list(range = c(-0.5, get_dim(dsl) + 0.5)))
  })
})

output$Parallel_Coord_Plot <- renderPlotly({
  render_parallel_coord()
})

output$ParCoordPlot.Download <- downloadHandler(
  filename = function() {
    paste0('Par_coord_', input$ParCoordPlot.Algs, '.', input$ParCoordPlot.Format )
  },
  content = function(file) {
    save_plotly(render_parallel_coord(), file)
  },
  contentType = paste0('image/', input$ParCoordPlot.Format)
)