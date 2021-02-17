render_selected_coords <- reactive({
  data <- DATA()
  req(attr(data[[1]], 'includes_pos')) #TODO: change this to be a datasetlist attr instead of dataset
  
  data <- subset(data, algId == input$CoordPlot.Algs)
  
  withProgress({
    pos <- get_position_dsl(data, input$CoordPlot.Iid)
    
    # p <- IOH_plot_ly_default("Coordinate plot animated", "C1", "C2")
    if (!'generation' %in% colnames(dt)) {
      ind_per_gen <- input$CoordPlot.Gen_size
      dt[,generation := ceiling(runtime/ind_per_gen)]
    }
    colname_x = paste0("x", input$CoordPlot.C1)
    colname_y = paste0("x", input$CoordPlot.C2)
    
    if (input$CoordPlot.PCA) {
      xnames <- paste0('x', seq(0, get_dim(data)[[1]] - 1))
      pca = prcomp(pos[, ..xnames], center = T, scale. = T)
      pos = cbind(pos, pca)
      colname_x = 'PC1'
      colname_y = 'PC2'
    }
    
    plot_general_data(dt[order(generation),], x_attr = colname_x, y_attr = colname_y, type = 'anim_scatter',
                      symbol_attr = 'run_nr', frame_attr = 'generation')
    # TODO: Make coordinates selectable
    # p %>% add_trace(data = dt[order(generation),], x = ~x0, y = ~x1, type = 'scatter',
    #                 mode = 'markers', marker = list(color = ~run_nr, 
    #                                                 symbol = ~algId),
    #                 legendgroup = ~algId, opacity = 1, frame = ~generation, showlegend = F) 
    
    
    # %>% layout(yaxis = list(range = c(-0.02,1.02)), xaxis = list(range = c(-0.5, get_dim(dsl) + 0.5)))
  })
})

output$Coord_Plot <- renderPlotly({
  render_selected_coords()
})

output$CoordPlot.Download <- downloadHandler(
  filename = function() {
    paste0('Coord_plot.', input$CoordPlot.Format )
  },
  content = function(file) {
    save_plotly(render_selected_coords(), file)
  },
  contentType = paste0('image/', input$CoordPlot.Format)
)



