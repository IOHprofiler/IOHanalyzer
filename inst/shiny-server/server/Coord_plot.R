render_selected_coords <- reactive({
  data <- DATA()
  req(attr(data[[1]], 'contains_position')) #TODO: change this to be a datasetlist attr instead of dataset
  
  data <- subset(data, algId == input$CoordPlot.Algs)
  
  withProgress({
    pos <- get_position_dsl(data, input$CoordPlot.Iid)
    
    # p <- IOH_plot_ly_default("Coordinate plot animated", "C1", "C2")
    if (!'generation' %in% colnames(pos)) {
      ind_per_gen <- input$CoordPlot.Gen_size
      pos[,generation := ceiling(runtime/ind_per_gen)]
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
    
    plot_general_data(pos[order(generation),], x_attr = colname_x, y_attr = colname_y, type = 'anim_scatter',
                      symbol_attr = 'run_nr', frame_attr = 'generation')
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

render_splom <- reactive({
  data <- DATA()
  req(attr(data[[1]], 'contains_position')) #TODO: change this to be a datasetlist attr instead of dataset
  
  data <- subset(data, algId == input$SplomPlot.Algs)
  
  withProgress({
    pos <- get_position_dsl(data, input$SplomPlot.Iid)
    
    # p <- IOH_plot_ly_default("Coordinate plot animated", "C1", "C2")
    if (!'generation' %in% colnames(pos)) {
      ind_per_gen <- input$SplomPlot.Gen_size
      pos[,generation := ceiling(runtime/ind_per_gen)]
    }
    
    #Temporary, to fit in the existing plot_general_function structure
    pos[, c('temp0', 'temp1') := 0]
    
    
    plot_general_data(pos[order(generation),], x_attr = 'temp0', y_attr = 'temp1', type = 'anim_splom',
                      symbol_attr = 'run_nr', frame_attr = 'generation', nr_dim = get_dim(data))
  })
})

output$Splom_Plot <- renderPlotly({
  render_splom()
})

output$SplomPlot.Download <- downloadHandler(
  filename = function() {
    paste0('Splom_plot.', input$SplomPlot.Format )
  },
  content = function(file) {
    save_plotly(render_splom(), file)
  },
  contentType = paste0('image/', input$SplomPlot.Format)
)

# plot_positions_high_d <- function(base_dir, fid, dim, budget, seed = 1) {
#   files <- get_accepted_files(base_dir, fid = fid, dim = dim, budget = budget)
#   plots <- lapply(c("default", "EGO"), function(type_) {
#     files_ <- files[grepl(type_, files)]
#     load(files_[[seed]])
#     dt <- as.data.table(iraceResults$experimentLog)
#     cutoffs <- dt[,max(configuration), by = iteration][['V1']]
#     dt <- as.data.table(iraceResults$allConfigurations)
#     get_iter <- function(x) {return(get_iteration(x, cutoffs))}
#     dt[, 'iteration' := sapply(.ID., get_iter)]
#     dt$.PARENT. <- NULL
#     dt$fid <- NULL
#     dt2 <- melt(dt, id.vars = c('.ID.', 'iteration'))
#     dt2$iteration_str <- paste0("I", dt2$iteration)
#     p <- IOH_plot_ly_default("Parallel coordinate plot attempt 1", "Coordinate", "Value")
#     p %<>% add_trace(data = dt2, x = ~variable, y = ~value, type = 'scatter',
#                      mode = 'markers', marker = list(color = ~iteration, colorscale = 'Viridis',
#                                                      colorbar = list(title = 'Iteration number'),
#                                                      symbol = 'cross'), opacity = 0.9, showlegend = F)
#     p
#   })
#   subplot(plots[[1]], plots[[2]], shareY = T, shareX = T) %>% layout(annotations = list(
#     list(x = 0.2 , y = 1.05, text = "Default irace", showarrow = F, xref='paper', yref='paper'),
#     list(x = 0.79 , y = 1.05, text = "EGO", showarrow = F, xref='paper', yref='paper'))
#   )
# }

