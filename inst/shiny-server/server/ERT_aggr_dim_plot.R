### Generate ERT table
get_data_ERT_aggr_dim <- reactive({
  input$ERTPlot.Aggr_Dim.Refresh
  req(length(DATA_RAW()) > 0)
  data <- ERTPlot.Aggr_Dim.data()
  if (is.null(data)) return(NULL)
  aggr_on <- 'DIM'
  targets <- isolate(ERTPlot.Aggr_Dim.Targets_obj)
  dt <- generate_data.Aggr(data, aggr_on, targets)
  dt
})

### Plot the data
render_ERTPlot_aggr_plot_dim <- reactive({
  withProgress({
    data <- get_data_ERT_aggr_dim()
    req(data)
    y_attr <- if (input$ERTPlot.Aggr_Dim.Ranking) 'rank' else 'value'
    y_title <- if (input$ERTPlot.Aggr_Dim.Ranking) 'Rank' else 'ERT'
    reverse_scale <- input$ERTPlot.Aggr_Dim.Mode == 'radar'
    
    plot_general_data(data, type = input$ERTPlot.Aggr_Dim.Mode, x_attr = 'DIM',
                      y_attr = y_attr, x_title = "DIM", y_title = y_title, show.legend = T,
                      scale.ylog = input$ERTPlot.Aggr_Dim.Logy, scale.xlog = T, 
                      scale.reverse = reverse_scale)
  },
  message = "Creating plot")
})

### Gather relevant datasetlist
ERTPlot.Aggr_Dim.data <- function() {
  data <- subset(DATA_RAW(), algId %in% isolate(input$ERTPlot.Aggr_Dim.Algs))
  if (length(data) == 0) return(NULL)
  data <- subset(data, funcId == input$Overall.Funcid)
  
  if (length(unique(get_dim(data))) <= 1) {
    shinyjs::alert("This plot is only available when the dataset contains 
                   multiple dimensions for the selected function")
    return(NULL)
  }
  
  if (length(unique(get_algId(data))) <= 1) {
    shinyjs::alert("This plot is only available when the dataset contains
                   multiple algorithms for the selected function")
    return(NULL)
  }
  data
}

### format table for display
ert_multi_dim <- function(){
  dt <- get_data_ERT_aggr_dim()
  dt <- dcast(dt, DIM~algId, value.var = 'value')
  format(dt, digits = 4) 
}

### Table with default targets
default_targets_table_dim <- reactive({
  data <- ERTPlot.Aggr_Dim.data()
  if (is.null(data)) return(NULL)
  get_target_dt(data)
  targets <- targets[, c('DIM','target')] 
})


### Target table object
ERTPlot.Aggr_Dim.Targets_obj <- NULL

### Target table proxy
proxy_ERTPlot.Aggr_Dim.Targets <- dataTableProxy('ERTPlot.Aggr_Dim.Targets')

### Target table print
output$ERTPlot.Aggr_Dim.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  ERTPlot.Aggr_Dim.Targets_obj <<- default_targets_table_dim()
  ERTPlot.Aggr_Dim.Targets_obj
}, editable = TRUE, rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))



### Target table edit
observeEvent(input$ERTPlot.Aggr_Dim.Targets_cell_edit, {
  info <- input$ERTPlot.Aggr_Dim.Targets_cell_edit
  i <- info$row
  req(i > 0)
  j <- info$col + 1
  v <- info$value
  ERTPlot.Aggr_Dim.Targets_obj[i, j] <<- 
    DT::coerceValue(v, ERTPlot.Aggr_Dim.Targets_obj[['target']][[i]])
  replaceData(proxy, ERTPlot.Aggr_Dim.Targets_obj, resetPaging = FALSE, rownames = FALSE)
})

### Table output
output$ERTPlot.Aggr_Dim.ERTTable <- DT::renderDataTable({
  input$ERTPlot.Aggr_Dim.Refresh
  req(length(DATA_RAW()) > 0)
  
  withProgress({
    ert_multi_dim()
  },
  message = "Creating table")
}, editable = FALSE, rownames = TRUE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))



### plot output
output$ERTPlot.Aggr_Dim.Plot <- renderPlotly(
  render_ERTPlot_aggr_plot_dim()
)

### Download table
output$ERTPlot.Aggr_Dim.DownloadTable <- downloadHandler(
  filename = function() {
    eval(ERT_multi_dim_name)
  },
  content = function(file) {
    df <- ert_multi_dim()
    if (input$ERTPlot.Aggr_Dim.TableFormat == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
  }
)

### Download plot
output$ERTPlot.Aggr_Dim.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_AGGR_DIM)
  },
  content = function(file) {
    save_plotly(render_ERTPlot_aggr_plot_dim(), file)
  },
  contentType = paste0('image/', input$ERTPlot.Aggr_Dim.Format)
)
