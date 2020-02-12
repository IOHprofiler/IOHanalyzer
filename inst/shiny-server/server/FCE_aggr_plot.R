### Generate FCE table
FCEPlot.Aggr.FCEs_obj <- reactive({
  input$FCEPlot.Aggr.Refresh
  dsList <- isolate(FCEPlot.Aggr.data())
  if (is.null(dsList)) return(NULL)
  aggr_on <- ifelse(input$FCEPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
  targets <- isolate(FCEPlot.Aggr.Targets_obj)
  dt <- generate_data.Aggr(dsList, aggr_on = aggr_on, targets = targets, 
                           which = 'by_FV')
  dt
})

### Plot the data 
render_FCEPlot_aggr_plot <- reactive({
  withProgress({
    y_attr <- if (input$FCEPlot.Aggr.Ranking) 'rank' else 'value'
    y_title <- if (input$FCEPlot.Aggr.Ranking) 'Rank' else 'Best-so-far f(x)'
    reverse_scale <- input$FCEPlot.Aggr.Mode == 'radar'
    dt <- FCEPlot.Aggr.FCEs_obj()
    plot_general_data(dt, type = input$FCEPlot.Aggr.Mode, x_attr = 'funcId',
                      y_attr = y_attr, x_title = "FuncId", y_title = y_title, show.legend = T,
                      scale.ylog = input$FCEPlot.Aggr.Logy, scale.reverse = reverse_scale)
  },
  message = "Creating plot")
})


### Gather relevant datasetlist 
FCEPlot.Aggr.data <- function() {
  data <- subset(DATA_RAW(), algId %in% isolate(input$FCEPlot.Aggr.Algs))
  if (length(data) == 0) return(NULL)
  if (input$FCEPlot.Aggr.Aggregator == 'Functions') {
    data <- subset(data, DIM == input$Overall.Dim)
    if (length(unique(get_funcId(data))) == 1) {
      shinyjs::alert("This plot is only available when the dataset contains multiple functions for the selected dimension.")
      return(NULL)
    }
  }
  else{
    data <- subset(data, funcId == input$Overall.Funcid)
    if (length(unique(get_dim(data))) == 1) {
      shinyjs::alert("This plot is only available when the dataset contains multiple dimensions for the selected function")
      return(NULL)
    }
  }
  if (length(unique(get_algId(data))) == 1) {
    shinyjs::alert("This plot is only available when the dataset contains multiple algorithms for the selected dimension.")
    return(NULL)
  }
  data
}


### format table for display 
FCE_multi_function <- function() {
  dt <- FCEPlot.Aggr.FCEs_obj()
  if (input$FCEPlot.Aggr.Aggregator == 'Functions')
    dt <- dcast(dt, funcId~algId, value.var = 'value')
  else
    dt <- dcast(dt, DIM~algId, value.var = 'value')
  dt 
}

### Table with default targets 
default_runtimes_table <- reactive({
  data <- FCEPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  targets <- get_target_dt(data, 'by_FV')
  if (input$FCEPlot.Aggr.Aggregator == 'Functions')
    targets <- targets[, c('funcId', 'target')] 
  else   
    targets <- targets[, c('DIM', 'target')] 
})

### Target table object 
FCEPlot.Aggr.Targets_obj <- NULL

### Target table proxy
proxy_FCEPlot.Aggr.Targets <- dataTableProxy('FCEPlot.Aggr.Targets')

### Target table print
output$FCEPlot.Aggr.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  FCEPlot.Aggr.Targets_obj <<- default_runtimes_table()
  FCEPlot.Aggr.Targets_obj
}, editable = TRUE, rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))




### Target table edit
observeEvent(input$FCEPlot.Aggr.Targets_cell_edit, {
  info <- input$FCEPlot.Aggr.Targets_cell_edit
  i <- info$row
  req(i > 0)
  j <- info$col + 1
  v <- info$value
  FCEPlot.Aggr.Targets_obj[i, j] <<- 
    DT::coerceValue(v, FCEPlot.Aggr.Targets_obj[['target']][[i]])
  replaceData(proxy, FCEPlot.Aggr.Targets_obj, resetPaging = FALSE, rownames = FALSE)
})

### Table output
output$FCEPlot.Aggr.FCETable <- DT::renderDataTable({
  input$FCEPlot.Aggr.Refresh
  req(length(DATA_RAW()) > 0)
  
  withProgress({
    dt <- FCE_multi_function()
  },
  message = "Creating table")
  dt
}, editable = FALSE, rownames = TRUE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))

### plot output
output$FCEPlot.Aggr.Plot <- renderPlotly(
  render_FCEPlot_aggr_plot()
)



### Download table
output$FCEPlot.Aggr.DownloadTable <- downloadHandler(
  filename = function() {
    eval(FCE_multi_func_name)
  },
  content = function(file) {
    df <- FCE_multi_function()
    if (input$FCEPlot.Aggr.TableFormat == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
  }
)

### Download plot
output$FCEPlot.Aggr.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_AGGR)
  },
  content = function(file) {
    save_plotly(render_FCEPlot_aggr_plot(), file)
  },
  contentType = paste0('image/', input$FCEPlot.Aggr.Format)
)



