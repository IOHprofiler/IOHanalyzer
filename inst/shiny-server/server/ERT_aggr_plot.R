### Generate ERT table 
ERTPlot.Aggr.ERTs_obj <- reactive({
  input$ERTPlot.Aggr.Refresh
  dsList <- isolate(ERTPlot.Aggr.data())
  if (is.null(dsList)) return(NULL)
  aggr_on <- 'funcId'
  targets <- isolate(ERTPlot.Aggr.Targets_obj)
  dt <- generate_data.Aggr(dsList, aggr_on = aggr_on, targets = targets)
  dt
})

### Plot the data
render_ERTPlot_aggr_plot <- reactive({
  withProgress({
    y_attr <- if (input$ERTPlot.Aggr.Ranking) 'rank' else 'value'
    y_title <- if (input$ERTPlot.Aggr.Ranking) 'Rank' else 'ERT'
    reverse_scale <- input$ERTPlot.Aggr.Mode == 'radar'
    dt <- ERTPlot.Aggr.ERTs_obj()
    if (is.null(dt))
      return(NULL)
    dt <- dt[funcId %in% isolate(input$ERTPlot.Aggr.Funcs), ]
    plot_general_data(dt, type = input$ERTPlot.Aggr.Mode, x_attr = 'funcId',
                      y_attr = y_attr, x_title = "FuncId", y_title = y_title, show.legend = T,
                      scale.ylog = input$ERTPlot.Aggr.Logy, scale.reverse = reverse_scale, 
                      inf.action = 'jitter')
  },
  message = "Creating plot")
})

### Gather relevant datasetlist
ERTPlot.Aggr.data <- function() {
  data <- subset(DATA_RAW(), algId %in% isolate(input$ERTPlot.Aggr.Algs))
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM == input$Overall.Dim)
  
  if (length(unique(get_funcId(data))) == 1) {
    shinyjs::alert("This plot is only available when the dataset contains 
                   multiple functions for the selected dimension.")
    return(NULL)
  }
  
  if (length(unique(get_algId(data))) == 1) {
    shinyjs::alert("This plot is only available when the dataset contains
                   multiple algorithms for the selected dimension.")
    return(NULL)
  }
  data
}

### format table for display
ert_multi_function <- function() {
  dt <- ERTPlot.Aggr.ERTs_obj()
  dt <- dcast(dt, funcId~algId, value.var = 'value')
  format(dt, digits = 4) 
}

### Table with default targets
default_targets_table <- reactive({
  data <- ERTPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  targets <- get_target_dt(data)
  targets <- targets[, c('funcId','target')] 
  # %>% melt(id.vars = 'funcId') %>% dcast(variable ~ funcId)
  # targets <- targets[, 'variable' = NULL]
  # rownames(targets) <- 'funcId'
  # targets
})

### Target table object
ERTPlot.Aggr.Targets_obj <- NULL

### Target table proxy
proxy_ERTPlot.Aggr.Targets <- dataTableProxy('ERTPlot.Aggr.Targets')

### Target table print
output$ERTPlot.Aggr.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  ERTPlot.Aggr.Targets_obj <<- default_targets_table()
  ERTPlot.Aggr.Targets_obj
}, editable = TRUE, rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


### Target table edit
observeEvent(input$ERTPlot.Aggr.Targets_cell_edit, {
  info <- input$ERTPlot.Aggr.Targets_cell_edit
  i <- info$row
  req(i > 0)
  j <- info$col + 1
  v <- info$value
  ERTPlot.Aggr.Targets_obj[i, j] <<- 
                     DT::coerceValue(v, ERTPlot.Aggr.Targets_obj[['target']][[i]])
  replaceData(proxy, ERTPlot.Aggr.Targets_obj, resetPaging = FALSE, rownames = FALSE)
})

### Table output
output$ERTPlot.Aggr.ERTTable <- DT::renderDataTable({
  input$ERTPlot.Aggr.Refresh
  req(length(DATA_RAW()) > 0)
  
  withProgress({
    dt <- ert_multi_function()
  },
  message = "Creating table")
  dt
}, editable = FALSE, rownames = TRUE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))

### plot output
output$ERTPlot.Aggr.Plot <- renderPlotly(
  render_ERTPlot_aggr_plot()
)

### Download table
output$ERTPlot.Aggr.DownloadTable <- downloadHandler(
  filename = function() {
    eval(ERT_multi_func_name)
  },
  content = function(file) {
    df <- ert_multi_function()
    save_table(df, file)
  }
)

### Download plot
output$ERTPlot.Aggr.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_AGGR)
  },
  content = function(file) {
    save_plotly(render_ERTPlot_aggr_plot(), file)
  },
  contentType = paste0('image/', input$ERTPlot.Aggr.Format)
)
