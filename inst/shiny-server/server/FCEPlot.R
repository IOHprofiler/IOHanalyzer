# Expected Target Value Convergence
output$FCE_PER_FUN <- renderPlotly({
  req(input$FCEPlot.Min, input$FCEPlot.Max, length(DATA()) > 0)
  render_FV_PER_FUN()
})

get_data_FCE_PER_FUN <- reactive({
  req(input$FCEPlot.Min, input$FCEPlot.Max, length(DATA()) > 0)
  data <- subset(DATA(), algId %in% input$FCEPlot.Algs)
  fstart <- input$FCEPlot.Min %>% as.numeric
  fstop <- input$FCEPlot.Max %>% as.numeric
  generate_data.Single_Function(data, fstart, fstop, input$FCEPlot.semilogx, 
                                'by_FV')
})

render_FV_PER_FUN <- reactive({
  withProgress({
    y_attrs <- c()
    if (input$FCEPlot.show.mean) y_attrs <- c(y_attrs, 'mean')
    if (input$FCEPlot.show.median) y_attrs <- c(y_attrs, 'median')
    if (length(y_attrs) > 0)
      p <- plot_general_data(get_data_FCE_PER_FUN(), x_attr = 'runtime', y_attr = y_attrs, 
                             type = 'line', legend_attr = 'algId', show.legend = T, 
                             scale.ylog = isolate(input$FCEPlot.semilogy),
                             scale.xlog = input$FCEPlot.semilogx, x_title = "Runtime",
                             y_title = "Best-so-far f(x)-value")
    else
      p <- NULL
    if (input$FCEPlot.show.CI)
      p <- plot_general_data(get_data_FCE_PER_FUN(), x_attr = 'runtime', y_attr = 'mean', 
                             type = 'ribbon', legend_attr = 'algId', lower_attr = 'lower', 
                             upper_attr = 'upper', p = p)
    p
  },
  message = "Creating plot"
  )
})

output$FCEPlot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_FV_PER_FUN(), file)
  },
  contentType = paste0('image/', input$FCEPlot.Format)
)


update_fv_per_fct_axis <- observe({
  plotlyProxy("FCE_PER_FUN", session) %>%
    plotlyProxyInvoke("relayout", list(yaxis = list(title = 'best-so-far-f(x)-value', type = ifelse(input$FCEPlot.semilogy, 'log', 'linear'))))
})


output$FCEPlot.Multi.Plot <- renderPlotly(
  render_FCEPlot_multi_plot()
)

get_data_FCEPlot_multi <- reactive({
  req(isolate(input$FCEPlot.Multi.Algs))
  input$FCEPlot.Multi.PlotButton
  data <- subset(DATA_RAW(),
                 algId %in% isolate(input$FCEPlot.Multi.Algs),
                 DIM == input$Overall.Dim)
  rbindlist(lapply(get_funcId(data), function(fid) {
    generate_data.Single_Function(subset(data, funcId == fid), scale_log = input$FCEPlot.Multi.Logx, 
                                  which = 'by_FV')
  }))
})

render_FCEPlot_multi_plot <- reactive({
  withProgress({
  plot_general_data(get_data_FCEPlot_multi(), x_attr = 'runtime', y_attr = 'mean', 
                    subplot_attr = 'funcId', type = 'line', scale.xlog = input$FCEPlot.Multi.Logx, 
                    scale.ylog = input$FCEPlot.Multi.Logy, x_title = 'Runtime', 
                    y_title = 'Best-so-far f(x)', show.legend = T)
  },
  message = "Creating plot")
})

output$FCEPlot.Multi.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PER_FUN_MULTI)
  },
  content = function(file) {
    save_plotly(render_FCEPlot_multi_plot(), file)
  },
  contentType = paste0('image/', input$FCEPlot.Multi.Format)
)

output$FCEPlot.Aggr.Plot <- renderPlotly(
  render_FCEPlot_aggr_plot()
)

get_max_runtimes <- function(data, aggr_on){
  runtimes <- c()
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)

  for (j in seq_along(aggr_attr)) {
    dsList_filetered <- if (aggr_on == 'funcId') subset(data,funcId == aggr_attr[[j]])
    else subset(data, DIM == aggr_attr[[j]])

    RTall <- get_runtimes(dsList_filetered)
    RTval <- max(RTall)
    runtimes <- c(runtimes,RTval)
  }
  names(runtimes) <- aggr_attr
  runtimes
}

get_data_FCE_aggr <- reactive({
  req(length(DATA_RAW()) > 0)
  data <- FCEPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  aggr_on <- ifelse(input$FCEPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
  
  runtimes <- FCEPlot.Aggr.Targets_obj
  names(runtimes) <- NULL
  generate_data.Aggr(data, aggr_on, runtimes, inf_action = 'none', which = 'by_FV')
})


render_FCEPlot_aggr_plot <- reactive({
  input$FCEPlot.Aggr.Refresh
  withProgress({
    y_attr <- if (input$FCEPlot.Aggr.Ranking) 'rank' else 'value'
    y_title <- if (input$FCEPlot.Aggr.Ranking) 'Rank' else 'Best-so-far f(x)'
    reverse_scale <- input$FCEPlot.Aggr.Mode == 'radar'
    plot_general_data(get_data_FCE_aggr(), type = input$FCEPlot.Aggr.Mode, x_attr = 'funcId',
                      y_attr = y_attr, x_title = "FuncId", y_title = y_title, show.legend = T,
                      scale.ylog = input$FCEPlot.Aggr.Logy, scale.reverse = reverse_scale)
  },
  message = "Creating plot")
})

FCEPlot.Aggr.data <- function() {
  data <- subset(DATA_RAW(), algId %in% isolate(input$FCEPlot.Aggr.Algs))
  if (length(data) == 0) return(NULL)
  if (input$FCEPlot.Aggr.Aggregator == 'Functions') {
    data <- subset(data, DIM == input$Overall.Dim)
    if (length(unique(get_funcId(data))) == 1) {
      shinyjs::alert("This plot is only available when the dataset contains multiple functions for the selected dimension.")
      return(NULL)
    }
    fvs <- MEAN_FVALS_FUNC()
  }
  else{
    data <- subset(data, funcId == input$Overall.Funcid)
    if (length(unique(get_dim(data))) == 1) {
      shinyjs::alert("This plot is only available when the dataset contains multiple dimensions for the selected function")
      return(NULL)
    }
    fvs <- MEAN_FVALS_DIM()
  }
  if (length(unique(get_algId(data))) == 1) {
    shinyjs::alert("This plot is only available when the dataset contains multiple algorithms for the selected dimension.")
    return(NULL)
  }
  data
}

default_runtimes_table <- reactive({
  data <- FCEPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  aggr_on <- ifelse(input$FCEPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
  get_max_runtimes(data, aggr_on) %>% t %>% as.data.table(keep.rownames = F)
})

FCEPlot.Aggr.Targets_obj <- NULL

proxy_FCEPlot.Aggr.Targets <- dataTableProxy('FCEPlot.Aggr.Targets')

output$FCEPlot.Aggr.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  FCEPlot.Aggr.Targets_obj <<- default_runtimes_table()
  FCEPlot.Aggr.Targets_obj
}, editable = TRUE, rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))

output$FCEPlot.Aggr.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_AGGR)
  },
  content = function(file) {
    save_plotly(render_FCEPlot_aggr_plot(), file)
  },
  contentType = paste0('image/', input$FCEPlot.Aggr.Format)
)

observeEvent(input$FCEPlot.Aggr.Targets_cell_edit, {
  info <- input$FCEPlot.Aggr.Targets_cell_edit
  i <- info$row
  j <- info$col + 1
  v <- info$value
  data <- FCEPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  aggr_on <- ifelse(input$FCEPlot.Aggr.Aggregator == 'Functions', 'funcId', 'DIM')
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)
  suppressWarnings(FCEPlot.Aggr.Targets_obj[i, paste0(aggr_attr[[j]])] <<- DT::coerceValue(v, FCEPlot.Aggr.Targets_obj[i, paste0(aggr_attr[[j]])]))
  replaceData(proxy, FCEPlot.Aggr.Targets_obj, resetPaging = FALSE, rownames = FALSE)
})

fce_multi_function <- function(){
  dsList <- FCEPlot.Aggr.data()
  if (is.null(dsList)) return(NULL)
  aggr_on <- 'funcId'
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(dsList) else get_dim(dsList)
  
  runtimes <- FCEPlot.Aggr.Targets_obj
  erts <- mean_FVs(dsList, aggr_on = aggr_on, runtimes = runtimes)
  rownames(erts) <- aggr_attr
  formatC(erts, digits = 4)
}

output$FCEPlot.Aggr.FCETable <- DT::renderDataTable({
  input$FCEPlot.Aggr.Refresh
  req(length(DATA_RAW()) > 0)
  
  withProgress({
    fce_multi_function()
  },
  message = "Creating table")
}, editable = FALSE, rownames = TRUE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))

output$FCEPlot.Aggr.DownloadTable <- downloadHandler(
  filename = function() {
    eval(FCE_multi_func_name)
  },
  content = function(file) {
    df <- fce_multi_function()
    if (input$FCEPlot.Aggr.TableFormat == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
  }
)
