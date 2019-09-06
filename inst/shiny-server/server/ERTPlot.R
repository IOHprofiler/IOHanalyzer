# The expected runtime plot ---------------------
# TODO: wrapp this as a separate function for DataSet class
output$ERT_PER_FUN <- renderPlotly({
  render_ert_per_fct()
})

output$ERTPlot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_ert_per_fct(), file,
                format = input$ERTPlot.Format)
  },
  contentType = paste0('image/', input$ERTPlot.Format)
)

update_ert_per_fct_axis <- observe({
  plotlyProxy("ERT_PER_FUN", session) %>%
    plotlyProxyInvoke("relayout", list(yaxis = list(title = 'function evaluations', type = ifelse(input$ERTPlot.semilogy, 'log', 'linear'))))
})


render_ert_per_fct <- reactive({
  withProgress({
  req(input$ERTPlot.Min, input$ERTPlot.Max, length(DATA()) > 0)
  selected_algs <- input$ERTPlot.Algs
  data <- subset(DATA(), algId %in% input$ERTPlot.Algs)
  fstart <- input$ERTPlot.Min %>% as.numeric
  fstop <- input$ERTPlot.Max %>% as.numeric
  Plot.RT.Single_Func(data, Fstart = fstart, Fstop = fstop,
                     show.CI = input$ERTPlot.show.CI,
                     show.ERT = input$ERTPlot.show.ERT,
                     show.mean = input$ERTPlot.show.mean,
                     show.median = input$ERTPlot.show.median,
                     scale.xlog = input$ERTPlot.semilogx,
                     scale.ylog = isolate(input$ERTPlot.semilogy),
                     includeOpts = input$ERTPlot.inclueOpts,
                     scale.reverse = !attr(data[[1]],'maximization'))
  },
  message = "Creating plot"
  )
})

output$ERTPlot.Multi.Plot <- renderPlotly(
  render_ERTPlot_multi_plot()
)

render_ERTPlot_multi_plot <- reactive({
  req(isolate(input$ERTPlot.Multi.Algs))
  input$ERTPlot.Multi.PlotButton
  withProgress({
  data <- subset(DATA_RAW(),
                 algId %in% isolate(input$ERTPlot.Multi.Algs),
                 DIM == input$Overall.Dim)
  req(length(data) > 0)
  if (length(unique(get_funcId(data))) == 1){
    shinyjs::alert("This plot is only available when the dataset contains multiple functions  for the selected dimension.")
    return(NULL)
  }
  Plot.RT.Multi_Func(data,
                   scale.xlog = input$ERTPlot.Multi.Logx,
                   scale.ylog = input$ERTPlot.Multi.Logy,
                   scale.reverse = !attr(data[[1]],'maximization'))
  },
  message = "Creating plot")
})

output$ERTPlot.Multi.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_PER_FUN_MULTI)
  },
  content = function(file) {
    save_plotly(render_ERTPlot_multi_plot(), file,
                format = input$ERTPlot.Multi.Format)
  },
  contentType = paste0('image/', input$ERTPlot.Multi.Format)
)

output$ERTPlot.Aggr.Plot <- renderPlotly(
  render_ERTPlot_aggr_plot()
)

get_max_targets <- function(data, aggr_on, maximize){
  targets <- c()
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)

  for (j in seq_along(aggr_attr)) {
    dsList_filetered <- if (aggr_on == 'funcId') subset(data,funcId == aggr_attr[[j]])
    else subset(data, DIM == aggr_attr[[j]])

    Fall <- get_funvals(dsList_filetered)
    Fval <- ifelse(maximize, max(Fall), min(Fall))
    targets <- c(targets,Fval)
  }
  names(targets) <- aggr_attr
  targets
}

# render_ERTPlot_aggr_plot <- reactive({
#   withProgress({
#   #TODO: figure out how to avoid plotting again when default targets are written to input
#   req(length(DATA_RAW()) > 0)
#   data <- subset(DATA_RAW(), algId %in% input$ERTPlot.Aggr.Algs)
#   if (length(data) == 0) return(NULL)
#   data <- subset(data, DIM == input$Overall.Dim)
#   if (length(unique(get_funcId(data))) == 1){
#     shinyjs::alert("This plot is only available when the dataset contains multiple functions for the selected dimension.")
#     return(NULL)
#   }
#   if (length(unique(get_algId(data))) == 1){
#     shinyjs::alert("This plot is only available when the dataset contains multiple algorithms for the selected dimension.")
#     return(NULL)
#   }
#   erts <- MAX_ERTS_FUNC()
#   aggr_on <- 'funcId'
#   aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_DIM(data)
#   targets <- get_max_targets(data, aggr_on, maximize = !(format == COCO || format == BIBOJ_COCO))
#   erts <- max_ERTs(data, aggr_on, targets, maximize = !(format == COCO || format == BIBOJ_COCO))
# 
#   Plot.RT.Aggregated(data, plot_mode = input$ERTPlot.Aggr.Mode, targets = targets,
#                 scale.ylog = input$ERTPlot.Aggr.Logy,
#                 maximize = attr(data[[1]],'maximization'),
#                 use_rank = input$ERTPlot.Aggr.Ranking,
#                 aggr_on = aggr_on, erts = erts)
#   },
#   message = "Creating plot")
# })

output$ERTPlot.Aggr.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_AGGR)
  },
  content = function(file) {
    save_plotly(render_ERTPlot_aggr_plot(), file,
                format = input$ERTPlot.Aggr.Format)
  },
  contentType = paste0('image/', input$ERTPlot.Aggr.Format)
)


# get_max_runtimes <- function(data, aggr_on){
#   runtimes <- c()
#   aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)
#   
#   for (j in seq_along(aggr_attr)) {
#     dsList_filetered <- if (aggr_on == 'funcId') subset(data,funcId==aggr_attr[[j]])
#     else subset(data, DIM==aggr_attr[[j]])
#     
#     RTall <- get_runtimes(dsList_filetered)
#     RTval <- max(RTall)
#     runtimes <- c(runtimes,RTval)
#   }
#   names(runtimes) <- aggr_attr
#   runtimes
# }

render_ERTPlot_aggr_plot <- reactive({
  input$ERTPlot.Aggr.Refresh
  withProgress({
    #TODO: figure out how to avoid plotting again when default targets are written to input
    req(length(DATA_RAW()) > 0)
    data <- ERTPlot.Aggr.data()
    if (is.null(data)) return(NULL)
    aggr_on <- 'funcId'
    aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)
    
    targets <- ERTPlot.Aggr.Targets_obj
    names(targets) <- NULL
    Plot.RT.Aggregated(data, plot_mode = input$ERTPlot.Aggr.Mode, targets = targets,
                       scale.ylog = input$ERTPlot.Aggr.Logy,
                       use_rank = input$ERTPlot.Aggr.Ranking,
                       aggr_on = aggr_on)
  },
  message = "Creating plot")
})

ERTPlot.Aggr.data <- function() {
  data <- subset(DATA_RAW(), algId %in% isolate(input$ERTPlot.Aggr.Algs))
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM == input$Overall.Dim)
  if (length(unique(get_funcId(data))) == 1){
    shinyjs::alert("This plot is only available when the dataset contains multiple functions for the selected dimension.")
    return(NULL)
  }
  if (length(unique(get_algId(data))) == 1){
    shinyjs::alert("This plot is only available when the dataset contains multiple algorithms for the selected dimension.")
    return(NULL)
  }
  data
}

default_targets_table <- reactive({
  data <- ERTPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  aggr_on <- 'funcId'
  as.data.table(
    t(
      get_max_targets(data, aggr_on, maximize = attr(data[[1]],'maximization'))
    ),
  keep.rownames = F)
})

ERTPlot.Aggr.Targets_obj <- NULL

proxy_ERTPlot.Aggr.Targets <- dataTableProxy('ERTPlot.Aggr.Targets')

output$ERTPlot.Aggr.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  ERTPlot.Aggr.Targets_obj <<- default_targets_table()
  ERTPlot.Aggr.Targets_obj
}, editable = TRUE, rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


observeEvent(input$ERTPlot.Aggr.Targets_cell_edit, {
  info <- input$ERTPlot.Aggr.Targets_cell_edit
  i <- info$row
  j <- info$col + 1
  v <- info$value
  data <- ERTPlot.Aggr.data()
  if (is.null(data)) return(NULL)
  aggr_on <- 'funcId'
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)
  suppressWarnings(ERTPlot.Aggr.Targets_obj[i, paste0(aggr_attr[[j]])] <<- DT::coerceValue(v, ERTPlot.Aggr.Targets_obj[i, paste0(aggr_attr[[j]])]))
  replaceData(proxy, ERTPlot.Aggr.Targets_obj, resetPaging = FALSE, rownames = FALSE)
})

