# The expected runtime plot ---------------------
# TODO: wrapp this as a separate function for DataSet class
# TODO: add very brief explanation on each function here

output$ERT_PER_FUN <- renderPlotly({
  render_ert_per_fct()
})

get_data_ERT_PER_FUN <- reactive({
  req(input$ERTPlot.Min, input$ERTPlot.Max, length(DATA()) > 0)
  selected_algs <- input$ERTPlot.Algs
  data <- subset(DATA(), algId %in% input$ERTPlot.Algs)
  fstart <- input$ERTPlot.Min %>% as.numeric
  fstop <- input$ERTPlot.Max %>% as.numeric
  generate_data.Single_Function(data, fstart, fstop, input$ERTPlot.semilogx, 
                                'by_RT', include_opts = input$ERTPlot.inclueOpts)
})

render_ert_per_fct <- reactive({
  withProgress({
    y_attrs <- c()
    if (input$ERTPlot.show.ERT) y_attrs <- c(y_attrs, 'ERT')
    if (input$ERTPlot.show.mean) y_attrs <- c(y_attrs, 'mean')
    if (input$ERTPlot.show.median) y_attrs <- c(y_attrs, 'median')
    show_legend <- T
    if (length(y_attrs) > 0) {
      p <- plot_general_data(get_data_ERT_PER_FUN(), x_attr = 'target', y_attr = y_attrs, 
                             type = 'line', legend_attr = 'algId', show.legend = show_legend, 
                             scale.ylog = input$ERTPlot.semilogy,
                             scale.xlog = input$ERTPlot.semilogx, x_title = "Best-so-far f(x)-value",
                             y_title = "Function Evaluations",
                             scale.reverse = !attr(DATA(), 'maximization'))
      show_legend <- F
    }
    else
      p <- NULL
    if (input$ERTPlot.show.CI) {
      p <- plot_general_data(get_data_ERT_PER_FUN(), x_attr = 'target', y_attr = 'mean', 
                             type = 'ribbon', legend_attr = 'algId', lower_attr = 'lower', 
                             upper_attr = 'upper', p = p, show.legend = show_legend, 
                             scale.ylog = input$ERTPlot.semilogy,
                             scale.xlog = input$ERTPlot.semilogx, x_title = "Best-so-far f(x)-value",
                             y_title = "Function Evaluations",
                             scale.reverse = !attr(DATA(), 'maximization'))
      show_legend <- F
    }
    if (input$ERTPlot.show.runs) {
      fstart <- isolate(input$ERTPlot.Min %>% as.numeric)
      fstop <- isolate(input$ERTPlot.Max %>% as.numeric)
      data <- isolate(subset(DATA(), algId %in% input$ERTPlot.Algs))
      dt <- get_RT_sample(data, seq_FV(get_funvals(data), from = fstart, to = fstop,
                                       scale = ifelse(isolate(input$ERTPlot.semilogx), 'log', 'linear')))
      nr_runs <- ncol(dt) - 4
      for (i in seq_len(nr_runs)) {
        p <- plot_general_data(dt, x_attr = 'target', y_attr = paste0('run.', i), type = 'line',
                               legend_attr = 'algId', p = p, show.legend = show_legend, 
                               scale.ylog = input$ERTPlot.semilogy,
                               scale.xlog = input$ERTPlot.semilogx, x_title = "Best-so-far f(x)-value",
                               y_title = "Function Evaluations",
                               scale.reverse = !attr(DATA(), 'maximization'))
        show_legend <- F
      }
    }
    p
  },
  message = "Creating plot"
  )
})

output$ERTPlot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_ert_per_fct(), file)
  },
  contentType = paste0('image/', input$ERTPlot.Format)
)

update_ert_per_fct_axis <- observe({
  plotlyProxy("ERT_PER_FUN", session) %>%
    plotlyProxyInvoke(
      "restyle", 
      list(
        xaxis = list(title = 'best-so-far-f(x)-value', 
                     type = ifelse(input$ERTPlot.semilogx, 'log', 'linear')),
        yaxis = list(title = 'function evaluations', 
                     type = ifelse(input$ERTPlot.semilogy, 'log', 'linear'))
      )
    )
})

render_ert_per_fct <- reactive({
  withProgress({
    req(input$ERTPlot.Min, input$ERTPlot.Max, length(DATA()) > 0)
    selected_algs <- input$ERTPlot.Algs
    
    data <- subset(DATA(), algId %in% input$ERTPlot.Algs)
    req(length(data) > 0)
    
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
                       scale.reverse = !attr(data, 'maximization'))
  },
  message = "Creating plot"
  )
})

output$ERTPlot.Multi.Plot <- renderPlotly(
  render_ERTPlot_multi_plot()
)

get_data_ERT_multi_func_bulk <- reactive({
  data <- subset(DATA_RAW(),
                 DIM == input$Overall.Dim)
  if (length(get_algId(data)) < 20) { #Arbitrary limit for the time being
    rbindlist(lapply(get_funcId(data), function(fid) {
      generate_data.Single_Function(subset(data, funcId == fid), scale_log = input$ERTPlot.Multi.Logx, 
                                    which = 'by_RT')
    }))
  }
  else
    NULL
})

get_data_ERT_multi_func <- reactive({
  req(isolate(input$ERTPlot.Multi.Algs))
  input$ERTPlot.Multi.PlotButton
  if (length(get_algId(data)) < 20) {
    get_data_ERT_multi_func_bulk()[algId %in% isolate(input$ERTPlot.Multi.Algs), ]
  }
  else {
    selected_algs <- isolate(input$ERTPlot.Multi.Algs)
    data <- subset(DATA_RAW(),
                   algId %in% selected_algs,
                   DIM == input$Overall.Dim)
    rbindlist(lapply(get_funcId(data), function(fid) {
    generate_data.Single_Function(subset(data, funcId == fid), scale_log = input$ERTPlot.Multi.Logx, 
                                  which = 'by_RT')
    }))
  }
})

render_ERTPlot_multi_plot <- reactive({
  req(isolate(input$ERTPlot.Multi.Algs))
  withProgress({
  plot_general_data(get_data_ERT_multi_func(), x_attr = 'target', y_attr = 'ERT', 
                    subplot_attr = 'funcId', type = 'line', scale.xlog = input$ERTPlot.Multi.Logx, 
                    scale.ylog = input$ERTPlot.Multi.Logy, x_title = 'Best-so-far f(x)', 
                    y_title = 'ERT', show.legend = T,
                    scale.reverse = !attr(DATA(), 'maximization'))
  },
  message = "Creating plot")
})

output$ERTPlot.Multi.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_PER_FUN_MULTI)
  },
  content = function(file) {
    save_plotly(render_ERTPlot_multi_plot(), file)
  },
  contentType = paste0('image/', input$ERTPlot.Multi.Format)
)





output$ERTPlot.Aggr_Dim.Plot <- renderPlotly(
  render_ERTPlot_aggr_plot_dim()
)

output$ERTPlot.Aggr_Dim.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_AGGR_DIM)
  },
  content = function(file) {
    save_plotly(render_ERTPlot_aggr_plot_dim(), file)
  },
  contentType = paste0('image/', input$ERTPlot.Aggr_Dim.Format)
)

get_data_ERT_aggr_dim <- reactive({
  input$ERTPlot.Aggr_Dim.Refresh
  req(length(DATA_RAW()) > 0)
  data <- ERTPlot.Aggr_Dim.data()
  if (is.null(data)) return(NULL)
  aggr_on <- 'DIM'
  targets <- ERTPlot.Aggr_Dim.Targets_obj
  names(targets) <- NULL
  generate_data.Aggr(data, aggr_on, targets, inf_action = 'overlap')
})

render_ERTPlot_aggr_plot_dim <- reactive({
  data <- get_data_ERT_aggr_dim()
  req(data)
  withProgress({
    y_attr <- if (input$ERTPlot.Aggr_Dim.Ranking) 'rank' else 'value'
    y_title <- if (input$ERTPlot.Aggr_Dim.Ranking) 'Rank' else 'ERT'
    reverse_scale <- input$ERTPlot.Aggr_Dim.Mode == 'radar'
    
    plot_general_data(data, type = input$ERTPlot.Aggr_Dim.Mode, x_attr = 'DIM',
                      y_attr = y_attr, x_title = "DIM", y_title = y_title, show.legend = T,
                      scale.ylog = input$ERTPlot.Aggr_Dim.Logy, scale.xlog = T, 
                      scale.reverse = reverse_scale)
    # Plot.RT.Aggregated(data, plot_mode = input$ERTPlot.Aggr_Dim.Mode, targets = targets,
    #                    scale.ylog = input$ERTPlot.Aggr_Dim.Logy,
    #                    use_rank = input$ERTPlot.Aggr_Dim.Ranking,
    #                    aggr_on = aggr_on)
  },
  message = "Creating plot")
})

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

default_targets_table_dim <- reactive({
  data <- ERTPlot.Aggr_Dim.data()
  if (is.null(data)) return(NULL)
  aggr_on <- 'DIM'
  as.data.table(
    t(
      get_max_targets(data, aggr_on, maximize = attr(data,'maximization'))
    ),
    keep.rownames = F)
})

ERTPlot.Aggr_Dim.Targets_obj <- NULL

proxy_ERTPlot.Aggr_Dim.Targets <- dataTableProxy('ERTPlot.Aggr_Dim.Targets')

output$ERTPlot.Aggr_Dim.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  ERTPlot.Aggr_Dim.Targets_obj <<- default_targets_table_dim()
  ERTPlot.Aggr_Dim.Targets_obj
}, editable = TRUE, rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


observeEvent(input$ERTPlot.Aggr_Dim.Targets_cell_edit, {
  info <- input$ERTPlot.Aggr_Dim.Targets_cell_edit
  i <- info$row
  j <- info$col + 1
  v <- info$value
  data <- ERTPlot.Aggr_Dim.data()
  if (is.null(data)) return(NULL)
  aggr_on <- 'DIM'
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_dim(data)
  suppressWarnings(ERTPlot.Aggr_Dim.Targets_obj[i, paste0(aggr_attr[[j]])] <<- 
                     DT::coerceValue(v, ERTPlot.Aggr_Dim.Targets_obj[i, paste0(aggr_attr[[j]])]))
  replaceData(proxy, ERTPlot.Aggr_Dim.Targets_obj, resetPaging = FALSE, rownames = FALSE)
})


ert_multi_dim <- function(){
  dsList <- ERTPlot.Aggr_Dim.data()
  if (is.null(dsList)) return(NULL)
  aggr_on <- 'DIM'
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(dsList) else get_dim(dsList)
  
  targets <- ERTPlot.Aggr_Dim.Targets_obj
  erts <- max_ERTs(dsList, aggr_on = aggr_on, targets = targets, 
                   maximize = attr(dsList, "maximization"))
  rownames(erts) <- aggr_attr
  formatC(erts, digits = 4)
}

output$ERTPlot.Aggr_Dim.ERTTable <- DT::renderDataTable({
  input$ERTPlot.Aggr_Dim.Refresh
  req(length(DATA_RAW()) > 0)
  
  withProgress({
    ert_multi_dim()
  },
  message = "Creating table")
}, editable = FALSE, rownames = TRUE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))

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
