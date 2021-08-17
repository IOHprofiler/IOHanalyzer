fv_render_heatmap <- reactive({
  req(length(DATA()) > 0)
  withProgress({
    target <- as.numeric(input$FV_Stats.Overview.Target)
    data <- subset(DATA(), ID %in% input$FV_Stats.Overview.ID)
    Plot.Stats.Significance_Heatmap(data, target, alpha = as.numeric(input$FV_Stats.Overview.Alpha),
                                    bootstrap.size = 0, which = 'by_RT')
  },
  message = "Creating plot")
})


output$FV_Stats.Overview.Heatmap <- renderPlotly(
  fv_render_heatmap()
)

fv_create_stats_table <- reactive({
  req(length(DATA()) > 0)
  req(length(get_id(DATA())) > 1)
  data <- subset(DATA(), ID %in% input$FV_Stats.Overview.ID)
  target <- as.numeric(input$FV_Stats.Overview.Target)
  df <- pairwise.test(data, target, bootstrap.size = 0, which = 'by_RT')
  df <- format(df, digits = 3)
  df
})

output$FV_Stats.Overview.Pmatrix <- DT::renderDataTable({
  fv_create_stats_table()
}, options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$FV_Stats.Overview.Graph <- renderPlot({
  fv_render_graph()
})

fv_render_graph <- reactive({
  req(length(DATA()) > 0)
  withProgress({
    target <- as.numeric(input$FV_Stats.Overview.Target)
    data <- subset(DATA(), ID %in% input$FV_Stats.Overview.ID)
    Plot.Stats.Significance_Graph(data, target, alpha = as.numeric(input$FV_Stats.Overview.Alpha),
                                  bootstrap.size = 0, which = 'by_RT')
  },
  message = "Creating plot")
})


output$FV_Stats.Overview.DownloadTable <- downloadHandler(
  filename = function() {
    eval(FV_Stats_table_name)
  },
  content = function(file) {
    df <- create_stats_table()
    save_table(df, file)
  }
)

output$FV_Stats.Overview.DownloadHeatmap <- downloadHandler(
  filename = function() {
    eval(FV_Stats_heatmap_name)
  },
  content = function(file) {
    save_plotly(render_heatmap(), file)
  },
  contentType = paste0('image/', input$FV_Stats.Overview.Format)
)

fv_data_table_glicko2 <- reactive({
  input$FV_Stats.Glicko.Create
  isolate({
    withProgress({
      data <- FV_glicko_data()
      req(length(data) > 0 && length(get_id(data)) > 0)
      nr_games <- as.numeric(input$FV_Stats.Glicko.Nrgames)
      df <- glicko2_ranking(data, nr_games, which = 'by_RT', 
                            target_dt = FV_stats_glicko_targets_obj)$ratings
      format(df, digits = 3)
    }, message = "Creating Ranking, this might take a while")
  })
})

output$FV_Stats.Glicko.Dataframe <- DT::renderDataTable({
  
  req(length(DATA_RAW()) > 0)
  fv_data_table_glicko2()
}, options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

fv_render_glico2_plot <- reactive({
  isolate({
    data <- FV_glicko_data()
    nr_games <- as.numeric(input$FV_Stats.Glicko.Nrgames)
  })
  Plot.Stats.Glicko2_Candlestick(data, nr_games, fv_data_table_glicko2(), which = 'by_RT', 
                                 target_dt = FV_stats_glicko_targets_obj)
})

output$FV_Stats.Glicko.Candlestick <- renderPlotly({
  fv_render_glico2_plot()
})

output$FV_Stats.Glicko.DownloadTable <- downloadHandler(
  filename = function() {
    eval(FV_Glicko2_table_name)
  },
  content = function(file) {
    df <- fv_data_table_glicko2()
    save_table(df, file)
  }
)

output$FV_Stats.Glicko.Download <- downloadHandler(
  filename = function() {
    eval(FV_Glicko2_figure_name)
  },
  content = function(file) {
    save_plotly(fv_render_glico2_plot(), file)
  },
  contentType = paste0('image/', input$FV_Stats.Glicko.Format)
)


FV_glicko_data <- function() {
  data <- subset(DATA_RAW(), ID %in% isolate(input$FV_Stats.Glicko.ID))
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM %in% input$FV_Stats.Glicko.Dim)
  data <- subset(data, funcId %in% input$FV_Stats.Glicko.Funcid)
  if (length(unique(get_id(data))) < 2) {
    shinyjs::alert("This plot is only available when the dataset contains
                   multiple IDs for the selected functions and dimensions.")
    return(NULL)
  }
  data
}

FV_stats_glicko_targets <- reactive({
  data <- FV_glicko_data()
  if (is.null(data)) return(NULL)
  get_target_dt(data, "by_RT")
})

FV_stats_glicko_targets_obj <- NULL

proxy_FV_Stats.Glicko.Targets <- dataTableProxy('FV_Stats.Glicko.Targets')

output$FV_Stats.Glicko.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  FV_stats_glicko_targets_obj <<- FV_stats_glicko_targets()
  FV_stats_glicko_targets_obj
}, editable = list(target = 'cell', disable = list(columns = c(0,1))), rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


observeEvent(input$FV_Stats.Glicko.Targets_cell_edit, {
  info <- input$FV_Stats.Glicko.Targets_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  data <- FV_glicko_data()
  if (is.null(data)) return(NULL)
  FV_stats_glicko_targets_obj$target[[i]] <<- v
  replaceData(proxy, FV_stats_glicko_targets_obj, resetPaging = FALSE, rownames = FALSE)
})