render_heatmap <- reactive({
  req(length(DATA()) > 0)
  withProgress({
    target <- as.numeric(input$RT_Stats.Overview.Target)
    data <- subset(DATA(), ID %in% input$RT_Stats.Overview.ID)
    Plot.Stats.Significance_Heatmap(data, target, alpha = as.numeric(input$RT_Stats.Overview.Alpha),
                                    bootstrap.size = input$RT_Stats.Overview.Samples)
  },
  message = "Creating plot")
})


output$RT_Stats.Overview.Heatmap <- renderPlotly(
  render_heatmap()
)

create_stats_table <- reactive({
  req(length(DATA()) > 0)
  req(length(get_id(DATA())) > 1)
  data <- subset(DATA(), ID %in% input$RT_Stats.Overview.ID)
  target <- as.numeric(input$RT_Stats.Overview.Target)
  df <- pairwise.test(data, target, bootstrap.size = input$RT_Stats.Overview.Samples)
  df <- format(df, digits = 3)
  df
})

output$RT_Stats.Overview.Pmatrix <- DT::renderDataTable({
  create_stats_table()
}, options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$RT_Stats.Overview.Graph <- renderPlot({
  render_graph()
})

render_graph <- reactive({
  req(length(DATA()) > 0)
  withProgress({
    target <- as.numeric(input$RT_Stats.Overview.Target)
    data <- subset(DATA(), ID %in% input$RT_Stats.Overview.ID)
    Plot.Stats.Significance_Graph(data, target, alpha = as.numeric(input$RT_Stats.Overview.Alpha),
                                    bootstrap.size = input$RT_Stats.Overview.Samples)
  },
  message = "Creating plot")
})


output$RT_Stats.Overview.DownloadTable <- downloadHandler(
  filename = function() {
    eval(RT_Stats_table_name)
  },
  content = function(file) {
    df <- create_stats_table()
    save_table(df, file)
  }
)

output$RT_Stats.Overview.DownloadHeatmap <- downloadHandler(
  filename = function() {
    eval(RT_Stats_heatmap_name)
  },
  content = function(file) {
    save_plotly(render_heatmap(), file)
  },
  contentType = paste0('image/', input$RT_Stats.Overview.Format)
)

data_table_glicko2 <- reactive({
  input$RT_Stats.Glicko.Create
  isolate({
    withProgress({
      data <- RT_glicko_data()
      nr_games <- as.numeric(input$RT_Stats.Glicko.Nrgames)
      df <- glicko2_ranking(data, nr_games, target_dt = RT_stats_glicko_targets_obj, which = 'by_FV')$ratings
      format(df, digits = 3)
    }, message = "Creating Ranking, this might take a while")
  })
})

output$RT_Stats.Glicko.Dataframe <- DT::renderDataTable({
  
  req(length(DATA_RAW()) > 0)
  data_table_glicko2()
}, options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

render_glico2_plot <- reactive({
  isolate({
    data <- RT_glicko_data()
    nr_games <- as.numeric(input$RT_Stats.Glicko.Nrgames)
  })
  Plot.Stats.Glicko2_Candlestick(data, nr_games, data_table_glicko2(), which = 'by_FV', 
                                 target_dt = RT_stats_glicko_targets_obj)
})

output$RT_Stats.Glicko.Candlestick <- renderPlotly({
  render_glico2_plot()
})

output$RT_Stats.Glicko.DownloadTable <- downloadHandler(
  filename = function() {
    eval(RT_Glicko2_table_name)
  },
  content = function(file) {
    df <- data_table_glicko2()
    save_table(df, file)
  }
)

output$RT_Stats.Glicko.Download <- downloadHandler(
  filename = function() {
    eval(RT_Glicko2_figure_name)
  },
  content = function(file) {
    save_plotly(render_glico2_plot(), file)
  },
  contentType = paste0('image/', input$RT_Stats.Glicko.Format)
)


RT_glicko_data <- function() {
  data <- subset(DATA_RAW(), ID %in% isolate(input$RT_Stats.Glicko.ID))
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM %in% input$RT_Stats.Glicko.Dim)
  data <- subset(data, funcId %in% input$RT_Stats.Glicko.Funcid)
  if (length(unique(get_id(data))) < 2) {
    shinyjs::alert("This plot is only available when the dataset contains
                   multiple IDs for the selected functions and dimensions.")
    return(NULL)
  }
  data
}

RT_stats_glicko_targets <- reactive({
  data <- RT_glicko_data()
  if (is.null(data)) return(NULL)
  get_target_dt(data, "by_FV")
})

RT_stats_glicko_targets_obj <- NULL

proxy_RT_Stats.Glicko.Targets <- dataTableProxy('RT_Stats.Glicko.Targets')

output$RT_Stats.Glicko.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  RT_stats_glicko_targets_obj <<- RT_stats_glicko_targets()
  RT_stats_glicko_targets_obj
}, editable = list(target = 'cell', disable = list(columns = c(0,1))), rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


observeEvent(input$RT_Stats.Glicko.Targets_cell_edit, {
  info <- input$RT_Stats.Glicko.Targets_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  data <- RT_glicko_data()
  if (is.null(data)) return(NULL)
  RT_stats_glicko_targets_obj$target[[i]] <<- v
  replaceData(proxy, RT_stats_glicko_targets_obj, resetPaging = FALSE, rownames = FALSE)
})
