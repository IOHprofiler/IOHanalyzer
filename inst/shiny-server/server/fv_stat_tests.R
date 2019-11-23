fv_render_heatmap <- reactive({
  req(length(DATA()) > 0)
  withProgress({
    target <- as.numeric(input$FV_Stats.Overview.Target)
    data <- subset(DATA(), algId %in% input$FV_Stats.Overview.Algid)
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
  req(length(get_algId(DATA())) > 1)
  data <- subset(DATA(), algId %in% input$FV_Stats.Overview.Algid)
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
    data <- subset(DATA(), algId %in% input$FV_Stats.Overview.Algid)
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
    if (input$FV_Stats.Overview.TableFormat == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
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
      data <- subset(DATA_RAW(), algId %in% input$FV_Stats.Glicko.Algid && funcId %in% input$FV_Stats.Glicko.Funcid && DIM %in% input$FV_Stats.Glicko.Dim)
      req(length(data) > 0 && length(get_algId(data)) > 0)
      nr_games <- as.numeric(input$FV_Stats.Glicko.Nrgames)
      df <- glicko2_ranking(data, nr_games, which = 'by_RT')$ratings
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
    data <- subset(DATA_RAW(), algId %in% input$FV_Stats.Glicko.Algid && funcId %in% input$FV_Stats.Glicko.Funcid && DIM %in% input$FV_Stats.Glicko.Dim)
    nr_games <- as.numeric(input$FV_Stats.Glicko.Nrgames)
  })
  Plot.Stats.Glicko2_Candlestick(data, nr_games, fv_data_table_glicko2(), which = 'by_RT')
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
    if (input$FV_Stats.Glicko.TableFormat == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
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