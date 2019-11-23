render_heatmap <- reactive({
  req(length(DATA()) > 0)
  withProgress({
    target <- as.numeric(input$RT_Stats.Overview.Target)
    data <- subset(DATA(), algId %in% input$RT_Stats.Overview.Algid)
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
  req(length(get_algId(DATA())) > 1)
  data <- subset(DATA(), algId %in% input$RT_Stats.Overview.Algid)
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
    data <- subset(DATA(), algId %in% input$RT_Stats.Overview.Algid)
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
    if (input$RT_Stats.Overview.TableFormat == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
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
      data <- subset(DATA_RAW(), algId %in% input$RT_Stats.Glicko.Algid && funcId %in% input$RT_Stats.Glicko.Funcid && DIM %in% input$RT_Stats.Glicko.Dim)
      req(length(data) > 0 && length(get_algId(data)) > 0)
      nr_games <- as.numeric(input$RT_Stats.Glicko.Nrgames)
      df <- glicko2_ranking(data, nr_games)$ratings
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
    data <- subset(DATA_RAW(), algId %in% input$RT_Stats.Glicko.Algid && funcId %in% input$RT_Stats.Glicko.Funcid && DIM %in% input$RT_Stats.Glicko.Dim)
    nr_games <- as.numeric(input$RT_Stats.Glicko.Nrgames)
  })
  Plot.Stats.Glicko2_Candlestick(data, nr_games, data_table_glicko2())
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
    if (input$RT_Stats.Glicko.TableFormat == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
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