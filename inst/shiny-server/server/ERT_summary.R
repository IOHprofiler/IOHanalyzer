# Data summary for Fixed-Target Runtime (ERT)  --------------
runtime_summary_condensed <- reactive({
  data <- DATA()
  req(data)
  fall <- get_funvals(data)
  df <- get_FV_overview(data, algorithm = input$RTSummary.Overview.Algid)
  df$budget %<>% as.integer
  df$runs %<>% as.integer
  df$funcId %<>% as.integer
  df$DIM %<>% as.integer
  df$succ %<>% as.integer
  df$"worst recorded" <- format_FV(df$"worst recorded")
  df$"worst reached" <- format_FV(df$"worst reached")
  df$"mean reached" <- format_FV(df$"mean reached")
  df$"median reached" <- format_FV(df$"median reached")
  df$"best reached" <- format_FV(df$"best reached")
  df
})

output$table_RT_overview <- renderDataTable({
  req(input$RTSummary.Overview.Algid)
  runtime_summary_condensed()
}, options = list(pageLength = 20, scrollX = T))


output$RTSummary.Overview.Download <- downloadHandler(
  filename = function() {
    eval(RT_overview_name)
  },
  content = function(file) {
    if (input$RTSummary.Overview.Format == 'csv')
      write.csv(runtime_summary_condensed(), file, row.names = F)
    else{
      print(xtable(runtime_summary_condensed()), file = file)
    }
  }
)

  # Data summary for Fixed-Target Runtime (ERT)  --------------
  runtime_summary <- reactive({
    req(input$RTSummary.Statistics.Min,
        input$RTSummary.Statistics.Max,
        input$RTSummary.Statistics.Step)

    fstart <- format_FV(input$RTSummary.Statistics.Min) %>% as.numeric
    fstop <- format_FV(input$RTSummary.Statistics.Max) %>% as.numeric
    fstep <- format_FV(input$RTSummary.Statistics.Step) %>% as.numeric
    data <- DATA()

    req(fstart <= fstop, fstep <= fstop - fstart, data)
    fall <- get_funvals(data)

    if (input$RTSummary.Statistics.Single)
      fstop <- fstart

    fseq <- seq_FV(fall, fstart, fstop, fstep)
    req(fseq)

    df <- get_RT_summary(data, fseq, algorithm = input$RTSummary.Statistics.Algid)
    df <- df[, c('DIM', 'funcId') := NULL]
    df$target <- format_FV(df$target)

    # format the integers
    probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.
    for (p in paste0(probs * 100, '%')) {
      df[[p]] %<>% as.integer
    }
    df

  })

  output$table_RT_summary <- renderDataTable({
    runtime_summary()
    
  }, options = list(pageLength = 20, scrollX = T))

output$RTSummary.Statistics.Download <- downloadHandler(
  filename = function() {
    eval(RT_csv_name)
  },
  content = function(file) {
    if (input$RTSummary.Statistics.Format == 'csv')
      write.csv(runtime_summary(), file, row.names = F)
    else{
      print(xtable(runtime_summary()), file = file)
    }
  }
)

get_RT <- reactive({
  req(input$RTSummary.Sample.Min,
      input$RTSummary.Sample.Max,
      input$RTSummary.Sample.Step)

  fstart <- format_FV(input$RTSummary.Sample.Min) %>% as.numeric
  fstop <- format_FV(input$RTSummary.Sample.Max) %>% as.numeric
  fstep <- format_FV(input$RTSummary.Sample.Step) %>% as.numeric
  data <- DATA()

  req(fstart <= fstop, fstep <= fstop - fstart, data)
  fall <- get_funvals(data)

  if (input$RTSummary.Sample.Single)
    fstop <- fstart

  fseq <- seq_FV(fall, fstart, fstop, fstep)
  req(fseq)

  get_RT_sample(data, ftarget = fseq, algorithm = input$RTSummary.Sample.Algid,
                output = input$RTSummary.Sample.DownloadFormat)
})

output$RTSummary.Sample.Download <- downloadHandler(
  filename = function() {
    eval(RTSample_csv_name)
  },
  content = function(file) {
    if (input$RTSummary.Sample.Format == 'csv')
      write.csv(get_RT(), file, row.names = F)
    else
      print(xtable(get_RT()), file = file)
    }
)

output$table_RT_sample <- renderDataTable({
  df <- get_RT()
  df[is.na(df)] <- 'NA'
  df
}, options = list(pageLength = 20, scrollX = T))
