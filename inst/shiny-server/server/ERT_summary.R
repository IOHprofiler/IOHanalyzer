# Data summary for Fixed-Target Runtime (ERT)  --------------
runtime_summary_condensed <- reactive({
  data <- DATA()
  req(data)
  fall <- get_funvals(data)
  df <- get_FV_overview(data, algorithm = input$RTSummary.Overview.Algid)
  df$"Budget" %<>% as.integer
  df$"runs" %<>% as.integer
  df$"runs reached" %<>% as.integer
  df$"Worst recorded f(x)" <- format_FV(df$"Worst recorded f(x)")
  df$"Worst reached f(x)" <- format_FV(df$"Worst reached f(x)")
  df$"mean reached f(x)" <- format_FV(df$"mean reached f(x)")
  df$"median reached f(x)" <- format_FV(df$"median reached f(x)")
  df$"Best reached f(x)" <- format_FV(df$"Best reached f(x)")
  df
})

output$table_RT_overview <- renderTable({
  req(input$RTSummary.Overview.Algid)
  runtime_summary_condensed()
})


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
