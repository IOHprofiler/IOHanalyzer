# Data summary for Fixed-Target Runtime (ERT)  --------------
runtime_summary_condensed <- reactive({
  data <- DATA()
  req(data)
  fall <- get_funvals(data)
  get_FV_overview(data, algorithm = input$RTSummary.Overview.Algid)
})

output$table_RT_overview <- renderTable({
  req(input$RTSummary.Overview.Algid)
  df <- runtime_summary_condensed()

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

output$RTSummary.Statistics.Download <- downloadHandler(
  filename = {
    fstart <- format_FV(input$RTSummary.Statistics.Min)
    fstop <- format_FV(input$RTSummary.Statistics.Max)
    fstep <- format_FV(input$RTSummary.Statistics.Step)
    eval(RT_csv_name)
  },
  content = function(file) {
    write.csv(runtime_summary(), file, row.names = F)
  },
  contentType = "text/csv"
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
  filename = {
    fstart <- input$RTSummary.Sample.Min %>% format_FV
    fstop <- input$RTSummary.Sample.Max %>% format_FV
    fstep <- input$RTSummary.Sample.Step %>% format_FV
    eval(RTSample_csv_name)
  },
  content = function(file) {
    write.csv(get_RT(), file, row.names = F)
  },
  contentType = "text/csv"
)

output$table_RT_sample <- renderDataTable({
  df <- get_RT()
  df[is.na(df)] <- 'NA'
  df
}, options = list(pageLength = 20, scrollX = T))
