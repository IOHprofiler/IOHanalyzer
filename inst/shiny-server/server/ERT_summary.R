# Data summary for Fixed-Target Runtime (ERT)  --------------
runtime_summary_condensed <- reactive({
  data <- DATA()
  req(length(data) > 0)
  fall <- get_funvals(data)
  df <- get_FV_overview(data, algorithm = input$RTSummary.Overview.Algid)
  df$budget %<>% as.integer
  df$runs %<>% as.integer
  df$funcId %<>% as.integer
  df$DIM %<>% as.integer
  df$succ %<>% as.integer
  df$"worst recorded" <- format_FV(df$"worst recorded") %>% as.numeric
  df$"worst reached" <- format_FV(df$"worst reached") %>% as.numeric
  df$"mean reached" <- format_FV(df$"mean reached") %>% as.numeric
  df$"median reached" <- format_FV(df$"median reached") %>% as.numeric
  df$"best reached" <- format_FV(df$"best reached") %>% as.numeric
  df
})

output$table_RT_overview <- DT::renderDataTable({
  req(input$RTSummary.Overview.Algid)
  runtime_summary_condensed()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$RTSummary.Overview.Download <- downloadHandler(
  filename = function() {
    eval(RT_overview_name)
  },
  content = function(file) {
    df <- runtime_summary_condensed()
    df <- df[input[["table_RT_overview_rows_all"]]]
    if (input$RTSummary.Overview.Format == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
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

  req(fstart <= fstop, fstep <= fstop - fstart, length(data) > 0)
  fall <- get_funvals(data)

  if (input$RTSummary.Statistics.Single)
    fstop <- fstart

  fseq <- seq_FV(fall, fstart, fstop, fstep)
  req(fseq)

  df <- get_RT_summary(data, fseq, algorithm = input$RTSummary.Statistics.Algid)
  df <- df[, c('DIM', 'funcId') := NULL]
  df$target <- format_FV(df$target) %>% as.numeric

  # format the integers
  probs <- getOption("IOHanalyzer.quantiles")
  for (p in paste0(probs * 100, '%')) {
    df[[p]] %<>% as.integer
  }
  df$mean <- round(df$mean, digits = 2)
  df$sd <- round(df$sd, digits = 2)
  df$ERT <- round(df$ERT, digits = 2)
  df$ps <- round(df$ps, digits = 2)
  if (format == COCO)
    df$target <- formatC(df$target, format = "e", digits = 2)


  df

})

output$table_RT_summary <- DT::renderDataTable({
  runtime_summary()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 10, scrollX = T, server = T))

output$RTSummary.Statistics.Download <- downloadHandler(
  filename = function() {
    eval(RT_csv_name)
  },
  content = function(file) {
    df <- runtime_summary()
    df <- df[input[["table_RT_summary_rows_all"]]]
    if (input$RTSummary.Statistics.Format == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
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

  req(fstart <= fstop, fstep <= fstop - fstart, length(data) > 0)
  fall <- get_funvals(data)

  if (input$RTSummary.Sample.Single)
    fstop <- fstart

  fseq <- seq_FV(fall, fstart, fstop, fstep)
  req(fseq)

  df <- get_RT_sample(data, ftarget = fseq, algorithm = input$RTSummary.Sample.Algid,
                output = input$RTSummary.Sample.DownloadFormat)
  if (format == COCO)
    df$target <- formatC(df$target, format = "e", digits = 2)
  df[is.na(df)] <- 'NA'
  df
})

output$RTSummary.Sample.Download <- downloadHandler(
  filename = function() {
    eval(RTSample_csv_name)
  },
  content = function(file) {
    df <- get_RT()
    df <- df[input[["table_RT_sample_rows_all"]]]
    if (input$RTSummary.Sample.Format == 'csv')
      write.csv(df, file, row.names = F)
    else
      print(xtable(df), file = file)
    }
)

output$table_RT_sample <- DT::renderDataTable({
  df <- get_RT()
  df[is.na(df)] <- 'NA'
  df
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 10, scrollX = T, server = T))
