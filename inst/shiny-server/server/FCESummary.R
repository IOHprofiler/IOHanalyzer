# TODO: rename 'FCE'...
# Data summary for Fixed-Budget target (FCE)  --------------
FCE_runtime_summary_condensed <- reactive({
  data <- DATA()
  data <- subset(data, ID %in% input$FCESummary.Overview.ID)
  # fall <- get_funvals(data)
  df <- get_RT_overview(data)
  df$"runs" %<>% as.integer
  df$"Budget" %<>% as.numeric
  df$"miminal runtime" %<>% as.numeric
  df$"maximal runtime" %<>% as.numeric

  df
})

output$table_FV_overview <- DT::renderDataTable({
  req(input$FCESummary.Overview.ID)
  FCE_runtime_summary_condensed()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$FCESummary.Overview.Download <- downloadHandler(
  filename = function() {
    eval(FV_overview_name)
  },
  content = function(file) {
    df <- FCE_runtime_summary_condensed()
    df <- df[input[["table_FV_overview_rows_all"]]]
    save_table(df, file)
  }
)

get_FCE_summary <- reactive({
  req(input$FCESummary.Statistics.Min, input$FCESummary.Statistics.Max, input$FCESummary.Statistics.Step, length(DATA()) > 0)

  rt_min <- input$FCESummary.Statistics.Min %>% as.numeric
  rt_max <- input$FCESummary.Statistics.Max %>% as.numeric
  rt_step <- input$FCESummary.Statistics.Step %>% as.numeric

  data <- DATA()
  data <- subset(data, ID %in% input$FCESummary.Statistics.ID)


  if (!input$FCESummary.Statistics.Single) {
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    rt <- get_runtimes(data)
    rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
    req(rt_seq)
  }
  else{
    rt_seq <- rt_min
  }

  df <- get_FV_summary(data, rt_seq)[
    , c('DIM', 'funcId') := NULL
    ]
  df$runs %<>% as.integer
  # df$runs.MaxFunvals %<>% as.integer
  df$median %<>% format(format = 'e', digits = 3)
  df$mean %<>% format(format = 'e', digits = 3)
  df$runtime %<>% as.numeric

  probs <- getOption("IOHanalyzer.quantiles")

  # format the integers
  for (p in paste0(probs * 100, '%')) {
    df[[p]] %<>% format(format = 'e', digits = 3)
  }
  df$sd <- round(df$sd, 2)
  df
})

output$FCE_SUMMARY <- DT::renderDataTable({
  get_FCE_summary()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$FCESummary.Statistics.Download <- downloadHandler(
  filename = function() {
    eval(FV_csv_name)
  },
  content = function(file) {
    df <- get_FCE_summary()
    df <- df[input[["FCE_SUMMARY_rows_all"]]]
    save_table(df, file)
  }
)

get_FCE <- reactive({
  req(input$FCESummary.Sample.Min, input$FCESummary.Sample.Max, input$FCESummary.Sample.Step, length(DATA()) > 0)
  rt_min <- input$FCESummary.Sample.Min %>% as.numeric
  rt_max <- input$FCESummary.Sample.Max %>% as.numeric
  rt_step <- input$FCESummary.Sample.Step %>% as.numeric

  data <- DATA()
  data <- subset(data, ID %in% input$FCESummary.Sample.ID)

  if (!input$FCESummary.Sample.Single) {
    req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
    rt <- get_runtimes(data)
    rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
    req(rt_seq)
  }
  else{
    rt_seq <- rt_min
  }

  get_FV_sample(data, rt_seq,
                output = input$FCESummary.Sample.Format)
})

output$FCESummary.Sample.Download <- downloadHandler(
  filename = function() {
    eval(FVSample_csv_name)
  },
  content = function(file) {
    df <- get_FCE()
    df <- df[input[["FCE_SAMPLE_rows_all"]]]
    save_table(df, file)
  }
)

output$FCE_SAMPLE <- DT::renderDataTable({
  df <- get_FCE()
  df[is.na(df)] <- 'NA'
  df
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))
