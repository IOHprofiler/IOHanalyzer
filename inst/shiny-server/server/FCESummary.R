# TODO: rename 'FCE'...
# Data summary for Fixed-Budget target (FCE)  --------------
FCE_runtime_summary_condensed <- reactive({
  data <- DATA()
  fall <- get_funvals(data)
  df <- get_RT_overview(data, algorithm = input$FCESummary.Overview.Algid)
  df$"runs" %<>% as.integer
  df$"Budget" %<>% as.numeric
  df$"miminal runtime" %<>% as.numeric
  df$"maximal runtime" %<>% as.numeric
  
  df
})

output$table_FV_overview <- DT::renderDataTable({
  req(input$FCESummary.Overview.Algid)
  FCE_runtime_summary_condensed()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$FCESummary.Overview.Download <- downloadHandler(
  filename = function() {
    eval(FV_overview_name)
  },
  content = function(file) {
    df <- FCE_runtime_summary_condensed()
    df <- df[input[["FCE_SAMPLE_rows_all"]]]
    if (input$FCESummary.Overview.Format == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
  }
)

get_FCE_summary <- reactive({
  req(input$FCESummary.Statistics.Min, input$FCESummary.Statistics.Max, input$FCESummary.Statistics.Step, length(DATA()) > 0)

  rt_min <- input$FCESummary.Statistics.Min %>% as.numeric
  rt_max <- input$FCESummary.Statistics.Max %>% as.numeric
  rt_step <- input$FCESummary.Statistics.Step %>% as.numeric

  req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
  data <- DATA()
  rt <- get_runtimes(data)

  if (input$FCESummary.Statistics.Single)
    rt_max <- rt_min

  rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
  req(rt_seq)

  df <- get_FV_summary(data, rt_seq, algorithm = input$FCESummary.Statistics.Algid)[
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
    df <- df[input[["FCE_SAMPLE_rows_all"]]]
    if (input$RTSummary.Overview.Format == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
  }
)

get_FCE <- reactive({
  req(input$FCESummary.Sample.Min, input$FCESummary.Sample.Max, input$FCESummary.Sample.Step, length(DATA()) > 0)
  rt_min <- input$FCESummary.Sample.Min %>% as.numeric
  rt_max <- input$FCESummary.Sample.Max %>% as.numeric
  rt_step <- input$FCESummary.Sample.Step %>% as.numeric

  req(rt_min <= rt_max, rt_step <= rt_max - rt_min)
  data <- DATA()
  rt <- get_runtimes(data)

  if (input$FCESummary.Sample.Single)
    rt_max <- rt_min

  rt_seq <- seq_RT(rt, rt_min, rt_max, by = rt_step)
  req(rt_seq)

  get_FV_sample(data, rt_seq, algorithm = input$FCESummary.Sample.Algid,
                output = input$FCESummary.Sample.Format)

  # res <- list()
  # n_runs_max <- sapply(data, function(x) length(attr(x, 'instance'))) %>% max
  #
  # for (i in seq_along(data)) {
  #   ds <- data[[i]]
  #   algId <- attr(ds, 'algId')
  #   if (input$FCESummary.Sample.Algid != 'all' && algId != input$FCESummary.Sample.Algid)
  #     next
  #
  #   rt <- get_FV_sample(ds, rt_seq, output = input$FCESummary.Sample.Format)
  #   if (input$FCESummary.Sample.Format == 'wide') {
  #     # impute the missing records
  #     n <- ncol(rt) - 2
  #     if (n < n_runs_max)
  #       rt %<>% cbind(., matrix(NA, nrow(.), n_runs_max - n))
  #   }
  #   res[[i]] <- rt
  # }
  # do.call(rbind, res)
})

output$FCESummary.Sample.Download <- downloadHandler(
  filename = function() {
    eval(FVSample_csv_name)
  },
  content = function(file) {
    df <- get_FCE()
    df <- df[input[["FCE_SAMPLE_rows_all"]]]
    if (input$FCESummary.Sample.FileFormat == 'csv')
      write.csv(df, file, row.names = F)
    else
      print(xtable(df), file = file)
  }
)

output$FCE_SAMPLE <- DT::renderDataTable({
  df <- get_FCE()
  df[is.na(df)] <- 'NA'
  df
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))
