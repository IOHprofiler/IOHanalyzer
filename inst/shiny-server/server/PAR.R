# Expected Evolution of parameters in the algorithm
render_PAR_PER_FUN <- reactive({
  req(input$PAR.Plot.Min, input$PAR.Plot.Max)
  withProgress({
  f_min <- format_FV(input$PAR.Plot.Min) %>% as.numeric
  f_max <- format_FV(input$PAR.Plot.Max) %>% as.numeric
  tryCatch({
    data <- subset(DATA(), algId %in% input$PAR.Plot.Algs)
    Plot.Parameters(data,f_min,f_max,
                  show.mean = (input$PAR.Plot.show.mean == 'mean'),
                  show.median = (input$PAR.Plot.show.mean == 'median'),
                  scale.xlog = input$PAR.Plot.Logx,
                  scale.ylog = input$PAR.Plot.Logy)
    },
    error = function(e) {
      #TODO: more robust error handling; don't assume this causes the error
      shinyjs::alert("Not all algorithms contain the same parameters. Please select a single algorithm to plot instead.")
    }
  )
  },
  message = "Creating plot")
})

output$PAR.Plot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_PAR_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_PAR_PER_FUN(), file,
                format = input$PAR.Plot.Format)
  },
  contentType = paste0('image/', input$PAR.Plot.Format)
)

output$PAR_PER_FUN <- renderPlotly({
  render_PAR_PER_FUN()
})

# TODO: add ks test for ECDF later
# output$ks <- renderPrint({
#   target <- input$target %>% as.numeric
#   df.aligneds <- aligned()
#
#   running_time <- list()
#   for (i in seq_along(df.aligneds)) {
#     df <- df.aligneds[[i]]
#     v <- rownames(df) %>% as.numeric
#     idx <- order(abs(target - v))[1]
#     running_time[[i]] <- df[idx, ] %>% as.vector
#   }
#   algorithm1 <- running_time[[1]]
#   algorithm2 <- running_time[[2]]
#   a <- ks.test(algorithm1, algorithm2, alternative = 'less')
#   print(a)
# })
#

parameter_summary <- reactive({
  req(input$PAR.Summary.Min, input$PAR.Summary.Max, input$PAR.Summary.Step)

  fstart <- format_FV(input$PAR.Summary.Min) %>% as.numeric
  fstop <- format_FV(input$PAR.Summary.Max) %>% as.numeric
  fstep <- format_FV(input$PAR.Summary.Step) %>% as.numeric

  req(fstart <= fstop, fstep <= fstop - fstart)
  data <- DATA()
  fall <- get_funvals(data)

  if (input$PAR.Summary.Single)
    fstop <- fstart

  fseq <- seq_FV(fall, fstart, fstop, by = fstep)
  req(fseq)

  dt <- get_PAR_summary(data, fseq, input$PAR.Summary.Algid, input$PAR.Summary.Param)
  req(length(dt) != 0)
  dt$runs %<>% as.integer
  dt$mean %<>% format(digits = 2, nsmall = 2)
  dt$median %<>% format(digits = 2, nsmall = 2)
  
  # probs <- get_property("probs")
  
  # format the integers
  # for (p in paste0(probs * 100, '%')) {
  #   df[[p]] %<>% format(digits = 2, nsmall = 2)
  # }
  dt
})

parameter_sample <- reactive({
  req(input$PAR.Sample.Algid, input$PAR.Sample.Max,
      input$PAR.Sample.Step, input$PAR.Sample.Min,
      input$PAR.Sample.Param)

  fstart <- format_FV(input$PAR.Sample.Min) %>% as.numeric
  fstop <- format_FV(input$PAR.Sample.Max) %>% as.numeric
  fstep <- format_FV(input$PAR.Sample.Step) %>% as.numeric

  req(fstart <= fstop, fstep <= fstop - fstart)
  data <- DATA()
  fall <- get_funvals(data)

  if (input$PAR.Sample.Single)
    fstop <- fstart

  fseq <- seq_FV(fall, fstart, fstop, by = fstep)
  req(fseq)

  get_PAR_sample(data, ftarget = fseq,
                 algorithm = input$PAR.Sample.Algid,
                 parId = input$PAR.Sample.Param,
                 output = input$PAR.Sample.Format)
})

output$table_PAR_SAMPLE <- renderDataTable({
  dt <- parameter_sample()
  req(length(dt) != 0)
  dt[is.na(dt)] <- 'NA'
  dt}, options = list(pageLength = 20, scrollX = T))

output$table_PAR_summary <- renderDataTable({
  parameter_summary()
}, options = list(pageLength = 20, scrollX = T))

output$PAR.Sample.Download <- downloadHandler(
  filename = function() {
    eval(PARSample_csv_name)
  },
  content = function(file) {
    if (input$PAR.Sample.FileFormat == 'csv')
      write.csv(parameter_sample(), file, row.names = F)
    else
      print(xtable(parameter_sample()), file = file)
  },
  contentType = "text/csv"
)

output$PAR.Summary.Download <- downloadHandler(
  filename = function() {
    eval(PAR_csv_name)
  },
  content = function(file) {
    if (input$PAR.Summary.Format == 'csv')
      write.csv(parameter_summary(), file, row.names = F)
    else
      print(xtable(parameter_summary()), file = file)
  },
  contentType = "text/csv"
)
