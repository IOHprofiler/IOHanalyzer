
get_data_RT_PAR_PER_FUN <- reactive({
  data <- subset(DATA(), algId %in% input$RT_PAR.Plot.Algs)
  generate_data.Parameters(data, scale_log = input$RT_PAR.Plot.Logx, which = 'by_FV')
})

# Expected Evolution of parameters in the algorithm
render_RT_PAR_PER_FUN <- reactive({
  req(input$RT_PAR.Plot.Min, input$RT_PAR.Plot.Max)
  withProgress({
    f_min <- as.numeric(format_FV(input$RT_PAR.Plot.Min))
    f_max <- as.numeric(format_FV(input$RT_PAR.Plot.Max)) 
    
    dt <- get_data_RT_PAR_PER_FUN()
    dt <- dt[parId %in% input$RT_PAR.Plot.Params]
    sub_attr <- if (length(input$RT_PAR.Plot.Params) > 1) 'parId' else NULL
    
    lower <- 'lower'
    upper <- 'upper'
    if (input$RT_PAR.Plot.CI == 'None') type <- 'line' 
    else {
      type <- 'line+ribbon'
      if (input$RT_PAR.Plot.CI == 'Outer Quantiles') {
        quantiles <- paste0(getOption("IOHanalyzer.quantiles", c(0.2, 0.98)) * 100, '%')
        lower <- quantiles[[1]]
        upper <- quantiles[[length(quantiles)]]
      }      
    }
    if (is.null(sub_attr) && type == 'line+ribbon') {
      p <- plot_general_data(dt, 'target', input$RT_PAR.Plot.show.mean, 'line',
                             subplot_attr = sub_attr, scale.xlog = input$RT_PAR.Plot.Logx,
                             scale.ylog = input$RT_PAR.Plot.Logy, 
                             lower_attr = lower, upper_attr = upper, show.legend = T)
      p <- plot_general_data(dt, 'target', input$RT_PAR.Plot.show.mean, 'ribbon',
                        subplot_attr = sub_attr, scale.xlog = input$RT_PAR.Plot.Logx,
                        scale.ylog = input$RT_PAR.Plot.Logy, 
                        lower_attr = lower, upper_attr = upper, show.legend = F, p = p)
    }
    else {
      p <- plot_general_data(dt, 'target', input$RT_PAR.Plot.show.mean, type,
                      subplot_attr = sub_attr, scale.xlog = input$RT_PAR.Plot.Logx,
                      scale.ylog = input$RT_PAR.Plot.Logy, 
                      lower_attr = lower, upper_attr = upper, show.legend = T)
    }
    p
  },
  message = "Creating plot")
})

output$RT_PAR.Plot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_RT_PAR_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_RT_PAR_PER_FUN(), file)
  },
  contentType = paste0('image/', input$RT_PAR.Plot.Format)
)

output$RT_PAR.Plot.Figure <- renderPlotly({
  render_RT_PAR_PER_FUN()
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

rt_parameter_summary <- reactive({
  req(input$RT_PAR.Summary.Min, input$RT_PAR.Summary.Max, input$RT_PAR.Summary.Step)
  
  fstart <- format_FV(input$RT_PAR.Summary.Min) %>% as.numeric
  fstop <- format_FV(input$RT_PAR.Summary.Max) %>% as.numeric
  fstep <- format_FV(input$RT_PAR.Summary.Step) %>% as.numeric
  data <- DATA()
  
  if (!input$RT_PAR.Summary.Single) {
    req(fstart <= fstop, fstep <= fstop - fstart)
    fall <- get_funvals(data)
    fseq <- seq_FV(fall, fstart, fstop, by = fstep)
    req(fseq)
  }
  else 
    fseq <- fstart
  
  dt <- get_PAR_summary(data, fseq, input$RT_PAR.Summary.Algid, input$RT_PAR.Summary.Param)
  req(length(dt) != 0)
  
  dt$runs %<>% as.integer
  dt$mean %<>% format(digits = 2, nsmall = 2)
  dt$median %<>% format(digits = 2, nsmall = 2)
  dt$sd %<>% format(digits = 2, nsmall = 2)
  
  probs <- getOption("IOHanalyzer.quantiles")
  
  # format the integers
  for (p in paste0(probs * 100, '%')) {
    dt[[p]] %<>% format(digits = 2, nsmall = 2)
  }
  dt
})

rt_parameter_sample <- reactive({
  req(input$RT_PAR.Sample.Algid, input$RT_PAR.Sample.Max,
      input$RT_PAR.Sample.Step, input$RT_PAR.Sample.Min,
      input$RT_PAR.Sample.Param)
  
  fstart <- format_FV(input$RT_PAR.Sample.Min) %>% as.numeric
  fstop <- format_FV(input$RT_PAR.Sample.Max) %>% as.numeric
  fstep <- format_FV(input$RT_PAR.Sample.Step) %>% as.numeric
  data <- DATA()
  
  if (!input$RT_PAR.Sample.Single) {
    req(fstart <= fstop, fstep <= fstop - fstart)
    fall <- get_funvals(data)
    fseq <- seq_FV(fall, fstart, fstop, by = fstep)
    req(fseq)
  }
  else
    fseq <- fstart
  
  df <- get_PAR_sample(data, idxValue = fseq, 
                       algorithm = input$RT_PAR.Sample.Algid,
                       parId = input$RT_PAR.Sample.Param,
                       output = input$RT_PAR.Sample.Format)
  
  for (p in paste0('run.', seq(ncol(data[[1]]$FV))))
    df[[p]] %<>% format(digits = 2, nsmall = 2)
  
  df
})

output$table_RT_PAR_SAMPLE <- DT::renderDataTable({
  dt <- rt_parameter_sample()
  req(length(dt) != 0)
  dt[is.na(dt)] <- 'NA'
  dt
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$table_RT_PAR_summary <- DT::renderDataTable({
  rt_parameter_summary()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T, digits = 2))

output$RT_PAR.Sample.Download <- downloadHandler(
  filename = function() {
    eval(PARSample_csv_name)
  },
  content = function(file) {
    df <- parameter_sample()
    df <- df[input[["table_RT_PAR_SAMPLE_rows_all"]]]
    if (input$RT_PAR.Sample.FileFormat == 'csv')
      write.csv(df, file, row.names = F)
    else
      print(xtable(df), file = file)
  },
  contentType = "text/csv"
)

output$RT_PAR.Summary.Download <- downloadHandler(
  filename = function() {
    eval(RT_PAR_csv_name)
  },
  content = function(file) {
    df <- rt_parameter_summary()
    df <- df[input[["table_RT_PAR_summary_rows_all"]]]
    if (input$RT_PAR.Summary.Format == 'csv')
      write.csv(df, file, row.names = F)
    else
      print(xtable(df), file = file)
  },
  contentType = "text/csv"
)