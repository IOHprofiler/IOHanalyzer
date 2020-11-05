# Expected Evolution of parameters in the algorithm
get_data_FV_PAR_PER_FUN <- reactive({
  data <- subset(DATA(), algId %in% input$FV_PAR.Plot.Algs)
  generate_data.Parameters(data, scale_log = input$FV_PAR.Plot.Logx, which = 'by_RT')
})

render_FV_PAR_PER_FUN <- reactive({
  req(input$FV_PAR.Plot.Min, input$FV_PAR.Plot.Max)
  withProgress({
    #TODO: use these parameters
    rt_min <- as.numeric(input$FV_PAR.Plot.Min)
    rt_max <- as.numeric(input$FV_PAR.Plot.Max)
    
    dt <- get_data_FV_PAR_PER_FUN()
    dt <- dt[parId %in% input$FV_PAR.Plot.Params]
    sub_attr <- if (length(input$FV_PAR.Plot.Params) > 1) 'parId' else NULL
    
    lower <- 'lower'
    upper <- 'upper'
    if (input$FV_PAR.Plot.CI == 'None') type <- 'line' 
    else {
      type <- 'line+ribbon'
      if (input$FV_PAR.Plot.CI == 'Outer Quantiles') {
        quantiles <- paste0(getOption("IOHanalyzer.quantiles", c(0.2, 0.98)) * 100, '%')
        lower <- quantiles[[1]]
        upper <- quantiles[[length(quantiles)]]
      }
    }
    if (is.null(sub_attr) && type == 'line+ribbon') {
      p <- plot_general_data(dt, 'runtime', input$FV_PAR.Plot.show.mean, 'line',
                             subplot_attr = sub_attr, scale.xlog = input$FV_PAR.Plot.Logx,
                             scale.ylog = input$FV_PAR.Plot.Logy, 
                             lower_attr = lower, upper_attr = upper, show.legend = T)
      p <- plot_general_data(dt, 'runtime', input$FV_PAR.Plot.show.mean, 'ribbon',
                             subplot_attr = sub_attr, scale.xlog = input$FV_PAR.Plot.Logx,
                             scale.ylog = input$FV_PAR.Plot.Logy, 
                             lower_attr = lower, upper_attr = upper, show.legend = F, p = p)
    }
    else {
      p <- plot_general_data(dt, 'runtime', input$FV_PAR.Plot.show.mean, type,
                        subplot_attr = sub_attr, scale.xlog = input$FV_PAR.Plot.Logx,
                        scale.ylog = input$FV_PAR.Plot.Logy, 
                        lower_attr = lower, upper_attr = upper)
    }
    p
  },
  message = "Creating plot")
})

output$FV_PAR.Plot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_FV_PAR_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_FV_PAR_PER_FUN(), file)
  },
  contentType = paste0('image/', input$FV_PAR.Plot.Format)
)

output$FV_PAR.Plot.Figure <- renderPlotly({
  render_FV_PAR_PER_FUN()
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

fv_parameter_summary <- reactive({
  req(input$FV_PAR.Summary.Min, input$FV_PAR.Summary.Max, input$FV_PAR.Summary.Step)
  
  rtstart <- as.numeric(input$FV_PAR.Summary.Min) 
  rtstop <- as.numeric(input$FV_PAR.Summary.Max)
  rtstep <- as.numeric(input$FV_PAR.Summary.Step)
  data <- DATA()
  
  if (!input$FV_PAR.Summary.Single) {
    req(rtstart <= rtstop, rtstep <= rtstop - rtstart)
    rtall <- get_runtimes(data)
    rtseq <- seq_RT(rtall, rtstart, rtstop, by = rtstep)
    req(rtseq)
  }
  else 
    rtseq <- rtstart
  
  dt <- get_PAR_summary(data, rtseq, input$FV_PAR.Summary.Algid, 
                        input$FV_PAR.Summary.Param, which = 'by_RT')
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

fv_parameter_sample <- reactive({
  req(input$FV_PAR.Sample.Algid, input$FV_PAR.Sample.Max,
      input$FV_PAR.Sample.Step, input$FV_PAR.Sample.Min,
      input$FV_PAR.Sample.Param)
  
  rtstart <- as.numeric(input$FV_PAR.Sample.Min) 
  rtstop <- as.numeric(input$FV_PAR.Sample.Max)
  rtstep <- as.numeric(input$FV_PAR.Sample.Step)
  data <- DATA()
  
  if (!input$FV_PAR.Sample.Single) {
    req(rtstart <= rtstop, rtstep <= rtstop - rtstart)
    rtall <- get_runtimes(data)
    rtseq <- seq_RT(rtall, rtstart, rtstop, by = rtstep)
    req(rtseq)
  }
  else
    rtseq <- rtstart
  
  df <- get_PAR_sample(data, idxValue = rtseq, 
                       algorithm = input$FV_PAR.Sample.Algid,
                       parId = input$FV_PAR.Sample.Param,
                       output = input$FV_PAR.Sample.Format,
                       which = 'by_RT')
  
  for (p in paste0('run.', seq(ncol(data[[1]]$FV))))
    df[[p]] %<>% format(digits = 2, nsmall = 2)
  
  df
})

output$table_FV_PAR_SAMPLE <- DT::renderDataTable({
  dt <- fv_parameter_sample()
  req(length(dt) != 0)
  dt[is.na(dt)] <- 'NA'
  dt
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$table_FV_PAR_summary <- DT::renderDataTable({
  fv_parameter_summary()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T, digits = 2))

output$FV_PAR.Sample.Download <- downloadHandler(
  filename = function() {
    eval(PARSample_csv_name)
  },
  content = function(file) {
    df <- parameter_sample()
    df <- df[input[["table_FV_PAR_SAMPLE_rows_all"]]]
    save_table(df, file)
  }
)

output$FV_PAR.Summary.Download <- downloadHandler(
  filename = function() {
    eval(FV_PAR_csv_name)
  },
  content = function(file) {
    df <- fv_parameter_summary()
    df <- df[input[["table_FV_PAR_summary_rows_all"]]]
    save_table(df, file)
  }
)