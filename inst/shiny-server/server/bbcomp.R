get_best_alg <- function(dsl_sub, aggr_attrs = c("dataset", "metric"), aggr_method = "Average") {
  dt_temp <- get_FV_summary(dsl_sub, 128)[, c("mean")]
  for (a in aggr_attrs) dt_temp[, (a) := attr(dsl_sub, a)]
  by_vals <- aggr_attrs
  dt_temp <- dt_temp[, .(value = mean(mean)), by = by_vals]
  dt_temp[order(value)]
}

#VERY UGLY, TEMPORARY SOLUTION. DO NOT MERGE WHILE THIS FUNCTION IS STILL HERE!!!
get_FV_sample_bbo <-
  function(ds, runtime, ...) {
    
    rbindlist(lapply(ds, function(ds) {
      res <-
        cbind(attr(ds, 'DIM'),
              attr(ds, 'funcId'),
              get_FV_sample_bbo_ds(ds, runtime, ...))
      colnames(res)[1] <- 'DIM'
      colnames(res)[2] <- 'funcId'
      res
    }),
    fill = T)
  }

get_FV_sample_bbo_ds <- function(ds, runtime,...) {
  data <- ds$FV
  N <- ncol(data)
  n_row <- nrow(data)
  algId <- attr(ds, 'algId')
  maximization <- attr(ds, 'maximization')
  classifier <- attr(ds, 'classifier')
  dataset <- attr(ds, 'dataset')
  metric <- attr(ds, 'metric')
  
  runtime <- sort(as.numeric(unique(c(runtime))))
  RT <- as.numeric(rownames(data))
  idx <- seq_along(RT)
  
  matched <- sapply(runtime, function(r) rev(idx[r >= RT])[1])
  res <- cbind(algId, classifier, dataset, metric, runtime, as.data.table(data[matched, , drop = FALSE])) %>%
    set_colnames(c('algId', 'classifier', 'dataset', 'metric', 'runtime', paste0('run.', seq(N))))
  
  res <- melt(res, id = c('algId', 'runtime', 'classifier', 'dataset', 'metric'), variable.name = 'run', value.name = 'f(x)')
  res[, run := as.integer(as.numeric(gsub('run.', '', run)))
      ][order(runtime, run)]
  
  res
}

aggr_dt <- reactive({
  
  data <- DATA_RAW()
  
  aggr_attrs <- c()
  if (input$BBOcomp.aggr_alg) aggr_attrs <- c(aggr_attrs, "algId")
  if (input$BBOcomp.aggr_class) aggr_attrs <- c(aggr_attrs, "classifier")
  if (input$BBOcomp.aggr_dataset) aggr_attrs <- c(aggr_attrs, "dataset")
  if (input$BBOcomp.aggr_metric) aggr_attrs <- c(aggr_attrs, "metric")
  
  if (length(aggr_attrs) == 0) {
    shinyjs::alert("Please make sure at least one aggregation is selected")
    return(NULL)
  }
  get_best_alg(data, aggr_attrs, aggr_method = input$BBOcomp.aggr_averaging)
})

output$table_BBOcomp_aggr <- DT::renderDataTable({
  dt <- aggr_dt()
  req(length(dt) != 0)
  dt[is.na(dt)] <- 'NA'
  dt
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

render_bbocomp_plot <- reactive({
  withProgress({
    data <- DATA_RAW()
    
    aggr_attrs <- c()

    
    dt <- get_FV_sample_bbo(data, 128, output = "long")
    sub_attr <- NULL
    if (input$BBOcomp.Plot.subattr != "none") {
      sub_attr <- input$BBOcomp.Plot.subattr
    }
    p <- plot_general_data(dt, input$BBOcomp.Plot.xattr, 'f(x)', "violin",
                           subplot_attr = sub_attr, legend_attr = input$BBOcomp.Plot.xattr)
    
    p
  },
  message = "Creating plot")
})




# output$FV_PAR.Plot.Download <- downloadHandler(
#   filename = function() {
#     eval(FIG_NAME_FV_PAR_PER_FUN)
#   },
#   content = function(file) {
#     save_plotly(render_FV_PAR_PER_FUN(), file)
#   },
#   contentType = paste0('image/', input$FV_PAR.Plot.Format)
# )

output$BBOcomp.Plot.Figure <- renderPlotly({
  render_bbocomp_plot()
})
