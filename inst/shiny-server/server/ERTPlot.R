# The expected runtime plot ---------------------
# TODO: wrapp this as a separate function for DataSet class
output$ERT_PER_FUN <- renderPlotly({
  render_ert_per_fct()
})

output$ERTPlot.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_PER_FUN)
  },
  content = function(file) {
    save_plotly(render_ert_per_fct(), file,
                format = input$ERTPlot.Format,
                width = fig_width2, height = fig_height)
  },
  contentType = paste0('image/', input$ERTPlot.Format)
)

update_ert_per_fct_axis <- observe({
  plotlyProxy("ERT_PER_FUN", session) %>%
    plotlyProxyInvoke("relayout", list(xaxis = list(type = ifelse(input$ERTPlot.semilogx, 'log', 'linear')),
                                       yaxis = list(type = ifelse(input$ERTPlot.semilogy, 'log', 'linear'))))
})

# ert_per_fct_data <- reactive({
#   req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
#   dsList <- DATA()
#   Fall <- get_funvals(dsList)
#
#   Fseq <- seq_FV(Fall, as.numeric(input$ERTPlot.Min), as.numeric(input$ERTPlot.Max), length.out = 60,
#                  scale = ifelse(isolate(input$ERTPlot.semilogx), 'log', 'linear'))
#
#   dt <- get_RT_summary(dsList, ftarget = Fseq)
#   dt[, `:=`(upper = mean + sd, lower = mean - sd)]
#   dt
# })
#
# #TODO: Think of a better method to add / remove traces. Keep an eye on https://github.com/ropensci/plotly/issues/1248 for possible fix
# update_ert_per_fct_ERT <- observe({
#   req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
#
#   dsList <- DATA()
#   N <- length(dsList)
#   if (input$ERTPlot.show.ERT){
#     nr_other_active = isolate(1+input$ERTPlot.show.mean + input$ERTPlot.show.median)
#     legends <- get_legends(dsList)
#     colors <- color_palettes(N)
#     dt <- ert_per_fct_data()
#
#     for (i in seq_along(dsList)) {
#       rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
#
#       legend <- legends[i]
#       ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
#                      funcId == attr(dsList[[i]], 'funcId') &
#                      DIM == attr(dsList[[i]], 'DIM')]
#       plotlyProxy("ERT_PER_FUN", session) %>%
#         plotlyProxyInvoke("addTraces", list(x = ds_ERT$target, y = ds_ERT$ERT, type = 'scatter',
#                         name = paste0(legend, '.ERT'), mode = 'lines+markers',
#                         marker = list(color = rgb_str), legendgroup = legend,
#                         line = list(color = rgb_str)),(i-1)*nr_other_active)
#     }
#   }
#   else{
#     nr_other_active = isolate(1+input$ERTPlot.show.mean + input$ERTPlot.show.median)
#     plotlyProxy("ERT_PER_FUN", session) %>%
#       plotlyProxyInvoke("deleteTraces", nr_other_active*0:(N-1))
#   }
# })
#
# update_ert_per_fct_mean <- observe({
#   req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
#
#   dsList <- DATA()
#   N <- length(dsList)
#   if (input$ERTPlot.show.mean){
#     nr_other_active = isolate(1+input$ERTPlot.show.ERT + input$ERTPlot.show.median)
#     legends <- get_legends(dsList)
#     colors <- color_palettes(N)
#     dt <- ert_per_fct_data()
#
#     for (i in seq_along(dsList)) {
#       rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
#
#       legend <- legends[i]
#       ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
#                      funcId == attr(dsList[[i]], 'funcId') &
#                      DIM == attr(dsList[[i]], 'DIM')]
#       plotlyProxy("ERT_PER_FUN", session) %>%
#         plotlyProxyInvoke("addTraces", list(x = ds_ERT$target, y = ds_ERT$mean, type = 'scatter',
#                                             name = paste0(legend, '.mean'), mode = 'lines+markers',
#                                             marker = list(color = rgb_str), legendgroup = legend,
#                                             line = list(color = rgb_str)),1+(i-1)*nr_other_active)
#     }
#   }
#   else{
#     nr_other_active = isolate(1+input$ERTPlot.show.ERT + input$ERTPlot.show.median)
#     plotlyProxy("ERT_PER_FUN", session) %>%
#       plotlyProxyInvoke("deleteTraces", 1+(nr_other_active*0:(N-1)))
#   }
# })
#
# update_ert_per_fct_median <- observe({
#   req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
#
#   dsList <- DATA()
#   N <- length(dsList)
#   if (input$ERTPlot.show.median){
#     nr_other_active = isolate(input$ERTPlot.show.mean + input$ERTPlot.show.ERT)
#     legends <- get_legends(dsList)
#     colors <- color_palettes(N)
#     dt <- ert_per_fct_data()
#
#     for (i in seq_along(dsList)) {
#       rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
#
#       legend <- legends[i]
#       ds_ERT <- dt[algId == attr(dsList[[i]], 'algId') &
#                      funcId == attr(dsList[[i]], 'funcId') &
#                      DIM == attr(dsList[[i]], 'DIM')]
#       plotlyProxy("ERT_PER_FUN", session) %>%
#         plotlyProxyInvoke("addTraces", list(x = ds_ERT$target, y = ds_ERT$median, type = 'scatter',
#                                             name = paste0(legend, '.median'), mode = 'lines+markers',
#                                             marker = list(color = rgb_str), legendgroup = legend,
#                                             line = list(color = rgb_str)),nr_other_active+(i-1)*(nr_other_active+1))
#     }
#   }
#   else{
#     nr_other_active = isolate(input$ERTPlot.show.mean + input$ERTPlot.show.ERT)
#     seq_del = nr_other_active+((1+nr_other_active)*0:(N-1))
#     plotlyProxy("ERT_PER_FUN", session) %>%
#       plotlyProxyInvoke("deleteTraces", seq_del)
#   }
# })


render_ert_per_fct <- reactive({
  req(input$ERTPlot.Min, input$ERTPlot.Max, DATA())
  selected_algs <- input$ERTPlot.Algs
  data <- subset(DATA(), algId %in% input$ERTPlot.Algs)
  fstart <- input$ERTPlot.Min %>% as.numeric
  fstop <- input$ERTPlot.Max %>% as.numeric
  plot_RT_single_fct(data, Fstart = fstart, Fstop = fstop,
                     show.CI = input$ERTPlot.show.CI,
                     show.density = input$ERTPlot.show.density,
                     show.runs = input$ERTPlot.show_all,
                     show.optimal = input$ERTPlot.show.best_of_all,
                     show.pareto = input$ERTPlot.show.pareto_optima,
                     show.ERT = input$ERTPlot.show.ERT,
                     show.mean = input$ERTPlot.show.mean,
                     show.median = input$ERTPlot.show.median,
                     scale.xlog = isolate(input$ERTPlot.semilogx),
                     scale.ylog = isolate(input$ERTPlot.semilogy),
                     show.grad = input$ERTPlot.show.grad,
                     show.intensity = input$ERTPlot.show.intensity,
                     scale.reverse = (format == COCO || format == BIBOJ_COCO))
})

output$ERTPlot.Multi.Plot <- renderPlotly(
  render_ERTPlot_multi_plot()
)

render_ERTPlot_multi_plot <- eventReactive(input$ERTPlot.Multi.PlotButton, {
  req(input$ERTPlot.Multi.Algs)
  data <- subset(DATA_RAW(),
                 algId %in% input$ERTPlot.Multi.Algs,
                 DIM == input$Overall.Dim)
  req(data)

  plot_RT_all_fcts(data,
                   scale.xlog = input$ERTPlot.Multi.Logx,
                   scale.ylog = input$ERTPlot.Multi.Logy,
                   scale.reverse = (format == COCO || format == BIBOJ_COCO))
})

output$ERTPlot.Multi.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_PER_FUN_MULTI)
  },
  content = function(file) {
    save_plotly(render_ERTPlot_multi_plot(), file,
                format = input$ERTPlot.Multi.Format,
                width = fig_width2, height = fig_height)
  },
  contentType = paste0('image/', input$ERTPlot.Multi.Format)
)

output$ERTPlot.Aggr.Plot <- renderPlotly(
  render_ERTPlot_aggr_plot()
)

get_max_targets <- function(data, aggr_on, maximize){
  targets <- c()
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_DIM(data)

  for (j in seq_along(aggr_attr)) {
    dsList_filetered <- if (aggr_on == 'funcId') subset(data,funcId == aggr_attr[[j]])
    else subset(data, DIM == aggr_attr[[j]])

    Fall <- get_funvals(dsList_filetered)
    Fval <- ifelse(maximize, max(Fall), min(Fall))
    targets <- c(targets,Fval)
  }
  targets
}

render_ERTPlot_aggr_plot <- reactive({
  #TODO: figure out how to avoid plotting again when default targets are written to input
  data <- DATA_RAW()
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM == input$Overall.Dim)
  erts <- MAX_ERTS_FUNC()
  aggr_on = 'funcId'
  aggr_attr <- if (aggr_on == 'funcId') get_funcId(data) else get_DIM(data)
  targets <- get_max_targets(data, aggr_on, maximize = !(format == COCO || format == BIBOJ_COCO))
  erts <- max_ERTs(data, aggr_on, targets, maximize = !(format == COCO || format == BIBOJ_COCO))

  plot_ERT_AGGR(data, plot_mode = input$ERTPlot.Aggr.Mode, targets = targets,
                scale.ylog = input$ERTPlot.Aggr.Logy,
                maximize = !(format == COCO || format == BIBOJ_COCO),
                use_rank = input$ERTPlot.Aggr.Ranking,
                aggr_on = aggr_on, erts = erts)
})

output$ERTPlot.Aggr.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_ERT_AGGR)
  },
  content = function(file) {
    save_plotly(render_ERTPlot_aggr_plot(), file,
                format = input$ERTPlot.Aggr.Format,
                width = fig_width2, height = fig_height)
  },
  contentType = paste0('image/', input$ERTPlot.Aggr.Format)
)
