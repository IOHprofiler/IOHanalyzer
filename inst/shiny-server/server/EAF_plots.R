### EAF for single functions

output$EAF.Single_Plot <- renderPlotly({
  req(length(DATA()) > 0)
  render_EAF_Plot()
})

get_data_EAF <- reactive({
  dsList <- subset(DATA(), ID %in% input$EAF.Single.Algs)

  generate_data.EAF(dsList, n_sets = input$EAF.Single.levels)
})

render_EAF_Plot <- reactive({
  withProgress({
    plot_eaf_data(get_data_EAF(), attr(DATA(), 'maximization'),
                  scale.xlog = input$EAF.Single.Logx, scale.ylog = input$EAF.Single.Logy,
                  xmin = input$EAF.Single.Min, xmax = input$EAF.Single.Min,
                  ymin = input$EAF.Single.yMin, ymax = input$EAF.Single.yMax,
                  subplot_attr = 'ID', x_title = "Evaluations",
                  y_title = "f(x)")
  },
  message = "Creating plot")
})

output$EAF.Single.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_EAF)
  },
  content = function(file) {
    save_plotly(render_EAF_Plot(), file)
  },
  contentType = paste0('image/', input$EAF.Single.Format)
)


### EAF-based ECDF
output$EAF.CDF_Plot <- renderPlotly({
  req(length(DATA()) > 0)
  render_EAFCDF_Plot()
})

get_data_EAFCDF <- reactive({
  dsList <- subset(DATA(), ID %in% input$EAF.CDF.Algs)
  dt_eaf <- generate_data.EAF(dsList)

  dt_ecdf <- rbindlist(lapply(input$EAF.CDF.Algs, function(id) {
    dt_sub <- dt_eaf[ID == id, ]
    temp <- generate_data.ECDF_From_EAF(dt_sub,
                                        min_val = input$EAF.CDF.yMin,
                                        max_val = input$EAF.CDF.yMax, scale_log=input$EAF.CDF.Logy)
    temp$ID <- id
    temp
  }))
})

render_EAFCDF_Plot <- reactive({
  withProgress({
    plot_general_data(get_data_EAFCDF(), 'runtime', 'fraction', 'line', 'ID',
                  scale.xlog = input$EAF.CDF.Logx, scale.ylog = F, x_title = "Evaluations",
                  y_title = "Fraction")
  },
  message = "Creating plot")
})

output$EAF.CDF.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_EAFCDF)
  },
  content = function(file) {
    save_plotly(render_EAFCDF_Plot(), file)
  },
  contentType = paste0('image/', input$EAF.CDF.Format)
)

### EAF-differences

output$EAF.Diff_Plot <- renderPlotly({
  req(length(DATA()) > 0)
  render_EAFDiff_Plot()
})

get_data_EAFDiff <- reactive({
  dsList <- subset(DATA(), ID %in% input$EAF.Diff.Algs)
  matrices <- generate_data.EAF_diff_Approximate(dsList, input$EAF.Diff.Min, input$EAF.Diff.Max,
                                                 input$EAF.Diff.yMin, input$EAF.Diff.yMax)

  matrices
})

render_EAFDiff_Plot <- reactive({
  withProgress({
    plot_eaf_differences(get_data_EAFDiff(), scale.xlog = input$EAF.Diff.Logx,
                         scale.ylog = input$EAF.Diff.Logy)
  },
  message = "Creating plot")
})

output$EAF.Diff.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_EAFDiff)
  },
  content = function(file) {
    save_plotly(render_EAFDiff_Plot(), file)
  },
  contentType = paste0('image/', input$EAF.Diff.Format)
)


### ### Multi-function view ### ###

### EAF for multiple functions

output$EAF.Multi_Plot <- renderPlotly({
  req(length(DATA_RAW()) > 0)
  render_EAF_multi_Plot()
})

get_data_EAF_multi <- reactive({
  dsList <- subset(DATA_RAW(), ID %in% input$EAF.Multi.Algs, funcId %in% input$EAF.Multi.FuncIds )

  generate_data.EAF(dsList, n_sets = input$EAF.Multi.levels)
})

render_EAF_multi_Plot <- reactive({
  withProgress({
    plot_eaf_data(get_data_EAF_multi(), attr(DATA_RAW(), 'maximization'),
                  scale.xlog = input$EAF.Multi.Logx, scale.ylog = input$EAF.Multi.Logy,
                  xmin = input$EAF.Multi.Min, xmax = input$EAF.Multi.Min,
                  ymin = input$EAF.Multi.yMin, ymax = input$EAF.Multi.yMax,
                  subplot_attr = 'ID', x_title = "Evaluations",
                  y_title = "f(x)")
  },
  message = "Creating plot")
})

output$EAF.Multi.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_EAF)
  },
  content = function(file) {
    save_plotly(render_EAF_multi_Plot(), file)
  },
  contentType = paste0('image/', input$EAF.Multi.Format)
)


### EAF-based ECDF
output$EAF.MultiCDF_Plot <- renderPlotly({
  req(length(DATA_RAW()) > 0)
  render_EAFMultiCDF_Plot()
})

get_data_EAFMultiCDF <- reactive({
  dsList <- subset(DATA_RAW(), ID %in% input$EAF.MultiCDF.Algs, funcId %in% input$EAF.MultiCDF.FuncIds )
  dt_eaf <- generate_data.EAF(dsList)

  dt_eMultiCDF <- rbindlist(lapply(input$EAF.MultiCDF.Algs, function(id) {
    dt_sub <- dt_eaf[ID == id, ]
    temp <- generate_data.ECDF_From_EAF(dt_sub,
                                        min_val = input$EAF.MultiCDF.yMin,
                                        max_val = input$EAF.MultiCDF.yMax, scale_log=input$EAF.MultiCDF.Logy)
    temp$ID <- id
    temp
  }))
})

render_EAFMultiCDF_Plot <- reactive({
  withProgress({
    plot_general_data(get_data_EAFMultiCDF(), 'runtime', 'fraction', 'line', 'ID',
                      scale.xlog = input$EAF.MultiCDF.Logx, scale.ylog = F, x_title = "Evaluations",
                      y_title = "Fraction")
  },
  message = "Creating plot")
})

output$EAF.MultiCDF.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_EAFMultiCDF)
  },
  content = function(file) {
    save_plotly(render_EAFMultiCDF_Plot(), file)
  },
  contentType = paste0('image/', input$EAF.MultiCDF.Format)
)
