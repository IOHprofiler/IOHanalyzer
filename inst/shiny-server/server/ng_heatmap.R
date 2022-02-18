render_rt_ng_heatmap <- reactive({
  input$RT_NG.Heatmap.Create
  isolate({
    data <- RT_NG_data()
  })
  req(length(data) > 0)
  Plot.Comparison.Heatmap(data, which = 'by_RT', target_dt = rt_ng_heatmap_targets_obj)
})

output$RT_NG.Heatmap.Plot <- renderPlotly({
  render_rt_ng_heatmap()
})

output$RT_NG.Heatmap.Download <- downloadHandler(
  filename = function() {
    eval(RT_NG_heatmap_name)
  },
  content = function(file) {
    save_plotly(render_rt_ng_heatmap(), file)
  },
  contentType = paste0('image/', input$RT_NG.Heatmap.Format)
)


RT_NG_data <- function() {
  data <- subset(DATA_RAW(), ID %in% isolate(input$RT_NG.Heatmap.ID))
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM %in% input$RT_NG.Heatmap.Dim)
  data <- subset(data, funcId %in% input$RT_NG.Heatmap.Funcid)
  if (length(unique(get_id(data))) < 2) {
    shinyjs::alert("This plot is only available when the dataset contains
                   multiple IDs for the selected functions and dimensions.")
    return(NULL)
  }
  data
}

rt_ng_heatmap_targets <- reactive({
  data <- RT_NG_data()
  if (is.null(data)) return(NULL)
  get_target_dt(data, "by_RT")
})

rt_ng_heatmap_targets_obj <- NULL

proxy_RT_NG.Heatmap.Targets <- dataTableProxy('RT_NG.Heatmap.Targets')

output$RT_NG.Heatmap.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  rt_ng_heatmap_targets_obj <<- rt_ng_heatmap_targets()
  rt_ng_heatmap_targets_obj
}, editable = list(target = 'cell', disable = list(columns = c(0,1))), rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


observeEvent(input$RT_NG.Heatmap.Targets_cell_edit, {
  info <- input$RT_NG.Heatmap.Targets_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  data <- RT_NG_data()
  if (is.null(data)) return(NULL)
  rt_ng_heatmap_targets_obj$target[[i]] <<- v
  replaceData(proxy, rt_ng_heatmap_targets_obj, resetPaging = FALSE, rownames = FALSE)
})

### FV

render_fv_ng_heatmap <- reactive({
  input$FV_NG.Heatmap.Create
  isolate({
    data <- FV_NG_data()
  })
  req(length(data) > 0)
  Plot.Comparison.Heatmap(data, which = 'by_FV', target_dt = fv_ng_heatmap_targets_obj)
})

output$FV_NG.Heatmap.Plot <- renderPlotly({
  render_fv_ng_heatmap()
})

output$FV_NG.Heatmap.Download <- downloadHandler(
  filename = function() {
    eval(FV_NG_heatmap_name)
  },
  content = function(file) {
    save_plotly(render_fv_ng_heatmap(), file)
  },
  contentType = paste0('image/', input$FV_NG.Heatmap.Format)
)


FV_NG_data <- function() {
  data <- subset(DATA_RAW(), ID %in% isolate(input$FV_NG.Heatmap.ID))
  if (length(data) == 0) return(NULL)
  data <- subset(data, DIM %in% input$FV_NG.Heatmap.Dim)
  data <- subset(data, funcId %in% input$FV_NG.Heatmap.Funcid)
  if (length(unique(get_id(data))) < 2) {
    shinyjs::alert("This plot is only available when the dataset contains
                   multiple IDs for the selected functions and dimensions.")
    return(NULL)
  }
  data
}

fv_ng_heatmap_targets <- reactive({
  data <- FV_NG_data()
  if (is.null(data)) return(NULL)
  get_target_dt(data, "by_FV")
})

fv_ng_heatmap_targets_obj <- NULL

proxy_FV_NG.Heatmap.Targets <- dataTableProxy('FV_NG.Heatmap.Targets')

output$FV_NG.Heatmap.Targets <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  fv_ng_heatmap_targets_obj <<- fv_ng_heatmap_targets()
  fv_ng_heatmap_targets_obj
}, editable = list(target = 'cell', disable = list(columns = c(0,1))), rownames = FALSE,
options = list(pageLength = 5, lengthMenu = c(5, 10, 25, -1), scrollX = T, server = T))


observeEvent(input$FV_NG.Heatmap.Targets_cell_edit, {
  info <- input$FV_NG.Heatmap.Targets_cell_edit
  i <- info$row
  j <- info$col
  v <- info$value
  data <- FV_NG_data()
  if (is.null(data)) return(NULL)
  fv_ng_heatmap_targets_obj$target[[i]] <<- v
  replaceData(proxy, fv_ng_heatmap_targets_obj, resetPaging = FALSE, rownames = FALSE)
})
