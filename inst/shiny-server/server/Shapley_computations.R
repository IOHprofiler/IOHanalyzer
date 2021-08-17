output$RT_SHAPLEY <- renderPlotly({
  req(length(DATA_RAW()) > 0)
  render_RT_SHAPLEY()
})

get_data_RT_SHAPLEY <- reactive({
  input$RTportfolio.Shapley.Refresh
  dsList <- subset(DATA_RAW(), ID %in% input$RTportfolio.Shapley.Algs & 
                     funcId %in% input$RTportfolio.Shapley.Funcs &
                     DIM %in% input$RTportfolio.Shapley.Dims)
  
  
  if (length(get_id(dsList)) <= 1) {
    shinyjs::alert("This is an invalid configuration for this plot. \n
                     Please ensure that the dataset contains multiple IDs.")
    return(NULL)
  }
  
  isolate({
    targets <- RT_SHAPLEY_TARGETS_TABLE()
  })
  get_shapley_values(dsList, targets, input$RTportfolio.Shapley.Logx, input$RTportfolio.Shapley.Groupsize, 
                     input$RTportfolio.Shapley.Permsize)
})

render_RT_SHAPLEY <- reactive({
  withProgress({
    dt <- get_data_RT_SHAPLEY()
    plot_general_data(dt, x_attr = 'ID', y_attr = 'shapley', type = 'bar', 
                      legend_attr = 'ID', show.legend = T)
  },
  message = "Creating plot")
})

output$RTportfolio.Shapley.Download <- downloadHandler(
  filename = function() {
    eval(FIG_NAME_RT_SHAPLEY)
  },
  content = function(file) {
    save_plotly(render_RT_SHAPLEY(), file)
  },
  contentType = paste0('image/', input$RTportfolio.Shapley.Format)
)

RT_SHAPLEY_TARGETS_TABLE <- reactiveVal(NULL)
trigger_renderDT_shap <- reactiveVal(NULL)
proxy <- dataTableProxy('RT_SHAPLEY_TARGETS_GENERATED')

observe({
  req(length(DATA_RAW()) > 0)
  dsList <- subset(DATA_RAW(), ID %in% input$RTportfolio.Shapley.Algs & 
                     funcId %in% input$RTportfolio.Shapley.Funcs &
                     DIM %in% input$RTportfolio.Shapley.Dims)
  req(length(dsList) > 0)

  targets <- get_ECDF_targets(dsList, input$RTportfolio.Shapley.Target_type, input$RTportfolio.Shapley.Target_number)
  
  RT_SHAPLEY_TARGETS_TABLE(targets) # add values to `RT_SHAPLEY_TARGETS_TABLE`
  trigger_renderDT_shap(rnorm(1))
})


output$RT_SHAPLEY_TARGETS_GENERATED <- DT::renderDataTable({
  req(length(DATA_RAW()) > 0)
  trigger_renderDT_shap()
  isolate({RT_SHAPLEY_TARGETS_TABLE()})
}, 
editable = TRUE, 
rownames = FALSE,
options = list(
  pageLength = 10, 
  lengthMenu = c(5, 10, 25, -1), 
  scrollX = T, 
  server = T,
  columnDefs = list(
    list(
      className = 'dt-right', targets = "_all"
    )
  )
),
filter = 'top'
)

observeEvent(input$RT_SHAPLEY_TARGETS_GENERATED_cell_edit, {
  info <- input$RT_SHAPLEY_TARGETS_GENERATED_cell_edit
  i <- info$row
  j <- info$col + 1
  v <- info$value
  
  suppressWarnings({
    df <- RT_SHAPLEY_TARGETS_TABLE()
    set(df, i, j, coerceValue(v, as.numeric(df[i, ..j])))
    RT_SHAPLEY_TARGETS_TABLE(df)
  })
  replaceData(proxy, RT_SHAPLEY_TARGETS_TABLE(), 
              resetPaging = FALSE, rownames = FALSE)
})

observeEvent(input$RTportfolio.Shapley.Table.Upload, {
  if (!is.null(input$RTportfolio.Shapley.Table.Upload)) {
    df <- read.csv2(input$RTportfolio.Shapley.Table.Upload$datapath, header = T, row.names = F) %>% 
      as.data.table
    
    if (ncol(df) == ncol(RT_SHAPLEY_TARGETS_TABLE())) {
      RT_SHAPLEY_TARGETS_TABLE(df)
      replaceData(proxy, RT_SHAPLEY_TARGETS_TABLE(), 
                  resetPaging = FALSE, 
                  rownames = FALSE)
    } else {
      RT_SHAPLEY_TARGETS_TABLE(df)
      trigger_renderDT_shap(rnorm(1))
    }
  } else
    NULL
})

output$RTportfolio.Shapley.Table.Download <- downloadHandler(
  filename = 'Example_ECDF_TARGETS.csv',
  content = function(file) {
    write.csv2(RT_SHAPLEY_TARGETS_TABLE(), file, row.names = F)
  },
  contentType = "text/csv"
)