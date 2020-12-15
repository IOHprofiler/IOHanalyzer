multi_function_data_summary <- reactive({
  data <- DATA_RAW()
  req(length(data) > 0)
  data <- subset(data, algId %in% input$RT.MultiERT.AlgId & 
                   funcId %in% input$RT.MultiERT.FuncId & 
                   DIM %in% input$RT.MultiERT.DIM)
  
  get_FV_summary(data, input$RT.MultiERT.Target)
})

output$RT.MultiERT.Table <- DT::renderDataTable({
  req(input$RT.MultiERT.AlgId)
  multi_function_data_summary()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$RTSummary.Overview.Download <- downloadHandler(
  filename = function() {
    eval(RT_multifunc_ERT)
  },
  content = function(file) {
    save_table(multi_function_data_summary(), file)
  }
)

multi_function_data_sample <- reactive({
  data <- DATA_RAW()
  req(length(data) > 0)
  data <- subset(data, algId %in% input$RT.Multisample.AlgId & 
                   funcId %in% input$RT.Multisample.FuncId & 
                   DIM %in% input$RT.Multisample.DIM)
  
  get_FV_sample(data, input$RT.Multisample.Target, output = input$RT.Multisample.mode)
})

output$RT.Multisample.Table <- DT::renderDataTable({
  req(input$RT.Multisample.AlgId)
  multi_function_data_sample()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$RTSummary.Overview.Download <- downloadHandler(
  filename = function() {
    eval(RT_multifunc_sample)
  },
  content = function(file) {
    save_table(multi_function_data_sample(), file)
  }
)



