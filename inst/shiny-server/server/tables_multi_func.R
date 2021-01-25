multi_function_data_summary <- reactive({
  data <- DATA_RAW()
  req(length(data) > 0)
  data <- subset(data, algId %in% input$RT.MultiERT.AlgId & 
                   funcId %in% input$RT.MultiERT.FuncId & 
                   DIM %in% input$RT.MultiERT.DIM)
  
  get_RT_summary(data, as.numeric(input$RT.MultiERT.Target))
})

output$RT.MultiERT.Table <- DT::renderDataTable({
  req(input$RT.MultiERT.AlgId)
  multi_function_data_summary()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$RT.MultiERT.Download <- downloadHandler(
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
  
  get_RT_sample(data, as.numeric(input$RT.Multisample.Target), output = input$RT.Multisample.mode)
})

output$RT.Multisample.Table <- DT::renderDataTable({
  req(input$RT.Multisample.AlgId)
  multi_function_data_sample()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$RT.Multisample.Download <- downloadHandler(
  filename = function() {
    eval(RT_multifunc_sample)
  },
  content = function(file) {
    save_table(multi_function_data_sample(), file)
  }
)


multi_function_data_summary_FV <- reactive({
  data <- DATA_RAW()
  req(length(data) > 0)
  req(as.numeric(input$FV.MultiFV.Target) > 0)
  data <- subset(data, algId %in% input$FV.MultiFV.AlgId & 
                   funcId %in% input$FV.MultiFV.FuncId & 
                   DIM %in% input$FV.MultiFV.DIM)
  
  get_FV_summary(data, as.numeric(input$FV.MultiFV.Target))
})

output$FV.MultiFV.Table <- DT::renderDataTable({
  req(input$FV.MultiFV.AlgId)
  multi_function_data_summary_FV()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$FV.MultiFV.Download <- downloadHandler(
  filename = function() {
    eval(FV_multifunc_FV)
  },
  content = function(file) {
    save_table(multi_function_data_summary_FV(), file)
  }
)

multi_function_data_sample_FV <- reactive({
  data <- DATA_RAW()
  req(length(data) > 0)
  data <- subset(data, algId %in% input$FV.Multisample.AlgId & 
                   funcId %in% input$FV.Multisample.FuncId & 
                   DIM %in% input$FV.Multisample.DIM)
  
  get_FV_sample(data, as.numeric(input$FV.Multisample.Target), output = input$FV.Multisample.mode)
})

output$FV.Multisample.Table <- DT::renderDataTable({
  req(input$FV.Multisample.AlgId)
  multi_function_data_sample_FV()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$FV.Multisample.Download <- downloadHandler(
  filename = function() {
    eval(FV_multifunc_sample)
  },
  content = function(file) {
    save_table(multi_function_data_sample_FV(), file)
  }
)

