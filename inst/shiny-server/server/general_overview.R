overview_table_single <- reactive({
  data <- DATA()
  req(length(data) > 0)
  df <- get_overview(subset(data, algId %in% input$Overview.Single.Algid))
                     
  df$budget %<>% as.numeric
  df$runs %<>% as.integer
  df$funcId %<>% as.integer
  df$DIM %<>% as.integer
  df$succ %<>% as.integer
  df$"worst recorded" <- format_FV(df$"worst recorded") %>% as.numeric
  df$"worst reached" <- format_FV(df$"worst reached") %>% as.numeric
  df$"mean reached" <- format_FV(df$"mean reached") %>% as.numeric
  df$"median reached" <- format_FV(df$"median reached") %>% as.numeric
  df$"best reached" <- format_FV(df$"best reached") %>% as.numeric
  df$"max evals used" %<>% as.numeric
  df
})

output$Overview.Single.Table <- DT::renderDataTable({
  req(input$Overview.Single.Algid)
  overview_table_single()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$Overview.Single.Download <- downloadHandler(
  filename = function() {
    eval(overview_single_name)
  },
  content = function(file) {
    df <- overview_table_single()
    df <- df[input[["Overview.Single.Table_rows_all"]]]
    if (input$Overview.Single.Format == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
  }
)

overview_table_all <- reactive({
  data <- DATA_RAW()
  req(length(data) > 0)
  df <- get_overview(data)
  
  df
})

output$Overview.All.Table <- DT::renderDataTable({
  overview_table_all()
}, filter = list(position = 'top', clear = FALSE),
options = list(dom = 'lrtip', pageLength = 15, scrollX = T, server = T))

output$Overview.All.Download <- downloadHandler(
  filename = function() {
    eval(overview_all_name)
  },
  content = function(file) {
    df <- overview_table_all()
    df <- df[input[["Overview.All.Table_rows_all"]]]
    if (input$Overview.All.Format == 'csv')
      write.csv(df, file, row.names = F)
    else{
      print(xtable(df), file = file)
    }
  }
)