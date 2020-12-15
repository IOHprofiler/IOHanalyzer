multi_function_ert_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Multi-Function Statistics</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which algorithms to show.</p>'),
        
        selectInput('RT.MultiERT.AlgId', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
        selectInput('RT.MultiERT.FuncId', 'Functions', choices = NULL, selected = NULL, multiple = T),
        selectInput('RT.MultiERT.DIM', 'Dimensions', choices = NULL, selected = NULL, multiple = T),
        hr(),
        numericInput('RT.MultiERT.Target', 'Target Value', value = 100, min = 1),
        hr(),
        selectInput('RT.MultiERT.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("RT.MultiERT.Download", "Save this table")
      ),
      
      mainPanel(
        width = 9,
        DT::dataTableOutput('RT.MultiERT.Table')
      )
  )
}


multi_function_sample_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Multi-Function Hitting Times</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which algorithms to show.</p>'),
        
        selectInput('RT.Multisample.AlgId', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
        selectInput('RT.Multisample.FuncId', 'Functions', choices = NULL, selected = NULL, multiple = T),
        selectInput('RT.Multisample.DIM', 'Dimensions', choices = NULL, selected = NULL, multiple = T),
        hr(),
        numericInput('RT.Multisample.Target', 'Target Value', value = 100, min = 1),
        hr(),
        selectInput('RT.Multisample.mode', 'Table style', choices = c('long', 'wide'), selected = 'wide'),
        selectInput('RT.Multisample.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("RT.Multisample.Download", "Save this table")
      ),
      
      mainPanel(
        width = 9,
        DT::dataTableOutput('RT.Multisample.Table')
      )
  )
}