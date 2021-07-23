multi_function_ert_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Multi-Function Statistics</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which IDs to include:</p>'),
        
        selectInput('RT.MultiERT.AlgId', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
        selectInput('RT.MultiERT.FuncId', 'Functions', choices = NULL, selected = NULL, multiple = T),
        selectInput('RT.MultiERT.DIM', 'Dimensions', choices = NULL, selected = NULL, multiple = T),
        hr(),
        textInput('RT.MultiERT.Target', 'Target Value', value = ''),
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
        HTML('<p align="justify">Select which IDs to include:</p>'),
        
        selectInput('RT.Multisample.AlgId', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
        selectInput('RT.Multisample.FuncId', 'Functions', choices = NULL, selected = NULL, multiple = T),
        selectInput('RT.Multisample.DIM', 'Dimensions', choices = NULL, selected = NULL, multiple = T),
        hr(),
        textInput('RT.Multisample.Target', 'Target Value', value = ''),
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


multi_function_fv_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Multi-Function Statistics</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which IDs to include:</p>'),
        
        selectInput('FV.MultiFV.AlgId', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
        selectInput('FV.MultiFV.FuncId', 'Functions', choices = NULL, selected = NULL, multiple = T),
        selectInput('FV.MultiFV.DIM', 'Dimensions', choices = NULL, selected = NULL, multiple = T),
        hr(),
        textInput('FV.MultiFV.Target', 'Budget Value', value = ''),
        hr(),
        selectInput('FV.MultiFV.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("FV.MultiFV.Download", "Save this table")
      ),
      
      mainPanel(
        width = 9,
        DT::dataTableOutput('FV.MultiFV.Table')
      )
  )
}


multi_function_sample_box_fv <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Multi-Function Hitting Times</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which IDs to include:</p>'),
        
        selectInput('FV.Multisample.AlgId', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
        selectInput('FV.Multisample.FuncId', 'Functions', choices = NULL, selected = NULL, multiple = T),
        selectInput('FV.Multisample.DIM', 'Dimensions', choices = NULL, selected = NULL, multiple = T),
        hr(),
        textInput('FV.Multisample.Target', 'Budget Value', value = ''),
        hr(),
        selectInput('FV.Multisample.mode', 'Table style', choices = c('long', 'wide'), selected = 'wide'),
        selectInput('FV.Multisample.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("FV.Multisample.Download", "Save this table")
      ),
      
      mainPanel(
        width = 9,
        DT::dataTableOutput('FV.Multisample.Table')
      )
  )
}