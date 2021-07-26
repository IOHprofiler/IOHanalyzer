general_overview_box_single <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Overview of Selected Function</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which IDs to include:</p>'),
        
        selectInput('Overview.Single.Algid', 'IDs:', choices = NULL, selected = NULL, multiple = T),
        hr(),
        selectInput('Overview.Single.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("Overview.Single.Download", "Save this table")
      ),
      
      mainPanel(
        width = 9,
        DT::dataTableOutput('Overview.Single.Table')
      )
  )
}

general_overview_box_all <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Overview of All Available Functions</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('Overview.All.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("Overview.All.Download", "Save this table")
      ),
      
      mainPanel(
        width = 9,
        DT::dataTableOutput('Overview.All.Table')
      )
  )
}