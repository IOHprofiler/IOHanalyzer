general_overview_box_single <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Data Overview (selected function)</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which algorithms to show.</p>'),
        
        selectInput('Overview.Single.Algid', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
        hr(),
        selectInput('Overview.Single.Format', 'Format', choices = c('csv','tex'), selected = 'csv'),
        downloadButton("Overview.Single.Download", "Save this table")
      ),
      
      mainPanel(
        width = 9,
        DT::dataTableOutput('Overview.Single.Table')
      )
  )
}

general_overview_box_all <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Data Overview (all functions)</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('Overview.All.Format', 'Format', choices = c('csv','tex'), selected = 'csv'),
        downloadButton("Overview.All.Download", "Save this table")
      ),
      
      mainPanel(
        width = 9,
        DT::dataTableOutput('Overview.All.Table')
      )
  )
}