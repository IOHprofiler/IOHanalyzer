rt_overview_box <- function() {
  box(title = HTML('<p style="font-size:120%;">Overview of runtime values</p>'), width = 12,
      solidHeader = T, status = "primary", collapsible = T,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which algorithms to show.</p>'),
        
        # TODO: find better naming scheme for 'fstart, fstop, singleF'
        selectInput('ALGID_INPUT_SUMMARY', 'Algorithms', choices = NULL, selected = NULL),
        downloadButton("downloadData_summary", "Save this table as csv")
      ),
      
      mainPanel(
        width = 9,
        tableOutput('table_FV_summary_condensed')
      )
  )
}