heatmap_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Statistical Significance</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which algorithms to show.</p>'),
        selectInput('Stats.Overview.Algid', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
        textInput('Stats.Overview.Target', label = 'Target value',),
        textInput('Stats.Overview.Alpha', label = 'Alpha value', value = 0.01),
        numericInput('Stats.Overview.Samples', label = 'Bootstrap size', min = 1, max = 1000, step = 1, value = 30)
      ),
      
      mainPanel(
        width = 9,
        HTML_P('This function performs a Kolmogorov-Smirnov test on each pair of 
                algorithms in the input x to determine which algorithm gives a significantly 
                smaller running time. The resulting p-values are arranged in a matrix, where
                each cell (i, j) contains a p-value from the test with alternative hypothesis:
                the running time of algorithm i is smaller (thus better) than that of j.'),
        DT::dataTableOutput('Stats.Overview.Pmatrix'),
        plotlyOutput.IOHanalyzer('Stats.Overview.Heatmap'),
        plotOutput("Stats.Overview.Graph", height = '70vh')
      )
  )
}
