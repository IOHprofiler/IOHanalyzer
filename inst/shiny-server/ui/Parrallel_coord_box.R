Par_coord_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Parallel coordinate plot of final position</p>'),
      width = width, collapsible = collapsible, solidHeader = T,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          selectInput('ParCoordPlot.Algs', label = 'Select which algorithm to plot:',
                      multiple = F, selected = NULL, choices = NULL),
          
          hr(),
          
          selectInput('ParCoordPlot.Format', label = 'Select the figure format',
                      choices = supported_fig_format, selected = 'pdf'),
          
          downloadButton('ParCoordPlot.Download', label = 'Download the figure')
          
        ),
        
        mainPanel(
          width = 9,
          column(
            width = 12,
            align = "center",
            HTML_P('The location of the best found solution by the algorithm, for each of the runs.
                   Each x-value corresponds to one coordinate of the solution.'),
            plotlyOutput.IOHanalyzer('Parallel_Coord_Plot')
          )
        )
      )
  )
}
