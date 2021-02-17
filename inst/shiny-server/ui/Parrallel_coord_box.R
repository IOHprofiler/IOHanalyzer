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
                      choices = supported_fig_format, selected = supported_fig_format[[1]]),
          
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

coord_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Animated Coordinate Plot</p>'),
      width = width, collapsible = collapsible, solidHeader = T,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          
          selectInput('CoordPlot.Algs', label = 'Select which algorithms to plot:',
                      multiple = T, selected = NULL, choices = NULL),
          numericInput('CoordPlot.Iid', label = 'Select which instance to show:', value = 1),
          numericInput('CoordPlot.C1', label = 'Select the first coordinate to show:', value = 0),
          numericInput('CoordPlot.C2', label = 'Select the second coordinate to show:', value = 1),
          numericInput('CoordPlot.Gen_size', label = "Generation size (For animation):", value = 10),
          checkboxInput('CoordPlot.PCA', label = "Use PCA instead of coordinated", value = F),
          hr(),
          
          selectInput('CoordPlot.Format', label = 'Select the figure format',
                      choices = supported_fig_format, selected = supported_fig_format[[1]]),
          
          downloadButton('CoordPlot.Download', label = 'Download the figure')
          
        ),
        
        mainPanel(
          width = 9,
          column(
            width = 12,
            align = "center",
            HTML_P('The location of the best found solution by the algorithm, for each of the runs.
                   Each x-value corresponds to one coordinate of the solution.'),
            plotlyOutput.IOHanalyzer('Coord_Plot')
          )
        )
      )
  )
}
