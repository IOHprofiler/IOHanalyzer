ERT_comparison_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Runtime Comparisons</p>'), 
      width = width, collapsible = collapsible, solidHeader = TRUE, 
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput('ERTPlot.Aggr.Algs', label = 'Select which algorithms to plot:',
                        multiple = T, selected = NULL, choices = NULL),
            selectInput('ERTPlot.Aggr.Mode', label = 'Select the plotting mode',
                        choices = c('radar', 'line'), selected = 'radar'),
            
            checkboxInput('ERTPlot.Aggr.Ranking', 
                          label = 'Use ranking instead of ERT-values',
                          value = T),
            
            checkboxInput('ERTPlot.Aggr.Logy', 
                          label = 'Scale y axis \\(\\log_{10}\\)',
                          value = F),
            
            selectInput('ERTPlot.Aggr.Format', label = 'Select the figure format',
                        choices = supported_fig_format, selected = 'pdf'),
            
            downloadButton('ERTPlot.Aggr.Download', label = 'Download the figure')
          ),
      
      mainPanel(
        width = 10,
        column(
          width = 12, align = "center",
          plotlyOutput.IOHanalyzer('ERTPlot.Aggr.Plot')
        )
      )
    )
  )
}