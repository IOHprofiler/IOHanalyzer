ERT_comparison_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Runtime comparisons</p>'), 
      width = width, collapsible = collapsible, solidHeader = TRUE, 
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
            width = 3,
            selectInput('ERTPlot.Aggr.Mode', label = 'Select the plotting mode',
                        choices = c('radar','line'), selected = 'radar'),
            
            selectInput('ERTPlot.Aggr.Aggregator', label = 'Create plot for all?',
                        choices = c('Functions','Dimensions'), selected = 'Functions'),
            
            checkboxInput('ERTPlot.Aggr.Ranking', 
                          label = 'Use ranking instead of ERT-values',
                          value = T),
            
            checkboxInput('ERTPlot.Aggr.Logy', 
                          label = 'scale y axis log10',
                          value = F),
            
            textInput('ERTPlot.Aggr.Targets', label = 'choose the ERT-targets (comma-separated)'),
            selectInput('ERTPlot.Aggr.Format', label = 'Select the figure format',
                        choices = supported_fig_format, selected = 'pdf'),
            
            downloadButton('ERTPlot.Aggr.Download', label = 'Download the figure')
          ),
      
      mainPanel(
        width = 9,
        column(
          width = 12, align = "center",
          plotlyOutput('ERTPlot.Aggr.Plot', height = plotly_height, width = plotly_width2)
        )
      )
    )
  )
}