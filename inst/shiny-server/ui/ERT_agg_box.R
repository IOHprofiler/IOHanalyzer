ERT_agg_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Runtime (aggregated)</p>'), 
      width = width, collapsible = collapsible, solidHeader = T, 
      status = "primary", collapsed = collapsed,
      sidebarLayout(
          sidebarPanel(
            width = 3,
            
            selectInput('ERTPlot.Multi.Algs', label = 'Select the algs to show', 
                        multiple = T, selected = NULL, choices = NULL),
            
            checkboxInput('ERTPlot.Multi.Logx', 
                          label = 'scale x axis log10',
                          value = T),
            
            checkboxInput('ERTPlot.Multi.Logy', 
                          label = 'scale y axis log10',
                          value = T),
            
            selectInput('ERTPlot.Multi.Mode', label = 'Select the plotting mode',
                        choices = c('overlay','subplot'), selected = 'subplot'),
            
            selectInput('ERTPlot.Multi.Aggregator', label = 'Create plot for all?',
                        choices = c('Functions','Dimensions'), selected = 'Functions'),
            
            actionButton('ERTPlot.Multi.PlotButton', label = 'Plot the figure'),
            
            selectInput('ERTPlot.Multi.Format', label = 'Select the figure format',
                        choices = supported_fig_format, selected = 'pdf'),
            
            downloadButton('ERTPlot.Multi.Download', label = 'Download the figure')
          ),
      
      mainPanel(
        width = 9,
        column(
          width = 12, align = "center",
          plotlyOutput.IOHanalyzer('ERTPlot.Multi.Plot')
        )
      )
    )
  )
}