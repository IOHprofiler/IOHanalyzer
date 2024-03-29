ERT_agg_box <- function(width = 12, height = '600px', collapsible = T, 
                        collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Expected Runtime (ERT): all functions</p>'), 
    width = width, collapsible = collapsible, solidHeader = T, 
    status = "primary", collapsed = collapsed,
    sidebarLayout(
      sidebarPanel(
        width = 2,
        selectInput('ERTPlot.Multi.Algs', label = 'Select which IDs to include:', 
                    multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "ID selection", content = alg_select_info, 
                          placement = "auto"
                        )
                    ),
        selectInput('ERTPlot.Multi.Funcs', label = 'Select which Functions to include:', 
                    multiple = T, selected = NULL, choices = NULL),        
        checkboxInput('ERTPlot.Multi.Logx', 
                      label = 'Scale x axis \\(\\log_{10}\\)',
                      value = T),
        
        checkboxInput('ERTPlot.Multi.Logy', 
                      label = 'Scale y axis \\(\\log_{10}\\)',
                      value = T),
        
        actionButton('ERTPlot.Multi.PlotButton', label = 'Refresh the figure'),
        hr(),
        selectInput('ERTPlot.Multi.Format', label = 'Select the figure format',
                    choices = supported_fig_format, selected = supported_fig_format[[1]]),
        
        downloadButton('ERTPlot.Multi.Download', label = 'Download the figure')
      ),
      
      mainPanel(
        width = 10,
        column(
          width = 12, align = "center",
          HTML_P('The <b><i>ERT</i></b> is shown against the target 
                 values for all functions in the selected dimension.'),
          plotlyOutput.IOHanalyzer('ERTPlot.Multi.Plot', aspect_ratio = 1)
        )
      )
    )
  )
}
