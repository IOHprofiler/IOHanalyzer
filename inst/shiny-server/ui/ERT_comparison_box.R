ERT_comparison_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Runtime Comparisons (across functions on one dimension)</p>'), 
      width = width, collapsible = collapsible, solidHeader = TRUE, 
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
            width = 2,
            selectInput('ERTPlot.Aggr.Algs', label = 'Select which IDs to include:',
                        multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                          custom_icon() %>%
                            bs_embed_popover(
                              title = "ID selection", content = alg_select_info, 
                              placement = "auto"
                            )
                        ),
            
            selectInput('ERTPlot.Aggr.Funcs', label = 'Select which functions to aggregate over:',
                        multiple = T, selected = NULL, choices = NULL),
            
            selectInput('ERTPlot.Aggr.Mode', label = 'Select the plotting mode',
                        choices = c('radar', 'line'), selected = 'radar'),
            
            checkboxInput('ERTPlot.Aggr.Ranking', 
                          label = 'Use ranking instead of ERT-values',
                          value = T),
            
            checkboxInput('ERTPlot.Aggr.Logy', 
                          label = 'Scale y axis \\(\\log_{10}\\)',
                          value = F),
            actionButton("ERTPlot.Aggr.Refresh", "Refresh the figure and table"),
            hr(),
            
            selectInput('ERTPlot.Aggr.Format', label = 'Select the figure format',
                        choices = supported_fig_format, selected = supported_fig_format[[1]]),
            
            downloadButton('ERTPlot.Aggr.Download', label = 'Download the figure'),
            hr(),
            selectInput('ERTPlot.Aggr.TableFormat', label = 'Select the table format',
                        choices = supported_table_format, selected = supported_table_format[[1]]),
            downloadButton('ERTPlot.Aggr.DownloadTable', label = 'Download the table')
            
          ),
      
      mainPanel(
        width = 10,
        column(
          width = 12, align = "center",
          HTML_P('The <b><i>ERT</i></b> of the runtime samples across all functions. 
                  ERT is decided based on the target values in the table below,
                  with the default being the <b>best reached f(x) by any of the 
                  selected algorithms</b>. <i>Infinite ERTS</i> are shown as 
                  seperate dots on the graph.'),
          plotlyOutput.IOHanalyzer('ERTPlot.Aggr.Plot'),
          hr(),
          HTML_P("The chosen <b>target values</b> per function are as follows 
                 (double click an entry to edit it):"),
          DT::dataTableOutput("ERTPlot.Aggr.Targets"),
          hr(),
          HTML_P("The raw <b>ERT</b>-values are:"),
          DT::dataTableOutput("ERTPlot.Aggr.ERTTable")
        )
      )
    )
  )
}

#TODO: combine with other function using proper namespacing and modularity
ERT_comparison_box_dim <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Runtime Comparisons (across dimensions)</p>'), 
      width = width, collapsible = collapsible, solidHeader = TRUE, 
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput('ERTPlot.Aggr_Dim.Algs', label = 'Select which IDs to include:',
                      multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                        custom_icon() %>%
                          bs_embed_popover(
                            title = "ID selection", content = alg_select_info, 
                            placement = "auto"
                          )
                      ),
          selectInput('ERTPlot.Aggr_Dim.Mode', label = 'Select the plotting mode',
                      choices = c('radar', 'line'), selected = 'line'),
          
          checkboxInput('ERTPlot.Aggr_Dim.Ranking', 
                        label = 'Use ranking instead of ERT-values',
                        value = F),
          
          checkboxInput('ERTPlot.Aggr_Dim.Logy', 
                        label = 'Scale y axis \\(\\log_{10}\\)',
                        value = T),
          actionButton("ERTPlot.Aggr_Dim.Refresh", "Refresh the figure and table"),
          hr(),
          
          selectInput('ERTPlot.Aggr_Dim.Format', label = 'Select the figure format',
                      choices = supported_fig_format, selected = supported_fig_format[[1]]),
          
          downloadButton('ERTPlot.Aggr_Dim.Download', label = 'Download the figure'),
          hr(),
          selectInput('ERTPlot.Aggr_Dim.TableFormat', label = 'Select the table format',
                      choices = supported_table_format, selected = supported_table_format[[1]]),
          downloadButton('ERTPlot.Aggr_Dim.DownloadTable', label = 'Download the table')
          
        ),
        
        mainPanel(
          width = 10,
          column(
            width = 12, align = "center",
            HTML_P('The <b><i>ERT</i></b> of the runtime samples across all functions. 
                  ERT is decided based on the target values in the table below,
                  with the default being the <b>best reached f(x) by any of the 
                  selected algorithms</b>. <i>Infinite ERTS</i> are shown as 
                  seperate dots on the graph.'),
            plotlyOutput.IOHanalyzer('ERTPlot.Aggr_Dim.Plot'),
            hr(),
            HTML_P("The chosen <b>target values</b> per dimension are as follows 
                 (double click an entry to edit it):"),
            DT::dataTableOutput("ERTPlot.Aggr_Dim.Targets"),
            hr(),
            HTML_P("The raw <b>ERT</b>-values are:"),
            DT::dataTableOutput("ERTPlot.Aggr_Dim.ERTTable")
          )
        )
      )
  )
}