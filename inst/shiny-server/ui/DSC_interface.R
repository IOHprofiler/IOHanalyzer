rt_dsc_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Deep Statistical Comparison (DSC) analysis</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('RT_Stats.DSC.Algid', 'Algorithms to compare', choices = NULL, 
                    selected = NULL, multiple = T),
        selectInput('RT_Stats.DSC.Funcid', 'Functions to use', choices = NULL, 
                    selected = NULL, multiple = T),
        selectInput('RT_Stats.DSC.Dim', 'Dimensions to use', choices = NULL, 
                    selected = NULL, multiple = T),
        textInput('RT_Stats.DSC.Alpha', 
                  label = "Threshold for statistical significance", 
                  value = 0.05),
        # selectInput('RT_Stats.DSC.Reference_alg', 'Reference algorithm', choices = NULL, 
        #             selected = NULL, multiple = F),
        selectInput('RT_Stats.DSC.method', 'Post-hoc Method', choices = 
                      c('Holm', 'Hochberg', 'unadjusted P'), 
                    selected = 'Holm', multiple = F),
        actionButton('RT_Stats.DSC.Create', 'Create Comparison'),
        hr(),
        selectInput('RT_Stats.DSC.Format', label = 'Select the figure format',
                    choices = supported_fig_format, selected = 'pdf'),
        
        downloadButton('RT_Stats.DSC.Download', label = 'Download the figure'),
        hr(),
        selectInput('RT_Stats.DSC.TableFormat', label = 'Select the table format',
                    choices = c('csv','tex'), selected = 'csv'),
        downloadButton('RT_Stats.DSC.DownloadTable', label = 'Download the table')
      ),
      
      mainPanel(
        width = 9,
        HTML_P('The <b>DSC</b> comparison is described in the paper: ...'),
        HTML_P("The chosen <b>target function values</b> per (function, dimension)-pair are as follows 
                 (double click an entry to edit it):"),
        DT::dataTableOutput("RT_Stats.DSC.Targets"),
        hr(), 
        HTML_P("The results of the ranking are:"),
        plotlyOutput.IOHanalyzer("RT_Stats.DSC.PosthocViz"),
        DT::dataTableOutput('RT_Stats.DSC.PosthocTable')
      )
  )
}

fv_dsc_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Deep Statistical Comparison (DSC) analysis</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('FV_Stats.DSC.Algid', 'Algorithms to compare', choices = NULL, 
                    selected = NULL, multiple = T),
        selectInput('FV_Stats.DSC.Funcid', 'Functions to use', choices = NULL, 
                    selected = NULL, multiple = T),
        selectInput('FV_Stats.DSC.Dim', 'Dimensions to use', choices = NULL, 
                    selected = NULL, multiple = T),
        textInput('FV_Stats.DSC.Alpha', 
                  label = "Threshold for statistical significance", 
                  value = 0.05),
        # selectInput('FV_Stats.DSC.Reference_alg', 'Reference algorithm', choices = NULL, 
        #             selected = NULL, multiple = F),
        selectInput('FV_Stats.DSC.method', 'Post-hoc Method', choices = 
                      c('Holm', 'Hochberg', 'unadjusted P'), 
                    selected = 'Holm', multiple = F),
        actionButton('FV_Stats.DSC.Create', 'Create Comparison'),
        hr(),
        selectInput('FV_Stats.DSC.Format', label = 'Select the figure format',
                    choices = supported_fig_format, selected = 'pdf'),
        
        downloadButton('FV_Stats.DSC.Download', label = 'Download the figure'),
        hr(),
        selectInput('FV_Stats.DSC.TableFormat', label = 'Select the table format',
                    choices = c('csv','tex'), selected = 'csv'),
        downloadButton('FV_Stats.DSC.DownloadTable', label = 'Download the table')
      ),
      
      mainPanel(
        width = 9,
        HTML_P('The <b>DSC</b> comparison is described in the paper: ...'),
        HTML_P("The chosen <b>budget values</b> per (function, dimension)-pair are as follows 
                 (double click an entry to edit it):"),
        DT::dataTableOutput("FV_Stats.DSC.Targets"),
        hr(), 
        HTML_P("The results of the ranking are:"),
        plotlyOutput.IOHanalyzer("FV_Stats.DSC.PosthocViz"),
        DT::dataTableOutput('FV_Stats.DSC.PosthocTable')
      )
  )
}