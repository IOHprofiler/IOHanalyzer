fv_dsc_box_rank <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Deep Statistical Comparison (DSC) 
                   analysis - Ranking per Function</p>'),
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
        hr(),
        numericInput('FV_Stats.DSC.Alpha_rank', 
                  label = "Threshold for statistical significance", 
                  value = 0.05, min = 0, max = 0.5),
        numericInput('FV_Stats.DSC.Epsilon_rank', 
                  label = "Threshold for practical significance (pDSC)", 
                  value = 0),
        numericInput('FV_Stats.DSC.MCsamples_rank', 
                  label = "Number of monte carlo samples to use in the DSC procdure", 
                  value = 0),
        selectInput("FV_Stats.DSC.Test_rank", "Test Type",  
                    choices = c("Anderson-Darling", "Kolmogorov-Smirnov"), 
                    selected = "Anderson-Darling"),
        actionButton('FV_Stats.DSC.Create_rank', 'Create Ranking'),
        hr(),
        selectInput('FV_Stats.DSC.Format_rank', label = 'Select the figure format',
                    choices = supported_fig_format, selected = 'pdf'),
        
        downloadButton('FV_Stats.DSC.Download_rank', label = 'Download the figure')
      ),
      
      mainPanel(
        width = 9,
        HTML_P("The <b>DSC</b> comparison is described in the paper: 'DSCTool: A web-service-based 
               framework for statistical comparison of stochastic optimization algorithms.' by 
               T. Eftimov et al.
               This is the first of 3 parts of the process: the per-function ranking procedure. 
               The two other processes are the omnibus test and the post-hoc processing, 
               which are shown in the two boxes below this one. 
               Note that both of these are impacted by the settings selected for this 
               ranking procedure!"),
        HTML_P("The chosen <b>budget values</b> per (function, dimension)-pair are as follows 
                 (double click an entry to edit it):"),
        DT::dataTableOutput("FV_Stats.DSC.Targets"),
        hr(), 
        HTML_P("The results of the ranking are shown in the following plot, using the visualization techniques 
               as described in the paper: 'PerformViz: A Machine Learning Approach to Visualize and
                Understand the Performance of Single-objective Optimization
                Algorithms' By T. Eftimov et al."),
        plotOutput("FV_Stats.DSC.PerformViz", height = "800px")
      )
  )
}

fv_dsc_box_omni <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Deep Statistical Comparison (DSC) 
                   analysis - Omnibus Test</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('FV_Stats.DSC.Omni_options', "Select which statistical test to use",
                    choices = NULL, selected = NULL),
        numericInput('FV_Stats.DSC.Alpha_omni', 
                     label = "Threshold for statistical significance", 
                     value = 0.05, min = 0, max = 0.5),
        actionButton('FV_Stats.DSC.Create_omni', 'Perform Omnibus Test')
      ),
      
      mainPanel(
        width = 9,
        HTML_P('This is the result of the omnibus test on the data from the ranking procedure above. 
                Note that this test has to be performed before doing the post-hoc comparison!'),
        hr(), 
        textOutput('FV_Stats.DSC.Output_omni')
      )
  )
}

fv_dsc_box_posthoc <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Deep Statistical Comparison (DSC) 
                   analysis - Posthoc comparison</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('FV_Stats.DSC.Posthoc_test', 'Post-hoc Test', choices = 
                      c('friedman', 'friedman-aligned-rank'), 
                    selected = 'friedman', multiple = F),
        selectInput('FV_Stats.DSC.Posthoc_method', 'Post-hoc P-value Correction Method', choices = 
                      c('Holm', 'Hochberg', 'unadjusted P'), 
                    selected = 'Holm', multiple = F),
        numericInput('FV_Stats.DSC.Alpha_posthoc', 
                     label = "Threshold for statistical significance", 
                     value = 0.05, min = 0, max = 0.5),
        actionButton('FV_Stats.DSC.Create_posthoc', 'Create Comparison'),
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
        HTML_P("The results of the post-hoc comparison are:"),
        plotlyOutput.IOHanalyzer("FV_Stats.DSC.PosthocViz"),
        DT::dataTableOutput('FV_Stats.DSC.PosthocTable')
      )
  )
}

rt_dsc_box_rank <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Deep Statistical Comparison (DSC) 
                   analysis - Ranking per Function</p>'),
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
        hr(),
        selectInput('RT_Stats.DSC.Value_type', "Select which type of hitting times to use",
                    choices = c('PAR-10', 'ERT', 'Remove-na', 'PAR-1')),
        numericInput('RT_Stats.DSC.Alpha_rank', 
                     label = "Threshold for statistical significance", 
                     value = 0.05, min = 0, max = 0.5),
        numericInput('RT_Stats.DSC.Epsilon_rank', 
                     label = "Threshold for practical significance (pDSC)", 
                     value = 0),
        numericInput('RT_Stats.DSC.MCsamples_rank', 
                     label = "Number of monte carlo samples to use in the DSC procdure", 
                     value = 0),
        selectInput("RT_Stats.DSC.Test_rank", "Test Type",  
                    choices = c("Anderson-Darling", "Kolmogorov-Smirnov"), 
                    selected = "Anderson-Darling"),
        actionButton('RT_Stats.DSC.Create_rank', 'Create Ranking'),
        hr(),
        selectInput('RT_Stats.DSC.Format_rank', label = 'Select the figure format',
                    choices = supported_fig_format, selected = 'pdf'),
        
        downloadButton('RT_Stats.DSC.Download_rank', label = 'Download the figure')
      ),
      
      mainPanel(
        width = 9,
        HTML_P("The <b>DSC</b> comparison is described in the paper: 'DSCTool: A web-service-based 
               framework for statistical comparison of stochastic optimization algorithms.' by 
               T. Eftimov et al.
               This is the first of 3 parts of the process: the per-function ranking procedure. 
               The two other processes are the omnibus test and the post-hoc processing, 
               which are shown in the two boxes below this one. 
               Note that both of these are impacted by the settings selected for this 
               ranking procedure!"),
        HTML_P("The chosen <b>budget values</b> per (function, dimension)-pair are as follows 
                 (double click an entry to edit it):"),
        DT::dataTableOutput("RT_Stats.DSC.Targets"),
        hr(), 
        HTML_P("The results of the ranking are shown in the following plot, using the visualization techniques 
               as described in the paper: 'PerformViz: A Machine Learning Approach to Visualize and
                Understand the Performance of Single-objective Optimization
                Algorithms' By T. Eftimov et al."),
        plotOutput("RT_Stats.DSC.PerformViz", height = "800px")
      )
  )
}

rt_dsc_box_omni <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Deep Statistical Comparison (DSC) 
                   analysis - Omnibus Test</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('RT_Stats.DSC.Omni_options', "Select which statistical test to use",
                    choices = NULL, selected = NULL),
        numericInput('RT_Stats.DSC.Alpha_omni', 
                     label = "Threshold for statistical significance", 
                     value = 0.05, min = 0, max = 0.5),
        actionButton('RT_Stats.DSC.Create_omni', 'Perform Omnibus Test')
      ),
      
      mainPanel(
        width = 9,
        HTML_P('This is the result of the omnibus test on the data from the ranking procedure above. 
                Note that this test has to be performed before doing the post-hoc comparison!'),
        hr(), 
        textOutput('RT_Stats.DSC.Output_omni')
      )
  )
}

rt_dsc_box_posthoc <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Deep Statistical Comparison (DSC) 
                   analysis - Posthoc comparison</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('RT_Stats.DSC.Posthoc_test', 'Post-hoc Test', choices = 
                      c('friedman', 'friedman-aligned-rank'), 
                    selected = 'friedman', multiple = F),
        selectInput('RT_Stats.DSC.Posthoc_method', 'Post-hoc Method', choices = 
                      c('Holm', 'Hochberg', 'unadjusted P'), 
                    selected = 'Holm', multiple = F),
        numericInput('RT_Stats.DSC.Alpha_posthoc', 
                     label = "Threshold for statistical significance", 
                     value = 0.05, min = 0, max = 0.5),
        actionButton('RT_Stats.DSC.Create_posthoc', 'Create Comparison'),
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
        HTML_P("The results of the post-hoc comparison are:"),
        plotlyOutput.IOHanalyzer("RT_Stats.DSC.PosthocViz"),
        DT::dataTableOutput('RT_Stats.DSC.PosthocTable')
      )
  )
}