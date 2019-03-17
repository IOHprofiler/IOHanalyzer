fv_ecdf_single_budget_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution of 
                 the Fixed-Budget Values: Single Budgets</p>'), 
    width = width, collapsible = collapsible, collapsed = collapsed, 
    solidHeader = TRUE, status = "primary",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        HTML('Select the budgets for which EDCF curves are displayed '),
        textInput('FCE_ECDF_RT', label = HTML('<p>\\(B_1\\)</p>'), value = ''),
        checkboxInput('FCE_ECDF_semilogx', label = 'scale x axis log10', value = F)
      ), 
        
      mainPanel(
        width = 9,
        column(
          width = 12, align = "center",
          HTML_P('Each EDCF curve shows the proportion of the runs that have found 
                  within the given budget B a solution of at least the required target 
                  value given by the x-axis. The displayed curves can be selected
                  by clicking on the legend on the right. A <b>tooltip</b> and <b>toolbar</b> 
                  appears when hovering over the figure.</p>'),
          plotlyOutput("FCE_ECDF_PER_TARGET", height = plotly_height, width = plotly_width2)
        )
      )
    )
  )
}

fv_ecdf_agg_budgets_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution 
                    of the Fixed-Budget Values: Aggregation</p>'), 
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary", 
    sidebarPanel(
      width = 3,
      HTML('<p align="justify">Set the range and the granularity of the budgets 
           taken into account in the ECDF curve. The plot will show the ECDF curves 
           for evenly spaced budgets.</p>'),
      
      textInput('FCE_ECDF_RT_MIN', label = RT_MIN_LABEL, value = ''),
      textInput('FCE_ECDF_RT_MAX', label = RT_MAX_LABEL, value = ''),
      textInput('FCE_ECDF_RT_STEP', label = RT_STEP_LABEL, value = ''),
        
      checkboxInput('FCE_ECDF_per_target', label = 'show ECDF for each budget', value = F),
      
      checkboxInput('FCE_ECDF_AGGR_semilogx', label = 'scale x axis log10', value = F),
      selectInput('FIG_FORMAT_FV_ECDF_AGGR', label = 'select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),
      
      downloadButton('FIG_DOWNLOAD_FV_ECDF_AGGR', label = 'download the figure')
    ),
      
    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The evenly spaced budget values are:'),
        verbatimTextOutput('FCE_RT_GRID'),
        
        HTML_P('The fraction of (run,budget) pairs \\((i,B)\\) satisfying that the best 
                solution that the algorithm has found in the \\(i\\)-th run within the 
                first \\(B\\) evaluations has quality at <b>most</b> \\(v\\) is plotted 
                against the target value \\(v\\). The displayed elements can be switched 
                on and off by clicking on the legend on the right. A <b>tooltip</b> and 
                <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput('FCE_ECDF_AGGR', height = plotly_height, width = plotly_width2)
      )
    )
  )
}

fv_ecdf_auc_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Area Under the ECDF</p>'),  
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary", 
    sidebarPanel(
      width = 3,
      HTML('<p align="justify">Set the range and the granularity of the evenly spaced budgets.</p>'),
      textInput('FCE_AUC_RT_MIN', label = RT_MIN_LABEL, value = ''),
      textInput('FCE_AUC_RT_MAX', label = RT_MAX_LABEL, value = ''),
      textInput('FCE_AUC_RT_STEP', label = RT_STEP_LABEL, value = ''),
      selectInput('FIG_FORMAT_FV_AUC', label = 'select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),
      downloadButton('FIG_DOWNLOAD_FV_AUC', label = 'download the figure')
    ),
    
    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b>area under the ECDF</b> is 
                caculated for the sequence of budget values specified on the left. The displayed
                values are normalized against the maximal target value recorded for 
                each algorithm. Intuitively, the <b>smaller</b> the area, the <b>better</b> the algorithm. 
                The displayed algorithms can be selected by clicking on the legend on the right. 
                A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput("FCE_AUC", height = plotly_height, width = plotly_width2)
      )
    )
  )
}