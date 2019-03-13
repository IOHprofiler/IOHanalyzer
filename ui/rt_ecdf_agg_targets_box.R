rt_ecdf_agg_targets_box <- function() {
  box(title = HTML('<p style="font-size:120%;">Aggregated Empirical Cumulative 
                                      Distribution: Single function</p>'), 
      width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = T,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Set the range and the granularity 
             of the quality targets taken into account in the ECDF curve. 
             The plot will show the ECDF curves for evenly spaced target values.</p>'),
        textInput('RT_fstart', label = F_MIN_LABEL, value = ''),
        textInput('RT_fstop', label = F_MAX_LABEL, value = ''),
        textInput('RT_fstep', label = F_STEP_LABEL, value = ''),
        checkboxInput('RT_ECDF_per_target',
                      label = 'show ECDFs for each target',
                      value = F),
        checkboxInput('RT_ECDF_AGGR_semilogx', 
                      label = 'scale x axis log10',
                      value = F),
        
        selectInput('FIG_FORMAT_RT_ECDF_AGGR', label = 'select the figure format',
                    choices = supported_fig_format, selected = 'pdf'),
        
        downloadButton('FIG_DOWNLOAD_RT_ECDF_AGGR', label = 'download the figure')
        ),
      
      mainPanel(width = 9,
                column(width = 12, align = "center",
                       HTML_P('The evenly spaced target values are:'),
                       verbatimTextOutput('RT_GRID'),
                       HTML_P('The fraction of (run,target value)
                              pairs \\((i,v)\\) satisfying that the best solution that the algorithm has 
                              found in the \\(i\\)-th run within the given time budget \\(t\\) has quality at least
                              \\(v\\) is plotted against the available budget \\(t\\). The displayed elements can be switched 
                              on and off by clicking on the legend on the right. A <b>tooltip</b> 
                              and <b>toolbar</b> appears when hovering over the figure.'),
                       plotlyOutput('RT_ECDF_AGGR', height = plotly_height, width = plotly_width2),
                       hr()
                       )
                )
      
      # HTML('<p>The <b>Kolmogorov–Smirnov test</b> is used to investigate
      # how the ditribution of runtimes of one algorithm differs from the other:</p><br>'),
      # column(10, align = "center", verbatimTextOutput('ks'))
      )
}