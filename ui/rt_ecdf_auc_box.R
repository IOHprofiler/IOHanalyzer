rt_ecdf_auc_box <- function() {
  box(
    title = HTML('<p style="font-size:120%;">Area Under the ECDF</p>'),  
    width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = T,
    sidebarPanel(
      width = 3,
      HTML('<p align="justify">Set the range and the granularity of
            the evenly spaced quality targets taken into account in the plot.</p>'),
      textInput('RT_AUC_FSTART', label = F_MIN_LABEL, value = ''),
      textInput('RT_AUC_FSTOP', label = F_MAX_LABEL, value = ''),
      textInput('RT_AUC_FSTEP', label = F_STEP_LABEL, value = ''),
      selectInput('FIG_FORMAT_RT_AUC', label = 'select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),
      downloadButton('FIG_DOWNLOAD_RT_AUC', label = 'download the figure')
    ),
      
    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b>area under the ECDF</b> is 
               caculated for the sequence of target values specified on the left. The displayed
               values are normalized against the maximal number of function evaluations for 
               each algorithm. Intuitively, the larger the area, the better the algorithm. 
               The displayed algorithms can be selected by clicking on the legend on the right. 
               A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.
               This also includes the option to download the plot as png file.'),
        plotlyOutput("RT_AUC", height = plotly_height, width = plotly_width2)
      )
    )
  )
}