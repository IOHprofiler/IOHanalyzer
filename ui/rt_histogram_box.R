rt_histogram_box <- function(width = 12, collapsed = T) {
  box(title = 'Histogram of Fixed-Target Runtimes', 
      width = 12, collapsible = TRUE, solidHeader = TRUE, collapsed = collapsed,
      status = "primary",
      sidebarPanel(
        width = 2,
        textInput('RT_PMF_HIST_FTARGET', label = HTML('Select the target value'), 
                  value = ''),
        
        HTML('Choose whether the histograms are <b>overlaid</b> in one plot 
                              or <b>separated</b> in several subplots:'),
        selectInput('ERT_illu_mode', '', 
                    choices = c("overlay", "subplot"), 
                    selected = 'subplot'),
        
        selectInput('FIG_FORMAT_RT_HIST', label = 'select the figure format',
                    choices = supported_fig_format, selected = 'pdf'),
        
        downloadButton('FIG_DOWNLOAD_RT_HIST', label = 'download the figure')
      ),
      
      mainPanel(
        width = 10,
        column(width = 12, align = "center",
               HTML_P('This histogram counts how many runs needed between 
                                    \\(t\\) and \\(t+1\\) function evaluations. The bins
                                    \\([t,t+1)\\) are chosen automatically. The bin size is determined
                                    by the so-called <b>Freedman–Diaconis rule</b>: \\(\\text{Bin size}=
                                    2\\frac{Q_3 - Q_1}{\\sqrt[3]{n}}\\), where \\(Q_1, Q_3\\) are the \\(25\\%\\) 
                                    and \\(75\\%\\) percentile of the runtime and \\(n\\) is the sample size.
                                    The displayed algorithms can be selected by clicking on the legend on the right. 
                                    A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
               plotlyOutput('RT_HIST', height = plotly_height2, width = plotly_width2)
        )
      )
  )
}