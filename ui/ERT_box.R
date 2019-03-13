ERT_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Runtime (per function)</p>'), 
      width = width, collapsible = collapsible, solidHeader = T,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML('<p style="font-size:120%;">Range of the displayed target values</p>'),
          
          textInput('ERT_FSTART', 
                    label = F_MIN_LABEL, 
                    value = ''),
          textInput('ERT_FSTOP', 
                    label = F_MAX_LABEL, 
                    value = ''),
          
          checkboxInput('show.ERT', 
                        label = 'show/hide ERT',
                        value = T),
          
          checkboxInput('show.mean', 
                        label = 'show/hide mean',
                        value = F),
          
          checkboxInput('show.CI', 
                        label = 'show/hide mean +/- sd',
                        value = F),
          
          checkboxInput('show.median', 
                        label = 'show/hide median',
                        value = F),
          
          checkboxInput('semilogx', 
                        label = 'scale x axis log10',
                        value = T),
          
          checkboxInput('semilogy', 
                        label = 'scale y axis log10',
                        value = T),
          
          checkboxInput('show_grad',
                        label = 'show runs intensity',
                        value = F),
          
          conditionalPanel(
            condition = "input.show_grad == true",
              column(
                width = 10, offset = 1,
                sliderInput('show.intensity', label = "Runs intensity(%)", min = -1, 
                            max = 1, value = 0, step = 0.1)
              )
          ),
          
          checkboxInput('show_all',
                        label = 'show/hide multiple runs',
                        value = F),
          
          conditionalPanel(
            condition = "input.show_all == true",
            column(
               width = 10, offset = 1,
               sliderInput('show.density',
                           label = "Runs density(%)",
                           min = 1, max = 100, value = 50, step = 1),
               checkboxInput('show.best_of_all',
                             label = 'show/hide best run',
                             value = F),
               checkboxInput('show.pareto_optima',
                             label = 'show/hide pareto optimal front',
                             value = F)
             )
          ),
          
          selectInput('FIG_FORMAT_ERT_PER_FUN', label = 'select the figure format',
                      choices = supported_fig_format, selected = 'pdf'),
          
          downloadButton('FIG_DOWNLOAD_ERT_PER_FUN', label = 'download the figure')
        ),
        
        mainPanel(
          width = 9,
          column(
            width = 12, 
            align = "center",
            HTML_P('The <b><i>mean, median 
                 and standard deviation</i></b> of the runtime samples 
                 are depicted against the best objective values. 
                 The displayed elements (mean, median, standard deviations) 
                 can be switched on and off by clicking on the legend on the right. 
                 A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
            plotlyOutput('ERT_PER_FUN', height = plotly_height, width = plotly_width2)
          )
        )
      )
  )
}