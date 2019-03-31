ERT_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Runtime (ERT): single function</p>'),
      width = width, collapsible = collapsible, solidHeader = T,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML('<p style="font-size:120%;">Range of the displayed target values</p>'),

          textInput('ERTPlot.Min',
                    label = F_MIN_LABEL,
                    value = ''),
          textInput('ERTPlot.Max',
                    label = F_MAX_LABEL,
                    value = ''),

          checkboxInput('ERTPlot.show.ERT',
                        label = 'show/hide ERT',
                        value = T),

          checkboxInput('ERTPlot.show.mean',
                        label = 'show/hide mean',
                        value = F),

          checkboxInput('ERTPlot.show.CI',
                        label = 'show/hide mean +/- sd',
                        value = F),

          checkboxInput('ERTPlot.show.median',
                        label = 'show/hide median',
                        value = F),

          checkboxInput('ERTPlot.semilogx',
                        label = 'scale x axis log10',
                        value = T),

          checkboxInput('ERTPlot.semilogy',
                        label = 'scale y axis log10',
                        value = T),

          checkboxInput('ERTPlot.show.grad',
                        label = 'show runs intensity',
                        value = F),

          conditionalPanel(
            condition = 'input["ERTPlot.show.grad"] == true',
            column(
              width = 11, offset = 1,
              sliderInput('ERTPlot.show.intensity', label = "Runs intensity(%)",
                          min = -1, max = 1, value = 0, step = 0.1)
            )
          ),

          checkboxInput('ERTPlot.show_all',
                        label = 'show/hide multiple runs',
                        value = F),
          conditionalPanel(condition = 'input["ERTPlot.show_all"] == true',

                           fluidRow(column(
                             11,
                             offset = 1,
                             sliderInput('ERTPlot.show.density',
                                         label = "Runs density(%)",
                                         min = 1, max = 100, value = 100, step = 1),
                             checkboxInput('ERTPlot.show.best_of_all',
                                           label = 'show/hide best run',
                                           value = F),
                             checkboxInput('ERTPlot.show.pareto_optima',
                                           label = 'show/hide pareto optimal front',
                                           value = F)
                           ))),

          selectInput('ERTPlot.Format', label = 'select the figure format',
                      choices = supported_fig_format, selected = 'pdf'),

          downloadButton('ERTPlot.Download', label = 'download the figure')

          # checkboxInput('show.instance',
          #               label = 'show each independent run',
          #               value = F)
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
            plotlyOutput.IOHanalyzer('ERT_PER_FUN')
          )
        )
      )
  )
}
