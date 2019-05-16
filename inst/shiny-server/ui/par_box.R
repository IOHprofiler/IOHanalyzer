par_expected_value_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Expected Parameter Value (per function)</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = TRUE, status = "primary",
    sidebarPanel(
      width = 3,
      HTML('<p style="font-size:120%;">Range of the function values (\\(x\\) axis)</p>'),

      textInput('PAR.Plot.Min', label = F_MIN_LABEL, value = ''),
      textInput('PAR.Plot.Max', label = F_MAX_LABEL, value = ''),

      selectInput('PAR.Plot.Algs', 'Algorithms', choices = NULL, selected = NULL, multiple = T),
      selectInput('PAR.Plot.show.mean', label = 'Mean/median',
                  choices = c('mean', 'median'),
                  selected = 'mean'),

      checkboxInput('PAR.Plot.Logx',
                    label = 'Scale x axis log10',
                    value = T),

      checkboxInput('PAR.Plot.Logy',
                    label = 'Scale y axis log10',
                    value = T),

      selectInput('PAR.Plot.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),
      downloadButton('PAR.Plot.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b><i>mean or median</i></b> of internal parameters of the algorithm
                      found with a fixed-budget of evaluations are depicted against the budget.
               The displayed elements can be switched on and off by clicking on the legend on the right.
               A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput.IOHanalyzer('PAR_PER_FUN')
      )
    )
   )
}

par_summary_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Parameter Statistics at Chosen Target Values</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML_P('Set the range and the granularity of the results.
             The table will show fixed-target parameter values for evenly spaced target values.'),

      textInput('PAR.Summary.Min', label = F_MIN_LABEL, value = ''),
      textInput('PAR.Summary.Max', label = F_MAX_LABEL, value = ''),
      textInput('PAR.Summary.Step', label = F_STEP_LABEL, value = ''),
      checkboxInput('PAR.Summary.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                       Once toggled, only \\(f_{\\text{min}}\\) is
                                                       used to generate the table on the right.</p>'), value = FALSE),
      selectInput('PAR.Summary.Algid', 'Algorithms', choices = NULL, selected = NULL),
      selectInput('PAR.Summary.Param', 'Parameters', choices = NULL, selected = NULL),
      selectInput('PAR.Summary.Format', 'Format', choices = c('csv','tex'), selected = 'csv'),
      downloadButton("PAR.Summary.Download", "Save this table as csv")
      ),

    mainPanel(
      width = 9,
      HTML(paste0('<div style="font-size:120%;">',
                  includeMarkdown('RMD/PAR_SUMMARY_TABLE.Rmd'), '</div>')),
      dataTableOutput('table_PAR_summary')
    )
  )
}

par_sample_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Parameter Sample at Chosen Target Values</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML_P('Set the range and the granularity of the results.
                                The table will show fixed-target parameter values for evenly spaced target values.'),

      textInput('PAR.Sample.Min', label = F_MIN_LABEL, value = ''),
      textInput('PAR.Sample.Max', label = F_MAX_LABEL, value = ''),
      textInput('PAR.Sample.Step', label = F_STEP_LABEL, value = ''),
      checkboxInput('PAR.Sample.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                                             Once toggled, only \\(f_{\\text{min}}\\) is
                                                                             used to generate the table on the right.</p>'), value = FALSE),
      selectInput('PAR.Sample.Algid', 'Algorithms', choices = NULL, selected = NULL),
      selectInput('PAR.Sample.Param', 'Parameters', choices = NULL, selected = NULL),
      selectInput('PAR.Sample.Format', 'Format of the csv',
                  choices = c('long', 'wide'), selected = 'wide'),
      selectInput('PAR.Sample.FileFormat', 'File-Format', choices = c('csv','tex'), selected = 'csv'),
      downloadButton("PAR.Sample.Download", "Save this table")
    ),

    mainPanel(
      width = 9,
      HTML('<p style="font-size:120%;">This table shows for each selected algorithm \\(A\\),
           each selected target value \\(f(x)\\), and each run \\(r\\) the parameter value
           observed when the target value \\(f(x)\\) is reached for the first time.</p>'),
      dataTableOutput('table_PAR_SAMPLE')
    )
  )
}
