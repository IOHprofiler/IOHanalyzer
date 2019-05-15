rt_ecdf_single_target_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution: Single target</p>'),
      width = width, collapsible = collapsible, solidHeader = TRUE,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML('Select the target values for which EDCF curves are displayed'),
          textInput('RTECDF.Single.Target', label = HTML('<p>\\(f_{target}\\)</p>'),
                    value = ''),

          checkboxInput('RTECDF.Single.Logx', label = 'scale x axis log10', value = F)
        ),

        mainPanel(
          width = 9,
          column(
            width = 12,
            align = "center",
            HTML_P('Each EDCF curve shows the proportion of the runs
                    that have found a solution of at least the required
                    target value within the budget given by the \\(x\\)-axis.
                    The displayed curves can be selected by clicking on the legend on the right. A <b>tooltip</b>
                    and <b>toolbar</b> appears when hovering over the figure.
                    This also includes the option to download the plot as png file.'),
            plotlyOutput.IOHanalyzer("RT_ECDF")
          )
        )
      )
  )
}

rt_ecdf_agg_targets_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Aggregated Empirical Cumulative
                  Distribution: Single function</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML('<p align="justify">Set the range and the granularity
           of the quality targets taken into account in the ECDF curve.
           The plot will show the ECDF curves for evenly spaced target values.</p>'),
      textInput('RTECDF.Multi.Min', label = F_MIN_LABEL, value = ''),
      textInput('RTECDF.Multi.Max', label = F_MAX_LABEL, value = ''),
      textInput('RTECDF.Multi.Step', label = F_STEP_LABEL, value = ''),
      checkboxInput('RTECDF.Multi.Targets',
                    label = 'show ECDFs for each target',
                    value = F),
      checkboxInput('RTECDF.Multi.Logx',
                    label = 'scale x axis log10',
                    value = F),

      selectInput('RTECDF.Multi.Format', label = 'select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),

      downloadButton('RTECDF.Multi.Download', label = 'download the figure')
      ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
       HTML_P('The evenly spaced target values are:'),
       verbatimTextOutput('RT_GRID'),
       HTML_P('The fraction of (run,target value)
              pairs \\((i,v)\\) satisfying that the best solution that the algorithm has
              found in the \\(i\\)-th run within the given time budget \\(t\\) has quality at least
              \\(v\\) is plotted against the available budget \\(t\\). The displayed elements can be switched
              on and off by clicking on the legend on the right. A <b>tooltip</b>
              and <b>toolbar</b> appears when hovering over the figure.'),
       plotlyOutput.IOHanalyzer('RT_ECDF_AGGR')
      )
    )
  )
}

rt_ecdf_agg_fct_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Aggregated Empirical Cumulative
                    Distribution: All functions</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      
      checkboxInput("RTECDF.Aggr.Func", "Aggregate functions", value = T),
      checkboxInput("RTECDF.Aggr.Dim", "Aggregate dimensions", value = F),
      checkboxInput("RTECDF.Aggr.Logx", "Scale X-axis logaritmically", value = T),
      
      HTML_P('Choose whether to upload a file containing the target-values for each (function, dimension)-pair
             or use the automatically generated targets (see table below the plot). Please consider keeping the file format when
             modifying the csv given below.'),
      downloadButton('RTECDF.Aggr.Table.Download', label = 'download the example targets'),

      hr(),
      br(),
      fileInput("RTECDF.Aggr.Table.Upload", label = HTML('<p align="left" style="font-size:120%;">Please choose a <i>csv file</i> containing the targets</p>'),
                multiple = FALSE, accept = c(
                  "text/csv",
                  "text/comma-separated-values,text/plain",
                  ".csv")),
      selectInput('RTECDF.Aggr.Format', label = 'select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),

      downloadButton('RTECDF.Aggr.Download', label = 'download the figure')

      ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        hr(),

        HTML_P('The fraction of (run,target value, ...)
                pairs \\((i,v, ...)\\) satisfying that the best solution that the algorithm has
                found in the \\(i\\)-th (run of function \\(f\\) in dimension \\(d\\)) within
                the given time budget \\(t\\) has quality at least \\(v\\) is plotted against
                the available budget \\(t\\). The displayed elements can be switched
                on and off by clicking on the legend on the right. A <b>tooltip</b>
                and <b>toolbar</b> appears when hovering over the figure. Aggregation over
                functions and dimension can be switched on or off using the checkboxes on
                the left; when aggregation is off the selected function / dimension
                is chosen according the the value in the bottom-left selection-box.'),
        plotlyOutput.IOHanalyzer('RT_ECDF_MULT'),
        HTML_P('The selected targets are:'),
        dataTableOutput('RT_GRID_GENERATED')
      )
    )
  )
}

rt_ecdf_auc_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Area Under the ECDF</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML('<p align="justify">Set the range and the granularity of
           the evenly spaced quality targets taken into account in the plot.</p>'),
      textInput('RTECDF.AUC.Min', label = F_MIN_LABEL, value = ''),
      textInput('RTECDF.AUC.Max', label = F_MAX_LABEL, value = ''),
      textInput('RTECDF.AUC.Step', label = F_STEP_LABEL, value = ''),

      selectInput('RTECDF.AUC.Format', label = 'select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),
      downloadButton('RTECDF.AUC.Download', label = 'download the figure')
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
        plotlyOutput.IOHanalyzer("RT_AUC")
      )
    )
  )
}
