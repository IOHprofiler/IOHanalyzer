fv_per_fct_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Expected Target Value (per function)</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = TRUE, status = "primary",
    sidebarPanel(
      width = 3,
      HTML('<p style="font-size:120%;">Range of the displayed budget values</p>'),

      textInput('FCEPlot.Min', label = RT_MIN_LABEL, value = ''),
      textInput('FCEPlot.Max', label = RT_MAX_LABEL, value = ''),
      selectInput('FCEPlot.Algs', label = 'Select which algorithms to plot:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "Algorithm selection", content = alg_select_info, 
                        placement = "auto"
                      )
                  ),
      checkboxInput('FCEPlot.show.mean',
                    label = 'Show/hide mean',
                    value = T),

      checkboxInput('FCEPlot.show.median',
                    label = 'Show/hide median',
                    value = F),
      checkboxInput('FCEPlot.show.CI',
                    label = 'Show/hide mean +/- sd',
                    value = F),

      checkboxInput('FCEPlot.semilogx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),

      checkboxInput('FCEPlot.semilogy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = T),

      hr(),
      selectInput('FCEPlot.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),

      downloadButton('FCEPlot.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b><i>mean, median and standard deviation</i></b> of the best function values
                found with a fixed-budget of evaluations are depicted against the budget.
                The displayed elements can be switched on and off by clicking on the legend on the right.
                A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput.IOHanalyzer('FCE_PER_FUN')
      )
    )
  )
}

fv_agg_box <- function(width = 12, height = '600px', collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Function values: all functions</p>'),
      width = width, collapsible = collapsible, solidHeader = T,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 2,
          selectInput('FCEPlot.Multi.Algs', label = 'Select which algorithms to plot:',
                      multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                        custom_icon() %>%
                          bs_embed_popover(
                            title = "Algorithm selection", content = alg_select_info, 
                            placement = "auto"
                          )
                      ),

          checkboxInput('FCEPlot.Multi.Logx',
                        label = 'Scale x axis \\(\\log_{10}\\)',
                        value = T),

          checkboxInput('FCEPlot.Multi.Logy',
                        label = 'Scale y axis \\(\\log_{10}\\)',
                        value = T),

          actionButton('FCEPlot.Multi.PlotButton', label = 'Refresh the figure'),
          hr(),
          selectInput('FCEPlot.Multi.Format', label = 'Select the figure format',
                      choices = supported_fig_format, selected = 'pdf'),

          downloadButton('FCEPlot.Multi.Download', label = 'Download the figure')
        ),

        mainPanel(
          width = 10,
          column(
            width = 12, align = "center",
            plotlyOutput.IOHanalyzer('FCEPlot.Multi.Plot', aspect_ratio = 1)
          )
        )
      )
  )
}

fv_comparison_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Function Value comparisons</p>'),
      width = width, collapsible = collapsible, solidHeader = TRUE,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput('FCEPlot.Aggr.Algs', label = 'Select which algorithms to plot:',
                      multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                        custom_icon() %>%
                          bs_embed_popover(
                            title = "Algorithm selection", content = alg_select_info, 
                            placement = "auto"
                          )
                      ),
          selectInput('FCEPlot.Aggr.Mode', label = 'Select the plotting mode',
                      choices = c('radar','line'), selected = 'radar'),

          selectInput('FCEPlot.Aggr.Aggregator', label = 'Create plot for all?',
                      choices = c('Functions','Dimensions'), selected = 'Functions'),

          checkboxInput('FCEPlot.Aggr.Ranking',
                        label = 'Use ranking instead of ERT-values',
                        value = T),

          checkboxInput('FCEPlot.Aggr.Logy',
                        label = 'Scale y axis \\(\\log_{10}\\)',
                        value = F),
          actionButton("FCEPlot.Aggr.Refresh", "Refresh the figure and table"),
          # textInput('FCEPlot.Aggr.Targets', label = 'Choose the runtimes (comma-separated)'),
          hr(),
          selectInput('FCEPlot.Aggr.Format', label = 'Select the figure format',
                      choices = supported_fig_format, selected = 'pdf'),

          downloadButton('FCEPlot.Aggr.Download', label = 'Download the figure'),
          hr(),
          selectInput('FCEPlot.Aggr.TableFormat', label = 'Select the table format',
                      choices = table_formats, selected = table_format_default),
          downloadButton('FCEPlot.Aggr.DownloadTable', label = 'Download the table')
        ),

        mainPanel(
          width = 9,
          column(
            width = 12, align = "center",
            plotlyOutput.IOHanalyzer('FCEPlot.Aggr.Plot'),
            hr(),
            HTML_P("The chosen budget values per function are as follows (double click an entry to edit it):"),
            DT::dataTableOutput("FCEPlot.Aggr.Targets"),
            hr(),
            HTML_P("The raw function-values are:"),
            DT::dataTableOutput("FCEPlot.Aggr.FCETable")
          )
        )
      )
  )
}
