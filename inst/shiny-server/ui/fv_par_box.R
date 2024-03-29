fv_par_expected_value_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Expected Parameter Value (per function)</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = TRUE, status = "primary",
    sidebarPanel(
      width = 3,
      HTML('<p style="font-size:120%;">Range of the budget values (\\(x\\) axis)</p>'),

      textInput('FV_PAR.Plot.Min', label = RT_MIN_LABEL, value = ''),
      textInput('FV_PAR.Plot.Max', label = RT_MAX_LABEL, value = ''),
      selectInput('FV_PAR.Plot.Params', 'Parameters', choices = NULL, selected = NULL, multiple = T),

      selectInput('FV_PAR.Plot.Algs', 'Select which IDs to include:', choices = NULL, selected = NULL, multiple = T) %>% shinyInput_label_embed(
        custom_icon() %>%
          bs_embed_popover(
            title = "ID selection", content = alg_select_info,
            placement = "auto"
          )
      ),
      selectInput('FV_PAR.Plot.show.mean', label = 'Mean/median',
                  choices = c('mean', 'median'),
                  selected = 'mean'),
      selectInput('FV_PAR.Plot.CI', "Show standard deviations or quantiles",
                    choices = c('Standard Deviation', 'Outer Quantiles', 'None'),
                    selected = 'None') %>%
        shinyInput_label_embed(
          custom_icon() %>%
            bs_embed_popover(
              title = "Outer quantiles", content = "This method uses the highest and lowest quantiles, which are
                  2% and 98% by default. This can be changed in the settings-tab.",
              placement = "auto"
            )
        ),
      checkboxInput('FV_PAR.Plot.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),

      checkboxInput('FV_PAR.Plot.Logy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = F) %>%
        shinyInput_label_embed(
          custom_icon("exclamation-triangle") %>%
            bs_embed_tooltip(
              title = "Be mindful of using logarithmic scaling when the parameter-values can be <= 0"
            )
        ),

      hr(),
      selectInput('FV_PAR.Plot.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),
      downloadButton('FV_PAR.Plot.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b><i>mean or median</i></b> of internal parameters of the algorithm
                      found with a fixed-budget of evaluations are depicted against the budget.
               The displayed elements can be switched on and off by clicking on the legend on the right.
               A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput.IOHanalyzer('FV_PAR.Plot.Figure')
      )
    )
   )
}

par_scatter_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Correlation of parameters over time</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = TRUE, status = "primary",
    sidebarPanel(
      width = 3,

      selectInput('FV_PAR.CorrPlot.Param1', 'Parameter X', choices = NULL, selected = NULL, multiple = F),
      selectInput('FV_PAR.CorrPlot.Param2', 'Parameter Y', choices = NULL, selected = NULL, multiple = F),

      selectInput('FV_PAR.CorrPlot.Algs', 'Select which IDs to include:', choices = NULL, selected = NULL, multiple = T) %>% shinyInput_label_embed(
        custom_icon() %>%
          bs_embed_popover(
            title = "ID selection", content = alg_select_info,
            placement = "auto"
          )
      ),

      checkboxInput('FV_PAR.CorrPlot.Animated',
                    label = 'Use runtime for animation',
                    value = T),

      conditionalPanel("input['FV_PAR.CorrPlot.Animated']",
                       numericInput('FV_PAR.CorrPlot.WindowSize',
                                    'Size of runtime window', value = 50,
                                    min = 0)),

      checkboxInput('FV_PAR.CorrPlot.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),

      checkboxInput('FV_PAR.CorrPlot.Logy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = F) %>%
        shinyInput_label_embed(
          custom_icon("exclamation-triangle") %>%
            bs_embed_tooltip(
              title = "Be mindful of using logarithmic scaling when the parameter-values can be <= 0"
            )
        ),

      # hr(),
      # selectInput('FV_PAR.CorrPlot.Format', label = 'Select the figure format',
      #             choices = supported_fig_format, selected = supported_fig_format[[1]]),
      # downloadButton('FV_PAR.CorrPlot.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The .'),
        plotlyOutput.IOHanalyzer('FV_PAR.CorrPlot.Figure')
      )
    )
  )
}

fv_par_summary_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Parameter Statistics at Chosen Runtime Values</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML_P('Set the range and the granularity of the results.
             The table will show fixed-budget parameter values for evenly spaced budget values.'),

      textInput('FV_PAR.Summary.Min', label = RT_MIN_LABEL, value = ''),
      textInput('FV_PAR.Summary.Max', label = RT_MAX_LABEL, value = ''),
      textInput('FV_PAR.Summary.Step', label = RT_STEP_LABEL, value = ''),
      checkboxInput('FV_PAR.Summary.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                       Once toggled, only \\(f_{\\text{min}}\\) is
                                                       used to generate the table on the right.</p>'), value = FALSE),
      selectInput('FV_PAR.Summary.ID', 'Select which IDs to include:', choices = NULL, selected = NULL, multiple = T),
      selectInput('FV_PAR.Summary.Param', 'Parameters', choices = NULL, selected = NULL),
      hr(),
      selectInput('FV_PAR.Summary.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
      downloadButton("FV_PAR.Summary.Download", "Save this table")
      ),

    mainPanel(
      width = 9,
      HTML(paste0('<div style="font-size:120%;">',
                  includeMarkdown('markdown/FV_PAR_SUMMARY_TABLE.Rmd'), '</div>')),
      DT::dataTableOutput('table_FV_PAR_summary')
    )
  )
}

fv_par_sample_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Parameter Sample at Chosen Runtime Values</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML_P('Set the range and the granularity of the results.
                                The table will show fixed-budget parameter values for evenly spaced budget values.'),

      textInput('FV_PAR.Sample.Min', label = RT_MIN_LABEL, value = ''),
      textInput('FV_PAR.Sample.Max', label = RT_MAX_LABEL, value = ''),
      textInput('FV_PAR.Sample.Step', label = RT_STEP_LABEL, value = ''),
      checkboxInput('FV_PAR.Sample.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                                             Once toggled, only \\(f_{\\text{min}}\\) is
                                                                             used to generate the table on the right.</p>'), value = FALSE),
      selectInput('FV_PAR.Sample.ID', 'Select which IDs to include:', choices = NULL, selected = NULL, multiple = T),
      selectInput('FV_PAR.Sample.Param', 'Parameters', choices = NULL, selected = NULL),
      hr(),
      selectInput('FV_PAR.Sample.Format', 'Format of the table',
                  choices = c('long', 'wide'), selected = 'wide'),
      selectInput('FV_PAR.Sample.FileFormat', 'File-Format', choices = supported_table_format, selected = supported_table_format[[1]]),
      downloadButton("FV_PAR.Sample.Download", "Save this table")
    ),

    mainPanel(
      width = 9,
      HTML('<p style="font-size:120%;">This table shows for each selected algorithm \\(A\\),
           each selected runtime value \\(b\\), and each run \\(r\\) the parameter value
           observed when the after a total budget of \\(b\\) function evaluations has been used.</p>'),
      DT::dataTableOutput('table_FV_PAR_SAMPLE')
    )
  )
}
