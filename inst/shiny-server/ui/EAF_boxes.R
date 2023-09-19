EAF_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(
    title = HTML('<p style="font-size:120%;">Empirical Attainment Function</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('EAF.Single.Algs', label = 'Select which IDs to include:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "ID selection", content = alg_select_info,
                        placement = "auto"
                      )
                  ),
      HTML('<p align="justify">Set the ranges for the budgets and function values.</p>'),
      textInput('EAF.Single.Min', label = RT_MIN_LABEL, value = ''),
      textInput('EAF.Single.Max', label = RT_MAX_LABEL, value = ''),

      textInput('EAF.Single.yMin', label = F_MIN_LABEL, value = ''),
      textInput('EAF.Single.yMax', label = F_MAX_LABEL, value = ''),

      actionButton('EAF.Single.Refresh', 'Force refresh plot'),

      checkboxInput('EAF.Single.Subsampling',
                    label = 'Use Subsampling for EAF calculation',
                    value = T) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "Subsampling", content = "When disabled,
                          all runtime values where improvements are present
                          will be used, resulting in a more accurate EAF.
                          This does come at the cost of longer execution time,
                          particularly when many runs are present.",
                          placement = "auto"
                        )
                    ),

      checkboxInput('EAF.Single.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),
      checkboxInput('EAF.Single.Logy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = T),
      checkboxInput('EAF.Single.Colorbar',
                    label = 'Show Colorbar',
                    value = F),
      checkboxInput('EAF.Single.Problines',
                    label = 'Show Fixed Probability Lines',
                    value = F),
      hr(),

      numericInput('EAF.Single.levels', label = 'Number of Levels in plot', value=11),

      hr(),

      selectInput('EAF.Single.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),

      downloadButton('EAF.Single.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The empirical attainment function (EAF) estimates the percentage
                of runs that attain an arbitrary target value not later than a given
               runtime. \n
               For more information on the EAF, please see https://mlopez-ibanez.github.io/eaf/'),

        plotlyOutput.IOHanalyzer('EAF.Single_Plot')
      )
    )
  )
}


EAF_CDF_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(
    title = HTML('<p style="font-size:120%;">Empirical Attainment Function Partial Integral (`improved` ECDF)</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('EAF.CDF.Algs', label = 'Select which IDs to include:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "ID selection", content = alg_select_info,
                        placement = "auto"
                      )
                  ),
      HTML('<p align="justify">Set the range of the budgets and targets
           taken into account in the ECDF curve.</p>'),

      textInput('EAF.CDF.yMin', label = F_MIN_LABEL, value = ''),
      textInput('EAF.CDF.yMax', label = F_MAX_LABEL, value = ''),

      checkboxInput('EAF.CDF.Logy',
                    label = 'Scale y space \\(\\log_{10}\\) before calculating the partial integrals',
                    value = T),
      hr(),
      checkboxInput('EAF.CDF.Subsampling',
                    label = 'Use Subsampling for EAF calculation',
                    value = T) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "Subsampling", content = "When disabled,
                          all runtime values where improvements are present
                          will be used, resulting in a more accurate EAF.
                          This does come at the cost of longer execution time,
                          particularly when many runs are present.",
                          placement = "auto"
                        )
                    ),
      checkboxInput('EAF.CDF.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),


      hr(),

      selectInput('EAF.CDF.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),

      downloadButton('EAF.CDF.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The empirical attainment function (EAF) estimates the percentage
                of runs that attain an arbitrary target value not later than a given
               runtime. Taking the partial integral of the EAF results in a more
               accurate version of the Empirical Cumulative Distribution Function,
               since it does not rely on discritization of the targets.\n
               For more information on the EAF, please see https://mlopez-ibanez.github.io/eaf/'),

        plotlyOutput.IOHanalyzer('EAF.CDF_Plot')
      )
    )
  )
}

EAF_Diff_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(
    title = HTML('<p style="font-size:120%;">EAF based differences</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('EAF.Diff.Algs', label = 'Select which IDs to include:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "ID selection", content = alg_select_info,
                        placement = "auto"
                      )
                  ),
      HTML('<p align="justify">Set the range of the budgets and targets
           taken into account in the EDiff curve.</p>'),
      textInput('EAF.Diff.Min', label = RT_MIN_LABEL, value = ''),
      textInput('EAF.Diff.Max', label = RT_MAX_LABEL, value = ''),

      textInput('EAF.Diff.yMin', label = F_MIN_LABEL, value = ''),
      textInput('EAF.Diff.yMax', label = F_MAX_LABEL, value = ''),

      checkboxInput('EAF.Diff.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),
      checkboxInput('EAF.Diff.Logy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = T),

      hr(),

      checkboxInput('EAF.Diff.ZeroTransparant',
                    label = 'Show values of 0 as transparant',
                    value = T),
      checkboxInput('EAF.Diff.ShowNegatives',
                    label = 'Show negative differences on each subplot as well',
                    value = F),
      hr(),

      selectInput('EAF.Diff.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),

      downloadButton('EAF.Diff.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The empirical attainment function (EAF) estimates the percentage
                of runs that attain an arbitrary target value not later than a given
               runtime. By taking the difference between two EAFs, we can see
               areas of the (runtime, target)-space where one algorithm
               dominates other algorithms.\n
               For more information on the EAF, please see https://mlopez-ibanez.github.io/eaf/'),

        plotlyOutput.IOHanalyzer('EAF.Diff_Plot')
      )
    )
  )
}

### Multi-function view
EAF_mult_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(
    title = HTML('<p style="font-size:120%;">Empirical Attainment Function</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('EAF.Multi.FuncIds', label = "Functions to include:",
                  selected = NULL, choices = NULL, multiple = T),
      selectInput('EAF.Multi.Algs', label = 'Select which IDs to include:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "ID selection", content = alg_select_info,
                        placement = "auto"
                      )
                  ),
      checkboxInput('EAF.Multi.CustomRanges', "Customize X and Y Ranges"),
      conditionalPanel(condition = "input['EAF.Multi.CustomRanges']",
        textInput('EAF.Multi.Min', label = RT_MIN_LABEL, value = ''),
        textInput('EAF.Multi.Max', label = RT_MAX_LABEL, value = ''),

        textInput('EAF.Multi.yMin', label = F_MIN_LABEL, value = ''),
        textInput('EAF.Multi.yMax', label = F_MAX_LABEL, value = '')
      ),
      actionButton('EAF.Multi.Refresh', 'Force refresh plot'),

      checkboxInput('EAF.Multi.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),
      checkboxInput('EAF.Multi.Logy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = T),
      checkboxInput('EAF.Multi.Colorbar',
                    label = 'Show Colorbar',
                    value = F),
      checkboxInput('EAF.Multi.Problines',
                    label = 'Show Fixed Probability Lines',
                    value = F),
      hr(),
      checkboxInput('EAF.Multi.Subsampling',
                    label = 'Use Subsampling for EAF calculation',
                    value = T) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "Subsampling", content = "When disabled,
                          all runtime values where improvements are present
                          will be used, resulting in a more accurate EAF.
                          This does come at the cost of longer execution time,
                          particularly when many runs are present.",
                          placement = "auto"
                        )
                    ),
      numericInput('EAF.Multi.levels', label = 'Number of Levels in plot', value=11),

      hr(),

      selectInput('EAF.Multi.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),

      downloadButton('EAF.Multi.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The empirical attainment function (EAF) estimates the percentage
                of runs that attain an arbitrary target value not later than a given
               runtime.\n
               For more information on the EAF, please see https://mlopez-ibanez.github.io/eaf/'),

        plotlyOutput.IOHanalyzer('EAF.Multi_Plot')
      )
    )
  )
}


EAF_CDF_mult_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(
    title = HTML('<p style="font-size:120%;">Empirical Attainment Function Partial Integral (`improved` ECDF)</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('EAF.MultiCDF.FuncIds', label = "Functions to include:",
                  selected = NULL, choices = NULL, multiple = T),
      selectInput('EAF.MultiCDF.Algs', label = 'Select which IDs to include:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "ID selection", content = alg_select_info,
                        placement = "auto"
                      )
                  ),
      HTML('<p align="justify">Set the range of the budgets and targets
           taken into account in the ECDF curve.</p>'),
      checkboxInput('EAF.MultiCDF.CustomRanges', "Customize Y Range"),
      conditionalPanel(condition = "input['EAF.MultiCDF.CustomRanges']",
                       textInput('EAF.MultiCDF.yMin', label = F_MIN_LABEL, value = ''),
                       textInput('EAF.MultiCDF.yMax', label = F_MAX_LABEL, value = '')
                       ),
      checkboxInput('EAF.MultiCDF.CustomRangeX', "Customize X Range"),
      conditionalPanel(condition = "input['EAF.MultiCDF.CustomRangeX']",
                       textInput('EAF.MultiCDF.xMin', label = RT_MIN_LABEL, value = ''),
                       textInput('EAF.MultiCDF.xMax', label = RT_MAX_LABEL, value = '')
      ),

      checkboxInput('EAF.MultiCDF.Logy',
                    label = 'Scale y space \\(\\log_{10}\\) before calculating the partial integrals',
                    value = T),
      hr(),
      checkboxInput('EAF.MultiCDF.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),

      checkboxInput('EAF.MultiCDF.Subsampling',
                    label = 'Use Subsampling for EAF calculation',
                    value = T) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "Subsampling", content = "When disabled,
                          all runtime values where improvements are present
                          will be used, resulting in a more accurate EAF.
                          This does come at the cost of longer execution time,
                          particularly when many runs are present.",
                          placement = "auto"
                        )
                    ),
      actionButton('EAF.MultiCDF.Refresh', 'Force refresh plot'),

      hr(),

      selectInput('EAF.MultiCDF.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),

      downloadButton('EAF.MultiCDF.Download', label = 'Download the figure'),

      hr(),

      checkboxInput("EAF.MultiCDF.Normalize_AUC", "Normalize AOC / AUC values", value = T) %>%
        shinyInput_label_embed(
          custom_icon() %>%
            bs_embed_popover(
              title = "Normalization of AOC / AUC", content = "By default, the AUC values are
              approximated based on the trapezium rule using the x-values as
              present in the ECDF-plot. This value can then be normalized to [0,1]
              by dividing by the maximum x-value (if no custom x-bounds are set,
              note that this is the maximum budget across all selected algorithms!).
              Please note that the AUC values generated are only comparable to other
              AUC values if the used target boundaries and scaling are identical!",
              placement = "auto"
            )
        ),

      selectInput('EAF.AUC.Table.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
      downloadButton('EAF.AUC.Table.Download', label = 'Download the AOC table')

    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The empirical attainment function (EAF) estimates the percentage
                of runs that attain an arbitrary target value not later than a given
               runtime. Taking the partial integral of the EAF results in a more
               accurate version of the Empirical Cumulative Distribution Function,
               since it does not rely on discritization of the targets.\n
               For more information on the EAF, please see https://mlopez-ibanez.github.io/eaf/'),

        plotlyOutput.IOHanalyzer('EAF.MultiCDF_Plot'),
        HTML_P('The approximated Area Over / Under the EAF is:'),
        DT::dataTableOutput('AUC_EAF_GRID_GENERATED'),
      )
    )
  )
}
