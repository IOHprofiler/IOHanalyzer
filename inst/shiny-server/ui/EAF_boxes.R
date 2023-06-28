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
      HTML('<p align="justify">Set the range and the granularity of the budgets
           taken into account in the ECDF curve. The plot will show the ECDF curves
           for evenly spaced budgets.</p>'),
      textInput('EAF.Single.Min', label = RT_MIN_LABEL, value = ''),
      textInput('EAF.Single.Max', label = RT_MAX_LABEL, value = ''),

      textInput('EAF.Single.yMin', label = F_MIN_LABEL, value = ''),
      textInput('EAF.Single.yMax', label = F_MAX_LABEL, value = ''),

      checkboxInput('EAF.Single.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),
      checkboxInput('EAF.Single.Logy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = T),
      hr(),

      numericInput('EAF.Single.levels', label = 'Number of Levels in plot', value=10),

      hr(),

      selectInput('EAF.Single.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),

      downloadButton('EAF.Single.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('For more information on the EAF, please see https://mlopez-ibanez.github.io/eaf/'),

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
        HTML_P('For more information on the EAF, please see https://mlopez-ibanez.github.io/eaf/'),

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

      selectInput('EAF.Diff.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),

      downloadButton('EAF.Diff.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('For more information on the EAF, please see https://mlopez-ibanez.github.io/eaf/'),

        plotlyOutput.IOHanalyzer('EAF.Diff_Plot')
      )
    )
  )
}
