BiObjDiff_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Biobjective Performance Differences</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = TRUE, status = "primary",
    sidebarPanel(
      width = 3,
      HTML('<p style="font-size:120%;">Range of the displayed budget values</p>'),

      textInput('Biobj.Diff.xMin', label = RT_MIN_LABEL, value = ''),
      textInput('Biobj.Diff.xMax', label = RT_MAX_LABEL, value = ''),
      textInput('Biobj.Diff.yMin', label = F_MIN_LABEL, value = ''),
      textInput('Biobj.Diff.yMax', label = F_MAX_LABEL, value = ''),

      selectInput('Biobj.Diff.Algs', label = 'Select which IDs to include:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "ID selection", content = alg_select_info,
                        placement = "auto"
                      )
                  ),
      checkboxInput('Biobj.Diff.show.min',
                    label = 'Show/hide minimal reached values',
                    value = T),
      checkboxInput('Biobj.Diff.show.max',
                    label = 'Show/hide maximal reached values',
                    value = T),

      checkboxInput('Biobj.Diff.semilogx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),

      checkboxInput('Biobj.Diff.semilogy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = T),


      hr(),
      selectInput('Biobj.Diff.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),

      downloadButton('Biobj.Diff.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('Explanation here.'),
        plotlyOutput.IOHanalyzer('Biobj.Diff.Plot')
      )
    )
  )
}
