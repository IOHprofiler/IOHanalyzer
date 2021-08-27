fv_overview_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Data Overview</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('FCESummary.Overview.ID', 'Select which IDs to include:', choices = NULL, selected = NULL, multiple = T),
        #TODO: implement this button
        hr(),
        selectInput('FCESummary.Overview.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("FCESummary.Overview.Download", "Save this table")
      ),

      mainPanel(
        width = 9,
        DT::dataTableOutput('table_FV_overview')
      )
  )
}

fv_stats_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Target Statistics at Chosen Budget Values</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML(FCE_GRID_INPUT_TEXT),

        textInput('FCESummary.Statistics.Min', label = RT_MIN_LABEL, value = ''),
        textInput('FCESummary.Statistics.Max', label = RT_MAX_LABEL, value = ''),
        textInput('FCESummary.Statistics.Step', label = RT_STEP_LABEL, value = ''),
        checkboxInput('FCESummary.Statistics.Single', label = HTML('<p>\\(B_{\\text{min}} = B_{\\text{max}}\\)?
                                                           Once toggled, only \\(B_{\\text{min}}\\) is
                                                           used to generate the table on the right.</p>'), value = FALSE),
        selectInput('FCESummary.Statistics.ID', 'Select which IDs to include:', choices = NULL, selected = NULL, multiple = T),
        hr(),
        selectInput('FCESummary.Statistics.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("FCESummary.Statistics.Download", "Save this table")
      ),

      mainPanel(
        width = 9,
        HTML(paste0('<div style="font-size:120%;">',
                    includeMarkdown('markdown/TAR_SUMMARY_TABLE.Rmd'),
                    '</div>')),
        DT::dataTableOutput('FCE_SUMMARY')
      )
    )
}

fv_sample_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Original Target Samples</p>'),
    width = width, solidHeader = TRUE, status = "primary",
    collapsible = collapsible, collapsed = collapsed,
    sidebarPanel(
      width = 3,
      HTML(FCE_GRID_INPUT_TEXT),

      textInput('FCESummary.Sample.Min', label = RT_MIN_LABEL, value = ''),
      textInput('FCESummary.Sample.Max', label = RT_MAX_LABEL, value = ''),
      textInput('FCESummary.Sample.Step', label = RT_STEP_LABEL, value = ''),
      checkboxInput('FCESummary.Sample.Single',
                    label = HTML('<p>\\(B_{\\text{min}} = B_{\\text{max}}\\)?
                                                       Once toggled, only \\(B_{\\text{min}}\\) is
                                                       used to generate the table on the right.</p>'), value = FALSE),
      selectInput('FCESummary.Sample.ID', 'Select which IDs to include:',
                  choices = NULL, selected = NULL, multiple = T),

      selectInput('FCESummary.Sample.Format', 'Format of the table',
                  choices = c('long', 'wide'), selected = 'wide'),
      hr(),
      selectInput('FCESummary.Sample.FileFormat', 'File-Format', choices = supported_table_format, selected = supported_table_format[[1]]),
      downloadButton("FCESummary.Sample.Download", "Save the aligned runtime samples")
    ),

    mainPanel(
      width = 9,
      HTML("<div style='font-size:120%;'>This table shows for each selected algorithm \\(A\\),
           each selected target value \\(f(x)\\), and each run \\(r\\)
           the number \\(T(A,f(x),r)\\) of evaluations performed by the
           algorithm until it evaluated for the first time a solution of
           quality at least \\(f(x)\\).</div>"),
      br(),
      DT::dataTableOutput('FCE_SAMPLE')
    )
  )
}
