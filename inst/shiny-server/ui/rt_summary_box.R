rt_stats_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Runtime Statistics at Chosen Target Values</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Set the range and the granularity of the results.
                          The table will show fixed-target runtimes for evenly spaced target values.</p>'),

        textInput('RTSummary.Statistics.Min', label = F_MIN_LABEL, value = ''),
        textInput('RTSummary.Statistics.Max', label = F_MAX_LABEL, value = ''),
        textInput('RTSummary.Statistics.Step', label = F_STEP_LABEL, value = ''),
        checkboxInput('RTSummary.Statistics.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                           Once toggled, only \\(f_{\\text{min}}\\) is
                                                           used to generate the table on the right.</p>'), value = FALSE),
        selectInput('RTSummary.Statistics.Algid', 'Select which IDs to include:', choices = NULL, selected = NULL, multiple = T),
        hr(),
        selectInput('RTSummary.Statistics.Format', 'Format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("RTSummary.Statistics.Download", "Save this table")
      ),

      mainPanel(
        width = 9,
        HTML(paste0('<div style="font-size:120%;">', includeMarkdown('markdown/RT_SUMMARY_TABLE.Rmd'),'</div>')),
        DT::dataTableOutput('table_RT_summary')
      )
  )
}

rt_sample_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Original Runtime Samples</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Set the range and the granularity of the results.
             The table will show fixed-target runtimes for evenly spaced target values.</p>'),

        textInput('RTSummary.Sample.Min', label = F_MIN_LABEL, value = ''),
        textInput('RTSummary.Sample.Max', label = F_MAX_LABEL, value = ''),
        textInput('RTSummary.Sample.Step', label = F_STEP_LABEL, value = ''),
        checkboxInput('RTSummary.Sample.Single',
                      label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                   Once toggled, only \\(f_{\\text{min}}\\) is
                                   used to generate the table on the right.</p>'), value = FALSE),

        # TODO: do we need this log scaling?
        # checkboxInput('F_LOGSPACE_DATA_SUMMARY',
        #               label = HTML('Evenly space target values in \\(log_{10}\\) space')),
        selectInput('RTSummary.Sample.Algid', 'Select which IDs to include:',
                    choices = NULL, selected = NULL, multiple = T),

        hr(),
        selectInput('RTSummary.Sample.DownloadFormat', 'Format of the table',
                    choices = c('long', 'wide'), selected = 'wide'),
        selectInput('RTSummary.Sample.Format', 'File-format', choices = supported_table_format, selected = supported_table_format[[1]]),
        
        downloadButton("RTSummary.Sample.Download", "Save this table")
                      ),

      mainPanel(
        width = 9,
        HTML('<p style="font-size:120%;">This table shows for each selected algorithm \\(A\\),
             each selected target value \\(f(x)\\), and each run \\(r\\)
             the number \\(T(A,f(x),r)\\) of evaluations performed by the
             algorithm until it evaluated for the first time a solution of
             quality at least \\(f(x)\\).</p>'),
        DT::dataTableOutput('table_RT_sample')
        )
      )
}

rt_overview_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Data Overview</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('RTSummary.Overview.Algid', 'Select which IDs to include:', choices = NULL, selected = NULL, multiple = T),
        hr(),
        selectInput('RTSummary.Overview.Format', 'File-format', choices = supported_table_format, selected = supported_table_format[[1]]),
        downloadButton("RTSummary.Overview.Download", "Save this table")
      ),

      mainPanel(
        width = 9,
        HTML(paste0('<div style="font-size:120%;">', includeMarkdown('markdown/RT_OVERVIEW_TABLE.Rmd'), '</div>')),
        DT::dataTableOutput('table_RT_overview')
      )
  )
}

