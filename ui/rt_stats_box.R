rt_stats_box <- function() {
  box(title = HTML('<p style="font-size:120%;">Runtime Statistics at Chosen Target Values</p>'), width = 12,
      solidHeader = T, status = "primary", collapsible = T, collapsed = T,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Set the range and the granularity of the results.
                          The table will show fixed-target runtimes for evenly spaced target values.</p>'),
        
        # TODO: find better naming scheme for 'fstart, fstop, singleF'
        textInput('fstart', label = F_MIN_LABEL, value = ''),
        textInput('fstop', label = F_MAX_LABEL, value = ''),
        textInput('fstep', label = F_STEP_LABEL, value = ''),
        checkboxInput('singleF', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                           Once toggled, only \\(f_{\\text{min}}\\) is 
                                                           used to generate the table on the right.</p>'), value = FALSE),
        selectInput('ALGID_INPUT', 'Algorithms', choices = NULL, selected = NULL),
        downloadButton("downloadData", "Save this table as csv")
      ),
      
      mainPanel(
        width = 9,
        HTML(paste0('<div style="font-size:120%;">', includeMarkdown('RMD/RT_SUMMARY_TABLE.Rmd'),'</div>')),
        tableOutput('table_RT_summary')
      )
  )
}
