rt_sample_box <- function() {
  box(title = HTML('<p style="font-size:120%;">Original Runtime Samples</p>'), width = 12,
      solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = T,
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Set the range and the granularity of the results.
                          The table will show fixed-target runtimes for evenly spaced target values.</p>'),
        
        textInput('F_MIN_SAMPLE', label = F_MIN_LABEL, value = ''),
        textInput('F_MAX_SAMPLE', label = F_MAX_LABEL, value = ''),
        textInput('F_STEP_SAMPLE', label = F_STEP_LABEL, value = ''),
        checkboxInput('F_SAMPLE_SINGLE', 
                      label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                  Once toggled, only \\(f_{\\text{min}}\\) is 
                                                  used to generate the table on the right.</p>'), value = FALSE),
        
        # TODO: do we need this log scaling?
        # checkboxInput('F_LOGSPACE_DATA_SUMMARY',
        #               label = HTML('Evenly space target values in \\(log_{10}\\) space')),
        selectInput('ALGID_RAW_INPUT', 'Algorithms', 
                    choices = NULL, selected = NULL),
        
        selectInput('RT_download_format', 'Format of the csv', 
                    choices = c('long', 'wide'), selected = 'wide'),
        downloadButton("download_runtime", "Save the aligned runtime samples as csv")
      ),
      
      mainPanel(
        width = 9,
        HTML('<p style="font-size:120%;">This table shows for each selected algorithm \\(A\\), 
                          each selected target value \\(f(x)\\), and each run \\(r\\) 
                          the number \\(T(A,f(x),r)\\) of evaluations performed by the 
                          algorithm until it evaluated for the first time a solution of 
                          quality at least \\(f(x)\\).</p>'),
        dataTableOutput('table_RT_sample')
      )
  )
}
