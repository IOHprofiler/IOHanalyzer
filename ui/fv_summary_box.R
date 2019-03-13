fv_overview_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Data Overview</p>'),
      width = width, solidHeader = T, status = "primary", 
      collapsible = collapsible, collapsed = collapsed, 
      sidebarPanel(
        width = 3,
        HTML('<p align="justify">Select which algorithms to show.</p>'),
        selectInput('FCE_ALGID_INPUT_SUMMARY', 'Algorithms', choices = NULL, selected = NULL),
        downloadButton("FCE_downloadData_summary", "Save this table as csv")
      ),
      
      mainPanel(
        width = 9,
        tableOutput('table_FV_overview')
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
        textInput('RT_MIN', label = RT_MIN_LABEL, value = ''),
        textInput('RT_MAX', label = RT_MAX_LABEL, value = ''),
        textInput('RT_STEP', label = RT_STEP_LABEL, value = ''),
        checkboxInput('RT_SINGLE', label = HTML('<p>\\(B_{\\text{min}} = B_{\\text{max}}\\)?
                                                Once toggled, only \\(B_{\\text{min}}\\) is 
                                                used to generate the table on the right.</p>'), value = FALSE),
        selectInput('FCE_ALGID_INPUT', 'Algorithms', choices = NULL, selected = NULL),
        downloadButton("FCE_SUMMARY_download", "Save this table as csv")
        ),
      
      mainPanel(
        width = 9,
        HTML(paste0('<div style="font-size:120%;">', 
                    includeMarkdown('RMD/TAR_SUMMARY_TABLE.Rmd'),
                    '</div>')),
        tableOutput('FCE_SUMMARY')
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
      textInput('RT_MIN_SAMPLE', label = RT_MIN_LABEL, value = ''),
      textInput('RT_MAX_SAMPLE', label = RT_MAX_LABEL, value = ''),
      textInput('RT_STEP_SAMPLE', label = RT_STEP_LABEL, value = ''),
      checkboxInput(
        'RT_SINGLE_SAMPLE', 
        label = HTML('<p>\\(B_{\\text{min}} = B_{\\text{max}}\\)? 
                      Once toggled, only \\(B_{\\text{min}}\\) is 
                      used to generate the table on the right.</p>'),
        value = FALSE
      ),
      selectInput('FCE_ALGID_RAW_INPUT', 'Algorithms', choices = NULL, selected = NULL),
        
      selectInput('download_format_FCE', 'Format of the csv', choices = c('long', 'wide'), selected = 'wide'),
      downloadButton("FCE_SAMPLE_download", "Save the aligned runtime samples as csv")
    ),
      
    mainPanel(
      width = 9,
      HTML("<div style='font-size:120%;'>This table shows for each selected algorithm \\(A\\), 
           each selected target value \\(f(x)\\), and each run \\(r\\) 
           the number \\(T(A,f(x),r)\\) of evaluations performed by the 
           algorithm until it evaluated for the first time a solution of 
           quality at least \\(f(x)\\).</div>"),
      br(),
      dataTableOutput('FCE_SAMPLE')
    )
  )
}