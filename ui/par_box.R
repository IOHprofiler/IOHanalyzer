par_expected_value_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Expected Parameter Value (per function)</p>'), 
    width = width, collapsible = collapsible, collapsed = collapsed, 
    solidHeader = TRUE, status = "primary", 
    sidebarPanel(
      width = 3,
      HTML('<p style="font-size:120%;">Range of the function values (\\(x\\) axis)</p>'),
       
      textInput('PAR_F_MIN', label = F_MIN_LABEL, value = ''),
      textInput('PAR_F_MAX', label = F_MAX_LABEL, value = ''),
       
      selectInput('PAR_ALGID_INPUT', 'Algorithms', choices = NULL, selected = NULL),
      selectInput('PAR_show.mean', label = 'mean/median', choices = c('mean', 'median'),
                   selected = 'mean'),
       
      checkboxInput('show.CI.PAR', label = 'show/hide mean +/- sd', value = F),
      checkboxInput('PAR_semilogx', label = 'scale x axis log10', value = T),
      checkboxInput('PAR_semilogy', label = 'scale y axis log10', value = T),
       
      selectInput('FIG_FORMAT_PAR_PER_FUN', label = 'select the figure format',
                   choices = supported_fig_format, selected = 'pdf'),
      downloadButton('FIG_DOWNLOAD_PAR_PER_FUN', label = 'download the figure')
    ),
       
    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b><i>mean or median</i></b> of internal parameters of the algorithm
                      found with a fixed-budget of evaluations are depicted against the budget. 
               The displayed elements can be switched on and off by clicking on the legend on the right. 
               A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput('PAR_PER_FUN', height = plotly_height, width = plotly_width2)
      )
    )
   )
}

par_summary_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Parameter Statistics at Chosen Target Values</p>'), 
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary", 
    sidebarPanel(
      width = 3,
      HTML_P('Set the range and the granularity of the results. The table will show 
             fixed-target parameter values for evenly spaced target values.'),
      
      textInput('PAR_F_MIN_SUMMARY', label = F_MIN_LABEL, value = ''),
      textInput('PAR_F_MAX_SUMMARY', label = F_MAX_LABEL, value = ''),
      textInput('PAR_F_STEP_SUMMARY', label = F_STEP_LABEL, value = ''),
      checkboxInput('PAR_F_SINGLE', 
                    label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)? 
                                  Once toggled, only \\(f_{\\text{min}}\\) is 
                                  used to generate the table on the right.</p>'),
                    value = FALSE),
      selectInput('PAR_ALGID_INPUT_SUMMARY', 'Algorithms', choices = NULL, selected = NULL),
      selectInput('PAR_INPUT', 'Parameters', choices = NULL, selected = NULL),
      downloadButton("PAR_downloadData", "Save this table as csv")
    ),
    
    mainPanel(
      width = 9,
      HTML(paste0('<div style="font-size:120%;">', 
                  includeMarkdown('RMD/PAR_SUMMARY_TABLE.Rmd'), '</div>')),
      tableOutput('table_PAR_summary')
    )
  )
}

par_sample_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Parameter Sample at Chosen Target Values</p>'), 
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary", 
    sidebarPanel(
      width = 3,
      HTML_P('Set the range and the granularity of the results.
              The table will show fixed-target parameter values for evenly spaced target values.'),
      
      textInput('PAR_F_MIN_SAMPLE', label = F_MIN_LABEL, value = ''),
      textInput('PAR_F_MAX_SAMPLE', label = F_MAX_LABEL, value = ''),
      textInput('PAR_F_STEP_SAMPLE', label = F_STEP_LABEL, value = ''),
      checkboxInput('PAR_SAMPLE_F_SINGLE', 
                    label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                  Once toggled, only \\(f_{\\text{min}}\\) is 
                                  used to generate the table on the right.</p>'), 
                    value = FALSE),
      selectInput('PAR_ALGID_INPUT_SAMPLE', 'Algorithms', choices = NULL, selected = NULL),
      selectInput('PAR_INPUT_SAMPLE', 'Parameters', choices = NULL, selected = NULL),
      selectInput('PAR_download_format', 'Format of the csv', 
                  choices = c('long', 'wide'), selected = 'wide'),
      downloadButton("PAR_SAMPLE_downloadData", "Save this table as csv")
    ),
    
    mainPanel(
      width = 9,
      HTML('<p style="font-size:120%;">This table shows for each selected algorithm \\(A\\), 
           each selected target value \\(f(x)\\), and each run \\(r\\) the parameter value 
           observed when the target value \\(f(x)\\) is reached for the first time.</p>'),
      dataTableOutput('table_PAR_SAMPLE')
    )
  )
}