rt_par_expected_value_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Expected Parameter Value (per function)</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = TRUE, status = "primary",
    sidebarPanel(
      width = 3,
      HTML('<p style="font-size:120%;">Range of the function values (\\(x\\) axis)</p>'),
      
      textInput('RT_PAR.Plot.Min', label = F_MIN_LABEL, value = ''),
      textInput('RT_PAR.Plot.Max', label = F_MAX_LABEL, value = ''),
      selectInput('RT_PAR.Plot.Params', 'Parameters', choices = NULL, selected = NULL, multiple = T),
      
      selectInput('RT_PAR.Plot.Algs', 'Select which algorithms to plot:', choices = NULL, selected = NULL, multiple = T) %>% shinyInput_label_embed(
        custom_icon() %>%
          bs_embed_popover(
            title = "Algorithm selection", content = alg_select_info, 
            placement = "auto"
          )
      ),
      selectInput('RT_PAR.Plot.show.mean', label = 'Mean/median',
                  choices = c('mean', 'median'),
                  selected = 'mean'),
      checkboxInput('RT_PAR.Plot.CI', "Show standard deviations", value = T),
      checkboxInput('RT_PAR.Plot.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),
      
      checkboxInput('RT_PAR.Plot.Logy',
                    label = 'Scale y axis \\(\\log_{10}\\)',
                    value = F) %>%
        shinyInput_label_embed(
          custom_icon("exclamation-triangle") %>%
            bs_embed_tooltip(
              title = "Be mindful of using logarithmic scaling when the parameter-values can be <= 0"
            )
        ),
      
      hr(),
      selectInput('RT_PAR.Plot.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),
      downloadButton('RT_PAR.Plot.Download', label = 'Download the figure')
    ),
    
    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b><i>mean or median</i></b> of internal parameters of the algorithm
                      found with a fixed-budget of evaluations are depicted against the budget.
               The displayed elements can be switched on and off by clicking on the legend on the right.
               A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput.IOHanalyzer('RT_PAR.Plot.Figure')
      )
    )
  )
}

rt_par_summary_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Parameter Statistics at Chosen Target Values</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML_P('Set the range and the granularity of the results.
             The table will show fixed-target parameter values for evenly spaced target values.'),
      
      textInput('RT_PAR.Summary.Min', label = F_MIN_LABEL, value = ''),
      textInput('RT_PAR.Summary.Max', label = F_MAX_LABEL, value = ''),
      textInput('RT_PAR.Summary.Step', label = F_STEP_LABEL, value = ''),
      checkboxInput('RT_PAR.Summary.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                       Once toggled, only \\(f_{\\text{min}}\\) is
                                                       used to generate the table on the right.</p>'), value = FALSE),
      selectInput('RT_PAR.Summary.Algid', 'Algorithms', choices = NULL, selected = NULL),
      selectInput('RT_PAR.Summary.Param', 'Parameters', choices = NULL, selected = NULL),
      hr(),
      selectInput('RT_PAR.Summary.Format', 'Format', choices = c('csv','tex'), selected = 'csv'),
      downloadButton("RT_PAR.Summary.Download", "Save this table as csv")
    ),
    
    mainPanel(
      width = 9,
      HTML(paste0('<div style="font-size:120%;">',
                  includeMarkdown('markdown/PAR_SUMMARY_TABLE.Rmd'), '</div>')),
      DT::dataTableOutput('table_RT_PAR_summary')
    )
  )
}

rt_par_sample_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Parameter Sample at Chosen Target Values</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML_P('Set the range and the granularity of the results.
                                The table will show fixed-target parameter values for evenly spaced target values.'),
      
      textInput('RT_PAR.Sample.Min', label = F_MIN_LABEL, value = ''),
      textInput('RT_PAR.Sample.Max', label = F_MAX_LABEL, value = ''),
      textInput('RT_PAR.Sample.Step', label = F_STEP_LABEL, value = ''),
      checkboxInput('RT_PAR.Sample.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                                             Once toggled, only \\(f_{\\text{min}}\\) is
                                                                             used to generate the table on the right.</p>'), value = FALSE),
      selectInput('RT_PAR.Sample.Algid', 'Algorithms', choices = NULL, selected = NULL),
      selectInput('RT_PAR.Sample.Param', 'Parameters', choices = NULL, selected = NULL),
      hr(),
      selectInput('RT_PAR.Sample.Format', 'Format of the table',
                  choices = c('long', 'wide'), selected = 'wide'),
      selectInput('RT_PAR.Sample.FileFormat', 'File-Format', choices = c('csv','tex'), selected = 'csv'),
      downloadButton("RT_PAR.Sample.Download", "Save this table")
    ),
    
    mainPanel(
      width = 9,
      HTML('<p style="font-size:120%;">This table shows for each selected algorithm \\(A\\),
           each selected target value \\(f(x)\\), and each run \\(r\\) the parameter value
           observed when the target value \\(f(x)\\) is reached for the first time.</p>'),
      DT::dataTableOutput('table_RT_PAR_SAMPLE')
    )
  )
}
