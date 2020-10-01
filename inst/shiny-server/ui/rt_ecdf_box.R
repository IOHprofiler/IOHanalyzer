rt_ecdf_single_target_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution: Single target</p>'),
      width = width, collapsible = collapsible, solidHeader = TRUE,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput('RTECDF.Single.Algs', label = 'Select which algorithms to plot:',
                      multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                        custom_icon() %>%
                          bs_embed_popover(
                            title = "Algorithm selection", content = alg_select_info, 
                            placement = "auto"
                          )
                      ),   
          HTML('Select the target values for which EDCF curves are displayed'),
          textInput('RTECDF.Single.Target', label = HTML('<p>\\(f_{target}\\)</p>'),
                    value = ''),

          checkboxInput('RTECDF.Single.Logx', label = 'Scale x axis \\(\\log_{10}\\)', value = F)
        ),

        mainPanel(
          width = 9,
          column(
            width = 12,
            align = "center",
            HTML_P('Each EDCF curve shows the proportion of the runs
                    that have found a solution of at least the required
                    target value within the budget given by the \\(x\\)-axis.
                    The displayed curves can be selected by clicking on the legend on the right. A <b>tooltip</b>
                    and <b>toolbar</b> appears when hovering over the figure.
                    This also includes the option to download the plot as png file.'),
            plotlyOutput.IOHanalyzer("RT_ECDF")
          )
        )
      )
  )
}

rt_ecdf_agg_targets_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Aggregated Empirical Cumulative
                  Distribution: Single function</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('RTECDF.Multi.Algs', label = 'Select which algorithms to plot:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "Algorithm selection", content = alg_select_info, 
                        placement = "auto"
                      )
                  ),   
      HTML('<p align="justify">Set the range and the granularity
           of the quality targets taken into account in the ECDF curve.
           The plot will show the ECDF curves for evenly spaced target values.</p>'),
      textInput('RTECDF.Multi.Min', label = F_MIN_LABEL, value = ''),
      textInput('RTECDF.Multi.Max', label = F_MAX_LABEL, value = ''),
      textInput('RTECDF.Multi.Step', label = F_STEP_LABEL, value = ''),
      # checkboxInput('RTECDF.Multi.Targets',
      #               label = 'Show ECDFs for each target',
      #               value = F),
      checkboxInput('RTECDF.Multi.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = T),

      hr(),
      selectInput('RTECDF.Multi.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),

      downloadButton('RTECDF.Multi.Download', label = 'download the figure')
      ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
       HTML_P('The evenly spaced target values are:'),
       verbatimTextOutput('RT_GRID'),
       HTML_P('The fraction of (run,target value)
              pairs \\((i,v)\\) satisfying that the best solution that the algorithm has
              found in the \\(i\\)-th run within the given time budget \\(t\\) has quality at least
              \\(v\\) is plotted against the available budget \\(t\\). The displayed elements can be switched
              on and off by clicking on the legend on the right. A <b>tooltip</b>
              and <b>toolbar</b> appears when hovering over the figure.'),
       plotlyOutput.IOHanalyzer('RT_ECDF_AGGR')
      )
    )
  )
}

rt_ecdf_agg_fct_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Aggregated Empirical Cumulative
                    Distribution: All functions</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput(
        'RTECDF.Aggr.Algs',
        label = 'Select which algorithms to plot:',
        multiple = T, 
        selected = NULL, 
        choices = NULL
        ) %>% shinyInput_label_embed(
          custom_icon() %>%
            bs_embed_popover(
              title = "Algorithm selection", content = alg_select_info, 
              placement = "auto"
            )
          ),
        
      checkboxInput("RTECDF.Aggr.Func", "Aggregate functions", value = T),
      checkboxInput("RTECDF.Aggr.Dim", "Aggregate dimensions", value = F),
      checkboxInput("RTECDF.Aggr.Logx", "Scale x axis \\(\\log_{10}\\)", value = T),
      checkboxInput("RTECDF.Aggr.Logy", "Scale y axis \\(\\log_{10}\\)", value = F) %>% 
        shinyInput_label_embed(
          custom_icon() %>%
            bs_embed_popover(
              title = "Scaling", content = "The logorithmic scaling might cause some visual issues
              when the smallest y-values are (very close to) 0. 
              Please be mindful of this fact when using this option.", 
              placement = "auto"
            )
        ),
      
      br(),
      actionButton(
        "RTECDF.Aggr.Refresh", 
        label = HTML('<p align="left" style="font-size:100%;">Refresh the figure</p>')
      ),
      
      hr(),
      selectInput("RTECDF.Aggr.Target_type", label = "Select the spacing for the
                  automatically generated ECDF-targets:", 
                  choices = c('linear', 'log-linear', 'bbob'), 
                  selected = 'linear') %>% 
        shinyInput_label_embed(
          custom_icon() %>%
            bs_embed_popover(
              title = "Default targets", content = "The log-linear spacing only works correctly
              when no negative target values are present in the data. The BBOB-spacing is pre-defined
              to 51 log-linear targets between 10^2 and 10^-8.", 
              placement = "auto"
            )
        ),
      numericInput("RTECDF.Aggr.Target_number", label = "Select the number of ECDF-targets to 
                   generate for each function/dimension", value = 10, min = 1, max = 100),
      
      HTML_P('Alternatively, you can download the table containing the target values for each 
              (function, dimension)-pair and edit the table as you want. Please keep 
             the file format when modifying it.'),
      downloadButton('RTECDF.Aggr.Table.Download', label = 'Download the table of targets'),
      br(),
      br(),
      br(),
      
      HTML_P('Upload the table you just downloaded and edited'),
      fileInput(
        "RTECDF.Aggr.Table.Upload", 
        label = NULL,
        multiple = FALSE, 
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
          )
      ),
      
      hr(),
      selectInput('RTECDF.Aggr.Format', label = 'figure format to download',
                  choices = supported_fig_format, selected = 'pdf'),

      downloadButton('RTECDF.Aggr.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        hr(),

        HTML_P('The fraction of (run,target value, ...)
                pairs \\((i,v, ...)\\) satisfying that the best solution that the algorithm has
                found in the \\(i\\)-th (run of function \\(f\\) in dimension \\(d\\)) within
                the given time budget \\(t\\) has quality at least \\(v\\) is plotted against
                the available budget \\(t\\). The displayed elements can be switched
                on and off by clicking on the legend on the right. A <b>tooltip</b>
                and <b>toolbar</b> appears when hovering over the figure. Aggregation over
                functions and dimension can be switched on or off using the checkboxes on
                the left; when aggregation is off the selected function / dimension
                is chosen according the the value in the bottom-left selection-box.'),
        plotlyOutput.IOHanalyzer('RT_ECDF_MULT'),
        HTML_P('The selected targets are:'),
        DT::dataTableOutput('RT_GRID_GENERATED')
      )
    )
  )
}

rt_ecdf_auc_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Area Under the ECDF</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('RTECDF.AUC.Algs', label = 'Select which algorithms to plot:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "Algorithm selection", content = alg_select_info, 
                        placement = "auto"
                      )
                  ),     
      HTML('<p align="justify">Set the range and the granularity of
           the evenly spaced quality targets taken into account in the plot.</p>'),
      textInput('RTECDF.AUC.Min', label = F_MIN_LABEL, value = ''),
      textInput('RTECDF.AUC.Max', label = F_MAX_LABEL, value = ''),
      textInput('RTECDF.AUC.Step', label = F_STEP_LABEL, value = ''),

      hr(),
      selectInput('RTECDF.AUC.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),
      downloadButton('RTECDF.AUC.Download', label = 'Download the figure')
      ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b>area under the ECDF</b> is
               caculated for the sequence of target values specified on the left. The displayed
               values are normalized against the maximal number of function evaluations for
               each algorithm. Intuitively, the larger the area, the better the algorithm.
               The displayed algorithms can be selected by clicking on the legend on the right.
               A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.
               This also includes the option to download the plot as png file.'),
        plotlyOutput.IOHanalyzer("RT_AUC")
      )
    )
  )
}
