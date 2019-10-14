fv_ecdf_single_budget_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution of
                 the Fixed-Budget Values: Single Budgets</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = TRUE, status = "primary",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        selectInput('FCEECDF.Single.Algs', label = 'Select which algorithms to plot:',
                    multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "Algorithm selection", content = alg_select_info, 
                          placement = "auto"
                        )
                    ),
        HTML('Select the budgets for which EDCF curves are displayed '),
        textInput('FCEECDF.Single.Target', label = HTML('<p>\\(B_1\\)</p>'), value = ''),
        checkboxInput('FCEECDF.Single.Logx', label = 'Scale x axis \\(\\log_{10}\\)', value = F)
      ),

      mainPanel(
        width = 9,
        column(
          width = 12, align = "center",
          HTML_P('Each EDCF curve shows the proportion of the runs that have found
                  within the given budget B a solution of at least the required target
                  value given by the x-axis. The displayed curves can be selected
                  by clicking on the legend on the right. A <b>tooltip</b> and <b>toolbar</b>
                  appears when hovering over the figure.</p>'),
          plotlyOutput.IOHanalyzer("FCE_ECDF_PER_TARGET")
        )
      )
    )
  )
}

fv_ecdf_agg_budgets_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution
                    of the Fixed-Budget Values: Aggregation</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('FCEECDF.Mult.Algs', label = 'Select which algorithms to plot:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "Algorithm selection", content = alg_select_info, 
                        placement = "auto"
                      )
                  ),
      HTML('<p align="justify">Set the range and the granularity of the budgets
           taken into account in the ECDF curve. The plot will show the ECDF curves
           for evenly spaced budgets.</p>'),
      textInput('FCEECDF.Mult.Min', label = RT_MIN_LABEL, value = ''),
      textInput('FCEECDF.Mult.Max', label = RT_MAX_LABEL, value = ''),
      textInput('FCEECDF.Mult.Step', label = RT_STEP_LABEL, value = ''),
# 
#       checkboxInput('FCEECDF.Mult.Targets',
#                     label = 'Show ECDF for each budget',
#                     value = F),

      checkboxInput('FCEECDF.Mult.Logx',
                    label = 'Scale x axis \\(\\log_{10}\\)',
                    value = F),
      hr(),
      selectInput('FCEECDF.Mult.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),

      downloadButton('FCEECDF.Mult.Download', label = 'Download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The evenly spaced budget values are:'),
        verbatimTextOutput('FCE_RT_GRID'),

        HTML_P('The fraction of (run,budget) pairs \\((i,B)\\) satisfying that the best
                solution that the algorithm has found in the \\(i\\)-th run within the
                first \\(B\\) evaluations has quality at <b>most</b> \\(v\\) is plotted
                against the target value \\(v\\). The displayed elements can be switched
                on and off by clicking on the legend on the right. A <b>tooltip</b> and
                <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput.IOHanalyzer('FCE_ECDF_AGGR')
      )
    )
  )
}

fv_ecdf_auc_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Area Under the ECDF</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('FCEECDF.AUC.Algs', label = 'Select which algorithms to plot:',
                  multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                    custom_icon() %>%
                      bs_embed_popover(
                        title = "Algorithm selection", content = alg_select_info, 
                        placement = "auto"
                      )
                  ),      
      HTML('<p align="justify">Set the range and the granularity of the evenly spaced budgets.</p>'),
      textInput('FCEECDF.AUC.Min', label = RT_MIN_LABEL, value = ''),
      textInput('FCEECDF.AUC.Max', label = RT_MAX_LABEL, value = ''),
      textInput('FCEECDF.AUC.Step', label = RT_STEP_LABEL, value = ''),

      hr(),
      selectInput('FCEECDF.AUC.Format', label = 'select the figure format',
                  choices = supported_fig_format, selected = 'pdf'),

      downloadButton('FCEECDF.AUC.Download', label = 'download the figure')
    ),

    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        HTML_P('The <b>area under the ECDF</b> is
                caculated for the sequence of budget values specified on the left. The displayed
                values are normalized against the maximal target value recorded for
                each algorithm. Intuitively, the <b>smaller</b> the area, the <b>better</b> the algorithm.
                The displayed algorithms can be selected by clicking on the legend on the right.
                A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
        plotlyOutput.IOHanalyzer("FCE_AUC")
      )
    )
  )
}
