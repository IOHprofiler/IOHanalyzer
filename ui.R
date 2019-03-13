#
# This is the user interface of the Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com

suppressMessages(library(shinyjs))
suppressMessages(library(shinydashboard))
suppressMessages(library(plotly))

for (f in list.files('ui', pattern = '.R', full.names = T)) {
  source(f)
}

# The side bar layout ---------------------------------------------
sidebar <- dashboardSidebar(
  useShinyjs(),
  hr(),
  sidebar_menu(),
  hr(),
  DIM_fID_panel()
)

body <- dashboardBody(
  # javascript, headers ----------------------
  # to show text on the header (heading banner)
  tags$head(tags$style(HTML(
    '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
  
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Post-Processing Part for Performance Evaluation</span>\');
      })
     ')),
  
  tags$script(HTML('
       window.setInterval(function() {
        var elem = document.getElementById("process_data_promt");
                   elem.scrollTop = elem.scrollHeight;
                   }, 20);
  ')),
  
  tags$script(HTML('
       window.setInterval(function() {
                   var elem = document.getElementById("upload_data_promt");
                   elem.scrollTop = elem.scrollHeight;
                   }, 20);
  ')),
  
  # using MathJax
  HTML("<head><script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML'
       async></script></head>"),
  
  # tabitems ----------------------
  tabItems(
    tabItem(tabName = 'about', includeMarkdown('RMD/about.Rmd')),
    tabItem(tabName = 'readme', includeMarkdown('README.md')),
    
    # data uploading functionalities -----------------
    tabItem(
      tabName = 'upload',
      fluidRow(
        column(
          width = 6, 
          upload_box(collapsible = F)
        ),
              
        column(
          width = 6,
          upload_prompt_box(collapsible = F)
        )
      ),
            
      fluidRow(
        column(
          width = 12,
          data_list_box(collapsible = F)
        )
      )
    ),
    
    # RT (RunTime): Data Summary -----------------
    tabItem(
      tabName = 'ERT_data', 
      fluidRow(
        column(
          width = 12,
          rt_overview_box(collapsed = F),
          rt_stats_box(collapsed = F),
          rt_sample_box()
        )
      )
    ),
    
    # RT: Expected Convergence Curve ---------------------------------------------
    tabItem(
      tabName = 'ERT_convergence', 
      fluidRow(
        column(
          width = 12,
          ERT_box(collapsed = F)
          # ERT_agg_box(),
          # ERT_comparison_box()
        )
      )
    ),
    
    # RT: histograms, violin plots ------------------------------------------
    tabItem(
      tabName = 'RT_PMF', 
      fluidRow(
        column(
          width = 12,     
          rt_histogram_box(collapsed = F),
          rt_pmf_box()
        )
      )
    ),
    
    # RT ECDF ------------------------------------------
    tabItem(
      tabName = 'RT_ECDF', 
      fluidRow(
        column(
          width = 12,
          rt_ecdf_single_target_box(collapsed = F),
          rt_ecdf_agg_targets_box(),
          rt_ecdf_agg_fct_box()
          # rt_ecdf_auc_box()   
        )
      )
    ),
    
    # FCE: Data Summary -----------------
    tabItem(
      tabName = 'FCE_DATA',
      fluidRow(
        column(
          width = 12,
          fv_overview_box(collapsed = F),
          fv_stats_box(collapsed = F),
          fv_sample_box(collapsed = F)
        )
      )
    ),
    
    # FCE: historgrams, p.d.f. --------
    tabItem(
      tabName = 'FCE_PDF', 
      fluidRow(
        column(
          width = 12,
          fv_histgram_box(collapsed = F),
          fv_pdf_box(collapsed = F)      
        )
      )
    ),
    
    # FCE: Expected Convergence Curve ---------------------------------------------
    tabItem(tabName = 'FCE_convergence', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Expected Target Value 
                                      (per function)</p>'), 
                         width = 12,
                         collapsible = TRUE, solidHeader = TRUE, status = "primary", 
                         div(style = "width: 90%;",
                             sidebarPanel(
                               width = 3,
                               HTML('<p style="font-size:120%;">Range of the displayed budget values</p>'),
                               
                               textInput('FCE_RT_MIN', label = RT_MIN_LABEL, value = ''),
                               textInput('FCE_RT_MAX', label = RT_MAX_LABEL, value = ''),
                               
                               checkboxInput('FCE_show.mean', 
                                             label = 'show/hide mean',
                                             value = T),
                               
                               checkboxInput('FCE_show.median', 
                                             label = 'show/hide median',
                                             value = F),
                               
                               checkboxInput('show.CI.FCE', 
                                             label = 'show/hide mean +/- sd',
                                             value = F),
                               
                               checkboxInput('FCE_semilogx', 
                                             label = 'scale x axis log10',
                                             value = T),
                               
                               checkboxInput('FCE_semilogy', 
                                             label = 'scale y axis log10',
                                             value = T),
                               
                               checkboxInput('FCE_show_grad',
                                             label = 'show runs intensity',
                                             value = F),
                               conditionalPanel(condition = "input.FCE_show_grad == true",
                                                
                                                fluidRow(column(
                                                  11,
                                                  offset = 1,
                                                  sliderInput('FCE_show.intensity',
                                                              label = "Runs intensity(%)",
                                                              min = -1, max = 1, value = 0, step = 0.1)
                                                ))),
                               
                               checkboxInput('FCE_show_all',
                                             label = 'show/hide multiple runs',
                                             value = F),
                               conditionalPanel(condition = "input.FCE_show_all == true",
                                                
                                                fluidRow(column(
                                                  11,
                                                  offset = 1,
                                                  sliderInput('FCE_show.density',
                                                              label = "Runs density(%)",
                                                              min = 1, max = 100, value = 50, step = 1),
                                                  checkboxInput('FCE_show.best_of_all',
                                                                label = 'show/hide best run',
                                                                value = F),
                                                  checkboxInput('FCE_show.pareto_optima',
                                                                label = 'show/hide pareto optimal front',
                                                                value = F)
                                                ))),
                               
                               selectInput('FIG_FORMAT_FV_PER_FUN', label = 'select the figure format',
                                           choices = supported_fig_format, selected = 'pdf'),
                               
                               downloadButton('FIG_DOWNLOAD_FV_PER_FUN', label = 'download the figure')
                             )
                         ),
                         
                         
                         mainPanel(width = 9, 
                                   column(width = 12, align = "center",
                                          HTML_P('The <b><i>mean, median 
                                                  and standard deviation</i></b> of the best function values
                                                  found with a fixed-budget of evaluations are depicted against the budget. 
                                                  The displayed elements can be switched on and off by clicking on the legend on the right. 
                                                  A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
                                          plotlyOutput('FCE_PER_FUN', height = plotly_height, width = plotly_width2)
                                          )
                                  )
                     )
                )
              )
      ),
    
    # FCE: empirical c.d.f. ------------------------------------------
    tabItem(tabName = 'FCE_ECDF', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution 
                                      of the Fixed-Budget Values: Aggregation</p>'), 
                         width = 12,
                         solidHeader = T, status = "primary", collapsible = T, collapsed = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify">Set the range and the granularity of the budgets 
                                taken into account in the ECDF curve. The plot will show the ECDF curves 
                                for evenly spaced budgets.</p>'),
                           textInput('FCE_ECDF_RT_MIN', label = RT_MIN_LABEL, value = ''),
                           textInput('FCE_ECDF_RT_MAX', label = RT_MAX_LABEL, value = ''),
                           textInput('FCE_ECDF_RT_STEP', label = RT_STEP_LABEL, value = ''),
                           
                           checkboxInput('FCE_ECDF_per_target',
                                         label = 'show ECDF for each budget',
                                         value = F),
                           
                           checkboxInput('FCE_ECDF_AGGR_semilogx', 
                                         label = 'scale x axis log10',
                                         value = F),
                           selectInput('FIG_FORMAT_FV_ECDF_AGGR', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           
                           downloadButton('FIG_DOWNLOAD_FV_ECDF_AGGR', label = 'download the figure')
                           ),
                         
                         mainPanel(width = 9,
                                   column(width = 12, align = "center",
                                          HTML_P('The evenly spaced budget values are:'),
                                          verbatimTextOutput('FCE_RT_GRID'),
                                          HTML_P('The fraction of (run,budget) pairs \\((i,B)\\) satisfying 
                                                  that the best solution that the algorithm has found in the 
                                                  \\(i\\)-th run within the first \\(B\\) evaluations has quality at 
                                                  <b>most</b> \\(v\\) is plotted against the target value \\(v\\). The
                                                  displayed elements can be switched on and off by clicking on the 
                                                  legend on the right. A <b>tooltip</b> and <b>toolbar</b> appears when 
                                                  hovering over the figure.'),
                                          plotlyOutput('FCE_ECDF_AGGR', height = plotly_height, width = plotly_width2),
                                          hr()
                                         )
                                   )
                    )
              ),
              
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Area Under the ECDF</p>'),  
                         width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify">Set the range and the granularity of the evenly spaced budgets.</p>'),
                           textInput('FCE_AUC_RT_MIN', label = RT_MIN_LABEL, value = ''),
                           textInput('FCE_AUC_RT_MAX', label = RT_MAX_LABEL, value = ''),
                           textInput('FCE_AUC_RT_STEP', label = RT_STEP_LABEL, value = ''),
                           
                           selectInput('FIG_FORMAT_FV_AUC', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           
                           downloadButton('FIG_DOWNLOAD_FV_AUC', label = 'download the figure')
                           ),
                         
                         mainPanel(width = 9,
                                   column(width = 12, align = "center",
                                          HTML_P('The <b>area under the ECDF</b> is 
                                                  caculated for the sequence of budget values specified on the left. The displayed
                                                  values are normalized against the maximal target value recorded for 
                                                  each algorithm. Intuitively, the <b>smaller</b> the area, the <b>better</b> the algorithm. 
                                                  The displayed algorithms can be selected by clicking on the legend on the right. 
                                                  A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
                                          plotlyOutput("FCE_AUC", height = plotly_height, width = plotly_width2)
                                         )
                                   )
                         )
              ),
              
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution of the Fixed-Budget Values: Single Budgets</p>'), 
                         width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             HTML('Select the budgets for which EDCF curves are displayed '),
                             textInput('FCE_ECDF_RT1', label = HTML('<p>\\(B_1\\)</p>'), 
                                       value = ''),
                             textInput('FCE_ECDF_RT2', label = HTML('<p>\\(B_2\\)</p>'), 
                                       value = ''),
                             textInput('FCE_ECDF_RT3', label = HTML('<p>\\(B_3\\)</p>'), 
                                       value = ''),
                             checkboxInput('FCE_ECDF_semilogx', label = 'scale x axis log10', value = F)
                           ), 
                           
                           mainPanel(width = 9,
                                     column(width = 12, align = "center",
                                            HTML_P('Each EDCF curve shows the proportion of the runs that have found within the 
given budget B a solution of at least the required target value given by the x-axis. The displayed curves can be selected
by clicking on the legend on the right. A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
                                            plotlyOutput("FCE_ECDF_PER_TARGET", height = plotly_height, width = plotly_width2)
                                            )
                                     )
                           )
                        )
                    )
                )
        ),
    
    # Parameter tab -------
    tabItem(tabName = 'PARAMETER', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Expected Parameter Value 
                                      (per function)</p>'), 
                      width = 12,
                      collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
                      div(style = "width: 90%;",
                          sidebarPanel(
                            width = 3,
                            HTML('<p style="font-size:120%;">Range of the function values (\\(x\\) axis)</p>'),
                            
                            textInput('PAR_F_MIN', label = F_MIN_LABEL, value = ''),
                            textInput('PAR_F_MAX', label = F_MAX_LABEL, value = ''),
                            
                            selectInput('PAR_ALGID_INPUT', 'Algorithms', choices = NULL, selected = NULL),
                            selectInput('PAR_show.mean', label = 'mean/median', 
                                        choices = c('mean', 'median'),
                                        selected = 'mean'),
                            
                            checkboxInput('show.CI.PAR', 
                                          label = 'show/hide mean +/- sd',
                                          value = F),
                            
                            checkboxInput('PAR_semilogx', 
                                          label = 'scale x axis log10',
                                          value = T),
                            
                            checkboxInput('PAR_semilogy', 
                                          label = 'scale y axis log10',
                                          value = T),
                            
                            selectInput('FIG_FORMAT_PAR_PER_FUN', label = 'select the figure format',
                                        choices = supported_fig_format, selected = 'pdf'),
                            downloadButton('FIG_DOWNLOAD_PAR_PER_FUN', label = 'download the figure')
                          )
                      ),
                      
                      mainPanel(width = 9,
                                HTML_P('The <b><i>mean or median</i></b> of internal parameters of the algorithm
                                        found with a fixed-budget of evaluations are depicted against the budget. 
                                        The displayed elements can be switched on and off by clicking on the legend on the right. 
                                        A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
                                column(width = 12, align = "center",
                                       plotlyOutput('PAR_PER_FUN', height = plotly_height, width = plotly_width2)
                                       )
                                )
                      ),
                     
                     box(title = HTML('<p style="font-size:120%;">Parameter Statistics at Chosen Target Values</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = T, collapsed = T,
                         sidebarPanel(
                           width = 3,
                           HTML_P('Set the range and the granularity of the results.
                                The table will show fixed-target parameter values for evenly spaced target values.'),
                           
                           textInput('PAR_F_MIN_SUMMARY', label = F_MIN_LABEL, value = ''),
                           textInput('PAR_F_MAX_SUMMARY', label = F_MAX_LABEL, value = ''),
                           textInput('PAR_F_STEP_SUMMARY', label = F_STEP_LABEL, value = ''),
                           checkboxInput('PAR_F_SINGLE', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                                       Once toggled, only \\(f_{\\text{min}}\\) is 
                                                                       used to generate the table on the right.</p>'), value = FALSE),
                           selectInput('PAR_ALGID_INPUT_SUMMARY', 'Algorithms', choices = NULL, selected = NULL),
                           selectInput('PAR_INPUT', 'Parameters', choices = NULL, selected = NULL),
                           downloadButton("PAR_downloadData", "Save this table as csv")
                           ),
                         
                         mainPanel(
                           width = 9,
                           HTML(paste0('<div style="font-size:120%;">', includeMarkdown('RMD/PAR_SUMMARY_TABLE.Rmd'),'</div>')),
                           tableOutput('table_PAR_summary')
                         )
                     ),
                     
                     box(title = HTML('<p style="font-size:120%;">Parameter Sample at Chosen Target Values</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = T, collapsed = T,
                         sidebarPanel(
                           width = 3,
                           HTML_P('Set the range and the granularity of the results.
                                The table will show fixed-target parameter values for evenly spaced target values.'),

                           textInput('PAR_F_MIN_SAMPLE', label = F_MIN_LABEL, value = ''),
                           textInput('PAR_F_MAX_SAMPLE', label = F_MAX_LABEL, value = ''),
                           textInput('PAR_F_STEP_SAMPLE', label = F_STEP_LABEL, value = ''),
                           checkboxInput('PAR_SAMPLE_F_SINGLE', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                                             Once toggled, only \\(f_{\\text{min}}\\) is 
                                                                             used to generate the table on the right.</p>'), value = FALSE),
                           selectInput('PAR_ALGID_INPUT_SAMPLE', 'Algorithms', choices = NULL, selected = NULL),
                           selectInput('PAR_INPUT_SAMPLE', 'Parameters', choices = NULL, selected = NULL),
                           selectInput('PAR_download_format', 'Format of the csv', 
                                       choices = c('long', 'wide'), selected = 'wide'),
                           downloadButton("PAR_SAMPLE_downloadData", "Save this table as csv")
                         ),

                         mainPanel(
                           width = 9,
                           HTML('<p style="font-size:120%;">This table shows for each selected algorithm \\(A\\), 
                                 each selected target value \\(f(x)\\), and each run \\(r\\) 
                                 the parameter value observed when the target value \\(f(x)\\) is reached for the first time.</p>'),
                           dataTableOutput('table_PAR_SAMPLE')
                         )
                     )
                  )
              )
        )
  )
)

# -----------------------------------------------------------
dashboardPage(title = IOHprofiler, header, sidebar, body)
