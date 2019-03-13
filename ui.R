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
    tabItem(tabName = 'upload',
            fluidRow(
              column(
                width = 6, 
                upload_box()
              )
              # column(width = 6,
              #        box(title = HTML('<p style="font-size:120%;">Load Data from repository</p>'), width = 12,
              #            solidHeader = T, status = "primary", collapsible = F, height = '620px',
              #            sidebarPanel(
              #              width = 12,
              #              
              #              selectInput('REPOSITORY_SUITE', label = HTML('<p align="left" style="font-size:120%;">Please choose the suite</p>'),
              #                          choices = c("none",IOHprofiler, COCO), selected = "none", width = '50%'),
              #              selectInput('REPOSITORY_FUNCID', label = HTML('<p align="left" style="font-size:120%;">Please choose the function</p>'),
              #                          choices = NULL, selected = NULL, width = '50%'),
              #              selectInput('REPOSITORY_DIM', label = HTML('<p align="left" style="font-size:120%;">Please choose the dimension</p>'),
              #                          choices = NULL, selected = NULL, width = '50%'),
              #              selectInput('REPOSITORY_ALGID', label = HTML('<p align="left" style="font-size:120%;">Please choose the algorithm</p>'),
              #                          choices = NULL, selected = NULL, width = '50%'),
              #              
              #              actionButton('REPOSITORY_LOAD', 'Load data')
              #            )
              #        )
              # )
            ),
            
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">List of Processed Data</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = T, 
                         dataTableOutput('DATASETLIST_INFO')
                     )
              )
            ),
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Data Processing Prompt</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = F,
                         verbatimTextOutput('process_data_promt'),
                         tags$head(tags$style("#process_data_promt{color:black; font-size:12px; font-style:italic;
                                                overflow-y:visible; max-height: 500px; background: ghostwhite;}"))
                     )
              )
            )
    ),
    
    # RT (RunTime): Data Summary -----------------
    tabItem(
      tabName = 'ERT_data', 
      fluidRow(
        column(
          width = 12,
          rt_overview_box(),
          rt_stats_box(),
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
          ERT_box(collapsed = F),
          ERT_agg_box(),
          ERT_comparison_box()
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
          rt_ecdf_single_target_box(collapsed = F)          
        ),
        
        column(
          width = 12,
          rt_ecdf_agg_targets_box()           
        ),
        
        column(
          width = 12,
          rt_ecdf_agg_fct_box()
        ),
              
        column(
          width = 12,
          rt_ecdf_auc_box()          
        ) 
      )
    ),
    
    # FCE: Data Summary -----------------
    tabItem(tabName = 'FCE_DATA', 
            fluidRow(
              column(width = 12,
                     #TODO: better naming scheme
                     box(title = HTML('<p style="font-size:120%;">Overview of runtime values</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify">Select which algorithms to show.</p>'),
                           
                           # TODO: find better naming scheme for 'fstart, fstop, singleF'
                           selectInput('FCE_ALGID_INPUT_SUMMARY', 'Algorithms', choices = NULL, selected = NULL),
                           downloadButton("FCE_downloadData_summary", "Save this table as csv")
                         ),
                         
                         mainPanel(
                           width = 9,
                           tableOutput('table_RT_summary_condensed')
                         )
                     ),
                     box(title = HTML('<p style="font-size:120%;">Target Statistics at Chosen Budget Values</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = T, collapsed = T,
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
                     ),
                     
                     box(title = HTML('<p style="font-size:120%;">Original Target Samples</p>'), width = 12,
                         solidHeader = TRUE, status = "primary", collapsible = T, collapsed = T,
                         sidebarPanel(
                           width = 3,
                           HTML(FCE_GRID_INPUT_TEXT),
                           
                           textInput('RT_MIN_SAMPLE', label = RT_MIN_LABEL, value = ''),
                           textInput('RT_MAX_SAMPLE', label = RT_MAX_LABEL, value = ''),
                           textInput('RT_STEP_SAMPLE', label = RT_STEP_LABEL, value = ''),
                           checkboxInput('RT_SINGLE_SAMPLE', 
                                         label = HTML('<p>\\(B_{\\text{min}} = B_{\\text{max}}\\)?
                                                       Once toggled, only \\(B_{\\text{min}}\\) is 
                                                       used to generate the table on the right.</p>'), value = FALSE),
                           selectInput('FCE_ALGID_RAW_INPUT', 'Algorithms', 
                                       choices = NULL, selected = NULL),
                           
                           selectInput('download_format_FCE', 'Format of the csv', 
                                       choices = c('long', 'wide'), selected = 'wide'),
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
                )
            )
        ),
    
    # FCE: historgrams, p.d.f. --------
    tabItem(tabName = 'FCE_PDF', 
            fluidRow(
              column(width = 12,     
                     box(title = 'Histogram of Fixed-Budget Targets', 
                         width = 12, collapsible = TRUE, solidHeader = TRUE,  collapsed = T,
                         status = "primary",
                         sidebarPanel(
                           width = 2,
                           textInput('FCE_HIST_RUNTIME', label = HTML('Select the budget value'), 
                                     value = ''),
                           
                           HTML('Choose whether the histograms are <b>overlaid</b> in one plot 
                                or <b>separated</b> in several subplots:'),
                           selectInput('FCE_illu_mode', '', 
                                       choices = c("overlay", "subplot"), 
                                       selected = 'subplot'),
                           
                           selectInput('FIG_FORMAT_FV_HIST', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           
                           downloadButton('FIG_DOWNLOAD_FV_HIST', label = 'download the figure')
                           
                           ),
                         
                         mainPanel(
                           width = 10,
                           column(width = 12, align = "center",
                                  HTML('<p align="left" "font-size:120%;">
                                       This histogram counts the number of runs whose best-so-far function values within the 
first \\(B\\) evaluations is between \\(v_i\\) and \\(v_{i+1}\\). The buckets \\([v_i,v_{i+1})\\) are chosen automatically 
according to the so-called <b>Freedman–Diaconis rule</b>: \\(\\text{Bin size}= 2\\frac{Q_3 - Q_1}{\\sqrt[3]{n}}\\), 
where \\(Q_1, Q_3\\) are the \\(25\\%\\) and \\(75\\%\\) percentile of the runtime and \\(n\\) is the sample size.
                                       The displayed algorithms can be selected by clicking on the legend on the right. 
                                       A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
                                  plotlyOutput('FCE_HIST', height = plotly_height2, width = plotly_width2)
                           )
                         )
                     )
              ),
              
              column(width = 12,
                     box(title = 'Empirical Probability Density Function of Fixed-Budget Function Values', 
                         width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
                         sidebarLayout(
                           sidebarPanel(
                             width = 2,
                             HTML('Select the budget for which the distribution of best-so-far function values is shown'),
                             
                             textInput('FCE_PDF_RUNTIME', label = '', value = ''),
                             checkboxInput('FCE_SHOW_SAMPLE', label = 'show runtime samples', value = T),
                             checkboxInput('FCE_LOGY', label = 'scale y axis log10', value = T),
                             
                             selectInput('FIG_FORMAT_FV_PDF', label = 'select the figure format',
                                         choices = supported_fig_format, selected = 'pdf'),
                             
                             downloadButton('FIG_DOWNLOAD_FV_PDF', label = 'download the figure')
                           ),
                           
                           mainPanel(
                             width = 10,
                             column(width = 12, align = "center", 
                                    HTML('<p align="left" style="font-size:120%;">
                                         The plot shows, for the budget selected on the left, the distribution 
of the best-so-far function values of the individual runs (dots), and an estimated distribution of the probability mass function. 
The displayed algorithms can be selected by clicking on the legend on the right. A <b>tooltip</b> and <b>toolbar</b> 
appear when hovering over the figure. A csv file with the runtime data can be downlowaded from the 
                                         <a href="#shiny-tab-FCE_DATA", data-toggle="tab"> Data Summary tab</a>.'),
                                    plotlyOutput('FCE_PDF', height = plotly_height, width = plotly_width2)
                             )
                          )
                       )
                     )
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
