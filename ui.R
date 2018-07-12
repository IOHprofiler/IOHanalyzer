#
# This is the user interface of the Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com

library(shinyjs)
library(shinydashboard)
library(plotly)

# TODO: rename most of the id of the control widgets

FCE_GRID_INPUT_TEXT <- '<p align="justify">Set the range and the granularity of the results. 
The table will show function values that have been reached within evenly spaced evaluation budgets.</p>'

RT_MIN_LABEL <- HTML('<p>\\(B_{\\text{min}}:\\) smallest budget value</p>')
RT_MAX_LABEL <- HTML('<p>\\(B_{\\text{max}}:\\) largest budget value</p>')
RT_STEP_LABEL <- HTML('<p>\\(\\Delta B:\\) granularity (step size)</p>')

F_MIN_LABEL <- HTML('<p>\\(f_{\\text{min}}:\\) smallest target value</p>')
F_MAX_LABEL <- HTML('<p>\\(f_{\\text{max}}:\\) largest target value</p>')
F_STEP_LABEL <- HTML('<p>\\(\\Delta f:\\) granularity (step size)</p>')

header <- dashboardHeader(title = HTML('<h4><div align="center"><b>IOHProfiler</b><br>
                                       <i>Post-Processing</i></div></h4>'))

HTML_P <- function(s) HTML(paste0('<p align="left" style="font-size:120%";>', s, '</p>'))

# The side bar layout ---------------------------------------------
sidebar <- dashboardSidebar(
  useShinyjs(),
  hr(),
  sidebarMenu(id = "tabs",
              menuItem("Upload Data", tabName = "upload", icon = icon('upload', lib = 'glyphicon'), 
                       selected = T),
              
              menuItem("Fixed-Target Results", tabName = "ERT", icon = icon("file-text-o"),
                       menuSubItem("Data Summary", tabName = "ERT_data", icon = icon("table")),
                       menuSubItem("Expected Runtime", tabName = "ERT_convergence", icon = icon("line-chart"), selected = F),
                       menuSubItem("Probility Mass Function", tabName = "RT_PMF", icon = icon("bar-chart"), selected = F),
                       menuSubItem("Cumulative Distribution", tabName = "RT_ECDF", icon = icon("line-chart"), selected = F)
                       ),
              
              menuItem("Fixed-Budget Results", tabName = "FCE", icon = icon("file-text-o"),
                       menuSubItem("Data Summary", tabName = "FCE_DATA", icon = icon("table")),
                       menuSubItem("Expected Target Value", tabName = "FCE_convergence", icon = icon("bar-chart")),
                       menuSubItem("Probility Density Function", tabName = "FCE_PDF", icon = icon("bar-chart"), selected = F),
                       menuSubItem("Cumulative Distribution", tabName = "FCE_ECDF", icon = icon("line-chart"))
              ),
              
              menuItem("Algorithm Parameters", tabName = "PARAMETER", icon = icon('file-text-o')),
              menuItem("ReadMe", tabName = "readme", icon = icon("mortar-board")),
              menuItem("About", tabName = "about", icon = icon("question"))
  ),
  
  hr(),
  conditionalPanel("input.tabs!='upload' && input.tabs!='readme' && input.tabs!='about'",
                   fluidRow(
                     column(11,
                            selectInput("FUNCID_INPUT", 
                                        label = HTML('<p style="font-size:120%;">Please select the function ID</p>'), 
                                        choices = NULL, selected = NULL)
                     )
                   ),
                   
                   fluidRow(
                     column(11,
                            selectInput("DIM_INPUT",
                                        label = HTML('<p style="font-size:120%;">Please select the dimension</p>'), 
                                        choices = NULL, selected = NULL)
                     )
                   )
  )
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
        $("header").find("nav").append(\'<span class="myClass"> <b>I</b>terative <b>O</b>ptimization <b>H</b>euristics <b>P</b>rofiler</span>\');
      })
     ')),
  
  tags$script(HTML('
       window.setInterval(function() {
        var elem = document.getElementById("process_data_promt");
                   elem.scrollTop = elem.scrollHeight;
                   }, 20);
  ')),
  
  # tags$script(HTML('
  #      window.setInterval(function() {
  #                  var elem = document.getElementById("upload_data_promt");
  #                  elem.scrollTop = elem.scrollHeight;
  #                  }, 20);
  # ')),
  
  # include MathJax
  HTML("<head><script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML'
       async></script></head>"),
  
  # tabitems ----------------------
  tabItems(
    tabItem(tabName = 'about', includeMarkdown('RMD/about.Rmd')),
    tabItem(tabName = 'readme', includeMarkdown('RMD/docs.Rmd')),
    
    # data uploading functionalities -----------------
    tabItem(tabName = 'upload',
            fluidRow(
              column(width = 6,
                     box(title = HTML('<p style="font-size:120%;">Upload Data</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = F, height = '500px',
                         sidebarPanel(
                           width = 12,
                           
                           HTML('<p align="left" style="font-size:120%;">Please choose a zip file containing the benchmark data</p>'),
                           fileInput("ZIP", "Choose a ZIP file", multiple = TRUE,
                                     accept = c("Application/zip", ".zip")),
                           
                           # TODO: keep this for the local version
                           # shinyDirButton('directory', 'Browse the folder', 
                           #                title = 'Please choose a directory containing the benchmark data'),
                           hr(),
                           HTML('<p align="left" style="font-size:120%;">You can also remove all data you uploaded</p>'),
                           actionButton('RM_DATA', 'Clear data')
                         ),
                         
                         mainPanel(
                           width = 12,
                           HTML('<p style="font-size:120%;">The processed folders are:</p>'),
                           verbatimTextOutput('upload_data_promt'),
                           tags$head(tags$style("#upload_data_promt{color:red; font-size:12px; font-style:italic;
                                                overflow-y:visible; max-height: 100px; background: ghostwhite;}"))
                         )
                     )
              ),
              
              column(width = 6,
                     box(title = HTML('<p style="font-size:120%;">Data Processing Prompt</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = F, height = '500px',
                         verbatimTextOutput('process_data_promt'),
                         tags$head(tags$style("#process_data_promt{color:red; font-size:12px; font-style:italic;
                                              overflow-y:visible; max-height: 425px; background: ghostwhite;}"))
                     )
              )
            ),
            
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">List of processed Data</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = T, 
                         dataTableOutput('DATASETLIST_INFO')
                     )
              )
            )
    ),
    
    # RT: Data Summary -----------------
    tabItem(tabName = 'ERT_data', 
      fluidRow(
        column(width = 12,
               box(title = HTML('<p style="font-size:120%;">Runtime Statistics at Chosen Target Values</p>'), width = 12,
                   solidHeader = T, status = "primary", collapsible = T,
                   sidebarPanel(
                     width = 3,
                     HTML('<p align="justify">Set the range and the granularity of the results.
                          The table will show fixed-target runtimes for evenly spaced target values.</p>'),
                     
                     textInput('fstart', label = F_MIN_LABEL, value = ''),
                     textInput('fstop', label = F_MAX_LABEL, value = ''),
                     textInput('fstep', label = F_STEP_LABEL, value = ''),
                     selectInput('ALGID_INPUT', 'Algorithms', choices = NULL, selected = NULL),
                     downloadButton("downloadData", "Save this table as csv")
                   ),
                   
                   mainPanel(
                     width = 9,
                     HTML(paste0('<div style="font-size:120%;">', includeMarkdown('RMD/RT_SUMMARY_TABLE.Rmd'),'</div>')),
                     tableOutput('table_RT_summary')
                   )
               ),
               
               box(title = HTML('<p style="font-size:120%;">Original Runtime Samples</p>'), width = 12,
                   solidHeader = TRUE, status = "primary",
                   sidebarPanel(
                     width = 3,
                     HTML('<p align="justify">Set the range and the granularity of the results.
                          The table will show fixed-target runtimes for evenly spaced target values.</p>'),
                     
                     textInput('F_MIN_SAMPLE', label = F_MIN_LABEL, value = ''),
                     textInput('F_MAX_SAMPLE', label = F_MAX_LABEL, value = ''),
                     textInput('F_STEP_SAMPLE', label = F_STEP_LABEL, value = ''),
                     checkboxInput('F_LOGSPACE_DATA_SUMMARY',
                                   label = HTML('Evenly space target values in \\(log_{10}\\) space')),
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
          )
      )
    ),
    
    # RT: Expected Convergence Curve ---------------------------------------------
    tabItem(tabName = 'ERT_convergence', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                                      (per function)</p>'), 
                         width = 12,
                         collapsible = TRUE, solidHeader = TRUE, status = "primary",
                         div(style = "width: 90%;",
                           sidebarPanel(
                             width = 3,
                             HTML('<p style="font-size:120%;">Range of the displayed target values</p>'),
                             
                             textInput('ERT_FSTART', 
                                       label = F_MIN_LABEL, 
                                       value = ''),
                             textInput('ERT_FSTOP', 
                                       label = F_MAX_LABEL, 
                                       value = ''),
                             
                             checkboxInput('show.mean', 
                                           label = 'show/hide mean',
                                           value = T),
                             
                             checkboxInput('show.median', 
                                           label = 'show/hide median',
                                           value = F),
                             
                             checkboxInput('semilogx', 
                                           label = 'scale x axis log10',
                                           value = T),
                             
                             checkboxInput('semilogy', 
                                           label = 'scale y axis log10',
                                           value = T)
                             
                             # checkboxInput('show.instance', 
                             #               label = 'show each independent run',
                             #               value = F)
                           )
                         ),
                         
                         HTML('<p style="font-size:120%";>The <b><i>mean, median 
                              and standard deviation</i></b> of the runtime samples 
                              are depicted against the best objective values. 
                              The displayed elements (mean, median, standard deviations) 
                              can be switched on and off by clicking on the legend on the right. 
                              A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
                         mainPanel(plotlyOutput('ERT_PER_FUN', height = "700px", width = "1157px"))
                     )
                     
                     # box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                     #                  (across all functions)</p>'), 
                     #     height = 800, width = 12,
                     #     collapsible = TRUE, solidHeader = TRUE, status = 'primary')
              )
            )
    ),
    
    # RT: histograms, violin plots ------------------------------------------
    tabItem(tabName = 'RT_PMF', 
          fluidRow(
            column(width = 12,     
                   box(title = 'Histogram of Fixed-Target Runtimes', 
                       width = 12, collapsible = TRUE, solidHeader = TRUE, 
                       status = "primary",
                       sidebarPanel(
                         width = 2,
                         textInput('RT_PMF_HIST_FTARGET', label = HTML('Select the target value'), 
                                   value = ''),
                         
                         HTML('Choose whether the histograms are <b>overlaid</b> in one plot 
                              or <b>separated</b> in several subplots:'),
                         selectInput('ERT_illu_mode', '', 
                                     choices = c("overlay", "subplot"), 
                                     selected = 'subplot')
                         ),
                       
                       mainPanel(
                         width = 10,
                         column(width = 12, align = "center",
                                HTML_P('This histogram counts how many runs needed between 
                                    \\(t\\) and \\(t+1\\) function evaluations. The bins
                                    \\([t,t+1)\\) are chosen automatically. The bin size is determined
                                    by the so-called <b>Freedman–Diaconis rule</b>: \\(\\text{Bin size}=
                                    2\\frac{Q_3 - Q_1}{\\sqrt[3]{n}}\\), where \\(Q_1, Q_3\\) are the \\(25\\%\\) 
                                    and \\(75\\%\\) percentile of the runtime and \\(n\\) is the sample size.
                                    The displayed algorithms can be selected by clicking on the legend on the right. 
                                    A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
                                plotlyOutput('RT_HIST', height = "800px", width = "1300px")
                                )
                       )
                       )
            ),
            
            column(width = 12,
              box(title = 'Empirical Probability Mass Function of the Runtime', 
                  width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary",
                  sidebarLayout(
                    sidebarPanel(
                      width = 2,
                      HTML('Select the target value for which the runtime distribution is shown'),
                      
                      textInput('RT_PMF_FTARGET', label = '', value = ''),
                      checkboxInput('RT_SHOW_SAMPLE', label = 'show runtime for each run', value = T),
                      checkboxInput('RT_PMF_LOGY', label = 'scale y axis log10', value = F)
                      
                      # HTML('Kernel density estimation uses the following <b>kernel function</b>:'),
                      # selectInput('RT_PMF_KER', '',
                      #             choices = c("gaussian", "epanechnikov", "rectangular",
                      #                         "triangular", "biweight", "cosine", "optcosine"),
                      #             selected = 'gaussian')

                    ),
                    
                    mainPanel(
                      width = 10,
                      column(width = 12, align = "center", 
                             HTML('<p class="alert alert-warning" align="left" style="font-size:120%;"><b>Warning! </b>The 
                                  <b>probability mass function</b> of the runtime is approximated by the 
                                  treating the runtime as a <i>continuous</i> random variable and applying the <b>kernel estimation</b> (KDE):</p>'),
                             HTML('<p align="left" style="font-size:120%;">
                                  The plot shows the distribution of the first hitting 
                                  times of the individual runs (dots), and an estimated 
                                  distribution of the probability mass function. 
                                  The displayed algorithms can be selected by clicking on 
                                  the legend on the right. A <b>tooltip</b> and <b>toolbar</b> 
                                  appear when hovering over the figure. This also includes the
                                  option to download the plot as png file. A csv file with the runtime 
                                  data can be downlowaded from the  
                                  <a href="#shiny-tab-ERT_data", data-toggle="tab"> Data Summary tab</a>.'),
                             plotlyOutput('RT_PMF', height = "800px", width = "1300px")
                      )
                    )
                  )
              )
            )
          )
    ),
    
    # RT empirical c.d.f. ------------------------------------------
    tabItem(tabName = 'RT_ECDF', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Empirical Cumulative 
                                      Distribution of the runtime: Aggregation</p>'), 
                         width = 12, solidHeader = T, status = "primary", collapsible = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify">Set the range and the granularity 
                                of the quality targets taken into account in the ECDF curve. 
                                The plot will show the ECDF curves for evenly spaced target values.</p>'),
                           textInput('RT_fstart', label = F_MIN_LABEL, value = ''),
                           textInput('RT_fstop', label = F_MAX_LABEL, value = ''),
                           textInput('RT_fstep', label = F_STEP_LABEL, value = ''),
                           checkboxInput('RT_ECDF_per_target',
                                         label = 'show ECDFs for each target',
                                         value = F),
                           checkboxInput('RT_ECDF_AGGR_semilogx', 
                                         label = 'scale x axis log10',
                                         value = F)
                         ),
                         
                         mainPanel(
                           width = 9,
                           HTML('<p style="font-size:120%;">The evenly spaced target values are:</p>'),
                           verbatimTextOutput('RT_GRID'),
                           HTML('<p style="font-size:120%;">The fraction of (run,target value)
                                pairs \\((i,v)\\) satisfying that the best solution that the algorithm has 
                                found in the \\(i\\)-th run within the given time budget \\(t\\) has quality at least
                                \\(v\\) is plotted against the available budget \\(t\\). The displayed elements can be switched 
                                on and off by clicking on the legend on the right. A <b>tooltip</b> 
                                and <b>toolbar</b> appears when hovering over the figure.</p>'),
                           plotlyOutput('RT_ECDF_AGGR', height = "600px", width = "1100px"),
                           hr()
                           
                           # HTML('<p>The <b>Kolmogorov–Smirnov test</b> is used to investigate
                           # how the ditribution of runtimes of one algorithm differs from the other:</p><br>'),
                           # column(10, align = "center", verbatimTextOutput('ks'))
                         )
                     )
              ),
              
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Area Under the ECDF</p>'),  
                         width = 12, solidHeader = T, status = "primary", collapsible = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify">Set the range and the granularity of
                                the evenly spaced quality targets taken into account in the plot.</p>'),
                           textInput('RT_AUC_FSTART', label = F_MIN_LABEL, value = ''),
                           textInput('RT_AUC_FSTOP', label = F_MAX_LABEL, value = ''),
                           textInput('RT_AUC_FSTEP', label = F_STEP_LABEL, value = '')
                         ),
                         
                         mainPanel(width = 9,
                                   HTML('<p align="left" style="font-size:120%;">The <b>area under the ECDF</b> is 
                                        caculated for the sequence of target values specified on the left. The displayed
                                        values are normalized against the maximal number of function evaluations for 
                                        each algorithm.
                                        Intuitively, the larger the area, the better the algorithm. 
                                        The displayed algorithms can be selected by clicking on the legend on the right. 
                                        A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.
                                        This also includes the option to download the plot as png file.</p>'),
                                   plotlyOutput("RT_AUC", height = "600px", width = "750px")
                                   )
                     )
              ),
              
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Empirical Cumulative 
                                      Distribution of the Runtime: Single Target</p>'), 
                         width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary",
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             HTML('Select the target values for which EDCF curves are displayed'),
                             textInput('RT_ECDF_FTARGET1', label = HTML('<p>\\(f_1\\)</p>'), 
                                       value = ''),
                             textInput('RT_ECDF_FTARGET2', label = HTML('<p>\\(f_2\\)</p>'), 
                                       value = ''),
                             textInput('RT_ECDF_FTARGET3', label = HTML('<p>\\(f_3\\)</p>'), 
                                       value = ''),
                             
                             checkboxInput('RT_ECDF_semilogx', label = 'scale x axis log10', value = F)
                           ), 
                           
                           mainPanel(width = 9,
                                     HTML('<p align="left" style="font-size:120%;">
                                          Each EDCF curve shows the proportion of the runs 
                                          that have found a solution of at least the required 
                                          target value within the budget given by the \\(x\\)-axis. 
                                          The displayed curves can be selected by clicking on the legend on the right. A <b>tooltip</b> 
                                          and <b>toolbar</b> appears when hovering over the figure. 
                                          This also includes the option to download the plot as png file.</p>'),
                                     plotlyOutput("RT_ECDF", height = "600px", width = "1100px"))
                         )
                     )
              )
            )
    ),
    
    # FCE: Data Summary -----------------
    tabItem(tabName = 'FCE_DATA', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Target Statistics at Chosen Budget Values</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = T,
                         sidebarPanel(
                           width = 3,
                           HTML(FCE_GRID_INPUT_TEXT),
                           
                           textInput('RT_MIN', label = RT_MIN_LABEL, value = ''),
                           textInput('RT_MAX', label = RT_MAX_LABEL, value = ''),
                           textInput('RT_STEP', label = RT_STEP_LABEL, value = ''),
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
                         solidHeader = TRUE, status = "primary",
                         sidebarPanel(
                           width = 3,
                           HTML(FCE_GRID_INPUT_TEXT),
                           
                           textInput('RT_MIN_SAMPLE', label = RT_MIN_LABEL, value = ''),
                           textInput('RT_MAX_SAMPLE', label = RT_MAX_LABEL, value = ''),
                           textInput('RT_STEP_SAMPLE', label = RT_STEP_LABEL, value = ''),
                           selectInput('FCE_ALGID_RAW_INPUT', 'Algorithms', 
                                       choices = NULL, selected = NULL),
                           
                           selectInput('download_format_FCE', 'Format of the csv', 
                                       choices = c('long', 'wide'), selected = 'wide'),
                           downloadButton("FCE_SAMPLE_download", "Save the aligned runtime samples as csv")
                           ),
                         
                         mainPanel(
                           width = 9,
                           HTML("<p>This table shows for each selected algorithm \\(A\\), 
                                each selected target value \\(f(x)\\), and each run \\(r\\) 
                                the number \\(T(A,f(x),r)\\) of evaluations performed by the 
                                algorithm until it evaluated for the first time a solution of 
                                quality at least \\(f(x)\\).</p>"),
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
                         width = 12, collapsible = TRUE, solidHeader = TRUE, 
                         status = "primary",
                         sidebarPanel(
                           width = 2,
                           textInput('FCE_HIST_RUNTIME', label = HTML('Select the budget value'), 
                                     value = ''),
                           
                           HTML('Choose whether the histograms are <b>overlaid</b> in one plot 
                                or <b>separated</b> in several subplots:'),
                           selectInput('FCE_illu_mode', '', 
                                       choices = c("overlay", "subplot"), 
                                       selected = 'subplot')
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
                                  plotlyOutput('FCE_HIST', height = "800px", width = "1300px")
                           )
                         )
                     )
              ),
              
              column(width = 12,
                     box(title = 'Empirical Probability Density Function of Fixed-Budget Function Values', 
                         width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary",
                         sidebarLayout(
                           sidebarPanel(
                             width = 2,
                             HTML('Select the budget for which the distribution of best-so-far function values is shown'),
                             
                             textInput('FCE_PDF_RUNTIME', label = '', value = ''),
                             checkboxInput('FCE_SHOW_SAMPLE', label = 'show runtime samples', value = T),
                             checkboxInput('FCE_LOGY', label = 'scale y axis log10', value = T)
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
                                    plotlyOutput('FCE_PDF', height = "800px", width = "1300px")
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
                               
                               checkboxInput('FCE_semilogx', 
                                             label = 'scale x axis log10',
                                             value = T),
                               
                               checkboxInput('FCE_semilogy', 
                                             label = 'scale y axis log10',
                                             value = T)
                             )
                         ),
                         
                         HTML('<p style="font-size:120%";>The <b><i>mean, median 
                              and standard deviation</i></b> of the best function values
                              found with a fixed-budget of evaluations are depicted against the budget. 
                              The displayed elements 
                              can be switched on and off by clicking on the legend on the right. 
                              A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
                         mainPanel(plotlyOutput('FCE_PER_FUN', height = "700px", width = "1257px"))
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
                         solidHeader = T, status = "primary", collapsible = T,
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
                                         value = F)
                           ),
                         
                         mainPanel(
                           width = 9,
                           HTML('<p style="font-size:120%;">The evenly spaced budget values are:</p>'),
                           verbatimTextOutput('FCE_RT_GRID'),
                           HTML('<p style="font-size:120%;">The fraction of (run,budget) pairs \\((i,B)\\) satisfying 
that the best solution that the algorithm has found in the \\(i\\)-th run within the first \\(B\\) evaluations has quality at 
least \\(v\\) is plotted against the target value \\(v\\). The displayed elements can be switched on and off by clicking on the 
legend on the right. A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
                           plotlyOutput('FCE_ECDF_AGGR', height = "600px", width = "1100px"),
                           hr()
                           )
                    )
              ),
              
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Area Under the ECDF</p>'),  
                         width = 12, solidHeader = T, status = "primary", collapsible = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify">Set the range and the granularity of the evenly spaced budgets.</p>'),
                           textInput('FCE_AUC_RT_MIN', label = RT_MIN_LABEL, value = ''),
                           textInput('FCE_AUC_RT_MAX', label = RT_MAX_LABEL, value = ''),
                           textInput('FCE_AUC_RT_STEP', label = RT_STEP_LABEL, value = '')
                           ),
                         
                         mainPanel(width = 9,
                                   HTML('<p align="left" style="font-size:120%;">The <b>area under the ECDF</b> is 
                                        caculated for the sequence of budget values specified on the left. The displayed
                                        values are normalized against the maximal target value recorded for 
                                        each algorithm. Intuitively, the larger the area, the better the algorithm. 
                                        The displayed algorithms can be selected by clicking on the legend on the right. 
                                        A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
                                   plotlyOutput("FCE_AUC", height = "600px", width = "750px")
                                   )
                         )
              ),
              
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution of the Fixed-Budget Values: Single Budgets</p>'), 
                         width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary",
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
                                     HTML('<p align="left" style="font-size:120%;">
                                          Each EDCF curve shows the proportion of the runs that have found within the 
given budget B a solution of at least the required target value given by the x-axis. The displayed curves can be selected
by clicking on the legend on the right. A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
                                     plotlyOutput("FCE_ECDF_PER_TARGET", height = "600px", width = "1100px"))
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
                      collapsible = TRUE, solidHeader = TRUE, status = "primary",
                      div(style = "width: 90%;",
                          sidebarPanel(
                            width = 3,
                            HTML('<p style="font-size:120%;">Range of the displayed budget values (\\(x\\) axis)</p>'),
                            
                            textInput('PAR_RT_MIN', label = RT_MIN_LABEL, value = ''),
                            textInput('PAR_RT_MAX', label = RT_MAX_LABEL, value = ''),
                            
                            selectInput('PAR_ALGID_INPUT', 'Algorithms', choices = NULL, selected = NULL),
                            selectInput('PAR_show.mean', label = 'mean/median', 
                                        choices = c('mean', 'median'),
                                        selected = 'mean'),
                            
                            checkboxInput('PAR_semilogx', 
                                          label = 'scale x axis log10',
                                          value = T)
                          )
                      ),
                      
                      HTML('<p style="font-size:120%";>The <b><i>mean or median</i></b> of internal parameters of the algorithm
                           found with a fixed-budget of evaluations are depicted against the budget. 
                           The displayed elements can be switched on and off by clicking on the legend on the right. 
                           A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
                      mainPanel(plotlyOutput('PAR_PER_FUN', height = "700px", width = "1157px"))
                      )
                  )
              )
        )
  )
)

# -----------------------------------------------------------
dashboardPage(title = 'IOHProfiler', header, sidebar, body)
