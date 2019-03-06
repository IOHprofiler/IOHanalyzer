#
# This is the user interface of the Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com

suppressMessages(library(shinyjs))
suppressMessages(library(shinydashboard))
suppressMessages(library(plotly))

# TODO: rename most of the id of the control widgets
FCE_GRID_INPUT_TEXT <- '<p align="justify">Set the range and the granularity of the results. 
                        The table will show function values that have been reached within evenly
                        spaced evaluation budgets.</p>'

RT_MIN_LABEL <- HTML('<p>\\(B_{\\text{min}}:\\) smallest budget value</p>')
RT_MAX_LABEL <- HTML('<p>\\(B_{\\text{max}}:\\) largest budget value</p>')
RT_STEP_LABEL <- HTML('<p>\\(\\Delta B:\\) granularity (step size)</p>')

F_MIN_LABEL <- HTML('<p>\\(f_{\\text{min}}:\\) smallest target value</p>')
F_MAX_LABEL <- HTML('<p>\\(f_{\\text{max}}:\\) largest target value</p>')
F_STEP_LABEL <- HTML('<p>\\(\\Delta f:\\) granularity (step size)</p>')

header <- dashboardHeader(title = HTML(paste0('<h4><div align="center"><b>', IOHprofiler, '</b><br>
                                       <i>Post-Processing</i></div></h4>')))

HTML_P <- function(s) HTML(paste0('<p align="left" style="font-size:120%;">', s, '</p>'))


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
                       menuSubItem("probability Mass Function", tabName = "RT_PMF", icon = icon("bar-chart"), selected = F),
                       menuSubItem("Cumulative Distribution", tabName = "RT_ECDF", icon = icon("line-chart"), selected = F)
                       ),
              
              menuItem("Fixed-Budget Results", tabName = "FCE", icon = icon("file-text-o"),
                       menuSubItem("Data Summary", tabName = "FCE_DATA", icon = icon("table")),
                       menuSubItem("Expected Target Value", tabName = "FCE_convergence", icon = icon("bar-chart")),
                       menuSubItem("probability Density Function", tabName = "FCE_PDF", icon = icon("bar-chart"), selected = F),
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
                            selectInput("Overall.Funcid", 
                                        label = HTML('<p style="font-size:120%;">Please select the function ID</p>'), 
                                        choices = NULL, selected = NULL)
                     )
                   ),
                   
                   fluidRow(
                     column(11,
                            selectInput("Overall.Dim",
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
              column(width = 6,
                     box(title = HTML('<p style="font-size:120%;">Upload Data</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = F, height = '620px',
                         sidebarPanel(
                           width = 12,
                           
                           selectInput('Upload.format', label = HTML('<p align="left" style="font-size:120%;">Please choose the format of your data sets</p>'),
                                       choices = c(AUTOMATIC,IOHprofiler, COCO, TWO_COL), selected = AUTOMATIC, width = '50%'),
                           selectInput('Upload.minmax', label = HTML('<p align="left" style="font-size:100%;">Use maximization or minimization?</p>'),
                                       choices = c(AUTOMATIC,"MAXIMIZE", "MINIMIZE"), selected = AUTOMATIC, width = '50%'),
                           
                           HTML('<p align="justify" style="font-size:120%;">When the data set is huge, the alignment
                                can take a very long time. In this case, you could toggle the efficient mode to subsample 
                                the data set. However, the precision of data will be compromised.</p>'),
                           checkboxInput('Upload.subsampling', label = HTML('<p align="left" style="font-size:120%;">Efficient mode</p>'), value = F),
                           
                           checkboxInput('Upload.add_repository', label = HTML('<p align="left" style="font-size:120%;">Upload data to IOHProfiler database for use by others?</p>'), value = F),
                           
                           fileInput("Upload.zip", label = HTML('<p align="left" style="font-size:120%;">Please choose a <i>zip file</i> containing the benchmark data</p>'),
                                     multiple = TRUE, accept = c("Application/zip", ".zip")),
                           
                           # TODO: keep this for the local version
                           # shinyDirButton('directory', 'Browse the folder', 
                           #                title = 'Please choose a directory containing the benchmark data'),
                           HTML('<p align="left" style="font-size:120%;"><b>Remove all data you uploaded</b></p>'),
                           actionButton('Upload.remove', 'Clear data')
                         )
                     )
              ),
              column(width = 6,
                     box(title = HTML('<p style="font-size:120%;">Load Data from repository</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = F, height = '620px',
                         sidebarPanel(
                           width = 12,
                           
                           radioButtons('Repository.source',label = "select the source to be used", choices = c("Official", "User-uploaded"),selected = "Official"),
                           
                           selectInput('Repository.suite', label = HTML('<p align="left" style="font-size:120%;">Please choose the suite</p>'),
                                       choices = c("none",IOHprofiler, COCO), selected = "none", width = '50%'),
                           selectInput('Repository.funcid', label = HTML('<p align="left" style="font-size:120%;">Please choose the function</p>'),
                                       choices = NULL, selected = NULL, width = '50%'),
                           selectInput('Repository.dim', label = HTML('<p align="left" style="font-size:120%;">Please choose the dimension</p>'),
                                       choices = NULL, selected = NULL, width = '50%'),
                           selectInput('Repository.algid', label = HTML('<p align="left" style="font-size:120%;">Please choose the algorithm</p>'),
                                       choices = NULL, selected = NULL, width = '50%'),
                           
                           actionButton('Repository.load', 'Load data')
                         )
                     )
              )
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
    tabItem(tabName = 'ERT_data', 
      fluidRow(
        column(width = 12,
               box(title = HTML('<p style="font-size:120%;">Overview of runtime values</p>'), width = 12,
                   solidHeader = T, status = "primary", collapsible = T,
                   sidebarPanel(
                     width = 3,
                     HTML('<p align="justify">Select which algorithms to show.</p>'),
                     
                     selectInput('RTSummary.Overview.Algid', 'Algorithms', choices = NULL, selected = NULL),
                     downloadButton("RTSummary.Overview.Download", "Save this table as csv")
                     ),
                   
                   mainPanel(
                     width = 9,
                     tableOutput('table_FV_summary_condensed')
                   )
                 ),
               
               box(title = HTML('<p style="font-size:120%;">Runtime Statistics at Chosen Target Values</p>'), width = 12,
                   solidHeader = T, status = "primary", collapsible = T, collapsed = T,
                   sidebarPanel(
                     width = 3,
                     HTML('<p align="justify">Set the range and the granularity of the results.
                          The table will show fixed-target runtimes for evenly spaced target values.</p>'),
                     
                     textInput('RTSummary.Statistics.Min', label = F_MIN_LABEL, value = ''),
                     textInput('RTSummary.Statistics.Max', label = F_MAX_LABEL, value = ''),
                     textInput('RTSummary.Statistics.Step', label = F_STEP_LABEL, value = ''),
                     checkboxInput('RTSummary.Statistics.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                           Once toggled, only \\(f_{\\text{min}}\\) is 
                                                           used to generate the table on the right.</p>'), value = FALSE),
                     selectInput('RTSummary.Statistics.Algid', 'Algorithms', choices = NULL, selected = NULL),
                     downloadButton("RTSummary.Statistics.Download", "Save this table as csv")
                   ),
                   
                   mainPanel(
                     width = 9,
                     HTML(paste0('<div style="font-size:120%;">', includeMarkdown('RMD/RT_SUMMARY_TABLE.Rmd'),'</div>')),
                     tableOutput('table_RT_summary')
                   )
               ),
               
               box(title = HTML('<p style="font-size:120%;">Original Runtime Samples</p>'), width = 12,
                   solidHeader = TRUE, status = "primary", collapsible = TRUE, collapsed = T,
                   sidebarPanel(
                     width = 3,
                     HTML('<p align="justify">Set the range and the granularity of the results.
                          The table will show fixed-target runtimes for evenly spaced target values.</p>'),
                     
                     textInput('RTSummary.Sample.Min', label = F_MIN_LABEL, value = ''),
                     textInput('RTSummary.Sample.Max', label = F_MAX_LABEL, value = ''),
                     textInput('RTSummary.Sample.Step', label = F_STEP_LABEL, value = ''),
                     checkboxInput('RTSummary.Sample.Single', 
                                    label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                  Once toggled, only \\(f_{\\text{min}}\\) is 
                                                  used to generate the table on the right.</p>'), value = FALSE),
                     
                     # TODO: do we need this log scaling?
                     # checkboxInput('F_LOGSPACE_DATA_SUMMARY',
                     #               label = HTML('Evenly space target values in \\(log_{10}\\) space')),
                     selectInput('RTSummary.Sample.Algid', 'Algorithms', 
                                 choices = NULL, selected = NULL),
                     
                     selectInput('RTSummary.Sample.DownloadFormat', 'Format of the csv', 
                                 choices = c('long', 'wide'), selected = 'wide'),
                     downloadButton("RTSummary.Sample.Download", "Save the aligned runtime samples as csv")
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
            fluidRow(column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                                      (per function)</p>'), 
                         width = 12,
                         collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
                         div(style = "width: 90%;",
                           sidebarPanel(
                             width = 3,
                             HTML('<p style="font-size:120%;">Range of the displayed target values</p>'),
                             
                             textInput('ERTPlot.Min', 
                                       label = F_MIN_LABEL, 
                                       value = ''),
                             textInput('ERTPlot.Max', 
                                       label = F_MAX_LABEL, 
                                       value = ''),
                             
                             checkboxInput('ERTPlot.show.ERT', 
                                           label = 'show/hide ERT',
                                           value = T),
                             
                             checkboxInput('ERTPlot.show.mean', 
                                           label = 'show/hide mean',
                                           value = F),
                             
                             checkboxInput('ERTPlot.show.CI', 
                                           label = 'show/hide mean +/- sd',
                                           value = F),
                             
                             checkboxInput('ERTPlot.show.median', 
                                           label = 'show/hide median',
                                           value = F),
                             
                             checkboxInput('ERTPlot.semilogx', 
                                           label = 'scale x axis log10',
                                           value = T),
                             
                             checkboxInput('ERTPlot.semilogy', 
                                           label = 'scale y axis log10',
                                           value = T),
                             
                             checkboxInput('ERTPlot.show_all',
                                           label = 'show/hide multiple runs',
                                           value = F),
                             conditionalPanel(condition = "ERTPlot.input.show_all == true",
                                              
                                              fluidRow(column(
                                                11,
                                                offset = 1,
                                                sliderInput('ERTPlot.show.density',
                                                            label = "Runs density(%)",
                                                            min = 1, max = 100, value = 50, step = 1),
                                                checkboxInput('ERTPlot.show.best_of_all',
                                                              label = 'show/hide best run',
                                                              value = F),
                                                checkboxInput('ERTPlot.show.pareto_optima',
                                                              label = 'show/hide pareto optimal front',
                                                              value = F)
                                              ))),
                             
                             selectInput('ERTPlot.Format', label = 'select the figure format',
                                         choices = supported_fig_format, selected = 'pdf'),
                             
                             downloadButton('ERTPlot.Download', label = 'download the figure')
                             
                             # checkboxInput('show.instance', 
                             #               label = 'show each independent run',
                             #               value = F)
                           )
                         ),
                         
                         mainPanel(width = 9,
                                   column(width = 12, align = "center",
                                          HTML_P('The <b><i>mean, median 
                                                  and standard deviation</i></b> of the runtime samples 
                                                  are depicted against the best objective values. 
                                                  The displayed elements (mean, median, standard deviations) 
                                                  can be switched on and off by clicking on the legend on the right. 
                                                  A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
                                          plotlyOutput('ERT_PER_FUN', height = plotly_height, width = plotly_width2)
                                          )
                                   )
                     )
                     
                     # box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                     #                  (across all functions)</p>'), 
                     #     height = 800, width = 12,
                 
                  #     collapsible = TRUE, solidHeader = TRUE, status = 'primary')
              ),
              column(width = 12,
                       box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                                        (aggregated)</p>'), 
                           width = 12,
                           collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
                           div(style = "width: 90%;",
                               box(
                                 width = 12,
                                
                                 selectInput('ERTPlot.Multi.Algs', label = 'Select the algs to show', 
                                             multiple = T, selected = NULL, choices = NULL),
                                 column( width = 6,
                                 checkboxInput('ERTPlot.Multi.Logx', 
                                               label = 'scale x axis log10',
                                               value = T),
                                 
                                 checkboxInput('ERTPlot.Multi.Logy', 
                                               label = 'scale y axis log10',
                                               value = T),
                                 
                                 selectInput('ERTPlot.Multi.Mode', label = 'Select the plotting mode',
                                             choices = c('overlay','subplot'), selected = 'subplot'),
                                 
                                 selectInput('ERTPlot.Multi.Aggregator', label = 'Create plot for all?',
                                             choices = c('Functions','Dimensions'), selected = 'Functions'),
                                 
                                 actionButton('ERTPlot.Multi.PlotButton', label = 'Plot the figure')
                                 ),
                                 column( width = 6,
                                   selectInput('ERTPlot.Multi.Format', label = 'Select the figure format',
                                             choices = supported_fig_format, selected = 'pdf'),
                                 
                                 downloadButton('ERTPlot.Multi.Download', label = 'Download the figure'))
                               )
                           ),
                           
                           mainPanel(width = 12,
                                     column(width = 12, align = "center",
                                            plotlyOutput('ERTPlot.Multi.Plot', height = "1800px", width = plotly_width2)
                                     )
                           )
                       )
                       
                       # box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                       #                  (across all functions)</p>'), 
                       #     height = 800, width = 12,
                       
                       #     collapsible = TRUE, solidHeader = TRUE, status = 'primary')
              ),
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                                      comparisons</p>'), 
                         width = 12,
                         collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
                         div(style = "width: 90%;",
                             sidebarPanel(
                               width = 3,
                               
                               
                               
                               selectInput('ERTPlot.Aggr.Mode', label = 'Select the plotting mode',
                                           choices = c('radar','line'), selected = 'radar'),
                               
                               selectInput('ERTPlot.Aggr.Aggregator', label = 'Create plot for all?',
                                           choices = c('Functions','Dimensions'), selected = 'Functions'),
                               
                               checkboxInput('ERTPlot.Aggr.Ranking', 
                                             label = 'Use ranking instead of ERT-values',
                                             value = T),
                               
                               checkboxInput('ERTPlot.Aggr.Logy', 
                                             label = 'scale y axis log10',
                                             value = F),
                               
                               textInput('ERTPlot.Aggr.Targets', label = 'choose the ERT-targets (comma-separated)'),
                               
                              
                               selectInput('ERTPlot.Aggr.Format', label = 'Select the figure format',
                                           choices = supported_fig_format, selected = 'pdf'),
                               
                               downloadButton('ERTPlot.Aggr.Download', label = 'Download the figure')
                             )
                         ),
                         
                         mainPanel(width = 9,
                                   column(width = 12, align = "center",
                                          plotlyOutput('ERTPlot.Aggr.Plot', height = plotly_height, width = plotly_width2)
                                   )
                         )
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
                       width = 12, collapsible = TRUE, solidHeader = TRUE,  collapsed = T,
                       status = "primary",
                       sidebarPanel(
                         width = 2,
                         textInput('RTPMF.Hist.Target', label = HTML('Select the target value'), 
                                   value = ''),
                         
                         HTML('Choose whether the histograms are <b>overlaid</b> in one plot 
                              or <b>separated</b> in several subplots:'),
                         selectInput('RTPMF.Hist.Mode', '', 
                                     choices = c("overlay", "subplot"), 
                                     selected = 'subplot'),
                         
                         selectInput('RTPMF.Hist.Format', label = 'select the figure format',
                                     choices = supported_fig_format, selected = 'pdf'),
                         
                         downloadButton('RTPMF.Hist.Download', label = 'download the figure')
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
                                plotlyOutput('RT_HIST', height = plotly_height2, width = plotly_width2)
                                )
                       )
                       )
            ),
            
            column(width = 12,
              box(title = 'Empirical Probability Mass Function of the Runtime', 
                  width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
                  sidebarLayout(
                    sidebarPanel(
                      width = 2,
                      HTML('Select the target value for which the runtime distribution is shown'),
                      
                      textInput('RTPMF.Bar.Target', label = '', value = ''),
                      checkboxInput('RTPMF.Bar.Sample', label = 'show runtime for each run', value = T),
                      checkboxInput('RTPMF.Bar.Logy', label = 'scale y axis log10', value = F),
                      
                      selectInput('RTPMF.Bar.Format', label = 'select the figure format',
                                  choices = supported_fig_format, selected = 'pdf'),
                      
                      downloadButton('RTPMF.Bar.Download', label = 'download the figure')
                      
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
                             plotlyOutput('RT_PMF', height = plotly_height, width = plotly_width2)
                      )
                    )
                  )
              )
            )
          )
    ),
    
    # RT ECDF ------------------------------------------
    tabItem(tabName = 'RT_ECDF', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Empirical Cumulative 
                                      Distribution: Single target</p>'), 
                         width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             HTML('Select the target values for which EDCF curves are displayed'),
                             textInput('RTECDF.Single.Target1', label = HTML('<p>\\(f_1\\)</p>'), 
                                       value = ''),
                             textInput('RTECDF.Single.Target2', label = HTML('<p>\\(f_2\\)</p>'), 
                                       value = ''),
                             textInput('RTECDF.Single.Target3', label = HTML('<p>\\(f_3\\)</p>'), 
                                       value = ''),
                             
                             checkboxInput('RTECDF.Single.Logx', label = 'scale x axis log10', value = F)
                           ), 
                           
                           mainPanel(width = 9,
                                     column(width = 12, align = "center",
                                            HTML_P('Each EDCF curve shows the proportion of the runs 
                                                   that have found a solution of at least the required 
                                                   target value within the budget given by the \\(x\\)-axis. 
                                                   The displayed curves can be selected by clicking on the legend on the right. A <b>tooltip</b> 
                                                   and <b>toolbar</b> appears when hovering over the figure. 
                                                   This also includes the option to download the plot as png file.'),
                                            plotlyOutput("RT_ECDF", height = plotly_height, width = plotly_width2)
                                            )
                                     )
                         )
                       )
                   ),
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Aggregated Empirical Cumulative 
                                      Distribution: Single function</p>'), 
                         width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify">Set the range and the granularity 
                                of the quality targets taken into account in the ECDF curve. 
                                The plot will show the ECDF curves for evenly spaced target values.</p>'),
                           textInput('RTECDF.Multi.Min', label = F_MIN_LABEL, value = ''),
                           textInput('RTECDF.Multi.Max', label = F_MAX_LABEL, value = ''),
                           textInput('RTECDF.Multi.Step', label = F_STEP_LABEL, value = ''),
                           checkboxInput('RTECDF.Multi.Targets',
                                         label = 'show ECDFs for each target',
                                         value = F),
                           checkboxInput('RTECDF.Multi.Logx', 
                                         label = 'scale x axis log10',
                                         value = F),
                           
                           selectInput('RTECDF.Multi.Format', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           
                           downloadButton('RTECDF.Multi.Download', label = 'download the figure')
                         ),
                         
                         mainPanel(width = 9,
                                   column(width = 12, align = "center",
                                          HTML_P('The evenly spaced target values are:'),
                                          verbatimTextOutput('RT_GRID'),
                                          HTML_P('The fraction of (run,target value)
                                                 pairs \\((i,v)\\) satisfying that the best solution that the algorithm has 
                                                 found in the \\(i\\)-th run within the given time budget \\(t\\) has quality at least
                                                 \\(v\\) is plotted against the available budget \\(t\\). The displayed elements can be switched 
                                                 on and off by clicking on the legend on the right. A <b>tooltip</b> 
                                                 and <b>toolbar</b> appears when hovering over the figure.'),
                                          plotlyOutput('RT_ECDF_AGGR', height = plotly_height, width = plotly_width2),
                                          hr()
                                          )
                                   )
                           
                           # HTML('<p>The <b>Kolmogorov–Smirnov test</b> is used to investigate
                           # how the ditribution of runtimes of one algorithm differs from the other:</p><br>'),
                           # column(10, align = "center", verbatimTextOutput('ks'))
                         )
              ),
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Aggregated Empirical Cumulative 
                                      Distribution: All functions</p>'), 
                         width = 12, solidHeader = T, status = "primary", collapsible = T, collapsed = T,
                         sidebarPanel(
                           width = 3,

                           # checkboxInput("Aggregate_dim","Aggregate dimensions", value = F),
                           # checkboxInput("Aggregate_fun","Aggregate functions", value = T),
                           
                           HTML_P('Choose whether to upload a file containing the target-values for each (function, dimension)-pair
                                  or use the automatically generated targets (see below). Please consider keeping the file format when
                                  modifying the csv given below.'),
                           tableOutput('RT_GRID_GENERATED'),
                           downloadButton('RTECDF.Aggr.Table.Download', label = 'download this example'),
                           
                           hr(),
                           br(),
                           fileInput("RTECDF.Aggr.Table.Upload", label = HTML('<p align="left" style="font-size:120%;">Please choose a <i>csv file</i> containing the targets</p>'),
                                     multiple = FALSE, accept = c(
                                       "text/csv",
                                       "text/comma-separated-values,text/plain",
                                       ".csv")),
                           selectInput('RTECDF.Aggr.Format', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           
                           downloadButton('RTECDF.Aggr.Download', label = 'download the figure')
                           
                         ),
                         
                         mainPanel(width = 9,
                                   column(width = 12, align = "center",
                                          hr(),
                                          HTML_P('The fraction of (run,target value, ...)
                                                 pairs \\((i,v, ...)\\) satisfying that the best solution that the algorithm has 
                                                 found in the \\(i\\)-th (run of function \\(f\\) in dimension \\(d\\)) within the given time budget \\(t\\) has quality at least
                                                 \\(v\\) is plotted against the available budget \\(t\\). The displayed elements can be switched 
                                                 on and off by clicking on the legend on the right. A <b>tooltip</b> 
                                                 and <b>toolbar</b> appears when hovering over the figure. Aggregation over functions and dimension can 
                                                 be switched on or off using the checkboxes on the left; when aggregation is off the selected function / dimension
                                                 is chosen according the the value in the bottom-left selection-box.'),
                                          plotlyOutput('RT_ECDF_MULT', height = plotly_height, width = plotly_width2),
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
                           HTML('<p align="justify">Set the range and the granularity of
                                the evenly spaced quality targets taken into account in the plot.</p>'),
                           textInput('RTECDF.AUC.Min', label = F_MIN_LABEL, value = ''),
                           textInput('RTECDF.AUC.Max', label = F_MAX_LABEL, value = ''),
                           textInput('RTECDF.AUC.Step', label = F_STEP_LABEL, value = ''),
                           
                           selectInput('RTECDF.AUC.Format', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           downloadButton('RTECDF.AUC.Download', label = 'download the figure')
                         ),
                         
                         mainPanel(width = 9,
                                   column(width = 12, align = "center",
                                          HTML_P('The <b>area under the ECDF</b> is 
                                                  caculated for the sequence of target values specified on the left. The displayed
                                                  values are normalized against the maximal number of function evaluations for 
                                                  each algorithm. Intuitively, the larger the area, the better the algorithm. 
                                                  The displayed algorithms can be selected by clicking on the legend on the right. 
                                                  A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.
                                                  This also includes the option to download the plot as png file.'),
                                          plotlyOutput("RT_AUC", height = plotly_height, width = plotly_width2)
                                         )
                                   )
                     )
              )
              
              # column(width = 12,
              #        box(title = HTML('<p style="font-size:120%;">>Aggregated Empirical Cumulative 
              #                         Distribution: Single target</p>'), 
              #            width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
              #            sidebarLayout(
              #              sidebarPanel(
              #                width = 3,
              #                HTML('Select the target values for which EDCF curves are displayed'),
              #                textInput('RT_ECDF_FTARGET1', label = HTML('<p>\\(f_1\\)</p>'), 
              #                          value = ''),
              #                textInput('RT_ECDF_FTARGET2', label = HTML('<p>\\(f_2\\)</p>'), 
              #                          value = ''),
              #                textInput('RT_ECDF_FTARGET3', label = HTML('<p>\\(f_3\\)</p>'), 
              #                          value = ''),
              #                
              #                checkboxInput('RT_ECDF_semilogx', label = 'scale x axis log10', value = F)
              #              ), 
              #              
              #              mainPanel(width = 9,
              #                        column(width = 12, align = "center",
              #                               HTML_P('Each EDCF curve shows the proportion of the runs 
              #                                       that have found a solution of at least the required 
              #                                       target value within the budget given by the \\(x\\)-axis. 
              #                                       The displayed curves can be selected by clicking on the legend on the right. A <b>tooltip</b> 
              #                                       and <b>toolbar</b> appears when hovering over the figure. 
              #                                       This also includes the option to download the plot as png file.'),
              #                                       plotlyOutput("RT_ECDF", height = plotly_height, width = plotly_width2)
              #                               )
              #                       )
              #            )
              #        )
              # )
            )
    ),
    
    # FCE: Data Summary -----------------
    tabItem(tabName = 'FCE_DATA', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Overview of runtime values</p>'), width = 12,
                         solidHeader = T, status = "primary", collapsible = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify">Select which algorithms to show.</p>'),
                           
                           selectInput('FCESummary.Overview.Algid', 'Algorithms', choices = NULL, selected = NULL),
                           #TODO: implement this button
                           downloadButton("FCESummary.Overview.Download", "Save this table as csv")
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
                           
                           textInput('FCESummary.Statistics.Min', label = RT_MIN_LABEL, value = ''),
                           textInput('FCESummary.Statistics.Max', label = RT_MAX_LABEL, value = ''),
                           textInput('FCESummary.Statistics.Step', label = RT_STEP_LABEL, value = ''),
                           checkboxInput('FCESummary.Statistics.Single', label = HTML('<p>\\(B_{\\text{min}} = B_{\\text{max}}\\)?
                                                           Once toggled, only \\(B_{\\text{min}}\\) is 
                                                           used to generate the table on the right.</p>'), value = FALSE),
                           selectInput('FCESummary.Statistics.Algid', 'Algorithms', choices = NULL, selected = NULL),
                           downloadButton("FCESummary.Statistics.Download", "Save this table as csv")
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
                           
                           textInput('FCESummary.Sample.Min', label = RT_MIN_LABEL, value = ''),
                           textInput('FCESummary.Sample.Max', label = RT_MAX_LABEL, value = ''),
                           textInput('FCESummary.Sample.Step', label = RT_STEP_LABEL, value = ''),
                           checkboxInput('FCESummary.Sample.Single', 
                                         label = HTML('<p>\\(B_{\\text{min}} = B_{\\text{max}}\\)?
                                                       Once toggled, only \\(B_{\\text{min}}\\) is 
                                                       used to generate the table on the right.</p>'), value = FALSE),
                           selectInput('FCESummary.Sample.Algid', 'Algorithms', 
                                       choices = NULL, selected = NULL),
                           
                           selectInput('FCESummary.Sample.Format', 'Format of the csv', 
                                       choices = c('long', 'wide'), selected = 'wide'),
                           downloadButton("FCESummary.Sample.Download", "Save the aligned runtime samples as csv")
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
                           textInput('FCEPDF.Hist.Runtime', label = HTML('Select the budget value'), 
                                     value = ''),
                           
                           HTML('Choose whether the histograms are <b>overlaid</b> in one plot 
                                or <b>separated</b> in several subplots:'),
                           selectInput('FCEPDF.Hist.Mode', '', 
                                       choices = c("overlay", "subplot"), 
                                       selected = 'subplot'),
                           
                           selectInput('FCEPDF.Hist.Format', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           
                           downloadButton('FCEPDF.Hist.Download', label = 'download the figure')
                           
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
                             
                             textInput('FCEPDF.Bar.Runtime', label = '', value = ''),
                             checkboxInput('FCEPDF.Bar.Samples', label = 'show runtime samples', value = T),
                             checkboxInput('FCEPDF.Bar.Logy', label = 'scale y axis log10', value = T),
                             
                             selectInput('FCEPDF.Bar.Format', label = 'select the figure format',
                                         choices = supported_fig_format, selected = 'pdf'),
                             
                             downloadButton('FCEPDF.Bar.Download', label = 'download the figure')
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
                               
                               textInput('FCEPlot.Min', label = RT_MIN_LABEL, value = ''),
                               textInput('FCEPlot.Max', label = RT_MAX_LABEL, value = ''),
                               
                               checkboxInput('FCEPlot.show.mean', 
                                             label = 'show/hide mean',
                                             value = T),
                               
                               checkboxInput('FCEPlot.show.median', 
                                             label = 'show/hide median',
                                             value = F),
                               
                               checkboxInput('FCEPlot.semilogx', 
                                             label = 'scale x axis log10',
                                             value = T),
                               
                               checkboxInput('FCEPlot.semilogy', 
                                             label = 'scale y axis log10',
                                             value = T),
                               
                               checkboxInput('FCEPlot.show.all',
                                             label = 'show/hide multiple runs',
                                             value = F),
                               conditionalPanel(condition = "FCEPlot.show.all == true",
                                                
                                                fluidRow(column(
                                                  11,
                                                  offset = 1,
                                                  sliderInput('FCEPlot.show.density',
                                                              label = "Runs density(%)",
                                                              min = 1, max = 100, value = 50, step = 1),
                                                  checkboxInput('FCEPlot.show.best_of_all',
                                                                label = 'show/hide best run',
                                                                value = F),
                                                  checkboxInput('FCEPlot.show.pareto_optima',
                                                                label = 'show/hide pareto optimal front',
                                                                value = F)
                                                ))),
                               
                               selectInput('FCEPlot.Format', label = 'select the figure format',
                                           choices = supported_fig_format, selected = 'pdf'),
                               
                               downloadButton('FCEPlot.Download', label = 'download the figure')
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
                           textInput('FCEECDF.Mult.Min', label = RT_MIN_LABEL, value = ''),
                           textInput('FCEECDF.Mult.Max', label = RT_MAX_LABEL, value = ''),
                           textInput('FCEECDF.Mult.Step', label = RT_STEP_LABEL, value = ''),
                           
                           checkboxInput('FCEECDF.Mult.Targets',
                                         label = 'show ECDF for each budget',
                                         value = F),
                           
                           checkboxInput('FCEECDF.Mult.Logx', 
                                         label = 'scale x axis log10',
                                         value = F),
                           selectInput('FCEECDF.Mult.Format', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           
                           downloadButton('FCEECDF.Mult.Download', label = 'download the figure')
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
                           textInput('FCEECDF.AUC.Min', label = RT_MIN_LABEL, value = ''),
                           textInput('FCEECDF.AUC.Max', label = RT_MAX_LABEL, value = ''),
                           textInput('FCEECDF.AUC.Step', label = RT_STEP_LABEL, value = ''),
                           
                           selectInput('FCEECDF.AUC.Format', label = 'select the figure format',
                                       choices = supported_fig_format, selected = 'pdf'),
                           
                           downloadButton('FCEECDF.AUC.Download', label = 'download the figure')
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
                             textInput('FCEECDF.Single.Target1', label = HTML('<p>\\(B_1\\)</p>'), 
                                       value = ''),
                             textInput('FCEECDF.Single.Target2', label = HTML('<p>\\(B_2\\)</p>'), 
                                       value = ''),
                             textInput('FCEECDF.Single.Target3', label = HTML('<p>\\(B_3\\)</p>'), 
                                       value = ''),
                             checkboxInput('FCEECDF.Single.Logx', label = 'scale x axis log10', value = F)
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
                            
                            textInput('PAR.Plot.Min', label = F_MIN_LABEL, value = ''),
                            textInput('PAR.Plot.Max', label = F_MAX_LABEL, value = ''),
                            
                            selectInput('PAR.Plot.Algid', 'Algorithms', choices = NULL, selected = NULL),
                            selectInput('PAR.Plot.show.mean', label = 'mean/median', 
                                        choices = c('mean', 'median'),
                                        selected = 'mean'),
                            
                            checkboxInput('PAR.Plot.Logx', 
                                          label = 'scale x axis log10',
                                          value = T),
                            
                            checkboxInput('PAR.Plot.Logy', 
                                          label = 'scale y axis log10',
                                          value = T),
                            
                            selectInput('PAR.Plot.Format', label = 'select the figure format',
                                        choices = supported_fig_format, selected = 'pdf'),
                            downloadButton('PAR.Plot.Download', label = 'download the figure')
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
                           
                           textInput('PAR.Summary.Min', label = F_MIN_LABEL, value = ''),
                           textInput('PAR.Summary.Max', label = F_MAX_LABEL, value = ''),
                           textInput('PAR.Summary.Step', label = F_STEP_LABEL, value = ''),
                           checkboxInput('PAR.Summary.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                                       Once toggled, only \\(f_{\\text{min}}\\) is 
                                                                       used to generate the table on the right.</p>'), value = FALSE),
                           selectInput('PAR.Summary.Algid', 'Algorithms', choices = NULL, selected = NULL),
                           selectInput('PAR.Summary.Param', 'Parameters', choices = NULL, selected = NULL),
                           downloadButton("PAR.Summary.Download", "Save this table as csv")
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

                           textInput('PAR.Sample.Min', label = F_MIN_LABEL, value = ''),
                           textInput('PAR.Sample.Max', label = F_MAX_LABEL, value = ''),
                           textInput('PAR.Sample.Step', label = F_STEP_LABEL, value = ''),
                           checkboxInput('PAR.Sample.Single', label = HTML('<p>\\(f_{\\text{min}} = f_{\\text{max}}\\)?
                                                                             Once toggled, only \\(f_{\\text{min}}\\) is 
                                                                             used to generate the table on the right.</p>'), value = FALSE),
                           selectInput('PAR.Sample.Algid', 'Algorithms', choices = NULL, selected = NULL),
                           selectInput('PAR.Sample.Param', 'Parameters', choices = NULL, selected = NULL),
                           selectInput('PAR.Sample.Format', 'Format of the csv', 
                                       choices = c('long', 'wide'), selected = 'wide'),
                           downloadButton("PAR.Sample.Download", "Save this table as csv")
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
