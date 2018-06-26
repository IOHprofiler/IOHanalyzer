#
# This is the user interface of the Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Author: Hao Wang
# Email: wangronin@gmail.com

library(shinydashboard)
library(plotly)
library(rCharts)

header <- dashboardHeader(title = HTML('<h4><div align="center"><b>IOHProfiler</b><br>
                                       <i>Post-Processing</i></div></h4>'))

# The side bar layout ---------------------------------------------
sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id = "tabs",
              menuItem("Upload data", tabName = "upload", icon = icon('upload', lib = 'glyphicon'), 
                       selected = T),
              
              menuItem("Fixed-Target Results", tabName = "ERT", icon = icon("file-text-o"),
                       menuSubItem("Data summary", tabName = "ERT_data", icon = icon("table")),
                       menuSubItem("Convergence", tabName = "ERT_convergence", icon = icon("line-chart"), selected = F),
                       menuSubItem("Probility Mass Function", tabName = "RT_PMF", icon = icon("bar-chart"), selected = F),
                       menuSubItem("Cumulative Distribution", tabName = "ERT_ECDF", icon = icon("line-chart"), selected = F)
                       ),
              
              menuItem("Fixed-Budget Results", tabName = "FCE", icon = icon("file-text-o"),
                       menuSubItem("data summary", tabName = "FCE_data", icon = icon("table")),
                       menuSubItem("plot", tabName = "FCE_plot", icon = icon("bar-chart")),
                       menuSubItem("Cumulative Distribution", tabName = "FCE_ECDF", icon = icon("line-chart"))
              ),
              
              menuItem("ReadMe", tabName = "readme", icon = icon("mortar-board")),
              menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr(),
  
  conditionalPanel("input.tabs!='upload' && input.tabs!='readme' && input.tabs!='about'",
                   fluidRow(
                     column(1),
                     column(10,
                            h5('Please select the test function ID'),
                            selectInput("fct", label = "fID",
                                        choices = seq(4),
                                        selected = 1)
                     )
                   )
  )
)

body <- dashboardBody(
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
  
  # include MathJax
  HTML("<head><script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML'
       async></script></head>"),
  
  tabItems(
    tabItem(tabName = 'about', 
            includeMarkdown('RMD/about.Rmd')
    ),
  
    tabItem(tabName = 'readme',
            includeMarkdown('RMD/docs.Rmd')
    ),
    
    # Data summary for Fixed-Target Runtime (ERT) -----------------
    tabItem(tabName = 'ERT_data', 
      fluidRow(
        column(width = 12,
               box(title = HTML('<p style="font-size:120%;">Runtime Statistics at chosen target values</p>'), width = 12,
                   solidHeader = T, status = "primary", collapsible = T,
                   sidebarPanel(
                     width = 3,
                     HTML('<p align="justify">Align the running time: the smallest 
                          running time achieving the selected target values.<br>
                          <i>Evenly spaced</i> target values are generated:</p>'),
                     
                     textInput('fstart', label = HTML('<p>\\(f_{start}:\\) starting objective value</p>'), value = ''),
                     textInput('fstop', label = HTML('<p>\\(f_{stop}:\\) stopping objective value</p>'), value = ''),
                     textInput('fstep', label = HTML('<p>\\(f_{step}:\\) objective value steps</p>'), value = ''),
                     selectInput('select.alg', 'which algorithm to show?', choices = c('algorithm1', 'algorithm2', 'all'), 
                                 selected = 'all'),
                     downloadButton("downloadData", "Save this table as csv"),
                     br(),
                     hr(),
                     HTML('<p align="justify">In addition, you can download the runtime samples 
                          aligned with respect to the specified targets.</p>'),
                     selectInput('RT_download_format', 'please choose the format of the csv', 
                                 choices = c('long', 'wide'), selected = 'wide'),
                     downloadButton("download_runtime", "Save the aligned runtime samples")
                   ),
                   
                   mainPanel(
                     width = 9,
                     HTML('<p>Firstly, some basic statistics (<i>mean, median, quantiles</i>) 
                          are shown for the runntime at the selected objective value:</p>'),
                     tableOutput('table_RT_summary'),
                     br(),
                     includeMarkdown('RMD/quantile.Rmd')
                   )
               ),
               
               box(title = HTML('<p style="font-size:120%;">Runtime Samples</p>'), width = 12,
                   solidHeader = TRUE, status = "primary",
                   sidebarPanel(
                     width = 2,
                     HTML('<p align="justify">Please specify the target value for the alignment:</p>'),
                     textInput('fselect', label = HTML('<p>\\(f:\\) target value</p>'), value = ''),
                     downloadButton("download_RT_sample", "Save this table as csv")
                     ),
                   
                   mainPanel(
                     width = 10,
                     HTML('<p>The raw runtime samples aligned with respect to the 
                          specified target value are listed as follows:</p>'),
                     verbatimTextOutput('table_RT_sample')
                    )
               )
          )
      )
    ),
    
    # Expected runtime curve ---------------------------------------------
    tabItem(tabName = 'ERT_convergence', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                                      (per function)</p>'), 
                         width = 12,
                         collapsible = TRUE, solidHeader = TRUE, status = "primary",
                         sidebarPanel(
                           width = 2,
                           HTML('Select the range of target values to zoom in:'),
                           
                           textInput('ERT_FSTART', 
                                     label = HTML('<p>\\(f_{start}:\\) starting value</p>'), 
                                     value = ''),
                           textInput('ERT_FSTOP', 
                                     label = HTML('<p>\\(f_{stop}:\\) ending value</p>'), 
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
                                         value = T),
                           
                           checkboxInput('show.instance', 
                                         label = 'show each independent run',
                                         value = F)
                         ),
                         HTML('<p style="font-size:120%";>The <i>mean, median 
                              and standard deviation</i> of the runtime samples 
                              are depicted against the best objective values:</p>'),
                         mainPanel(plotlyOutput('ERT_PER_FUN', height = "650px", width = "1157px"))
                     )
                     
                     # box(title = HTML('<p style="font-size:120%;">Expected Runtime 
                     #                  (across all functions)</p>'), 
                     #     height = 800, width = 12,
                     #     collapsible = TRUE, solidHeader = TRUE, status = 'primary')
              )
            )
    ),
    
    # Runtime: histograms, violin plots ------------------------------------------
    tabItem(tabName = 'RT_PMF', 
          fluidRow(
            column(width = 12,
              box(title = 'Empirical Probability Mass Function of the Runtime', 
                  width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary",
                  sidebarLayout(
                    sidebarPanel(
                      width = 2,
                      HTML('Align the runtime: the smallest running time 
                           achieving the specified target value:'),
                      
                      textInput('RT_PMF_FTARGET', label = HTML('<p>\\(f:\\) target value</p>'), 
                                value = ''),
                      checkboxInput('RT_SHOW_SAMPLE', label = 'show runtime samples', value = T)
                      
                      # HTML('Kernel density estimation uses the following <b>kernel function</b>:'),
                      # selectInput('RT_PMF_KER', '',
                      #             choices = c("gaussian", "epanechnikov", "rectangular",
                      #                         "triangular", "biweight", "cosine", "optcosine"),
                      #             selected = 'gaussian')

                    ),
                    
                    mainPanel(
                      width = 10,
                      column(width = 12, align = "center", 
                             HTML('<p align="left" "font-size:150%;">The <b>probability mass function</b> (p.m.f.) 
                                     of the runtime is estimated via <b>kernel 
                                     estimation</b> (KDE). The estimation and the boxplot are shown as follows:</p>'),
                             plotlyOutput('RT_PMF', height = "600px", width = "1000px")
                      )
                    )
                  )
              )
            ),
            
            column(width = 12,     
                   box(title = 'Historgram of the Runtime Samples', 
                       width = 12, collapsible = TRUE, solidHeader = TRUE, 
                       status = "primary",
                       sidebarPanel(
                         width = 2,
                         HTML('Please select the target value:'),
                         textInput('RT_PMF_HIST_FTARGET', label = HTML('<p>\\(f:\\) target value</p>'), 
                                   value = ''),
                         
                         HTML('Please choose whether the histograms are <b>overlaid</b> in one plot 
                              or <b>separated</b> in several subplots:'),
                         selectInput('ERT_illu_mode', '', 
                                     choices = c("overlay", "subplot"), 
                                     selected = 'subplot')
                         ),
                       
                       mainPanel(
                         width = 10,
                         column(width = 12, align = "center",
                                HTML('<p align="left" "font-size:150%;">In addition, 
                                     the <i>histogram</i> of the runtime is also illustrated:</p>'),
                                plotlyOutput('RT_HIST', height = "600px", width = "1000px")
                         )
                      )
                   )
            )
          )

    ),
    
    # Runtime empirical c.d.f. ------------------------------------------
    tabItem(tabName = 'ERT_ECDF', 
            fluidRow(
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Plot of the Runtime Distribution</p>'), 
                         width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary",
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             sliderInput("target.ecdf1",
                                         "target1",
                                         min = 0, 
                                         max = 1, 
                                         value = 0),
                             
                             # checkboxInput('show.ecdf1', 
                             #               label = 'show target1',
                             #               value = F),
                             
                             sliderInput("target.ecdf2",
                                         "target2",
                                         min = 0, 
                                         max = 1, 
                                         value = 0),
                             
                             # checkboxInput('show.ecdf2', 
                             #               label = 'show target2',
                             #               value = F),
                             
                             sliderInput("target.ecdf3",
                                         "target3",
                                         min = 0, 
                                         max = 1, 
                                         value = 0),
                             
                             checkboxInput('RT_ECDF_semilogx',
                                           label = 'scale x axis log10',
                                           value = F)

                             # checkboxInput('RT_ECDF_semilogy', 
                             #               label = 'scale y axis log10',
                             #               value = F)
                             
                             # checkboxInput('show.ecdf3', 
                             #               label = 'show target3',
                             #               value = F),
                             
                             # sliderInput("target.ecdf4",
                             #             "target4",
                             #             min = 0, 
                             #             max = 1, 
                             #             value = 0),
                             # 
                             # checkboxInput('show.ecdf4', 
                             #               label = 'show target4',
                             #               value = F),
                             # 
                             # sliderInput("target.ecdf5",
                             #             "target5",
                             #             min = 0, 
                             #             max = 1, 
                             #             value = 0),
                             # 
                             # checkboxInput('show.ecdf5', 
                             #               label = 'show target5',
                             #               value = T),
                             # 
                             # checkboxInput('ecdf.aggr', 
                             #               label = 'aggregate all ECDFs',
                             #               value = F)
                             
                           ), 
                           
                           mainPanel(width = 9,
                                     plotlyOutput("ecdf", height = "600px", width = "1100px"))
                         )
                     )
              ),
              
              column(width = 12,
                     box(title = HTML('<p style="font-size:120%;">Empirical Cumulative 
                                      Distribution of the runtime: Aggregation</p>'), 
                         width = 12,
                         solidHeader = T, status = "primary", collapsible = T,
                         sidebarPanel(
                           width = 3,
                           HTML('<p align="justify"><i>Evenly spaced</i> target values are used to aggregate the ECDF:</p>'),
                           textInput('RT_fstart', label = HTML('<p>\\(f_{start}:\\) starting objective value</p>'), value = ''),
                           textInput('RT_fstop', label = HTML('<p>\\(f_{stop}:\\) stopping objective value</p>'), value = ''),
                           textInput('RT_fstep', label = HTML('<p>\\(f_{step}:\\) objective value steps</p>'), value = ''),
                           
                           checkboxInput('RT_ECDF_per_target',
                                         label = 'show ECDFs for each target',
                                         value = F),
                           
                           checkboxInput('RT_ECDF_AGGR_semilogx', 
                                         label = 'scale x axis log10',
                                         value = F)
                           ),
                         
                         mainPanel(
                           width = 9,
                           HTML('<p>The evenly spaced target values are:</p>'),
                           verbatimTextOutput('RT_GRID'),
                           HTML('<p>Then the mean function of the corresponding ECDFs is illustrated:</p>'),
                           plotlyOutput('RT_ECDF_AGGR', height = "600px", width = "1100px"),
                           hr()
                           
                           # HTML('<p>The <b>Kolmogorov–Smirnov test</b> is used to investigate
                           # how the ditribution of runtimes of one algorithm differs from the other:</p><br>'),
                           # column(10, align = "center", verbatimTextOutput('ks'))
                        )
                     )
                )
              )
              
              # column(width = 4,
              #        box(title = 'Area under the ECDF', width = 550, height = 600,
              #            solidHeader = TRUE, status = "primary",
              #            showOutput("auc.radar", "highcharts")
              #        )
              # )
            
    )
  )
)

# -----------------------------------------------------------
dashboardPage(title = 'IOHProfiler',
              header, sidebar, body)
