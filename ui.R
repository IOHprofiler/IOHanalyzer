library(shinydashboard)
library(rCharts)

header <- dashboardHeader(title = HTML('<h4><div align="center"><b>ProfilePBO</b><br><i>Post-Processing</i></div></h4>'))

sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id = "tabs",
              menuItem("Upload data", tabName = "upload", icon = icon('upload', lib = 'glyphicon'), 
                       selected = T),
              menuItem("Fixed-Target Results", tabName = "ERT", icon = icon("file-text-o"),
                       menuSubItem("Data summary", tabName = "ERT_data", icon = icon("table"), selected = F),
                       menuSubItem("Convergence", tabName = "ERT_convergence", icon = icon("line-chart")),
                       menuSubItem("Probility Density", tabName = "ERT_plot", icon = icon("bar-chart")),
                       menuSubItem("Cumulative Distribution", tabName = "ERT_ECDF", icon = icon("line-chart"))
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

precison_slider <- sliderInput('target', 
                               label = '',
                               min = 0, 
                               max = 1, 
                               value = .5)

body <- dashboardBody(
  # include MathJax
  HTML("<head><script src='https://cdnjs.cloudflare.com/ajax/libs/mathjax/2.7.4/MathJax.js?config=TeX-MML-AM_CHTML' async></script></head>"),
  
  tabItems(
    tabItem(tabName = 'about', 
            includeMarkdown('about.Rmd')
    ),
  
    tabItem(tabName = 'readme',
            includeMarkdown('docs.Rmd')
    ),
    
    # -----------------------------------------------------------
    # summary for Fixed-Target Runtime (ERT)
    # -----------------------------------------------------------
    tabItem(tabName = 'ERT_data', 
      fluidRow(
        column(width = 12,
               box(title = HTML('<p style="font-size:120%;">Runntime Statistics at chosen target values</p>'), width = 12,
                   solidHeader = T, status = "primary", collapsible = T,
                   sidebarPanel(
                     width = 2,
                     HTML('<p align="justify">Align the running time: the smallest running time achieving the selected target values.<br>
                          <i>Evenly spaced</i> target values are generated:</p>'),
                     # precison_slider,
                     textInput('fstart', label = HTML('<p>\\(f_{start}:\\) starting objective value</p>'), value = ''),
                     textInput('fstop', label = HTML('<p>\\(f_{stop}:\\) stopping objective value</p>'), value = ''),
                     textInput('fstep', label = HTML('<p>\\(f_{step}:\\) objective value steps</p>'), value = ''),
                     selectInput('select.alg', 'which algorithm to show?', choices = c('algorithm1', 'algorithm2', 'all'), selected = 'all'),
                     downloadButton("downloadData", "Save this table as csv")
                   ),
                   
                   mainPanel(
                     width = 10,
                     HTML('<p>Firstly, some basic statistics (<i>mean, median, quantiles</i>) are shown for the runntime at the selected objective value:</p>'),
                     tableOutput('summary'),
                     br(),
                     includeMarkdown('quantile.Rmd')
                     # HTML('<p>Secondly, the <b>Kolmogorov–Smirnov test</b> is used to investigate how the ditribution of runtimes of one algorithm differs from the other:</p><br>'),
                     # column(10, align = "center", verbatimTextOutput('ks'))
                   )
               ),
               
               box(title = HTML('<p style="font-size:120%;">Runntime Samples</p>'), width = 12,
                   solidHeader = TRUE, status = "primary",
                   sidebarPanel(
                     width = 2,
                     HTML('<p align="justify">Please specify the target value for the alignment:</p>'),
                     textInput('fselect', label = HTML('<p>\\(f:\\) target value</p>'), value = ''),
                     downloadButton("downloadData.ert", "Save this table as csv")
                     ),
                   
                   mainPanel(
                     width = 10,
                     HTML('<p>The raw runtime samples aligned with respect to the specified target value are listed as follows:</p>'),
                     verbatimTextOutput('table.RT.sample')
                    )
               )
          )
      )
    ),
    
    tabItem(tabName = 'ERT_convergence', 
            fluidRow(
              column(width = 12,
                     box(title = 'Plot of Expected Running Time', width = 12,
                         collapsible = TRUE, solidHeader = TRUE, status = "primary",
                         sidebarPanel(
                           width = 2,
                           HTML('Select the range of target values to zoom in:'),
                           sliderInput("plot.range", 
                                       label = '', 
                                       min = 0, 
                                       max = 1, 
                                       value = c(0, 1)),
                           
                           checkboxInput('show.instance', 
                                         label = 'show each independent run',
                                         value = F),
                           
                           checkboxInput('show.mean', 
                                         label = 'show/hide mean',
                                         value = T),
                           
                           checkboxInput('show.median', 
                                         label = 'show/hide median',
                                         value = F),
                           
                           checkboxInput('semilogx', 
                                         label = 'scale x axis log10',
                                         value = F),
                           
                           checkboxInput('semilogy', 
                                         label = 'scale y axis log10',
                                         value = F)
                         ),
                         mainPanel(plotOutput('mean.convergence', height = "600px", width = "1000px"))
                     )
              )
            )
    ),
    
    # -----------------------------------------------------------
    # plots for Fixed-Target Runtime (ERT)
    # -----------------------------------------------------------
    tabItem(tabName = 'ERT_plot', 
          fluidRow(
            column(width = 12,
                   box(title = 'Plot of the Runtime Distribution', width = 12,
                       collapsible = TRUE,
                       solidHeader = TRUE, status = "primary",
                       sidebarPanel(
                         width = 2,
                         HTML('Align the running time: the smallest running time achieving the selected <b>target value</b>:'),
                         sliderInput('target.plot', 
                                     label = '',
                                     min = 0, 
                                     max = 1, 
                                     value = .5),
                        
                         HTML('Kernel density estimation uses the following <b>kernel function</b>:'),
                         selectInput('kernel', '', choices = c("gaussian", "epanechnikov", "rectangular",
                                                               "triangular", "biweight", "cosine", "optcosine"), 
                                     selected = 'gaussian'),
                         
                         checkboxInput('show.RT.sample',
                                       label = 'show runtime samples',
                                       value = T)
                       ),
                       
                       mainPanel(
                         width = 10,
                         column(width = 10, align = "center", 
                                HTML('<p align="left" "font-size:150%;">The <i>probability density function</i> (p.d.f.) 
                                     of the runtime is estimated via <b>kernel density 
                                     estimation</b> (KDE). The estimates and the runtime samples are shown as follows:</p>'),
                                plotOutput('bean.plot', height = "500px", width = "900px"),
                                br(),
                                
                                HTML('<p align="left" "font-size:150%;">In addition, the <i>histogram</i> (p.d.f.) of the runtime is also illustrated:</p>'),
                                plotOutput("histogram", height = "500px", width = "900px")
                                # showOutput("histogram", "highcharts")
                         )
                       )
                   )
            )
          )
    ),
    
    tabItem(tabName = 'ERT_ECDF', 
            fluidRow(
              column(width = 8,
                     box(title = 'Plot of the Runtime Distribution', width = 12,
                         collapsible = TRUE,
                         solidHeader = TRUE, status = "primary",
                         sidebarLayout(
                           sidebarPanel(
                             width = 3,
                             sliderInput("target.ecdf1",
                                         "target1",
                                         min = 0, 
                                         max = 1, 
                                         value = 0),
                             
                             checkboxInput('show.ecdf1', 
                                           label = 'show target1',
                                           value = F),
                             
                             sliderInput("target.ecdf2",
                                         "target2",
                                         min = 0, 
                                         max = 1, 
                                         value = 0),
                             
                             checkboxInput('show.ecdf2', 
                                           label = 'show target2',
                                           value = F),
                             
                             sliderInput("target.ecdf3",
                                         "target3",
                                         min = 0, 
                                         max = 1, 
                                         value = 0),
                             
                             checkboxInput('show.ecdf3', 
                                           label = 'show target3',
                                           value = F),
                             
                             sliderInput("target.ecdf4",
                                         "target4",
                                         min = 0, 
                                         max = 1, 
                                         value = 0),
                             
                             checkboxInput('show.ecdf4', 
                                           label = 'show target4',
                                           value = F),
                             
                             sliderInput("target.ecdf5",
                                         "target5",
                                         min = 0, 
                                         max = 1, 
                                         value = 0),
                             
                             checkboxInput('show.ecdf5', 
                                           label = 'show target5',
                                           value = T),
                             
                             checkboxInput('ecdf.aggr', 
                                           label = 'aggregate all ECDFs',
                                           value = F)
                             
                           ), 
                           
                           mainPanel(width = 9,
                                     showOutput("ecdf", "nvd3"))
                         )
                     )
              ),
              
              column(width = 4,
                     box(title = 'Area under the ECDF', width = 550, height = 600,
                         solidHeader = TRUE, status = "primary",
                         showOutput("auc.radar", "highcharts")
                     )
              )
            )
    )
  )
)

dashboardPage(header, sidebar, body)
