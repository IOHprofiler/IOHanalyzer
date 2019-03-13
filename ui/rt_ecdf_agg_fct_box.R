rt_ecdf_agg_fct_box <- function() {
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
        downloadButton('TARGET_TABLE_EXAMPLE_DOWNLOAD', label = 'download this example'),
        
        hr(),
        br(),
        fileInput("CSV_Targets_upload", label = HTML('<p align="left" style="font-size:120%;">Please choose a <i>csv file</i> containing the targets</p>'),
                  multiple = FALSE, accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")),
        selectInput('FIG_FORMAT_RT_ECDF_MULT', label = 'select the figure format',
                    choices = supported_fig_format, selected = 'pdf'),
        
        downloadButton('FIG_DOWNLOAD_RT_ECDF_MULT', label = 'download the figure')
        
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
}