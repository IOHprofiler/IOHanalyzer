bbocomp_table_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(
    title = HTML('<p style="font-size:120%;">Aggregated comparisons</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML_P('Select which attributes to aggregate over.'),
      checkboxInput('BBOcomp.aggr_alg', 'Algorithms', value = TRUE),
      checkboxInput('BBOcomp.aggr_class', 'Classifiers', value = TRUE),
      checkboxInput('BBOcomp.aggr_dataset', 'Datasets', value = FALSE),
      checkboxInput('BBOcomp.aggr_metric', 'Metrics', value = TRUE),
      selectInput("BBOcomp.aggr_averaging", "Aggregation_method", 
                  c("Average"), selected = "Average"),
      hr(),
      selectInput('BBOcomp.Format', 'Format', choices = c('csv','tex'), selected = 'csv')
      # downloadButton("BBOcomp.Download", "Save this table as csv")
    ),
    
    mainPanel(
      width = 9,
      DT::dataTableOutput('table_BBOcomp_aggr')
    )
  )
}

bbocomp_plot_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Value distribution</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = TRUE, status = "primary",
    sidebarPanel(
      width = 3,
      
      selectInput('BBOcomp.Plot.xattr', "Select the attribute to use in x-axis",
                  choices = c("algId", "classifier", "dataset", "metric"), selected = "algId"),
      selectInput('BBOcomp.Plot.subattr', "Select the attribute to use for subplots",
                  choices = c("none", "algId", "classifier", "dataset", "metric"), selected = "none"),
      hr(),
      selectInput('BBOcomp.Plot.Format', label = 'Select the figure format',
                  choices = supported_fig_format, selected = 'pdf')
      # downloadButton('BBOcomp.Plot.Download', label = 'Download the figure')
    ),
    
    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        plotlyOutput.IOHanalyzer('BBOcomp.Plot.Figure')
      )
    )
  )
}

bbocomp_table_pos_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Best found configurations</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      HTML_P('Select which classifier to use.'),
      selectInput("BBOcomp.Postable.classifier", "Classifier", NULL),
      hr(),
      selectInput('BBOcomp.Postable.Format', 'Format', choices = c('csv','tex'), selected = 'csv')
      # downloadButton("BBOcomp.Download", "Save this table as csv")
    ),
    
    mainPanel(
      width = 9,
      DT::dataTableOutput('table_BBOcomp_pos')
    )
  )
}

