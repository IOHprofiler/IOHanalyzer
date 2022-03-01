rt_nevergrad_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Winning-fraction-based heatmaps</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('RT_NG.Heatmap.ID', 'Algorithms to compare', choices = NULL,
                    selected = NULL, multiple = T),
        selectInput('RT_NG.Heatmap.Funcid', 'Functions to use', choices = NULL,
                    selected = NULL, multiple = T),
        selectInput('RT_NG.Heatmap.Dim', 'Dimensions to use', choices = NULL,
                    selected = NULL, multiple = T),
        actionButton('RT_NG.Heatmap.Create', 'Create heatmap'),
        hr(),
        selectInput('RT_NG.Heatmap.Format', label = 'Select the figure format',
                    choices = supported_fig_format, selected = supported_fig_format),

        downloadButton('RT_NG.Heatmap.Download', label = 'Download the figure')
      ),

      mainPanel(
        width = 9,
        HTML_P('The algorithms are compared pairwise according to the fraction
               of times their mean is better over all selected (function,dimension)-pairs'),
        HTML_P("The chosen <b>target function values</b> per (function, dimension)-pair are as follows
                 (double click an entry to edit it):"),
        DT::dataTableOutput("RT_NG.Heatmap.Targets"),
        hr(),
        HTML_P("The results of the ranking are:"),
        plotlyOutput.IOHanalyzer("RT_NG.Heatmap.Plot")
      )
  )
}


fv_nevergrad_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Winning-fraction-based heatmaps</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('FV_NG.Heatmap.ID', 'Algorithms to compare', choices = NULL,
                    selected = NULL, multiple = T),
        selectInput('FV_NG.Heatmap.Funcid', 'Functions to use', choices = NULL,
                    selected = NULL, multiple = T),
        selectInput('FV_NG.Heatmap.Dim', 'Dimensions to use', choices = NULL,
                    selected = NULL, multiple = T),
        actionButton('FV_NG.Heatmap.Create', 'Create heatmap'),
        hr(),
        selectInput('FV_NG.Heatmap.Format', label = 'Select the figure format',
                    choices = supported_fig_format, selected = supported_fig_format),

        downloadButton('FV_NG.Heatmap.Download', label = 'Download the figure')
      ),

      mainPanel(
        width = 9,
        HTML_P('The algorithms are compared pairwise according to the fraction
               of times their mean is better over all selected (function,dimension)-pairs'),
        HTML_P("The chosen <b>target function values</b> per (function, dimension)-pair are as follows
                 (double click an entry to edit it):"),
        DT::dataTableOutput("FV_NG.Heatmap.Targets"),
        hr(),
        HTML_P("The results of the ranking are:"),
        plotlyOutput.IOHanalyzer("FV_NG.Heatmap.Plot")
      )
  )
}
