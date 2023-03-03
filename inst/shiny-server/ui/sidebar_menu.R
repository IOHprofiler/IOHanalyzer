sidebar_menu <- function() {
  sidebarMenu(id = "tabs",
              menuItem("Upload Data", tabName = "upload", icon = icon('upload', lib = 'glyphicon'),
                       selected = T),
              # menuItem("OPTION Data", tabName = "ontology", icon = icon('upload', lib = 'glyphicon'),
              #          selected = T),
              menuItem("General Overview", tabName = "overview", icon = icon("table")),

              menuItem("Fixed-Target Results", tabName = "ERT", icon = icon("file"),
                       menuItem("Single Function", tabName = "RT_single", icon = icon("chart-line"), selected = F,
                            menuSubItem("Data Summary", tabName = "ERT_data", icon = icon("table")),
                            menuSubItem("Expected Runtime", tabName = "ERT_convergence_single", icon = icon("chart-line"), selected = F),
                            menuSubItem("Probability Mass Function", tabName = "RT_PMF", icon = icon("chart-area"), selected = F),
                            menuSubItem("Cumulative Distribution", tabName = "RT_ECDF_single", icon = icon("chart-line"), selected = F),
                            menuSubItem("Algorithm Parameters", tabName = "RT_PARAMETER", icon = icon('bars'), selected = F),
                            menuSubItem("Statistics", tabName = "RT_Statistics_single", icon = icon("search-plus"), selected = F)
                       ),
                       menuItem("Multiple Functions", tabName = "RT_aggr", icon = icon("chart-bar"), selected = F,
                            menuSubItem("Data Summary", tabName = "RT_table_multi", icon = icon("table"), selected = F),
                            menuSubItem("Expected Runtime", tabName = "ERT_convergence_aggr", icon = icon("chart-line"), selected = F),
                            menuSubItem("Cumulative Distribution", tabName = "RT_ECDF_aggr", icon = icon("chart-line"), selected = F),
                            menuSubItem("Deep Statistics", tabName = "RT_DSC", icon = icon("not-equal"), selected = F),
                            menuSubItem("Ranking", tabName = "RT_Statistics_aggr", icon = icon("project-diagram"), selected = F),
                            menuSubItem("Portfolio", tabName = "RT_portfolio", icon = icon("chart-bar"), selected = F)
                       )
                       ),

              menuItem("Fixed-Budget Results", tabName = "FCE", icon = icon("file"),
                       menuItem("Single Function", tabName = "FCE_single", icon = icon("chart-line"), selected = F,
                           menuSubItem("Data Summary", tabName = "FCE_DATA", icon = icon("table")),
                           menuSubItem("Expected Target Value", tabName = "FCE_convergence_single", icon = icon("chart-bar")),
                           menuSubItem("Probability Density Function", tabName = "FCE_PDF", icon = icon("chart-area"), selected = F),
                           menuSubItem("Cumulative Distribution", tabName = "FCE_ECDF", icon = icon("chart-line")),
                           menuSubItem("Algorithm Parameters", tabName = "FCE_PARAMETER", icon = icon('bars'), selected = F),
                           menuSubItem('Statistics', tabName = "FCE_Statistics_single", icon = icon('search-plus'))
                       ),
                       menuItem("Multiple Functions", tabName = "RT_aggr", icon = icon("chart-bar"), selected = F,
                           menuSubItem("Data Summary", tabName = "FV_table_multi", icon = icon("table"), selected = F),
                           menuSubItem("Expected Target Value", tabName = "FCE_convergence_aggr", icon = icon("chart-bar")),
                           menuSubItem('Deep Statistics', tabName = "FCE_DSC", icon = icon('not-equal')),
                           menuSubItem('Ranking', tabName = "FCE_Statistics_aggr", icon = icon('project-diagram'))
                       )
                       ),
              menuItem("Other Perspectives", tabName = "Other", icon = icon('file'),
                       menuItem("Position Information", tabName = "Positions", icon = icon("arrows-alt")),
                       menuItem("Biobjective Plots", tabName = "BiObj", icon = icon("project-diagram"))
                       ),
              # menuItem("Data Format", tabName = "dataformat", icon = icon("fas fa-database")),
              menuItem("About", tabName = "about", icon = icon("question")),
              menuItem("Settings", tabName = "Settings", icon = icon("cog"))
              # ,
              # menuItem("Report", tabName = "Report", icon = icon("file-pdf"))
  )
}
