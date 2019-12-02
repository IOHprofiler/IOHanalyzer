sidebar_menu <- function() {
  sidebarMenu(id = "tabs",
              menuItem("Upload Data", tabName = "upload", icon = icon('upload', lib = 'glyphicon'),
                       selected = T),
              menuItem("General Overview", tabName = "overview", icon = icon("table")),
              
              menuItem("Fixed-Target Results V1", tabName = "ERT", icon = icon("file-text-o"),
                       menuSubItem("Data Summary", tabName = "ERT_data", icon = icon("table")),
                       menuItem("Expected Runtime", tabName = "ERT_convergence", icon = icon("line-chart"), selected = F,
                                menuSubItem("Single Function", tabName = "ERT_convergence_single", icon = icon("line-chart"), selected = F),
                                menuSubItem("Aggregated", tabName = "ERT_convergence_aggr", icon = icon("line-chart"), selected = F)),
                       menuSubItem("Probability Mass Function", tabName = "RT_PMF", icon = icon("bar-chart"), selected = F),
                       menuItem("Cumulative Distribution", tabName = "RT_ECDF", icon = icon("line-chart"), selected = F,
                                   menuSubItem("Single Function", tabName = "ERT_ECDF_single", icon = icon("line-chart"), selected = F),
                                   menuSubItem("Aggregated", tabName = "RT_ECDF_aggr", icon = icon("line-chart"), selected = F)),
                       menuSubItem("Algorithm Parameters", tabName = "RT_PARAMETER", icon = icon('file-text-o'), selected = F),
                       menuItem('Statistics', tabName = "RT_Statistics", icon = icon('file-text-o'), 
                                   menuSubItem("Single Function", tabName = "RT_Statistics_single", icon = icon("file-text-o"), selected = F),
                                   menuSubItem("Aggregated", tabName = "RT_Statistics_aggr", icon = icon("file-text-o"), selected = F))
              ),
              menuItem("Fixed-Target Results V2", tabName = "ERT", icon = icon("file-text-o"),
                       menuItem("Single Function", tabName = "RT_single", icon = icon("line-chart"), selected = F,
                            menuSubItem("Data Summary", tabName = "ERT_data", icon = icon("table")),
                            menuSubItem("Expected Runtime", tabName = "ERT_convergence_single", icon = icon("line-chart"), selected = F),
                            menuSubItem("Probability Mass Function", tabName = "RT_PMF", icon = icon("bar-chart"), selected = F),
                            menuSubItem("Cumulative Distribution", tabName = "ERT_ECDF_single", icon = icon("line-chart"), selected = F),
                            menuSubItem("Algorithm Parameters", tabName = "RT_PARAMETER", icon = icon('file-text-o'), selected = F),
                            menuSubItem("Statistics", tabName = "RT_Statistics_single", icon = icon("file-text-o"), selected = F)
                       ),
                       menuItem("Aggregated", tabName = "RT_aggr", icon = icon("bar-chart"), selected = F,
                            menuSubItem("Expected Runtime", tabName = "ERT_convergence_aggr", icon = icon("line-chart"), selected = F),
                            menuSubItem("Cumulative Distribution", tabName = "RT_ECDF_aggr", icon = icon("line-chart"), selected = F),
                            menuSubItem("Statistics", tabName = "RT_Statistics_aggr", icon = icon("file-text-o"), selected = F)
                       )
                       ),
              #          menuSubItem("Data Summary", tabName = "ERT_data", icon = icon("table")),
              #          menuItem("Expected Runtime", tabName = "ERT_convergence", icon = icon("line-chart"), selected = F,
              #                   menuSubItem("Single Function", tabName = "ERT_convergence_single", icon = icon("line-chart"), selected = F),
              #                   menuSubItem("Aggregated", tabName = "ERT_convergence_aggr", icon = icon("line-chart"), selected = F)),
              #          menuSubItem("Probability Mass Function", tabName = "RT_PMF", icon = icon("bar-chart"), selected = F),
              #          menuItem("Cumulative Distribution", tabName = "RT_ECDF", icon = icon("line-chart"), selected = F,
              #                   menuSubItem("Single Function", tabName = "ERT_ECDF_single", icon = icon("line-chart"), selected = F),
              #                   menuSubItem("Aggregated", tabName = "RT_ECDF_aggr", icon = icon("line-chart"), selected = F)),
              #          menuSubItem("Algorithm Parameters", tabName = "RT_PARAMETER", icon = icon('file-text-o'), selected = F),
              #          menuItem('Statistics', tabName = "RT_Statistics", icon = icon('file-text-o'), 
              #                   menuSubItem("Single Function", tabName = "RT_Statistics_single", icon = icon("file-text-o"), selected = F),
              #                   menuSubItem("Aggregated", tabName = "RT_Statistics_aggr", icon = icon("file-text-o"), selected = F))
              # ),
              menuItem("Fixed-Budget Results", tabName = "FCE", icon = icon("file-text-o"),
                       menuSubItem("Data Summary", tabName = "FCE_DATA", icon = icon("table")),
                       menuSubItem("Expected Target Value", tabName = "FCE_convergence", icon = icon("bar-chart")),
                       menuSubItem("probability Density Function", tabName = "FCE_PDF", icon = icon("bar-chart"), selected = F),
                       menuSubItem("Cumulative Distribution", tabName = "FCE_ECDF", icon = icon("line-chart")),
                       menuSubItem("Algorithm Parameters", tabName = "FCE_PARAMETER", icon = icon('file-text-o'), selected = F),
                       menuSubItem('Statistics', tabName = "FCE_Statistics", icon = icon('file-text-o'))
                       ),

              menuItem("Data Format", tabName = "dataformat", icon = icon("fas fa-database")),
              menuItem("About", tabName = "about", icon = icon("question")),
              menuItem("Settings", tabName = "Settings", icon = icon("cog")),
              menuItem("Report", tabName = "Report", icon = icon("file-pdf"))
  )
}
