sidebar_menu <- function() {
  sidebarMenu(id = "tabs",
              menuItem("Upload Data", tabName = "upload", icon = icon('upload', lib = 'glyphicon'),
                       selected = T),

              menuItem("Fixed-Target Results", tabName = "ERT", icon = icon("file-text-o"),
                       menuSubItem("Data Summary", tabName = "ERT_data", icon = icon("table")),
                       menuSubItem("Expected Runtime", tabName = "ERT_convergence", icon = icon("line-chart"), selected = F),
                       menuSubItem("Probability Mass Function", tabName = "RT_PMF", icon = icon("bar-chart"), selected = F),
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
              menuItem("About", tabName = "about", icon = icon("question")),
              menuItem("Settings", tabName = "Settings", icon = icon("cog")),
              menuItem("Report", tabName = "Report", icon = icon("file-pdf"))
  )
}
