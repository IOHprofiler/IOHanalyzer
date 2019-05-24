FCE_GRID_INPUT_TEXT <- '<p align="justify">Set the range and the granularity of the results.
                        The table will show function values that have been reached within evenly
                        spaced evaluation budgets.</p>'

RT_MIN_LABEL <- HTML('<p>\\(B_{\\text{min}}:\\) Smallest budget value</p>')
RT_MAX_LABEL <- HTML('<p>\\(B_{\\text{max}}:\\) Largest budget value</p>')
RT_STEP_LABEL <- HTML('<p>\\(\\Delta B:\\) Granularity (step size)</p>')

F_MIN_LABEL <- HTML('<p>\\(f_{\\text{min}}:\\) Smallest target value</p>')
F_MAX_LABEL <- HTML('<p>\\(f_{\\text{max}}:\\) Largest target value</p>')
F_STEP_LABEL <- HTML('<p>\\(\\Delta f:\\) Granularity (step size)</p>')

header <- dashboardHeader(title = HTML('<div align="center"><b>IOHanalyzer</b></div>'))

HTML_P <- function(s) HTML(paste0('<p align="left" style="font-size:120%;">', s, '</p>'))

alg_select_info <- "Use this option to select which algorithms to plot. 
      This will hava an effect the dowloaded plot, 
      as opposed to using the legend-entries to show or hide algorithms "

custom_icon <- function(name = NULL){
  if (is.null(name)) name <- "info-circle"
  htmltools::tags$a(shiny::icon(name = name), href = "javascript:void(0);")
}