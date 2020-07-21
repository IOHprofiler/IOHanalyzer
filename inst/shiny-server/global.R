suppressMessages(library(IOHanalyzer))
suppressMessages(library(shiny))
suppressMessages(library(shinyjs))
suppressMessages(library(reshape2))
suppressMessages(library(magrittr))
suppressMessages(library(data.table))
suppressMessages(library(dplyr))
suppressMessages(library(plotly))
suppressMessages(library(shinydashboard))
suppressMessages(library(xtable))
suppressMessages(library(colourpicker))
suppressMessages(library(bsplus))
suppressMessages(library(DT))
suppressMessages(library(knitr))
suppressMessages(library(kableExtra))

# global options
options(datatable.print.nrows = 20)
options(width = 80)
options(shiny.maxRequestSize = 200 * 1024 ^ 2)  # maximal upload file size

# for customized 'plotlyOutput' function -----
widget_html <- function(name, package, id, style, class, inline = FALSE, ...) {
  # attempt to lookup custom html function for widget
  fn <- tryCatch(get(paste0(name, "_html"),
                     asNamespace(package),
                     inherits = FALSE),
                 error = function(e) NULL)
  
  # call the custom function if we have one, otherwise create a div
  if (is.function(fn)) {
    fn(id = id, style = style, class = class, ...)
  } else if (inline) {
    tags$span(id = id, style = style, class = class)
  } else {
    tags$div(id = id, style = style, class = class)
  }
}

checkShinyVersion <- function(error = TRUE) {
  x <- utils::packageDescription('htmlwidgets', fields = 'Enhances')
  r <- '^.*?shiny \\(>= ([0-9.]+)\\).*$'
  if (is.na(x) || length(grep(r, x)) == 0 || system.file(package = 'shiny') == '')
    return()
  v <- gsub(r, '\\1', x)
  f <- if (error) stop else packageStartupMessage
  if (utils::packageVersion('shiny') < v)
    f("Please upgrade the 'shiny' package to (at least) version ", v)
}

widget_dependencies <- function(name, package){
  htmlwidgets::getDependency(name, package)
}

plotlyOutput.IOHanalyzer <- function(outputId, width = '100%', aspect_ratio = 16/10) {
  padding_bottom <- paste0(100 / aspect_ratio, '%')
  reportSize <- TRUE
  inline <- FALSE
  
  checkShinyVersion()
  html <- htmltools::tagList(
    widget_html('plotly', 'plotly', id = outputId, 
                class = paste0('plotly', " html-widget html-widget-output", 
                               if (reportSize) 
                                 " shiny-report-size"), 
                style = sprintf("width:%s; height: 0; padding-bottom:%s; %s", 
                                htmltools::validateCssUnit(width), 
                                htmltools::validateCssUnit(padding_bottom), 
                                if (inline) 
                                  "display: inline-block;"
                                else ""), 
                width = width, height = 0)
  )
  dependencies <- widget_dependencies('plotly', 'plotly')
  htmltools::attachDependencies(html, dependencies)
}

# markers for plotly
symbols <- c("circle-open", "diamond-open", "square-open", "cross-open",
             "triangle-up-open", "triangle-down-open")

# ploting settings for UI ---------------------
plotly_height <- "auto"
plotly_width <- "auto"
plotly_height2 <- "auto"
plotly_width2 <- "auto"

IOHprofiler <- 'IOHprofiler'
COCO <- 'COCO'
TWO_COL <- 'TWO_COL'
AUTOMATIC <- 'AUTOMATIC'
BIBOJ_COCO <- 'BIBOJ_COCO'
NEVERGRAD <- 'NEVERGRAD'

# directory where rds-data is stored
get_repo_location <- function() {
  user_repo <- file.path(Sys.getenv('HOME'), 'repository')
  if (file.exists(user_repo)) user_repo else ''
}

print_html <- function(s, widget_id = 'process_data_promt') 
  shinyjs::html(widget_id, s, add = TRUE)


# download file names: csv, image ---------------------
overview_single_name <- parse(text = "paste0('Overview-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$Overview.Single.Format)")
overview_all_name <- parse(text = "paste0('Overview-All-', '.', input$Overview.All.Format)")
RT_csv_name <- parse(text = "paste0('RTStats-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$RTSummary.Statistics.Format)")
RT_overview_name <- parse(text = "paste0('RTOverview-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$RTSummary.Overview.Format)")
RTSample_csv_name <- parse(text = "paste0('RTSample-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$RTSummary.Sample.Format)")
FV_csv_name <- parse(text = "paste0('FVStats-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$FCESummary.Statistics.Format)")
FV_overview_name <- parse(text = "paste0('FVOverview-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$FCESummary.Overview.Format)")
FVSample_csv_name <- parse(text = "paste0('FVSample-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$FCESummary.Sample.FileFormat)")
FV_PAR_csv_name <- parse(text = "paste0('PARSummary-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$FV_PAR.Summary.Format)")
FV_PARSample_csv_name <- parse(text = "paste0('PARSample-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$FV_PAR.Sample.FileFormat)")
RT_PAR_csv_name <- parse(text = "paste0('PARSummary-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$RT_PAR.Summary.Format)")
RT_PARSample_csv_name <- parse(text = "paste0('PARSample-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$RT_PAR.Sample.FileFormat)")
ERT_multi_func_name <- parse(text = "paste0('MultiERT-', paste0(input$Overall.Dim, 'D'),
                             '.', input$ERTPlot.Aggr.TableFormat)")
ERT_multi_dim_name <- parse(text = "paste0('MultiERT-', paste0('F', input$Overall.Funcid),
                             '.', input$ERTPlot.Aggr_Dim.TableFormat)")
FCE_multi_func_name <- parse(text = "paste0('MultiFCE-', paste0(input$Overall.Dim, 'D'),
                             '.', input$FCEPlot.Aggr.TableFormat)")
RT_Glicko2_table_name <- parse(text = "paste0('RT_Glicko2', '.', input$RT_Stats.Glicko.TableFormat)")
RT_Glicko2_figure_name <- parse(text = "paste0('RT_Glicko2', '.', input$RT_Stats.Glicko.Format)")

RT_DSC_table_name <- parse(text = "paste0('RT_DSC', '.', input$RT_Stats.DSC.TableFormat)")
RT_DSC_figure_name <- parse(text = "paste0('RT_DSC', '.', input$RT_Stats.DSC.Format)")
FV_DSC_figure_name_rank <- parse(text = "paste0('RT_DSC_PerformViz', '.', input$RT_Stats.DSC.Format_rank)")

FV_DSC_table_name <- parse(text = "paste0('FV_DSC', '.', input$FV_Stats.DSC.TableFormat)")
FV_DSC_figure_name <- parse(text = "paste0('FV_DSC', '.', input$FV_Stats.DSC.Format)")
FV_DSC_figure_name_rank <- parse(text = "paste0('FV_DSC_PerformViz', '.', input$FV_Stats.DSC.Format_rank)")

RT_Stats_table_name <- parse(text = "paste0('RT_Stat_Comp-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$RT_Stats.Overview.TableFormat)")
RT_Stats_heatmap_name <- parse(text = "paste0('RT_Stat_Heatmap-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$RT_Stats.Overview.Format)")
RT_Stats_network_name <- parse(text = "paste0('RT_Stat_Network-', paste0(input$Overall.Dim, 'D'),
                             paste0('F', input$Overall.Funcid), '.', input$RT_Stats.Overview.Format)")
# max_samples <- 100

FIG_NAME_ERT_PER_FUN <- parse(text = "paste0('ERT-', Sys.Date(), '.', input$ERTPlot.Format)")
FIG_NAME_ERT_PER_FUN_MULTI <- parse(text = "paste0('ERT_Mult-', Sys.Date(), '.', input$ERTPlot.Multi.Format)")
FIG_NAME_ERT_AGGR <- parse(text = "paste0('ERT_Aggr-', Sys.Date(), '.', input$ERTPlot.Aggr.Format)")
FIG_NAME_ERT_AGGR_DIM <- parse(text = "paste0('ERT_Aggr_Dim-', Sys.Date(), '.', input$ERTPlot.Aggr_Dim.Format)")
FIG_NAME_RT_PMF <- parse(text = "paste0('RT_PMF-', Sys.Date(), '.', input$RTPMF.Bar.Format)")
FIG_NAME_RT_HIST <- parse(text = "paste0('RT_HIST-', Sys.Date(), '.', input$RTPMF.Hist.Format)")
FIG_NAME_RT_ECDF_AGGR <- parse(text = "paste0('RT_ECDF_AGGR-', Sys.Date(), '.', input$RTECDF.Multi.Format)")
FIG_NAME_RT_ECDF_MULT <- parse(text = "paste0('RT_ECDF_MULT-', Sys.Date(), '.', input$RTECDF.Aggr.Format)")
FIG_NAME_RT_AUC <- parse(text = "paste0('RT_AUC-', Sys.Date(), '.', input$RTECDF.AUC.Format)")

FIG_NAME_FV_PER_FUN <- parse(text = "paste0('FV-', Sys.Date(), '.', input$FCEPlot.Format)")
FIG_NAME_FV_PER_FUN_MULTI <- parse(text = "paste0('FCE_Mult-', Sys.Date(), '.', input$FCEPlot.Multi.Format)")
FIG_NAME_FV_AGGR <- parse(text = "paste0('FCE_Aggr-', Sys.Date(), '.', input$FCEPlot.Aggr.Format)")
FIG_NAME_FV_PDF <- parse(text = "paste0('FV_PMF-', Sys.Date(), '.', input$FCEPDF.Bar.Format)")
FIG_NAME_FV_HIST <- parse(text = "paste0('FV_HIST-', Sys.Date(), '.', input$FCEPDF.Hist.Format)")
FIG_NAME_FV_ECDF_AGGR <- parse(text = "paste0('FV_ECDF_AGGR-', Sys.Date(), '.', input$FCEECDF.Mult.Format)")
FIG_NAME_FV_AUC <- parse(text = "paste0('FV_AUC-', Sys.Date(), '.', input$FCEECDF.AUC.Format)")

FIG_NAME_RT_PAR_PER_FUN <- parse(text = "paste0('RT_PAR-', Sys.Date(), '.', input$RT_PAR.Plot.Format)")
FIG_NAME_FV_PAR_PER_FUN <- parse(text = "paste0('FV_PAR-', Sys.Date(), '.', input$FV_PAR.Plot.Format)")


# ID of the control widget, whose current value should de always recorded and restored ----
# those control widget are switched on and off
widget_id <- c('RTSummary.Statistics.Min',
               'RTSummary.Statistics.Max',
               'RTSummary.Statistics.Step',
               'RTSummary.Sample.Min',
               'RTSummary.Sample.Max',
               'RTSummary.Sample.Step',
               'RTECDF.Multi.Min',
               'RTECDF.Multi.Max',
               'RTECDF.Multi.Step',
               'RTECDF.Single.Target',
               'RTPMF.Bar.Target',
               'RTPMF.Hist.Target',
               'ERTPlot.Min',
               'ERTPlot.Max',
               'ERTPlot.Aggr.Targets',
               'RTECDF.AUC.Min',
               'RTECDF.AUC.Max',
               'RTECDF.AUC.Step',
               'FV_PAR.Plot.Min',
               'FV_PAR.Plot.Max',
               'FV_PAR.Summary.Min',
               'FV_PAR.Summary.Max',
               'FV_PAR.Summary.Step',
               'FV_PAR.Sample.Min',
               'FV_PAR.Sample.Max',
               'FV_PAR.Sample.Step',
               'RT_PAR.Plot.Min',
               'RT_PAR.Plot.Max',
               'RT_PAR.Summary.Min',
               'RT_PAR.Summary.Max',
               'RT_PAR.Summary.Step',
               'RT_PAR.Sample.Min',
               'RT_PAR.Sample.Max',
               'RT_PAR.Sample.Step',
               'FCESummary.Statistics.Min',
               'FCESummary.Statistics.Max',
               'FCESummary.Statistics.Step',
               'FCESummary.Sample.Min',
               'FCESummary.Sample.Max',
               'FCESummary.Sample.Step',
               'FCEPDF.Hist.Runtime',
               'FCEPDF.Bar.Runtime',
               'FCEPlot.Min',
               'FCEPlot.Max',
               'FCEECDF.Mult.Min',
               'FCEECDF.Mult.Max',
               'FCEECDF.Mult.Step',
               'FCEECDF.AUC.Min',
               'FCEECDF.AUC.Max',
               'FCEECDF.AUC.Step',
               'FCEECDF.Single.Target')

eventExpr <- parse(text = paste0('{', paste(paste0('input$', widget_id), collapse = "\n"), '}'))

# token needed for mapbox, which is again needed for ocra... ------
supported_fig_format <- c('png', 'eps', 'svg', 'pdf')
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoid2FuZ3JvbmluIiwiYSI6ImNqcmIzemhvMDBudnYzeWxoejh5c2Y5cXkifQ.9XGMWTDOsgi3-b5qG594kQ')

sanity_check_id <- function(input) {
  for (id in widget_id) {
    tryCatch(eval(parse(text = paste0('input$', id))),
             error = function(e) {
               cat(paste('widget', id, 'does not exist!\n'))
             })
  }
}






