# some global variables shared between the server and ui
#
# Author: Hao Wang
# Email: wangronin@gmail.com
library(magrittr)

options(datatable.print.nrows = 20)
options(width = 120)

# ploting settings for UI ---------------------
aspect_ratio <-  4 / 3
fig_height <- 1100
fig_height2 <- 1100
fig_width <- fig_height * aspect_ratio
fig_width2 <- fig_height * (16 / 10)

# plotly_height <- paste0(fig_height, "px")
# plotly_width <- paste0(fig_width, "px")
# plotly_height2 <- paste0(fig_height2, "px")
# plotly_width2 <- paste0(fig_width2, "px")

plotly_height <- "auto"
plotly_width <- "auto"
plotly_height2 <- "auto"
plotly_width2 <- "auto"

IOHprofiler <- 'IOHprofiler'
COCO <- 'COCO'
TWO_COL <- 'TWO_COL'
AUTOMATIC <- 'AUTOMATIC'
BIBOJ_COCO <- 'BIBOJ_COCO'

# directory where rds-data is stored
get_repo_location <- function(official) {
  if (official) {
    user_repo <- file.path(Sys.getenv('HOME'), 'repository')
    installed_repo <- file.path(find.package('IOHProfiler'), 'data')
    if (file.exists(user_repo)) return(user_repo) else return(installed_repo)
  }
  else {
    user_repo <- file.path(Sys.getenv('HOME'), 'repository_unofficial')
    return(user_repo)
  }
}


# download file names: csv, image ---------------------
RT_csv_name <- parse(text = "paste0('RT-', paste(Sys.Date(), input$Overall.Dim,
                             paste0('F', input$Overall.Funcid), fstart, fstop, fstep,
                             sep = '-'), '.csv')")
RTSample_csv_name <- parse(text = "paste0('RTSample-', paste(Sys.Date(), input$Overall.Dim,
                                   paste0('F', input$Overall.Funcid), fstart, fstop,
                                   fstep, sep = '-'), '.csv')")
FV_csv_name <- parse(text = "paste0('FV-', paste(Sys.Date(), input$Overall.Dim,
                             paste0('F', input$Overall.Funcid), rt_min, rt_max, rt_step,
                             sep = '-'), '.csv')")
FVSample_csv_name <- parse(text = "paste0('FVSample-', paste(Sys.Date(), input$Overall.Dim,
                                   paste0('F', input$Overall.Funcid), rt_min, rt_max,
                                   rt_step, sep = '-'), '.csv')")
PAR_csv_name <- parse(text = "paste0('PAR-', paste(Sys.Date(), input$Overall.Dim,
                              paste0('F', input$Overall.Funcid), fstart, fstop, fstep,
                              sep = '-'), '.csv')")
PARSample_csv_name <- parse(text = "paste0('PARSample-', paste(Sys.Date(), input$Overall.Dim,
                                    paste0('F', input$Overall.Funcid), fstart, fstop, fstep,
                                    sep = '-'), '.csv')")

max_samples <- 100

FIG_NAME_ERT_PER_FUN <- parse(text = "paste0('ERT-', Sys.Date(), '.', input$ERTPlot.Format)")
FIG_NAME_ERT_PER_FUN_MULTI <- parse(text = "paste0('ERT_Mult-', Sys.Date(), '.', input$ERTPlot.Multi.Format)")
FIG_NAME_ERT_AGGR <- parse(text = "paste0('ERT_Aggr-', Sys.Date(), '.', input$ERTPlot.Aggr.Format)")
FIG_NAME_RT_PMF <- parse(text = "paste0('RT_PMF-', Sys.Date(), '.', input$RTPMF.Bar.Format)")
FIG_NAME_RT_HIST <- parse(text = "paste0('RT_HIST-', Sys.Date(), '.', input$RTPMF.Hist.Format)")
FIG_NAME_RT_ECDF_AGGR <- parse(text = "paste0('RT_ECDF_AGGR-', Sys.Date(), '.', input$RTECDF.Multi.Format)")
FIG_NAME_RT_ECDF_MULT <- parse(text = "paste0('RT_ECDF_MULT-', Sys.Date(), '.', input$RTECDF.Aggr.Format)")
FIG_NAME_RT_AUC <- parse(text = "paste0('RT_AUC-', Sys.Date(), '.', input$RTECDF.AUC.Format)")

FIG_NAME_FV_PER_FUN <- parse(text = "paste0('FV-', Sys.Date(), '.', input$FCEPlot.Format)")
FIG_NAME_FV_PDF <- parse(text = "paste0('FV_PMF-', Sys.Date(), '.', input$FCEPDF.Bar.Format)")
FIG_NAME_FV_HIST <- parse(text = "paste0('FV_HIST-', Sys.Date(), '.', input$FCEPDF.Hist.Format)")
FIG_NAME_FV_ECDF_AGGR <- parse(text = "paste0('FV_ECDF_AGGR-', Sys.Date(), '.', input$FCEECDF.Mult.Format)")
FIG_NAME_FV_AUC <- parse(text = "paste0('FV_AUC-', Sys.Date(), '.', input$FCEECDF.AUC.Format)")

FIG_NAME_PAR_PER_FUN <- parse(text = "paste0('PAR-', Sys.Date(), '.', input$PAR.Plot.Format)")


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
               'RTPMF.Bar.Target',
               'RTPMF.Hist.Target',
               'ERTPlot.Min',
               'ERTPlot.Max',
               'ERTPlot.Aggr.Targets',
               'RTECDF.Single.Target1',
               'RTECDF.Single.Target2',
               'RTECDF.Single.Target3',
               'RTECDF.AUC.Min',
               'RTECDF.AUC.Max',
               'RTECDF.AUC.Step',
               'PAR.Plot.Min',
               'PAR.Plot.Max',
               'PAR.Summary.Min',
               'PAR.Summary.Max',
               'PAR.Summary.Step',
               'PAR.Sample.Min',
               'PAR.Sample.Max',
               'PAR.Sample.Step',
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
               'FCEECDF.Single.Target1',
               'FCEECDF.Single.Target2',
               'FCEECDF.Single.Target3')

eventExpr <- parse(text = paste0('{', paste(paste0('input$', widget_id), collapse = "\n"), '}'))

# token needed for mapbox, which is again needed for ocra... ------
supported_fig_format <- c('png', 'eps', 'svg', 'pdf')
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoid2FuZ3JvbmluIiwiYSI6ImNqcmIzemhvMDBudnYzeWxoejh5c2Y5cXkifQ.9XGMWTDOsgi3-b5qG594kQ')






