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

plotly_height <- paste0(fig_height, "px")
plotly_width <- paste0(fig_width, "px")
plotly_height2 <- paste0(fig_height2, "px")
plotly_width2 <- paste0(fig_width2, "px")

IOHprofiler <- 'IOHprofiler'
COCO <- 'COCO'
TWO_COL <- 'TWO_COL'
AUTOMATIC <- 'AUTOMATIC'
probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.

.mean <- function(x) mean(x, na.rm = T)
.median <- function(x) median(x, na.rm = T)
.sd <- function(x) sd(x, na.rm = T)
.sum <- function(x) sum(x, na.rm = T)

D_quantile <- function(x, pct = probs) quantile(x, pct, names = F, type = 3, na.rm = T)
C_quantile <- function(x, pct = probs) quantile(x, pct, names = F, na.rm = T)


# download file names: csv, image ---------------------
RT_csv_name <- parse(text = "paste0('RT-', paste(Sys.Date(), input$DIM_INPUT, 
                             paste0('F', input$FUNCID_INPUT), fstart, fstop, fstep, 
                             sep = '-'), '.csv')")
RTSample_csv_name <- parse(text = "paste0('RTSample-', paste(Sys.Date(), input$DIM_INPUT, 
                                   paste0('F', input$FUNCID_INPUT), fstart, fstop, 
                                   fstep, sep = '-'), '.csv')")
FV_csv_name <- parse(text = "paste0('FV-', paste(Sys.Date(), input$DIM_INPUT, 
                             paste0('F', input$FUNCID_INPUT), rt_min, rt_max, rt_step,
                             sep = '-'), '.csv')")
FVSample_csv_name <- parse(text = "paste0('FVSample-', paste(Sys.Date(), input$DIM_INPUT, 
                                   paste0('F', input$FUNCID_INPUT), rt_min, rt_max, 
                                   rt_step, sep = '-'), '.csv')")
PAR_csv_name <- parse(text = "paste0('PAR-', paste(Sys.Date(), input$DIM_INPUT, 
                              paste0('F', input$FUNCID_INPUT), fstart, fstop, fstep, 
                              sep = '-'), '.csv')")
PARSample_csv_name <- parse(text = "paste0('PARSample-', paste(Sys.Date(), input$DIM_INPUT,
                                    paste0('F', input$FUNCID_INPUT), fstart, fstop, fstep, 
                                    sep = '-'), '.csv')")

max_samples <- 100

FIG_NAME_ERT_PER_FUN <- parse(text = "paste0('ERT-', Sys.Date(), '.', input$FIG_FORMAT_ERT_PER_FUN)")
FIG_NAME_RT_PMF <- parse(text = "paste0('RT_PMF-', Sys.Date(), '.', input$FIG_FORMAT_RT_PMF)")
FIG_NAME_RT_HIST <- parse(text = "paste0('RT_HIST-', Sys.Date(), '.', input$FIG_FORMAT_RT_HIST)")
FIG_NAME_RT_ECDF_AGGR <- parse(text = "paste0('RT_ECDF_AGGR-', Sys.Date(), '.', input$FIG_FORMAT_RT_ECDF_AGGR)")
FIG_NAME_RT_ECDF_MULT <- parse(text = "paste0('RT_ECDF_MULT-', Sys.Date(), '.', input$FIG_FORMAT_RT_ECDF_MULT)")
FIG_NAME_RT_AUC <- parse(text = "paste0('RT_AUC-', Sys.Date(), '.', input$FIG_FORMAT_RT_AUC)")

FIG_NAME_FV_PER_FUN <- parse(text = "paste0('FV-', Sys.Date(), '.', input$FIG_FORMAT_FV_PER_FUN)")
FIG_NAME_FV_PDF <- parse(text = "paste0('FV_PMF-', Sys.Date(), '.', input$FIG_FORMAT_FV_PDF)")
FIG_NAME_FV_HIST <- parse(text = "paste0('FV_HIST-', Sys.Date(), '.', input$FIG_FORMAT_FV_HIST)")
FIG_NAME_FV_ECDF_AGGR <- parse(text = "paste0('FV_ECDF_AGGR-', Sys.Date(), '.', input$FIG_FORMAT_FV_ECDF_AGGR)")
FIG_NAME_FV_AUC <- parse(text = "paste0('FV_AUC-', Sys.Date(), '.', input$FIG_FORMAT_FV_AUC)")

FIG_NAME_PAR_PER_FUN <- parse(text = "paste0('PAR-', Sys.Date(), '.', input$FIG_FORMAT_PAR_PER_FUN)")

# function for generating sequences for RT and FV ---------------------
# TODO: add Roxygen docs...
# TODO: maybe merge 'seq_FV' and 'seq_RT'...
# TODO: determine when the sequence should be generate in log-linear way
seq_FV <- function(FV, from = NULL, to = NULL, by = NULL, length.out = NULL, scale = NULL) {
  from <- max(from, min(FV))
  to <- min(to, max(FV))
  
  rev_trans <- function(x) x
  
  # Auto detect scaling
  # TODO: Improve this detection (based on FV?). Currently very arbitrary
  if (is.null(scale)) {
    if (to < 0 || from < 0)
      scale <- 'linear'
    else if (abs(log10(mean(FV)) - log10(median(FV))) > 1)
      scale <- 'log'
    else
      scale <- 'linear'
  }
  
  if (scale == 'log') {
    trans <- log10
    rev_trans <- function(x) 10 ^ x
    # TODO: Better way to deal with negative values 
    #       set lowest possible target globally instead of arbitrary 1e-12
    from <- max(1e-12, from)
    to <- max(1e-12 ,to)
    from <- trans(from)
    to <- trans(to)
  }
  
  #Avoid generating too many samples
  if(!is.null(by)){
    nr_samples_generated <- (to-from)/by
    if (nr_samples_generated > max_samples){
      by <- NULL
      if(is.null(length.out))
        length.out <- max_samples
    }
  }

  if (is.null(by) || by > to - from) {
    if (is.null(length.out)) {
      length.out <- 10
      args <- list(from = from, to = to, by = (to - from) / (length.out - 1))
    } else 
      args <- list(from = from, to = to, length.out = length.out)
  } else 
    args <- list(from = from, to = to, by = by)
  
  # tryCatch({
    do.call(seq, args) %>% 
      c(from, ., to) %>%    # always include the starting / ending value
      unique %>%
      rev_trans
  # }, error = function(e) {
    # c()
  # })
}

# TODO: Roxygen doc...
seq_RT <- function(RT, from = NULL, to = NULL, by = NULL, length.out = NULL, 
                   scale = 'linear') {
  rev_trans <- function(x) x
  
  # Do this first to avoid the log-values being overwritten.
  from <- max(from, min(RT))
  to <- min(to, max(RT))
  
  if (scale == 'log') {
    RT <- log10(RT)
    rev_trans <- function(x) 10 ^ x
    if (!is.null(from))
      from <- log10(from)
    if (!is.null(to))
      to <- log10(to)
    if (!is.null(by))
      by <- log10(by)
  }
  
  #Avoid generating too many samples
  if(!is.null(by)){
    nr_samples_generated <- (to-from)/by
    if (nr_samples_generated > max_samples){
      by <- NULL
      if(is.null(length.out))
        length.out <- max_samples
    }
  }
  
  # Also reset by if it is too large
  if (is.null(by) || by > to - from) {
    if (is.null(length.out)) {
      length.out <- 10
      args <- list(from = from, to = to, by = (to - from) / (length.out - 1))
    } else 
      args <- list(from = from, to = to, length.out = length.out)
  } else 
    args <- list(from = from, to = to, by = by)
  
  do.call(seq, args) %>% 
    c(from, ., to) %>%    # always include the starting / ending value
    unique %>%
    rev_trans
}


# ID of the control widget, whose current value should de always recorded and restored ----
# those control widget are switched on and off
widget_id <- c('fstart',
               'fstop',
               'fstep',
               'F_MIN_SAMPLE', 
               'F_MAX_SAMPLE',
               'F_STEP_SAMPLE', 
               'RT_fstart',
               'RT_fstop', 
               'RT_fstep', 
               'RT_fselect', 
               'RT_PMF_FTARGET', 
               'RT_PMF_HIST_FTARGET', 
               'ERT_FSTART', 
               'ERT_FSTOP', 
               'RT_ECDF_FTARGET1', 
               'RT_ECDF_FTARGET2', 
               'RT_ECDF_FTARGET3', 
               'RT_AUC_FSTART', 
               'RT_AUC_FSTOP', 
               'RT_AUC_FSTEP', 
               'PAR_F_MIN', 
               'PAR_F_MAX', 
               'PAR_F_MIN_SUMMARY', 
               'PAR_F_MAX_SUMMARY', 
               'PAR_F_STEP_SUMMARY', 
               'PAR_F_MIN_SAMPLE', 
               'PAR_F_MAX_SAMPLE', 
               'PAR_F_STEP_SAMPLE',
               'RT_MIN',
               'RT_MAX',
               'RT_STEP',
               'RT_MIN_SAMPLE',
               'RT_MAX_SAMPLE',
               'RT_STEP_SAMPLE',
               'FCE_HIST_RUNTIME',
               'FCE_PDF_RUNTIME',
               'FCE_RT_MIN',
               'FCE_RT_MAX',
               'FCE_ECDF_RT_MIN',
               'FCE_ECDF_RT_MAX',
               'FCE_ECDF_RT_STEP',
               'FCE_AUC_RT_MIN',
               'FCE_AUC_RT_MAX',
               'FCE_AUC_RT_STEP',
               'FCE_ECDF_RT1',
               'FCE_ECDF_RT2',
               'FCE_ECDF_RT3')

eventExpr <- parse(text = paste0('{', paste(paste0('input$', widget_id), collapse = "\n"), '}'))

# token needed for mapbox, which is again needed for ocra... ------
supported_fig_format <- c('png', 'eps', 'svg', 'pdf')
Sys.setenv('MAPBOX_TOKEN' = 'pk.eyJ1Ijoid2FuZ3JvbmluIiwiYSI6ImNqcmIzemhvMDBudnYzeWxoejh5c2Y5cXkifQ.9XGMWTDOsgi3-b5qG594kQ')






