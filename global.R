# some global variables shared between the server and ui
# 
# Author: Hao Wang
# Email: wangronin@gmail.com
library(magrittr)

options(datatable.print.nrows = 20)
options(width = 120)

IOHprofiler <- 'IOHprofiler'
COCO <- 'COCO'
probs <- c(2, 5, 10, 25, 50, 75, 90, 95, 98) / 100.

.mean <- function(x) mean(x, na.rm = T)
.median <- function(x) median(x, na.rm = T)
.sd <- function(x) sd(x, na.rm = T)
.sum <- function(x) sum(x, na.rm = T)

D_quantile <- function(x, pct = probs) quantile(x, pct, names = F, type = 3, na.rm = T)
C_quantile <- function(x, pct = probs) quantile(x, pct, names = F, na.rm = T)

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

# TODO: add Roxygen docs...
# TODO: maybe merge 'seq_FV' and 'seq_RT'...
# TODO: determine when the sequence should be generate in log-linear way
seq_FV <- function(FV, from = NULL, to = NULL, by = NULL, length.out = NULL) {
  from <- max(from, min(FV))
  to <- min(to, max(FV))
  
  if (is.null(by)) {
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
      unique 
  # }, error = function(e) {
    # c()
  # })
}

# TODO: Roxygen doc...
seq_RT <- function(RT, from = NULL, to = NULL, by = NULL, length.out = NULL, 
                   scale = 'linear') {
  rev_trans <- function(x) x
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
  
  from <- max(from, min(RT))
  to <- min(to, max(RT))
  
  if (is.null(by)) {
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






