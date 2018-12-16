# some global variables shared between the server and ui
# 
# Author: Hao Wang
# Email: wangronin@gmail.com

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

RT_csv_name <- parse(text = "paste0('RT-', paste(Sys.Date(), input$DIM_INPUT, paste0('F', input$FUNCID_INPUT), fstart, fstop, fstep, sep = '-'), '.csv')")
RTSample_csv_name <- parse(text = "paste0('RTSample-', paste(Sys.Date(), input$DIM_INPUT, paste0('F', input$FUNCID_INPUT), fstart, fstop, fstep, sep = '-'), '.csv')")
FV_csv_name <- parse(text = "paste0('FV-', paste(Sys.Date(), input$DIM_INPUT, paste0('F', input$FUNCID_INPUT), rt_min, rt_max, rt_step, sep = '-'), '.csv')")
FVSample_csv_name <- parse(text = "paste0('FVSample-', paste(Sys.Date(), input$DIM_INPUT, paste0('F', input$FUNCID_INPUT), rt_min, rt_max, rt_step, sep = '-'), '.csv')")
PAR_csv_name <- parse(text = "paste0('PAR-', paste(Sys.Date(), input$DIM_INPUT, paste0('F', input$FUNCID_INPUT), fstart, fstop, fstep, sep = '-'), '.csv')")
PARSample_csv_name <- parse(text = "paste0('PARSample-', paste(Sys.Date(), input$DIM_INPUT, paste0('F', input$FUNCID_INPUT), fstart, fstop, fstep, sep = '-'), '.csv')")



