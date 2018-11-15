# setup the default layout for 
# 1. plotly 2. ggplot
# Author: Hao Wang
# Email: wangronin@gmail.com


suppressMessages(library(plotly))
suppressMessages(library(ggplot2))

plot_ly_default <- function(title = NULL,
                            x.title = NULL,
                            y.title = NULL) {
  plot_ly() %>% 
    layout(title = title,
           autosize = T, hovermode = 'compare',
           legend = list(x = 0, y = -0.2, orientation = 'h'),
           paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)',
           font = list(size = 18, family = 'sans-serif'),
           titlefont = list(size = 16, family = 'sans-serif'),
           xaxis = list(title = x.title,
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        ticklen = 9,
                        zeroline = F),
           yaxis = list(title = y.title,
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        ticklen = 9,
                        zeroline = F)) 
}

t <- theme_grey() +
  theme(text = element_text(size = 15),
        plot.title = element_text(hjust = 0.5)
        # legend.position = c(0.15, 0.7),       # legend position
        # legend.key = element_blank(),         # no small box around symbol
        # legend.key.size = unit(1.3, "line"),  # bigger symbols
        # legend.background = element_rect(color = alpha("black", 0.5),
        #                                  fill = alpha('blue', 0.0),
        #                                  size = 1,
        #                                  linetype = "solid")
  )
theme_set(t)


gg_beanplot <- function(mapping, data, p = NULL, width = 3, fill = 'grey', 
                        colour = 'grey', alpha = 1, kernel = 'gaussian', bw = 'SJ', 
                        draw_quantiles = NULL, trim = TRUE, na.rm = FALSE, 
                        show.legend = NA, point.shape = 20, show.sample = T,
                        show.violin = T, linetype = 'solid') {
  
  set.seed(42)
  x <- as.character(mapping$x)
  y <- as.character(mapping$y)
  df <- data[, c(x, y)] %>% rename_(.dots = c('x' = x, 'y' = y))
  
  if (!is.numeric(df$x)) 
    df$x <- tryCatch(as.numeric(df$x), # in case x is a factor...
                     warning = function(w) return(match(x, as.factor(x)))) 
  
  if (is.null(p))
    p <- ggplot()
  
  if (show.violin)
    p <- p + geom_violin(data = data, mapping = mapping, trim = trim, 
                         draw_quantiles = draw_quantiles, bw = bw, 
                         kernel = kernel, scale = 'width',
                         width = width, alpha = alpha)
  if (show.sample)
    p <- p + geom_jitter(data = df, aes(x, y), height = 0, width = width / 2, 
                         alpha = 0.45, shape = point.shape, size = 3.5)
  # geom_segment(aes(x = x - width / 2.2, xend = x + width / 2.2, y = y, yend = y),
  #              df, col = 'black', size = 0.2, alpha = 0.3, linetype = linetype)
  p
}
