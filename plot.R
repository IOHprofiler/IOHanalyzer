library(plotly)

# setup the default layout for 
# 1. plotly 2. ggplot

plot_ly_default <- function(title = NULL,
                            x.title = NULL,
                            y.title = NULL) {
  plot_ly() %>% 
    layout(title = title,
           autosize = T, hovermode = 'compare',
           paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)',
           xaxis = list(title = x.title,
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        ticklen = 7,
                        zeroline = F),
           yaxis = list(title = y.title,
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        ticklen = 7,
                        zeroline = F)) 
}
