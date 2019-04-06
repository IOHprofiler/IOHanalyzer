# font No. 1...
f1 <- list(
  family = 'Old Standard TT, serif',
  size = 11,
  color = 'black'
)

# font No. 2...
f2 <- list(
  family = 'Old Standard TT, serif',
  size = 13,
  color = 'black'
)

# font No. 3...
f3 <- list(
  family = 'Old Standard TT, serif',
  size = 16,
  color = 'black'
)

plot_ly_default <- function(title = NULL, x.title = NULL, y.title = NULL) {
  plot_ly() %>%
    layout(title = title,
           autosize = T, hovermode = 'compare',
           legend = list(x = 1.01, y = 0.9, orientation = 'v',
                         font = list(size = 13, family = 'Old Standard TT, serif')),
           paper_bgcolor = 'rgb(255,255,255)', plot_bgcolor = 'rgb(229,229,229)',
           font = list(size = 13, family = 'Old Standard TT, serif'),
           titlefont = list(size = 13, family = 'Old Standard TT, serif'),
           autosize = T,
           showlegend = T, 
           xaxis = list(
                        # title = list(text = x.title, font = f3),
                        title = x.title,
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        ticklen = 9,
                        tickfont = f2,
                        exponentformat = 'E',
                        zeroline = F),
           yaxis = list(
                        # title = list(text = y.title, font = f3),
                        title = y.title,
                        gridcolor = 'rgb(255,255,255)',
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = 'rgb(127,127,127)',
                        ticks = 'outside',
                        ticklen = 9,
                        tickfont = f2,
                        exponentformat = 'E',
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

Set1 <- function(n) colorspace::sequential_hcl(n, h = c(360, 40), c. = c(100, NA, 90), l = c(28, 90),
                                   power = c(1, 1.1), gamma = NULL, fixup = TRUE,
                                   alpha = 1)#, palette = NULL, rev = FALSE)

Set2 <- function(n) colorspace::sequential_hcl(n, c(261, 26), c. = c(50, NA, 70), l = c(54, 77),
                                   power = c(0.5, NA), gamma = NULL,
                                   fixup = TRUE, alpha = 1)#, palette = NULL, rev = FALSE)

Set3 <- function(n) colorspace::sequential_hcl(n, c(-88, 59), c. = c(60, 75, 55), l = c(40, 90),
                                   power = c(0.1, 1.2), gamma = NULL,
                                   fixup = TRUE, alpha = 1)#, palette = NULL, rev = FALSE)

# TODO: incoporate more colors
color_palettes <- function(ncolor) {
  if (ncolor < 5) return(Set3(ncolor)) #Was set2, which gave NAFF as color?

  brewer <- function(n) {
    colors <- RColorBrewer::brewer.pal(n, 'Spectral')
    colors[colors == "#FFFFBF"] <- "#B2B285"
    colors
  }

  color_fcts <- c(colorRamps::primary.colors, Set3)

  n <- min(11, ncolor)
  colors <- brewer(n)
  ncolor <- ncolor - n

  i <- 1
  while (ncolor > 0) {
    n <- min(8, ncolor)
    if (i > length(color_fcts)) {
      colors <- c(colors, colorRamps::primary.colors(ncolor))
      break
    } else {
      colors <- c(colors, color_fcts[[i]](n))
      ncolor <- ncolor - n
    }
    i <- i + 1
  }
  colors
}

# TODO: we have to change the working directory back and force because
# function 'orca' always generates figures in the current folder
save_plotly <- function(p, file, format = 'svg', ...) {
  pwd.calling <- getwd()
  des <- dirname(file)
  file <- basename(file)

  pwd <- file.path(Sys.getenv('HOME'))
  dir.create(pwd, showWarnings = FALSE)
  setwd(pwd)

  if (format %in% c('svg', 'png'))
    orca(p, file, format = format, ...)
  else {
    file_svg <- paste0(file, '.svg')
    orca(p, file_svg, format = 'svg', ...)
    invisible(system(paste('inkscape', file_svg, paste0('--export-', format, '=', file)),
                     intern = T))
    file.remove(file_svg)
  }

  file.rename(file, file.path(des, file))
  setwd(pwd.calling)
}
