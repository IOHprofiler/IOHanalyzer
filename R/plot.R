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
f3 <- function() {
  list(
    family = 'Old Standard TT, serif',
    size = getOption("IOHanalyzer.tick_fontsize", default = 12),
    color = 'black'
  )
}

legend_right <- function() {
  list(x = 1.01, y = 1, orientation = 'v',
       font = list(size = getOption("IOHanalyzer.legend_fontsize", default = 18),
                   family = 'Old Standard TT, serif'))
}

legend_inside <- function() {
  list(x = .01, y = 1, orientation = 'v',
       bgcolor = 'rgba(255, 255, 255, 0)',
       bordercolor = 'rgba(255, 255, 255, 0)',
       font = list(size = getOption("IOHanalyzer.legend_fontsize", default = 18),
                   family = 'Old Standard TT, serif'))
}

legend_inside2 <- function() {
  list(x = 0.7, y = 0.1, orientation = 'v',
       bgcolor = 'rgba(255, 255, 255, 0.5)',
       bordercolor = 'rgba(255, 255, 255, 0.8)',
       font = list(size = getOption("IOHanalyzer.legend_fontsize", default = 18),
                  family = 'Old Standard TT, serif'))
}

#TODO: Make the y-value configurable
legend_below <- function() {
  list(y = -0.2, orientation = 'h',
       font = list(size = getOption("IOHanalyzer.legend_fontsize", default = 18),
                   family = 'Old Standard TT, serif'))
}

legend_custom <- function() {
  list(x = getOption("IOHanalyzer.custom_legend_x", default = 0.5),
       y = getOption("IOHanalyzer.custom_legend_y", default = -0.2), orientation = 'h',
       font = list(size = getOption("IOHanalyzer.legend_fontsize", default = 18),
                   family = 'Old Standard TT, serif'))
}


legend_location <- function(){
  opt <- getOption('IOHanalyzer.legend_location', default = 'below')
  if (opt == 'outside_right') return(legend_right())
  else if (opt == 'inside_left') return(legend_inside())
  else if (opt == 'inside_right') return(legend_inside2())
  else if (opt == 'below') return(legend_below())
  else if (opt == 'custom') return(legend_custom())
  # else if (opt == 'below2') return(legend_below2())
  else warning(paste0("The selected legend option (", opt, ") is not implemented"))
}

# TODO: create font object as above for title, axis...

#' Template for creating plots in the IOHanalyzer-style
#'
#' @param title Title for the plot
#' @param x.title X-axis label
#' @param y.title Y-axis label
#'
#' @export
#' @examples
#' IOH_plot_ly_default("Example plot","x-axis","y-axis")
IOH_plot_ly_default <- function(title = NULL, x.title = NULL, y.title = NULL) {
  plot_ly() %>%
    layout(title = list(text = title,
                        font = list(size = getOption("IOHanalyzer.title_fontsize", default = 16),
                                    family = 'Old Standard TT, serif')),
           autosize = T, hovermode = 'compare',
           legend = legend_location(),
           paper_bgcolor = 'rgb(255,255,255)',
           plot_bgcolor = getOption('IOHanalyzer.bgcolor'),
           font = list(size = getOption("IOHanalyzer.label_fontsize", default = 16),
                       family = 'Old Standard TT, serif'),
           autosize = T,
           showlegend = T,
           xaxis = list(
                        # title = list(text = x.title, font = f3),
                        title = x.title,
                        gridcolor = getOption('IOHanalyzer.gridcolor'),
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = getOption('IOHanalyzer.tickcolor'),
                        ticks = 'outside',
                        ticklen = 9,
                        tickfont = f3(),
                        exponentformat = 'e',
                        zeroline = F),
           yaxis = list(
                        # title = list(text = y.title, font = f3),
                        title = y.title,
                        gridcolor = getOption('IOHanalyzer.gridcolor'),
                        showgrid = TRUE,
                        showline = FALSE,
                        showticklabels = TRUE,
                        tickcolor = getOption('IOHanalyzer.tickcolor'),
                        ticks = 'outside',
                        ticklen = 9,
                        tickfont = f3(),
                        exponentformat = 'e',
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

IOHanalyzer_env$used_colorscheme <- Set2
IOHanalyzer_env$id_colors <- NULL

#' Set the colorScheme of the IOHanalyzer plots
#'
#' @param schemename Three default colorschemes are implemented:
#' \itemize{
#' \item Default
#' \item Variant 1
#' \item Variant 2
#' \item Variant 3
#' }
#' And it is also possible to select "Custom", which allows uploading of a custom set of colors
#' @param ids The names of the algorithms (or custom ids, see `change_id`) for which to set the colors
#' @param path The path to the file containing the colors to use. Only used if
#' schemename is "Custom"
#'
#' @export
#'
#' @examples
#' set_color_scheme("Default", get_algId(dsl))
set_color_scheme <- function(schemename, ids, path = NULL){
  if (schemename == "Custom" && !is.null(path)) {
    dt <- fread(path, header = T)
    if (any(colnames(dt) != c("ids", "colors", "linestyles"))) {
      warning("Incorrect file-format has been uploaded.")
    }
    else
      IOHanalyzer_env$id_colors <- dt
    return()
  }
  else {
    if (schemename == "Default") {
      options(IOHanalyzer.max_colors = 2)
    }
    else {
      if (schemename == "Variant 1") IOHanalyzer_env$used_colorscheme <- Set1
      else if (schemename == "Variant 2") IOHanalyzer_env$used_colorscheme <- Set2
      else if (schemename == "Variant 3") IOHanalyzer_env$used_colorscheme <- Set3
      options(IOHanalyzer.max_colors = length(ids))
    }
    create_color_scheme(ids)
  }
}

#' Get datatable of current color (and linestyle) scheme to file
#'
#' @return data.table object with 3 columns: ids, colors, linestyles
#' @export
#' @examples
#' get_color_scheme_dt()
get_color_scheme_dt <- function(){
  return(IOHanalyzer_env$id_colors)
}

#' Helper function to create default color scheme
#'
#' @noRd
create_color_scheme <- function(ids) {
  if (length(ids) == 0) {
    return(NULL)
  }
  colors <- color_palettes(length(ids))
  linestyles <- rep(c("solid", "dash", "dot"), ceiling(length(colors)/3))[1:length(colors)]
  IOHanalyzer_env$id_colors <- data.table(ids, colors, linestyles)
}

#' Get colors according to the current colorScheme of the IOHanalyzer
#'
#' @param ids_in List of algorithms (or custom ids, see `change_id`) for which to get colors
#'
#' @export
#'
#' @examples
#' get_color_scheme(get_algId(dsl))
get_color_scheme <- function(ids_in){
  if (is.null(IOHanalyzer_env$id_colors))
    create_color_scheme(ids_in)
  cdt <- IOHanalyzer_env$id_colors
  colors <- subset(cdt, ids %in% ids_in)[['colors']]
  if (is.null(colors) || length(colors) != length(ids_in)) {
    return(color_palettes(length(ids_in)))
  }
  names(colors) <- ids_in
  return(colors)
}


#' Get line styles according to the current styleScheme of the IOHanalyzer
#'
#' @param ids_in List of algorithms (or custom ids, see `change_id`) for which to get linestyles
#'
#' @export
#'
#' @examples
#' get_line_style(get_algId(dsl))
get_line_style <- function(ids_in){
  if (is.null(IOHanalyzer_env$id_colors))
    create_color_scheme(ids_in)
  cdt <- IOHanalyzer_env$id_colors
  linestyles <- subset(cdt, ids %in% ids_in)[['linestyles']]
  if (is.null(linestyles) || length(linestyles) != length(ids_in)) {
    return(rep(c("solid", "dot", "dash", "longdash", "dashdot", "longdashdot"),
               ceiling(length(ids_in)/3))[1:length(ids_in)])
  }
  return(linestyles)
}

# TODO: incoporate more colors
color_palettes <- function(ncolor) {
  # TODO: FIX IT!
  max_colors <- getOption("IOHanalyzer.max_colors", 2)
  if (ncolor <= max_colors) return(IOHanalyzer_env$used_colorscheme(ncolor))

  brewer <- function(n) {
    colors <- RColorBrewer::brewer.pal(n, 'Spectral')
    colors[colors == "#FFFFBF"] <- "#B2B285"
    colors[colors == "#E6F598"] <- "#86FF33"
    colors[colors == '#FEE08B'] <- "#FFFF33"
    colors
  }

  color_fcts <- c(colorRamps::primary.colors, IOHanalyzer_env$used_colorscheme)

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

#' Save plotly figure in multiple format
#'
#' NOTE: This function requires orca to be installed
#'
#' @param p plotly object. The plot to be saved
#' @param file String. The name of the figure file, with the extension of the required file-format
#' @param width Optional. Width of the figure
#' @param height Optional. Height of the figure
#' @param ... Additional arguments for orca
#' @export
#' @examples
#' \dontrun{
#' p <- Plot.RT.Single_Func(dsl[1])
#' save_plotly(p, 'example_file.png')
#' }
save_plotly <- function(p, file, width = NULL, height = NULL, ...) {

  if (!requireNamespace("withr", quietly = TRUE)) {
    stop("Package \"withr\" needed for this function to work. Please install it.",
      call. = FALSE)
  }



  des <- dirname(file)
  file <- basename(file)

  pwd <- tempdir()
  if (is.null(width)) width <- getOption("IOHanalyzer.figure_width", default = NULL)
  if (is.null(height)) height <- getOption("IOHanalyzer.figure_height", default = NULL)

  use_kaleido <- T

  if (!requireNamespace("reticulate", quietly = TRUE)) {
    use_kaleido <- F
  }

  if (use_kaleido) {
    use_kaleido <- reticulate::py_module_available('kaleido')
  }

  if (use_kaleido) {
    reticulate::py_run_string("import sys")
    withr::with_dir(pwd, save_image(p, file, width = width, height = height, ...))
  } else {
    tryCatch({
      more_args <- NULL
      format <- tools::file_ext(file)

      if (!getOption("IOHanalyzer.orca_use_gpu", TRUE)) {
        more_args <- c('--disable-gpu')
      }

      if (format %in% c('svg', 'png', 'jpeg', 'webp', 'pdf', 'eps'))
        withr::with_dir(pwd, orca(p, file, format = format, width = width, height = height, more_args = more_args, ...))
      else {
        file_svg <- paste0(file, '.svg')
        withr::with_dir(pwd, orca(p, file_svg, format = 'svg', width = width, height = height, more_args = more_args, ...))
        invisible(
          system(
            paste(
              'inkscape', file.path(pwd,file_svg),
              paste0('--export-', format, ' ', file.path(pwd, file))
            ),
            intern = T
          )
        )
      }
    }, error = function(e) {stop("Image saving failed. Please ensure that either kaleido or orca is available.")})
  }

  file.rename(file.path(pwd, file), file.path(des, file))
}
