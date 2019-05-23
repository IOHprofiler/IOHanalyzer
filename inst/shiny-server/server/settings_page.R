observe({
  if (input$Settings.Color.Scheme != "Custom"){
    set_color_scheme(input$Settings.Color.Scheme, NULL)
  }
})

#TODO: change number of colors to match number of algorithms when applicable + Add example of colors
output$Settings.Color.Example <- downloadHandler(
  filename = function() {
    "Example_Colorfile"
  },
  content = function(file) {
    n <- length(DATA())
    if (n == 0) n <- 5
    writeLines(get_color_scheme(n=n), file)
  }
)

output$Settings.Color.Plot <- renderPlotly({
  plot_color_example(DATA())
})

plot_color_example <- function(ds){
  curr_settings <- c(input$Settings.Color.Bg,
                     input$Settings.Color.Grid,
                     input$Settings.Color.Tick,
                     input$Settings.Legend.Location,
                     input$Settings.Font.Title,
                     input$Settings.Font.Legend,
                     input$Settings.Font.Label,
                     input$Settings.Font.Tick)
  if (any(is.null(curr_settings))) return (NULL)
  if (length(ds) > 0){
    algnames <- get_algId(ds)
  }
  else algnames <- c("Alg 1", "Alg 2", "Alg 3", "Alg 4", "Alg 5")
  colors <- get_color_scheme(n=length(algnames))
  schemename <- input$Settings.Color.Scheme
  if(schemename == "Custom" && !is.null(input$Settings.Color.Upload)){
    schemename <- paste0(schemename, ": ", input$Settings.Color.Upload$datapath)
  }
  p <- IOH_plot_ly_default(schemename, "X-axis label", "Y-axis label")

  for (i in seq_along(algnames)){
    rgb_str <- paste0('rgb(', paste0(col2rgb(colors[i]), collapse = ','), ')')
    
    p %<>% add_segments(y = i, yend = i, x = 0, xend = 10, name = sprintf('%s', algnames[[i]]),
                        line = list(color = rgb_str, width = 6))    
  }
  p
}

selected_color_congfig <- observe({
  if(!is.null(input$Settings.Color.Upload)){
    datapath <- input$Settings.Color.Upload$datapath
    tryCatch(
      expr = {
        set_color_scheme("Custom", path=datapath)
      },
      error = function(e) {
        shinyjs::alert("File could not be read, please upload a file in the same format as the example.")
      }
    )
  }
})

observe({
 input$Settings.General.Probs %>% 
    strsplit(.,',') %>% 
    .[[1]] %>% 
    as.numeric %>%
    options("IOHanalyzer.quantiles" = .)
})

observe({
  options("IOHanalyzer.max_samples" = input$Settings.General.Max_samples)
})

observe({
  options("IOHanalyzer.backend" = input$Settings.General.Backend)
})

observe({
  options("IOHanalyzer.bgcolor" = input$Settings.Color.Bg)
})

observe({
  options("IOHanalyzer.gridcolor" = input$Settings.Color.Grid)
})

observe({
  options("IOHanalyzer.tickcolor" = input$Settings.Color.Tick)
})

observe({
  options("IOHanalyzer.figure_width" = input$Settings.Download.Width)
})

observe({
  options("IOHanalyzer.figure_height" = input$Settings.Download.Height)
})

observe({
  legend_loc <- input$Settings.Legend.Location
  if (legend_loc == "Outside, right") legend_loc_str <- "outside_right"
  else if (legend_loc == "Inside, right") legend_loc_str <- "inside_right"
  else if (legend_loc == "Inside, left") legend_loc_str <- "inside_left"
  else if (legend_loc == "Below") legend_loc_str <- "below"
  options("IOHanalyzer.legend_location" = legend_loc_str)
})

observe({
  options("IOHanalyzer.tick_fontsize" = input$Settings.Font.Tick)
})

observe({
  options("IOHanalyzer.legend_fontsize" = input$Settings.Font.Legend)
})

observe({
  options("IOHanalyzer.title_fontsize" = input$Settings.Font.Title)
})

observe({
  options("IOHanalyzer.label_fontsize" = input$Settings.Font.Label)
})

output$Settings.Download <- downloadHandler(
  filename = "IOHanalyzer_settings.rds",
  content = function(file){
    curr_opts <- options()
    IOH_opts <- curr_opts[grep(names(curr_opts), pattern = "IOH")]
    saveRDS(IOH_opts, file)
  },
  contentType = "rds"
)

observe({
  if (!is.null(input$Settings.Upload)){
    file <- input$Settings.Upload$datapath
    IOH_opts <- readRDS(file)
    options(IOH_opts[grep(names(IOH_opts), pattern = "IOH")]) #Ensure no other options get changed by the user
  }
})