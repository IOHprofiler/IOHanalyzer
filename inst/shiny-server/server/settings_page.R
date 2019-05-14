observe({
  if (input$Settings.Color.Scheme != "Custom"){
    set_colorScheme(input$Settings.Color.Scheme, NULL)
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
    writeLines(get_colorScheme(n=n), file)
  }
)

output$Settings.Color.Plot <- renderPlotly({
  plot_color_example(DATA())
})

plot_color_example <- function(ds){
  if (length(ds) > 0){
    algnames <- get_algId(ds)
  }
  else algnames <- c("Alg 1", "Alg 2", "Alg 3", "Alg 4", "Alg 5")
  colors <- get_colorScheme(n=length(algnames))
  schemename <- input$Settings.Color.Scheme
  if(schemename == "Custom" && !is.null(input$Settings.Color.Upload)){
    schemename <- paste0(schemename, ": ", input$Settings.Color.Upload$datapath)
  }
  p <- plot_ly() %>% layout(title = list(text = schemename), plot_bgcolor = input$Settings.Color.Bg,
                            xaxis = list(gridcolor = input$Settings.Color.Grid),
                            yaxis = list(gridcolor = input$Settings.Color.Grid))

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
        set_colorScheme("Custom", path=datapath)
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