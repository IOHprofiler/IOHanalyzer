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
    writeLines(get_colorScheme(n), file)
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
  colors <- get_colorScheme(length(algnames))
  schemename <- input$Settings.Color.Scheme
  if(schemename == "Custom" && !is.null(input$Settings.Color.Upload)){
    schemename <- paste0(schemename, ": ", input$Settings.Color.Upload$datapath)
  }
  p <- plot_ly_default(title = schemename)

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
        set_colorScheme("Custom", datapath)
        colors <- fread(datapath, header = F)[[1]]
        N <- length(colors)
        custom_set <- function(n){
          return(colors[mod(seq(n),N)+1])
        }
      },
      error = function(e) {
        shinyjs::alert("File could not be read, please upload a file in the same format as the example.")
      }
    )
  }
})