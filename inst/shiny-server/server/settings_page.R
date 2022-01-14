observe({
  req(length(DATA()) > 0)
  if (input$Settings.Color.Scheme != "Custom") {
    set_color_scheme(input$Settings.Color.Scheme, get_id(DATA()), NULL)
  }
})

output$Settings.Color.Example <- downloadHandler(
  filename = function() {
    "Example_Colorfile"
  },
  content = function(file) {
    write.csv2(get_color_scheme_dt(), file, row.names = F)
  }
)

output$Settings.Color.Plot <- renderPlotly({
  plot_color_example()
})

plot_color_example <- function(){
  curr_settings <- c(input$Settings.Color.Bg,
                     input$Settings.Color.Grid,
                     input$Settings.Color.Tick,
                     input$Settings.Legend.Location,
                     input$Settings.Font.Title,
                     input$Settings.Font.Legend,
                     input$Settings.Font.Label,
                     input$Settings.Font.Tick,
                     input$Settings.Color.Linewidth,
                     input$Settings.Color.Markersize,
                     input$IOHanalyzer.custom_legend_x,
                     input$IOHanalyzer.custom_legend_y
                     )
  if (any(is.null(curr_settings))) return(NULL)
  if (length(DATA_RAW()) > 0) {
    algnames <- get_id(DATA_RAW())
  }
  else algnames <- c("Alg 1", "Alg 2", "Alg 3", "Alg 4", "Alg 5")
  colors <- get_color_scheme(algnames)
  schemename <- input$Settings.Color.Scheme
  if (schemename == "Custom" && !is.null(input$Settings.Color.Upload)) {
    schemename <- paste0(schemename, ": ", input$Settings.Color.Upload$datapath)
  }
  
  x <- c(rep(1, length(algnames)),rep(2, length(algnames)))
  y <- seq_len(length(algnames))
  dt <- data.table(ID = rep(algnames, 2), x, y)
  
  p <- plot_general_data(dt, 'x', 'y', 'line', show.legend = T,
                         x_title = 'X-title', y_title = 'Y-title', plot_title = 'Plot Title')
  
  p
}

selected_color_congfig <- observe({
  if (!is.null(input$Settings.Color.Upload)) {
    datapath <- input$Settings.Color.Upload$datapath
    tryCatch(
      expr = {
        set_color_scheme("Custom", path = datapath)
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
  options("IOHanalyzer.linewidth" = input$Settings.Color.Linewidth)
})

observe({
  options("IOHanalyzer.markersize" = input$Settings.Color.Markersize)
})

observe({
  options("IOHanalyzer.figure_width" = input$Settings.Download.Width)
})

observe({
  options("IOHanalyzer.figure_height" = input$Settings.Download.Height)
})

observe({
  options("IOHanalyzer.custom_legend_x" = input$Settings.Legend.LocationX)
})

observe({
  options("IOHanalyzer.custom_legend_y" = input$Settings.Legend.LocationY)
})

observe({
  legend_loc <- input$Settings.Legend.Location
  if (legend_loc == "Outside, right") legend_loc_str <- "outside_right"
  else if (legend_loc == "Inside, right") legend_loc_str <- "inside_right"
  else if (legend_loc == "Inside, left") legend_loc_str <- "inside_left"
  else if (legend_loc == "Below") legend_loc_str <- "below"
  else if (legend_loc == "Custom") legend_loc_str <- "custom"
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

observe({
  options("IOHanalyzer.precision" = input$Settings.General.Precision)
})

observe({
  options("IOHanalyzer.margin_horizontal" = input$Settings.Subplot.Margin_horizontal)
})

observe({
  options("IOHanalyzer.margin_vertical" = input$Settings.Subplot.Margin_vertical)
})

observe({
  options("Settings.Subplot.LocationX" = input$IOHanalyzer.annotation_x)
})

observe({
  options("Settings.Subplot.LocationY" = input$IOHanalyzer.annotation_y)
})

observe({
  if (input$Settings.Subplot.Include_annotations) {
    options("Settings.Subplot.LocationX" = input$IOHanalyzer.annotation_x)
    options("Settings.Subplot.LocationY" = input$IOHanalyzer.annotation_y)
  }
  else {
    options("Settings.Subplot.LocationX" = -1)
    options("Settings.Subplot.LocationY" = -1)
  }
})

observe({
  req(input$Settings.ID.Variables)
  withProgress({
  id_vars <- input$Settings.ID.Variables
  if (!setequal(id_vars,getOption('IOHanalyzer.ID_vars', c('algId')))) {
    options("IOHanalyzer.ID_vars" = input$Settings.ID.Variables)
    DataList$data <- change_id(DataList$data, input$Settings.ID.Variables)
  }
  if ('algId' %in% id_vars) 
    shinyjs::hide(id = "overall_algid_box")
  else
    shinyjs::show(id = "overall_algid_box")
}, message = "Processing IDs")
})

observe({
  if (input$Settings.Use_Funcname) {
    shinyjs::show(id = "overall_funcname_box")
    shinyjs::hide(id = "overall_funcid_box")
    options('IOHanalyzer.function_representation' = 'funcname')
  }
  else {
    shinyjs::hide(id = "overall_funcname_box")
    shinyjs::show(id = "overall_funcid_box")
    options('IOHanalyzer.function_representation' = 'funcId')
  }
})

observe({
  setting_preset <- input$Settings.Download.Preset
  if (setting_preset == "Default") {
    updateNumericInput(session, 'Settings.Download.Width', value = 1000)
    updateNumericInput(session, 'Settings.Download.Height', value = 1000)
    updateNumericInput(session, 'Settings.Font.Tick', value = 12)
    updateNumericInput(session, 'Settings.Font.Legend', value = 13)
    updateNumericInput(session, 'Settings.Font.Title', value = 16)
    updateNumericInput(session, 'Settings.Font.Label', value = 16)
  }
  else if (setting_preset == "Paper-1col") {
    updateNumericInput(session, 'Settings.Download.Width', value = 700)
    updateNumericInput(session, 'Settings.Download.Height', value = 400)
    updateNumericInput(session, 'Settings.Font.Tick', value = 9)
    updateNumericInput(session, 'Settings.Font.Legend', value = 10)
    updateNumericInput(session, 'Settings.Font.Title', value = 13)
    updateNumericInput(session, 'Settings.Font.Label', value = 13)    
  }
  else if (setting_preset == "Paper-2col") {
    updateNumericInput(session, 'Settings.Download.Width', value = 900)
    updateNumericInput(session, 'Settings.Download.Height', value = 600)
    updateNumericInput(session, 'Settings.Font.Tick', value = 11)
    updateNumericInput(session, 'Settings.Font.Legend', value = 12)
    updateNumericInput(session, 'Settings.Font.Title', value = 16)
    updateNumericInput(session, 'Settings.Font.Label', value = 15)    
  }
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
  if (!is.null(input$Settings.Upload)) {
    file <- input$Settings.Upload$datapath
    IOH_opts <- readRDS(file)
    options(IOH_opts[grep(names(IOH_opts), pattern = "IOH")]) #Ensure no other options get changed by the user
  }
})

output$Settings.Plot.Download <- downloadHandler(
  filename = "Sample_plot.pdf",
  content = function(file) {
    save_plotly(plot_color_example(), file)
  },
  contentType = 'image/pdf'
)