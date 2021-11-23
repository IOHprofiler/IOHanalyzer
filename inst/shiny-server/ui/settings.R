color_settings_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Color Settings</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectInput(inputId = "Settings.Color.Scheme", label = "Color schemes", 
                      choices = c("Default", "Variant 1", "Variant 2", "Variant 3", "Custom")),
          conditionalPanel(condition = 'input["Settings.Color.Scheme"] == "Custom"',
                           downloadButton("Settings.Color.Example","Download an example color settings file"),
                           fileInput("Settings.Color.Upload","Upload a color settings file")
                           ),
          colourInput("Settings.Color.Bg", "Plot background colour", value = "#E6E6E6"),
          colourInput("Settings.Color.Grid", "Plot gridline colour", value = "#FFFFFF"),
          colourInput("Settings.Color.Tick", "Plot ticks colour", value = "#333333"),
          numericInput("Settings.Color.Linewidth", "Line Width", value = 2),
          numericInput("Settings.Color.Markersize", "Marker Size", value = 4),
          selectInput("Settings.Legend.Location", "Legend location", 
                      c("Outside, right", "Inside, right", "Inside, left", "Below", "Custom"), "Below"),
          conditionalPanel(condition = 'input["Settings.Legend.Location"] == "Custom"',
                           numericInput("Settings.Legend.LocationX", "X-position of Legend", value = 0.5, step = 0.05),
                           numericInput("Settings.Legend.LocationY", "Y-position of Legend", value = -0.2, step = 0.05)
          ),
          downloadButton("Settings.Plot.Download", label = "Download sample plot")
        ),

        mainPanel(
          width = 9,
          column(
            width = 12,
            align = 'center',
            HTML_P('Example of the current colorscheme.'),
            plotlyOutput.IOHanalyzer('Settings.Color.Plot')
          )
        )
      )
  )
}

general_settings_box <- function(width=12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">General settings</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      mainPanel(
        width = 12,
        column(
          width = 3,
          align = "Left",
          HTML_P('Set the general properties'),
          #TODO: get probabilities from get_probability and put them as default
          textInput("Settings.General.Probs", label = "Probability-quantiles", 
                    value = "0.02,0.05,0.10,0.25,0.50,0.75,0.90,0.95,0.98"),
          numericInput("Settings.General.Max_samples", 
                       label = "Maximum samples shown per algorithm", 
                       value = 100),
          selectInput("Settings.General.Backend", "Plotting backend", 
                      c('plotly', 'ggplot2'), 'plotly'),
          numericInput("Settings.General.Precision", label = "Function value precision (digits)",
                       value = 2),
          hr(),
          HTML_P("ID variables"),
          selectInput("Settings.ID.Variables", label = "Attributes used to create IDs",
                      multiple = T, selected = NULL, choices = NULL),
          checkboxInput("Settings.Use_Funcname", "Use function names instead of IDs"),
          hr(),
          downloadButton("Settings.Download", "Download current general settings file"),
          fileInput("Settings.Upload", "Upload a settings file", accept = "rds")
        ),
        column(
          width = 3,
          align = "Left",
          HTML_P('Set the figure download properties'),
          selectInput("Settings.Download.Preset", label = "Choose preset for download and font sizes", 
                      choices = c("Default", "Paper-1col", "Paper-2col"), selected = "Default"),
          hr(),
          numericInput("Settings.Download.Width", label = "Image width (px)", value = 1000, min = 100, max = 4096),
          numericInput("Settings.Download.Height", label = "Image height (px)", value = 1000, min = 100, max = 4096)
        ),
        column(
          width = 3,
          align = "Left",
          HTML_P('Set the figure fontsizes'),
          numericInput("Settings.Font.Title", label = "Title", value = 16, min = 8, max = 100),
          numericInput("Settings.Font.Label", label = "Axis labels", value = 16, min = 8, max = 100),
          numericInput("Settings.Font.Legend", label = "Legend", value = 13, min = 8, max = 100),
          numericInput("Settings.Font.Tick", label = "Ticks", value = 12, min = 8, max = 100),
          box(title = HTML('<p style="font-size:120%;color:black;">Subplot Options</p>'), collapsible = T, collapsed = T, solidHeader = T, status = 'info',
              checkboxInput('Settings.Subplot.Include_annotations',
                            label = "Include subplot name when using subplots",
                            value = T),
              conditionalPanel(condition = 'input["Settings.Subplot.Include_annotations"]',
                               numericInput("Settings.Subplot.LocationX", "X-position of subplot name", 
                                            value = 0.5, step = 0.05, min = 0, max = 1),
                               numericInput("Settings.Subplot.LocationY", "Y-position of subplot name", 
                                            value = 1, step = 0.05, min = 0, max = 1)
              ),
              numericInput("Settings.Subplot.Margin_horizontal", "Horizontal margin between subplots", 
                           value = 0.02, step = 0.005, min = 0, max = 0.1),
              numericInput("Settings.Subplot.Margin_horizontal", "Vertical margin between subplots", 
                           value = 0.02, step = 0.005, min = 0, max = 0.1)
          ),
        )
      )
         
  )
}