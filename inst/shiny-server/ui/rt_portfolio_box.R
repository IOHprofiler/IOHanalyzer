rt_shapleys_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Contribution to portfolio (Shapley-values)</p>'),
    width = width, collapsible = collapsible, collapsed = collapsed,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 3,
      selectInput('RTportfolio.Shapley.Algs', 'Algorithms to include:', multiple = T, selected = NULL, choices = NULL),
      selectInput("RTportfolio.Shapley.Funcs", "Functions to include:", multiple = T, selected = NULL, choices = NULL),
      selectInput("RTportfolio.Shapley.Dims", "Dimensions to include:", multiple = T, selected = NULL, choices = NULL),
      hr(),
      checkboxInput("RTportfolio.Shapley.Logx", "Scale runtimes to sample at \\(\\log_{10}\\)", value = T),
      numericInput("RTportfolio.Shapley.Groupsize", "Maximum permutation size:", value = 5, min = 1, max = 100) %>% 
        shinyInput_label_embed(
          custom_icon() %>%
            bs_embed_popover(
              title = "Groupsize", content = "This parameter controls how many groups of permutations are used to 
              calculate the Shapley-values. Larger values give more accurate estimates, but take longer to compute.", 
              placement = "auto"
            )
        ),
      numericInput("RTportfolio.Shapley.Permsize", "Maximum permutation size", value = 0) %>% 
        shinyInput_label_embed(
          custom_icon() %>%
            bs_embed_popover(
              title = "Scaling", content = "This parameter controls the maximum size of permutations to include 
              in the calculation of the marginal contribution. So, if this parameter is low, only small portfolios
              will be considered. Higher values lead to more accurate Shapley-values (relative to eachother) but take
              longer to compute.", 
              placement = "auto"
            )
        ),
      
      br(),
      actionButton(
        "RTportfolio.Shapley.Refresh", 
        label = HTML('<p align="left" style="font-size:100%;">Refresh the figure</p>')
      ),
      
      hr(),
      selectInput("RTportfolio.Shapley.Target_type", label = "Select the spacing for the
                  automatically generated ECDF-targets:", 
                  choices = c('linear', 'log-linear', 'bbob'), 
                  selected = 'linear') %>% 
        shinyInput_label_embed(
          custom_icon() %>%
            bs_embed_popover(
              title = "Default targets", content = "The log-linear spacing only works correctly
              when no negative target values are present in the data. The BBOB-spacing is pre-defined
              to 51 log-linear targets between 10^2 and 10^-8.", 
              placement = "auto"
            )
        ),
      numericInput("RTportfolio.Shapley.Target_number", label = "Select the number of ECDF-targets to 
                   generate for each function/dimension", value = 10, min = 1, max = 100),
      
      HTML_P('Alternatively, you can download the table containing the target values for each 
              (function, dimension)-pair and edit the table as you want. Please keep 
             the file format when modifying it.'),
      downloadButton('RTportfolio.Shapley.Table.Download', label = 'Download the table of targets'),
      br(),
      br(),
      br(),
      
      HTML_P('Upload the table you just downloaded and edited'),
      fileInput(
        "RTportfolio.Shapley.Table.Upload", 
        label = NULL,
        multiple = FALSE, 
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      ),
      
      hr(),
      selectInput('RTportfolio.Shapley.Format', label = 'figure format to download',
                  choices = supported_fig_format, selected = supported_fig_format[[1]]),
      
      downloadButton('RTportfolio.Shapley.Download', label = 'Download the figure')
    ),
    
    mainPanel(
      width = 9,
      column(
        width = 12, align = "center",
        hr(),
        
        HTML_P('DESCRIPTION OF SHAPLEY VALUES HERE'),
        plotlyOutput.IOHanalyzer('RT_SHAPLEY'),
        HTML_P('The selected targets are:'),
        DT::dataTableOutput('RT_SHAPLEY_TARGETS_GENERATED')
      )
    )
  )
}