ERT_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Expected Runtime (ERT): single function</p>'),
      width = width, collapsible = collapsible, solidHeader = T,
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,

          selectInput('ERTPlot.Algs', label = 'Select which IDs to include:',
                      multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                        custom_icon() %>%
                          bs_embed_popover(
                            title = "ID selection", content = alg_select_info, 
                            placement = "auto"
                          )
                      ),
          
          HTML('<p style="font-size:120%;">Range of the displayed target values</p>'),
          
          textInput('ERTPlot.Min',
                    label = F_MIN_LABEL,
                    value = '') %>% 
            shinyInput_label_embed(
              custom_icon() %>%
                bs_embed_popover(
                  title = "Interpolation", content = "The points on this plot are all interpollated where needed
                    to create solid lines. This means that for functions with limited domains (such as integers),
                    artifacts like step-wise behaviour can occur. Please carefully consider the domain of the function
                    when interpreting this plot.", 
                  placement = "auto"
                )
            ),
          
          textInput('ERTPlot.Max',
                    label = F_MAX_LABEL,
                    value = ''),

          checkboxInput('ERTPlot.show.ERT',
                        label = 'Show/hide ERT',
                        value = T),

          checkboxInput(
            'ERTPlot.show.Par',
            label = 'Show/hide PAR-X',
            value = F
          ) %>% 
            shinyInput_label_embed(
              custom_icon() %>%
                bs_embed_popover(
                  title = "Penalized Average Runtime", content = "PAR-X score is ther average of running time values, 
                    where non-successful runs are counted as X times the evaluation budget B.", 
                  placement = "auto"
                )
            ),
          conditionalPanel(condition = 'input["ERTPlot.show.Par"]', 
                           numericInput('ERTPlot.ParX', label = "Choose the used penaltiy factor:",
                                        value = 1, min = 1, max = 500, step = 1)
          ),

          checkboxInput('ERTPlot.show.CI',
                        label = 'Show/hide mean +/- sd',
                        value = F),
          
          checkboxInput('ERTPlot.show.Quantiles',
                        label = 'Show/hide outer quantiles',
                        value = F) %>% 
            shinyInput_label_embed(
              custom_icon() %>%
                bs_embed_popover(
                  title = "Outer quantiles", content = "This method uses the highest and lowest quantiles, which are 
                  2% and 98% by default. This can be changed in the settings-tab.", 
                  placement = "auto"
                )
            ),

          checkboxInput('ERTPlot.show.median',
                        label = 'Show/hide median',
                        value = F),
          
          checkboxInput('ERTPlot.show.fixed_prob',
                        label = "Show/hide fixed-probability line",
                        value = F),
          conditionalPanel(condition = 'input["ERTPlot.show.fixed_prob"]', 
                           numericInput('ERTPlot.Fixed_Prob', label = "Fixed Probability",
                                        value = 0.9, min = 0, max = 1, step = 0.01)
          ),
          
          checkboxInput('ERTPlot.semilogx',
                        label = 'Scale x axis \\(\\log_{10}\\)',
                        value = T),

          checkboxInput('ERTPlot.semilogy',
                        label = 'Scale y axis \\(\\log_{10}\\)',
                        value = T),
          box(title = HTML('<p style="font-size:120%;color:black;">Additional Options</p>'), collapsible = T, collapsed = T, width = width, solidHeader = T, status = 'info',
            checkboxInput('ERTPlot.inclueOpts',
                          label = "Include optimal points found by each algorithm",
                          value = F),
            checkboxInput('ERTPlot.show.runs',
                          label = 'Show individual runs',
                          value = F) %>% 
              shinyInput_label_embed(
                            custom_icon("exclamation-triangle") %>%
                              bs_embed_popover(
                                title = "Individual runs", content = "This procedure can be slow when many
                                runs are present in the data. Please use with caution.", 
                                placement = "auto"
                              )
                          ), 
            numericInput("ERTPlot.Additional.Budget", "Choose Custom Budget for ERT calculation", value = NA) %>% 
              shinyInput_label_embed(
                custom_icon("info") %>%
                  bs_embed_popover(
                    title = "Custom Budget", content = "This will use this budget value instead of the ones in the 
                    data for the ERT and PAR-X calculation. Any hitting times larger than this value are treated as unfinished runs.
                    Please use with caution.", 
                    placement = "auto"
                  )
              ) 
          ),
          hr(),
          
          selectInput('ERTPlot.Format', label = 'Select the figure format',
                      choices = supported_fig_format, selected = supported_fig_format[[1]]),

          downloadButton('ERTPlot.Download', label = 'Download the figure')

        ),

        mainPanel(
          width = 9,
          column(
            width = 12,
            align = "center",
            HTML_P('The <b><i>mean, median, standard deviation and ERT</i></b> of the runtime samples
                 are depicted against the best objective values.
                 The displayed elements (mean, median, standard deviations and ERT)
                 can be switched on and off by clicking on the legend on the right.
                 A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.'),
            plotlyOutput.IOHanalyzer('ERT_PER_FUN')
          )
        )
      )
  )
}
