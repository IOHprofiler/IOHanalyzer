# FCEPDF.Hist.Runtime -> FCEPDF.CDP.Runtime
# FCE_HIST -> FCE_CDP

fv_CDP_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = 'Cumulative Difference Plot of Fixed-Budget Function Values',
      width = width, collapsible = collapsible,
      solidHeader = TRUE,  collapsed = collapsed,
      status = "primary",
      sidebarPanel(
        width = 3,
        HTML('Select the budget value.'),
        textInput('FCEPDF.CDP.Runtime', label = '',
                  value = ''),
        hr(),
        HTML('Select <strong>TWO</strong> IDs.'),
        selectInput('FCEPDF.CDP.Algs', label = "",
                    multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "ID selection", content = "The Cumulative Difference Plot can only compare two IDs at a time.",
                          placement = "auto"
                        )
                    ),
        hr(),
        HTML('Select maximization or minimization.'),
        selectInput('FCEPDF.CDP.Mode', label = '',
                    choices = c("max.", "min.","Choose one"), selected = "Choose one")%>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "max. or min.", content = "Choose maximization when higher is better and minimization when lower is better.",
                          placement = "auto"
                        )
                    ),
        hr(),
        HTML('Select confidence level.'),
        selectInput('FCEPDF.CDP.Confidence', label = '',
                    choices = c(0.9, 0.95, 0.99), selected = 0.95)%>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "Confidence level", content = "Choose the confidence level of the confidence band.",
                          placement = "auto"
                        )
                    ),
        hr(),
        downloadButton('FCEPDF.CDP.Download', label = 'Download the figure')

      ),

      mainPanel(
        width = 9,
        column(
          width = 12, align = "center",
          HTML('<p align="left" "font-size:120%;">
               The cumulative difference plot can be useful when box/violin-plots
               fail to provide a clear result. Generating this plot has two requirements:
               </p>

               <p align="left" "font-size:120%;">
               - Select exactly two algorithms (two IDs)<br>
               - Select max. (when higher is better) or min. (when lower is better)
               </p>

               <p align="left" "font-size:120%;">
               The cumulative difference plot compares the samples from two algorithms
               through the first order stochastic dominance. In the left side of the x-axis,
               the best values that the algorithms produced are compared. While in the right side,
               the worst values are compared.
               </p>

               <p align="left" "font-size:120%;">
               If the curve does not cross the x-axis, this means that the algorithm
               that is in the same side as the curve stochasticaly dominates the
               other one.
               If the curve is positive and negative, then one of the algorithms
               might be better at getting good values (if in the left part the
               plot, the curve is in the side of that algorithm), or better at
               avoiding bad values (if in the right part of the plot, the curve is
               in the side of that algorithm).
              </p>'),
          plotlyOutput.IOHanalyzer('FCE_CDP')
        )
      )
  )
}





rt_CDP_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = 'Cumulative Difference Plot of Fixed-Target Runtimes',
      width = width, collapsible = collapsible,
      solidHeader = TRUE,  collapsed = collapsed,
      status = "primary",
      sidebarPanel(
        width = 3,
        HTML('Select the target value.'),
        textInput('RTPMF.CDP.Target', label = '',
                  value = ''),
        hr(),
        HTML('Select <strong>TWO</strong> IDs.'),
        selectInput('RTPMF.CDP.Algs', label = "",
                    multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "ID selection", content = "The Cumulative Difference Plot can only compare two IDs at a time.",
                          placement = "auto"
                        )
                    ),
        hr(),
        HTML('Select confidence level.'),
        selectInput('RTPMF.CDP.Confidence', label = '',
                    choices = c(0.9, 0.95, 0.99), selected = 0.95)%>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "Confidence level", content = "Choose the confidence level of the confidence band.",
                          placement = "auto"
                        )
                    ),
        hr(),
        downloadButton('RTPMF.CDP.Download', label = 'Download the figure')

      ),

      mainPanel(
        width = 9,
        column(
          width = 12, align = "center",
          HTML('<p align="left" "font-size:120%;">
               The cumulative difference plot can be useful when box/violin-plots
               fail to provide a clear result. Generating this plot requires exactly
               two algorithms (two IDs) to be selected.
               </p>

               <p align="left" "font-size:120%;">
               The cumulative difference plot compares the samples from two algorithms
               through the first order stochastic dominance. In the left side of the x-axis,
               the shortest runtimes of the algorithms are compared. While in the right side,
               the longest runtimes are compared.
               </p>

               <p align="left" "font-size:120%;">
               If the curve does not cross the x-axis, this means that the algorithm
               that is in the same side as the curve stochasticaly dominates the
               other one.
               If the curve is positive and negative, then one of the algorithms
               might be better at getting short runtimes (if in the left part the
               plot, the curve is in the side of that algorithm), or better at
               avoiding long runtimes (if in the right part of the plot, the curve is
               in the side of that algorithm).
              </p>'),
          plotlyOutput.IOHanalyzer('RT_CDP')
        )
      )
  )
}



