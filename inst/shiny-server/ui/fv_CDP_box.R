# FCEPDF.Hist.Runtime -> FCEPDF.CDP.Runtime
# FCE_HIST -> FCE_CDP

fv_CDP_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = 'Cumulative difference plot with fixed budget',
      width = width, collapsible = collapsible,
      solidHeader = TRUE,  collapsed = collapsed,
      status = "primary",
      sidebarPanel(
        width = 2,
        textInput('FCEPDF.CDP.Runtime', label = HTML('Select the budget value'),
                  value = ''),
        selectInput('FCEPDF.CDP.Algs', label = 'Select which IDs to include:',
                    multiple = T, selected = NULL, choices = NULL) %>% shinyInput_label_embed(
                      custom_icon() %>%
                        bs_embed_popover(
                          title = "ID selection", content = alg_select_info,
                          placement = "auto"
                        )
                    ),
        HTML('Choose whether the histograms are <b>overlaid</b> in one plot
                                or <b>separated</b> in several subplots:'),
        selectInput('FCEPDF.CDP.Mode', '',
                    choices = c("overlay", "subplot"),
                    selected = 'subplot'),
        checkboxInput("FCEPDF.CDP.Equal", "Use equal bins for all IDs", F),

        hr(),
        selectInput('FCEPDF.Hist.Format', label = 'Select the figure format',
                    choices = supported_fig_format, selected = supported_fig_format[[1]]),

        downloadButton('FCEPDF.CDP.Download', label = 'Download the figure')

      ),

      mainPanel(
        width = 10,
        column(
          width = 12, align = "center",
          HTML('<p align="left" "font-size:120%;">
               This histogram counts the number of runs whose best-so-far function
               values within the first \\(B\\) evaluations is between \\(v_i\\) and
               \\(v_{i+1}\\). The buckets \\([v_i,v_{i+1})\\) are chosen automatically
               according to the so-called <b>Freedman–Diaconis rule</b>: \\(\\text{Bin size}=
               2\\frac{Q_3 - Q_1}{\\sqrt[3]{n}}\\), where \\(Q_1, Q_3\\) are the \\(25\\%\\)
               and \\(75\\%\\) percentile of the runtime and \\(n\\) is the sample size.
               The displayed IDs can be selected by clicking on the legend on the right.
               A <b>tooltip</b> and <b>toolbar</b> appears when hovering over the figure.</p>'),
          plotlyOutput.IOHanalyzer('FCE_CDP')
        )
      )
  )
}