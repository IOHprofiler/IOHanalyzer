rt_pmf_box <- function() {
  box(title = 'Empirical Probability Mass Function of the Runtime', 
      width = 12, collapsible = TRUE, solidHeader = TRUE, status = "primary", collapsed = T,
      sidebarLayout(
        sidebarPanel(
          width = 2,
          HTML('Select the target value for which the runtime distribution is shown'),
          
          textInput('RT_PMF_FTARGET', label = '', value = ''),
          checkboxInput('RT_SHOW_SAMPLE', label = 'show runtime for each run', value = T),
          checkboxInput('RT_PMF_LOGY', label = 'scale y axis log10', value = F),
          
          selectInput('FIG_FORMAT_RT_PMF', label = 'select the figure format',
                      choices = supported_fig_format, selected = 'pdf'),
          
          downloadButton('FIG_DOWNLOAD_RT_PMF', label = 'download the figure')
          
          # HTML('Kernel density estimation uses the following <b>kernel function</b>:'),
          # selectInput('RT_PMF_KER', '',
          #             choices = c("gaussian", "epanechnikov", "rectangular",
          #                         "triangular", "biweight", "cosine", "optcosine"),
          #             selected = 'gaussian')
          
        ),
        
        mainPanel(
          width = 10,
          column(width = 12, align = "center", 
                 HTML('<p class="alert alert-warning" align="left" style="font-size:120%;"><b>Warning! </b>The 
                      <b>probability mass function</b> of the runtime is approximated by the 
                      treating the runtime as a <i>continuous</i> random variable and applying the <b>kernel estimation</b> (KDE):</p>'),
                 HTML('<p align="left" style="font-size:120%;">
                      The plot shows the distribution of the first hitting 
                      times of the individual runs (dots), and an estimated 
                      distribution of the probability mass function. 
                      The displayed algorithms can be selected by clicking on 
                      the legend on the right. A <b>tooltip</b> and <b>toolbar</b> 
                      appear when hovering over the figure. This also includes the
                      option to download the plot as png file. A csv file with the runtime 
                      data can be downlowaded from the  
                      <a href="#shiny-tab-ERT_data", data-toggle="tab"> Data Summary tab</a>.'),
                 plotlyOutput('RT_PMF', height = plotly_height, width = plotly_width2)
                 )
                 )
          )
        )
}
