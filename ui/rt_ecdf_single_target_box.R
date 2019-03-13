rt_ecdf_single_target_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Empirical Cumulative Distribution: Single target</p>'), 
      width = width, collapsible = collapsible, solidHeader = TRUE, 
      status = "primary", collapsed = collapsed,
      sidebarLayout(
        sidebarPanel(
          width = 3,
          HTML('Select the target values for which EDCF curves are displayed'),
          textInput('RT_ECDF_FTARGET', label = HTML('<p>\\(f_{target}\\)</p>'), 
                    value = ''),
          
          checkboxInput('RT_ECDF_semilogx', label = 'scale x axis log10', value = F)
        ), 
        
        mainPanel(
          width = 9,
          column(
            width = 12, 
            align = "center",
            HTML_P('Each EDCF curve shows the proportion of the runs 
                    that have found a solution of at least the required 
                    target value within the budget given by the \\(x\\)-axis. 
                    The displayed curves can be selected by clicking on the legend on the right. A <b>tooltip</b> 
                    and <b>toolbar</b> appears when hovering over the figure. 
                    This also includes the option to download the plot as png file.'),
            plotlyOutput("RT_ECDF", height = plotly_height, width = plotly_width2)
          )
        )
      )
  )
}