heatmap_box <- function(width = 12, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Statistical Significance</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      sidebarPanel(
        width = 3,
        selectInput('Stats.Overview.Algid', 'Algorithms to compare', choices = NULL, 
                    selected = NULL, multiple = T),
        textInput('Stats.Overview.Target', label = F_TAR_LABEL),
        textInput('Stats.Overview.Alpha', 
                  label = HTML('<p>significant level \\(\\alpha\\)'), value = 0.01),
        numericInput('Stats.Overview.Samples', 
                     label = 'size of the bootstrap sample', 
                     min = 1, max = 1000, step = 1, value = 30)
      ),
      
      mainPanel(
        width = 9,
        HTML_P('The <b>Kolmogorov-Smirnov test</b> is performed on empirical CDFs of running times for each pair of 
                algorithms, in order to determine which algorithm gives a significantly 
                smaller running time distribution. The resulting p-values are arranged in a matrix, where
                each cell \\((i, j)\\) contains a p-value from the test with the alternative hypothesis:
                the running time of algorithm \\(i\\) is smaller (thus better) than that of \\(j\\).'),
        DT::dataTableOutput('Stats.Overview.Pmatrix')
      ),

      fluidRow(
        column(
          width = 12,
          HTML('<div style="margin-top: 50px;"></div>'),
          HTML_P('Decisions of the test, based on the \\(p-\\) value matrix and the \\(\\alpha\\) value,
                  are visualized in a heatmap (<b>left</b>) and a network (<b>right</b>).
                  In each cell (row, column) of the heatmap, the alternative hypothesis is again:
                  the row algorithm has smaller (better) running time than the column. 
                  The color indicates:
                  <ul>
                    <li><font color="red">Red: A row is better than the column</font></li>
                    <li><font color="blue">Blue: A row is worse than the column</font></li>
                    <li><font color="grey">Gray: no significant decision between the row and column</font></li>
                  </ul>
                  On the right subplot, the partial order resulted from the test is visualized as a network, 
                  where an arrow from algorithm \\(A\\) to \\(B\\) indicates \\(A\\) is siginicantly better than 
                  \\(B\\) with the specified \\(\\alpha\\) value.')
        )
      ),

      fluidRow(
        column(
          width = 6, align = 'center',
          HTML('<div style="margin-top: 50px;"></div>'),
          plotlyOutput.IOHanalyzer('Stats.Overview.Heatmap', aspect_ratio = 1)
        ),
        column(
          width = 6, align = 'center',
          plotOutput("Stats.Overview.Graph", height = '70vh')
        )
      )
  )
}
