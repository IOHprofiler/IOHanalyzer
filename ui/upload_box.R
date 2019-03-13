upload_box <- function() {
  box(title = HTML('<p style="font-size:120%;">Upload Data</p>'), width = 12,
      solidHeader = T, status = "primary", collapsible = F, height = '620px',
      sidebarPanel(
        width = 12,
        selectInput('DATA_SRC_FORMAT', label = HTML('<p align="left" style="font-size:120%;">Please choose the format of your data sets</p>'),
                    choices = c(AUTOMATIC,IOHprofiler, COCO, TWO_COL), selected = AUTOMATIC, width = '50%'),
        selectInput('DATA_SRC_MINMAX', label = HTML('<p align="left" style="font-size:100%;">Use maximization or minimization?</p>'),
                    choices = c(AUTOMATIC,"MAXIMIZE", "MINIMIZE"), selected = AUTOMATIC, width = '50%'),
        
        HTML('<p align="justify" style="font-size:120%;">When the data set is huge, the alignment
                                can take a very long time. In this case, you could toggle the efficient mode to subsample 
                                the data set. However, the precision of data will be compromised.</p>'),
        checkboxInput('SUBSAMPLING', label = HTML('<p align="left" style="font-size:120%;">Efficient mode</p>'), value = F),
        
        fileInput("ZIP", label = HTML('<p align="left" style="font-size:120%;">Please choose a <i>zip file</i> containing the benchmark data</p>'),
                  multiple = TRUE),
        # accept = c("Application/zip", ".zip")),
        
        # TODO: keep this for the local version
        # shinyDirButton('directory', 'Browse the folder', 
        # title = 'Please choose a directory containing the benchmark data'),
        HTML('<p align="left" style="font-size:120%;"><b>Remove all data you uploaded</b></p>'),
        actionButton('RM_DATA', 'Clear data')
      )
  )
}

