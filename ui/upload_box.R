upload_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Upload Data</p>'), 
      width = width, solidHeader = T, status = "primary", collapsible = collapsible, 
      collapsed = collapsed, height = '620px',
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

upload_prompt_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Data Processing Prompt</p>'), 
      width = width, height = '620px', solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
      
      verbatimTextOutput('process_data_promt'),
      # modifying CSS for 'process_data_promt'
      tags$head(tags$style("#process_data_promt{
                           color:black; font-size:12px; font-style:italic;
                           max-height: 500px;
                           overflow-y:visible; overflow-x: auto; 
                           white-space: pre-wrap;
                           white-space: -moz-pre-wrap;
                           white-space: -pre-wrap;
                           white-space: -o-pre-wrap; 
                           word-wrap: normal; 
                           background: ghostwhite;}"))
      )
}

data_list_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">List of Processed Data</p>'),
      width = width, solidHeader = T, status = "primary", 
      collapsible = collapsible, collapsed = collapsed,
      dataTableOutput('DATASETLIST_INFO')
  )
}

