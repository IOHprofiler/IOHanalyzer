upload_box <- function(width = 12, collapsible = T, collapsed = T,   # TODO: find a way to include all potential arguments
                       height = '600px') {  
  box(
    title = HTML('<p style="font-size:120%;">Upload Data</p>'), 
    width = width, height = height, collapsed = collapsed, collapsible = collapsible, 
    solidHeader = T, status = "primary", 
    sidebarPanel(
      width = 12,
      selectInput('upload.data_format', 
                  label = HTML('<p align="left" style="font-size:120%;">
                               Please choose the format of your datasets</p>'),
                  choices = c(AUTOMATIC,IOHprofiler, COCO, TWO_COL), 
                  selected = AUTOMATIC, width = '50%'),
      
      selectInput('upload.maximization', 
                  label = HTML('<p align="left" style="font-size:120%;">
                               Maximization or minimization?</p>'),
                  choices = c(AUTOMATIC,"MAXIMIZE", "MINIMIZE"), 
                  selected = AUTOMATIC, width = '50%'),
      HTML_P("<b>Note</b>: when using two-column format, please select the format and maximization manually."),
      
      HTML('<p align="justify" style="font-size:120%;">When the dataset is huge, 
           the alignment can take a very long time. In this case, you could toggle 
           the efficient mode to subsample the dataset. However, 
           the precision of data will be compromised.</p>'),
      
      checkboxInput('upload.subsampling', 
                    label = HTML('<p align="left" style="font-size:120%;">
                                 Efficient mode</p>'), 
                    value = F),

      fileInput("upload.add_zip", 
                label = HTML('<p align="left" style="font-size:120%;">
                             Please choose a <i>zip file</i> containing the 
                             benchmark data</p>'),
                multiple = TRUE, accept = c("Application/zip", ".zip",
                                            ".csv", 'bz2', 'bz', 'gz', 'tar', 'tgz', 'tar.gz', 'xz')),

      actionButton('upload.remove_data', 
                   label = HTML('<p align="center" style="font-size:120%; margin-bottom:0;"><b> 
                   Remove all the data</b></p>'))
      )
    )
}

upload_prompt_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(title = HTML('<p style="font-size:120%;">Data Processing Prompt</p>'),
      width = width, solidHeader = T, status = "primary",
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
  box(
    title = HTML('<p style="font-size:120%;">List of Processed Data</p>'),
    width = width, solidHeader = T, status = "primary",
    collapsible = collapsible, collapsed = collapsed,
    dataTableOutput('data_info')
  )
}

repository_box <- function(width = 12, collapsible = F, collapsed = T, 
                           height = '600px') {
  box(
    title = HTML('<p style="font-size:120%;">Load Data from Repository</p>'), 
    width = width, height = height, collapsed = collapsed, collapsible = collapsible,
    solidHeader = T, status = "primary",  
    sidebarPanel(
      width = 12,
      selectInput('repository.dataset', 
                  label = HTML('<p align="left" style="font-size:120%;">
                               Select the dataset</p>'),
                  choices = NULL, selected = NULL, width = '50%'),

      selectInput('repository.funcId', 
                  label = HTML('<p align="left" style="font-size:120%;">
                               Please choose the function</p>'),
                  choices = NULL, selected = NULL, width = '50%'),
      
      selectInput('repository.dim', 
                  label = HTML('<p align="left" style="font-size:120%;">
                               Please choose the dimension</p>'),
                  choices = NULL, selected = NULL, width = '50%'),
      
      selectInput('repository.algId', 
                  label = HTML('<p align="left" style="font-size:120%;">
                               Please choose the algorithm</p>'),
                  choices = NULL, selected = NULL, width = '50%'),
      
      shinyjs::disabled(
        actionButton('repository.load_button', 'load data')
      )
    )
  )
}

