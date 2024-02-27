upload_box <- function(width = 12, collapsible = T, collapsed = T,   # TODO: find a way to include all potential arguments
                       height = '85vh') {
  box(
    title = HTML('<p style="font-size:120%;">Upload Data</p>'),
    width = width, height = height, collapsed = collapsed, collapsible = collapsible,
    solidHeader = T, status = "primary",
    sidebarPanel(
      width = 12,

      HTML_P('<b>IOHexperimenter, Nevergrad, and BBOB/COCO data files are accepted.
             For alternative data files, please convert them to the format described <a href="https://iohprofiler.github.io/IOHanalyzer/data/">here</a>,
             or use a single csv-file.</b>'),

      checkboxInput('upload.use_custom_csv',
                    label = HTML('<p align="left">
                                 Use custom csv format</p>'),
                    value = F),

      conditionalPanel(condition = "input['upload.use_custom_csv']",
           fileInput("upload.custom_csv",
                 label = HTML('<p align="left">
               Please upload a single <i>csv file</i> containing the
               benchmark data</p>'), multiple = FALSE, accept = c(".csv")),
           checkboxInput('upload.maximization',
                 label = "Is the data from a maximization setting?",
                 value = F),
           selectInput('upload.neval_name',
                         label = "Column to use for evaluation count",
                         choices = NULL, selected = NULL) %>%
               shinyInput_label_embed(
                 custom_icon("info") %>%
                   bs_embed_tooltip(
                     title = "When set to 'None', it will be assumed to be sequential for each run."
                   )
               ),
           selectInput('upload.fval_name',
                       label = "Column to use for function values",
                       choices = NULL, selected = NULL),
           hr(),
           selectInput('upload.fname_name',
                       label = "Column to use for function ID",
                       choices = NULL, selected = NULL) %>%
             shinyInput_label_embed(
               custom_icon("info") %>%
                 bs_embed_tooltip(
                   title = "When set to 'None', you can enter a fixed function ID"
                 )
             ),
           conditionalPanel("input['upload.fname_name'] == 'None'",
                            textInput("upload.fname_static", "Function ID", "Fname")
                            ),
           hr(),
           selectInput('upload.algname_name',
                       label = "Column to use for algorithm ID",
                       choices = NULL, selected = NULL) %>%
             shinyInput_label_embed(
               custom_icon("info") %>%
                 bs_embed_tooltip(
                   title = "When set to 'None', you can enter a fixed algorithm ID"
                 )
             ),
           conditionalPanel("input['upload.algname_name'] == 'None'",
                            textInput("upload.algname_static", "Algorithm ID", "Alg1")
           ),
           hr(),
           selectInput('upload.dim_name',
                       label = "Column to use for problem dimension",
                       choices = NULL, selected = NULL) %>%
             shinyInput_label_embed(
               custom_icon("info") %>%
                 bs_embed_tooltip(
                   title = "When set to 'None', you can enter a fixed dimension."
                 )
             ),
           conditionalPanel("input['upload.dim_name'] == 'None'",
                            numericInput("upload.dim_static", "Dimension", 2)
           ),
           hr(),
           selectInput('upload.run_name',
                       label = "Column to use for run ID",
                       choices = NULL, selected = NULL) %>%
             shinyInput_label_embed(
               custom_icon("info") %>%
                 bs_embed_tooltip(
                   title = "When set to 'None', it will be assumed there is only one run for each function,algorithm,dimension triplet."
                 )
             ),
           actionButton('upload.process_csv',
                        label = HTML('<p align="center" style="margin-bottom:0;"><b>
                   Process uploaded file with selected settings</b></p>')),
           hr()
                       ),
      conditionalPanel(condition = "!input['upload.use_custom_csv']",
         fileInput("upload.add_zip",
                   label = HTML('<p align="left">
               Please choose one or more files containing the
               benchmark data</p>'),
                   multiple = TRUE, accept = c("Application/zip", ".zip",
                                               ".csv", 'bz2', 'bz', 'gz', 'tar', 'tgz', 'tar.gz', 'xz')),



      ),


      # selectInput(
      #   'upload.data_format',
      #   label = "Please choose the format of your datasets",
      #   choices = c(AUTOMATIC, TWO_COL),
      #   selected = AUTOMATIC, width = '60%'
      # ) %>%
      #   shinyInput_label_embed(
      #     custom_icon("info") %>%
      #       bs_embed_tooltip(
      #         title = "The IOHprofiler, COCO and Nevergrad formats can be automatically detected."
      #       )
      #   ),
      #
      # selectInput('upload.maximization',
      #             label = "Maximization or minimization?",
      #             choices = c(AUTOMATIC,"MAXIMIZE", "MINIMIZE"),
      #             selected = AUTOMATIC, width = '60%') ,
      #
      # HTML('<p align="justify" style="font-size:120%;">When the dataset is huge,
      #      the alignment can take a very long time. In this case, you could toggle
      #      the efficient mode to subsample the dataset. However,
      #      the precision of data will be compromised.</p>'),
      #
      # checkboxInput('upload.subsampling',
      #               label = HTML('<p align="left">
      #                            Efficient mode</p>'),
      #               value = F),



      actionButton(
        'upload.remove_data',
        label = HTML('<p align="center" style="margin-bottom:0;"><b>Remove all the data</b></p>'),
      ),

      actionButton(
        'upload.add_data',
        label = HTML('<p align="center" style="margin-bottom:0;"><b>Save data on cloud</b></p>'),
      )
    ),
    downloadButton('upload.Download_processed',
                   label = 'Download RDS of loaded data.')
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


welcome_bar <- function(width = 12, collapsible = T, collapsed = F) {
  box(
    title = HTML('<p style="font-size:140%;">Welcome to IOHanalyzer!</p>'),
    width = width, collapsed = collapsed, collapsible = collapsible,
    solidHeader = T, status = "primary",
    mainPanel(
      width = 12,
      HTML(paste0(
        '<font size="4">',
        includeMarkdown('markdown/welcome.md'),
        '</font>')
      )),
      fluidRow(valueBoxOutput("VersionBox"),
               valueBoxOutput("WikiBox"),
               valueBoxOutput("ContactBox")
      )
  )
}


overal_loading_box <- function(width = 12, collapsible = T,
                               collapsed = F, height='85vh') {
  box(
    title = HTML('<p style="font-size:120%;">Load from repositories</p>'),
    width = width,
    height = height,
    collapsed = collapsed,
    collapsible = collapsible,
    solidHeader = T,
    status = "primary",
    sidebarPanel(
      width = 12,

      selectInput('Loading.Type', label='Repository',
                  choices = c('IOH', 'OPTION'),
                  selected = 'IOH', width='80%', multiple = F) %>%
        shinyInput_label_embed(
          custom_icon("info") %>%
            bs_embed_popover(
              title = "Repository-selection",
              content = "There are currently 2 distinct sources of data available.
              The first is IOH, which is our local repository and contains a wide
              variety of data from PBO, BBOB, Nevergrad and more.
              The second is the OPTION-Ontology, which has annotated performance
              data for a wide variety of settings, and supports additional query types.
              However, the processing time from OPTION is longer thand for the IOH-repository.",
              placement = "auto"
            )
        ),

      conditionalPanel(style = "display: none;",
                       condition = 'input["Loading.Type"] == "IOH"',
                       # repository_box()
                       HTML_P("Load the data from the available repositories. There are currently three available sources:
             <ul>
                <li>
                  Data generated with the PBO-suite, implemented in the <a href=https://github.com/IOHprofiler/IOHexperimenter>IOHexperimenter</a>
                </li>
                <li>
                  All data generated by the <a href=https://github.com/facebookresearch/nevergrad>nevergrad</a> benchmarking framework
                </li>
                <li>
                  The majority of the publicly available benchmark data on the single-objective <a href=https://github.com/numbbo/coco>BBOB</a> framework
                </li>
               </ul>"),

                       selectInput('repository.type', label = "Select the dataset source",
                                   choices = NULL, selected = NULL, width = '80%'),

                       selectInput('repository.dataset',
                                   label = "Select the dataset",
                                   choices = NULL, selected = NULL, width = '80%', multiple = T),

                       selectInput('repository.ID',
                                   label = "Please choose the algorithm",
                                   choices = NULL, selected = NULL, width = '80%', multiple = T),

                       selectInput('repository.funcId',
                                   label = "Please choose the function",
                                   choices = NULL, selected = NULL, width = '80%', multiple = T),

                       selectInput('repository.dim',
                                   label = "Please choose the dimension",
                                   choices = NULL, selected = NULL, width = '80%', multiple = T),


                       shinyjs::disabled(
                         actionButton('repository.load_button', 'load data')
                       )
      ),
      conditionalPanel(style = "display: none;",
                       condition = 'input["Loading.Type"] == "OPTION"',
                       HTML_P("Load the data from the OPTION-Ontology"),

                       selectInput(
                         'Ontology.Study',
                         label = "Pre-select algorithms from existing study",
                         choices = NULL,
                         selected = NULL,
                         width = '80%',
                         multiple = F
                       )  %>%
                         shinyInput_label_embed(
                           custom_icon("info") %>%
                             bs_embed_popover(
                               title = "Study-selection",
                               content = "This will limit the available options to
              those used in the selected study. To remove the restriction,
              you can change this back to 'None'.",
                               placement = "auto"
                             )
                         ),

                       selectInput(
                         'Ontology.Source',
                         label = "Please choose the data source",
                         choices = c("BBOB", "Nevergrad"),
                         selected = "BBOB",
                         width = '80%',
                         multiple = F
                       ),


                       conditionalPanel(
                         condition = 'input["Ontology.Source"] == "Nevergrad"',
                         selectInput(
                           'Ontology.NG_Suite',
                           label = "Please choose the Function Suite",
                           choices = NULL,
                           selected = NULL,
                           width = '80%',
                           multiple = F
                         )
                       ),

                       selectInput(
                         'Ontology.Algorithms',
                         label = "Please choose the algorithms",
                         choices = NULL,
                         selected = NULL,
                         width = '80%',
                         multiple = T
                       ),

                       selectInput(
                         'Ontology.Functions',
                         label = "Please choose the functions",
                         choices = NULL,
                         selected = NULL,
                         width = '80%',
                         multiple = T
                       ),
                       selectInput(
                         'Ontology.Dimensions',
                         label = "Please choose the dimensions",
                         choices = NULL,
                         selected = NULL,
                         width = '80%',
                         multiple = T
                       ),




                       conditionalPanel(
                         condition = 'input["Ontology.Source"] == "BBOB"',
                         selectInput(
                           'Ontology.Iids',
                           label = "Please choose the instances",
                           choices = NULL,
                           selected = NULL,
                           width = '80%',
                           multiple = T
                         )
                       ),

                       box(
                         title = HTML(
                           '<p style="font-size:120%;color:black;">Additional Options</p>'
                         ),
                         collapsible = T,
                         collapsed = T,
                         width = width,
                         solidHeader = T,
                         status = 'info',




                         checkboxInput(
                           'Ontology.Limit_Targets',
                           label = "Limit runs based on target reached",
                           value = F
                         ) %>%
                           shinyInput_label_embed(
                             custom_icon("info") %>%
                               bs_embed_popover(
                                 title = "Limit Targets",
                                 content = "This will limit the target values for
                which data is returned.
                  This means that run integrity can not be guaranteed,
                and fixed-budget data might get skewed.
                  Use this setting with care.",
                                 placement = "auto"
                               )
                           )
                         ,
                         conditionalPanel(
                           'input["Ontology.Limit_Targets"]',
                           numericInput("Ontology.Min_Target", label = "Minimum", value = 0),
                           numericInput("Ontology.Max_Target", label = "Maximum", value = 10000)
                         ),
                         checkboxInput(
                           'Ontology.Limit_Budgets',
                           label = "Limit runs based on used budget",
                           value = F
                         ) %>%
                           shinyInput_label_embed(
                             custom_icon("info") %>%
                               bs_embed_popover(
                                 title = "Limit Budget",
                                 content = "This will limit the budget values for
                which data is returned.
                  This means that run integrity can not be guaranteed,
                and fixed-target data might get skewed.
                  Use this setting with care.",
                                 placement = "auto"
                               )
                           ),
                         conditionalPanel(
                           'input["Ontology.Limit_Budgets"]',
                           numericInput(
                             "Ontology.Min_Budget",
                             label = "Minimum",
                             value = 1,
                             min = 1
                           ),
                           numericInput("Ontology.Max_Budget", label = "Maximum", value = 10000)
                         )
                       ),
                       actionButton('Ontology.Load', 'Load Data')
      )
    )
  )
}

