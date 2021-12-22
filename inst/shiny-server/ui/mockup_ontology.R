# filler_box <- function(width = 12, collapsible = T, collapsed = T) {
#   box(
#     title = HTML('<p style="font-size:120%;">Get Provenance Information</p>'),
#     width = width, solidHeader = T, status = "primary",
#     collapsible = collapsible, collapsed = collapsed,
#     # dataTableOutput('data_info')
#     selectInput('i9', label = "Please Select the Study Name", choices = c("Dimension Selection in Axis-Parallel Brent-STEP Method
# for Black-Box Optimization of Separable Continuous
# Functions", "The Impact of Initial Designs on the Performance of
# MATSuMoTo on the Noiseless BBOB-2015 Testbed"), selected = "")
#   )
# }

ontology_box <- function(width = 12,
                         collapsible = T,
                         collapsed = F,
                         height = '75vh') {
  box(
    title = HTML('<p style="font-size:120%;">Create Query on Ontology</p>'),
    width = width,
    height = height,
    collapsed = collapsed,
    collapsible = collapsible,
    solidHeader = T,
    status = "primary",
    sidebarPanel(
      width = 12,

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
}
