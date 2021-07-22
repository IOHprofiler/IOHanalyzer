filler_box <- function(width = 12, collapsible = T, collapsed = T) {
  box(
    title = HTML('<p style="font-size:120%;">Get Provenance Information</p>'),
    width = width, solidHeader = T, status = "primary",
    collapsible = collapsible, collapsed = collapsed,
    # dataTableOutput('data_info')
    selectInput('i9', label = "Please Select the Study Name", choices = c("Dimension Selection in Axis-Parallel Brent-STEP Method
for Black-Box Optimization of Separable Continuous
Functions", "The Impact of Initial Designs on the Performance of
MATSuMoTo on the Noiseless BBOB-2015 Testbed"), selected = "")
  )
}

ontology_box <- function(width = 12, collapsible = T, collapsed = F, 
                           height = '1200px') {
  box(
    title = HTML('<p style="font-size:120%;">Create Query on Ontology</p>'), 
    width = width, height = height, collapsed = collapsed, collapsible = collapsible,
    solidHeader = T, status = "primary",  
    sidebarPanel(
      width = 12,
      
      HTML_P("Create a Query from the ontology:"),
      
      # selectInput('i1', label = "Select the va type source",
      #             choices = c(""), selected = NULL, width = '50%'),
      
      # checkboxInput("c3", "Choose Specific Data Source", value = FALSE),
      # conditionalPanel(condition = 'input["c3"] == true',
      selectInput('Ontology.Source', label = "Please choose the data source", choices = c("BBOB", "Nevergrad"), selected = "BBOB", width = '50%', multiple = F),
      # ),


      conditionalPanel(condition = 'input["Ontology.Source"] == "Nevergrad"',
                       selectInput('Ontology.NG_Suite',
                                   label = "Please choose the Function Suite",
                                   choices = NULL, selected = NULL, width = '50%', multiple = F)),
      
      selectInput('Ontology.Functions',
                  label = "Please choose the functions",
                  choices = NULL, selected = NULL, width = '50%', multiple = T),
      selectInput('Ontology.Dimensions',
                  label = "Please choose the dimensions",
                  choices = NULL, selected = NULL, width = '50%', multiple = T),
      selectInput('Ontology.Algorithms',
                  label = "Please choose the algorithms",
                  choices = NULL, selected = NULL, width = '50%', multiple = T),
      

      
      conditionalPanel(condition = 'input["Ontology.Source"] == "BBOB"',
                       selectInput('Ontology.Iids',
                                   label = "Please choose the instances",
                                   choices = NULL, selected = NULL, width = '50%', multiple = T)),

      # checkboxInput("c6", "Use Specific Algorithms", value = FALSE),
      # conditionalPanel(condition = 'input["c6"] == true',
      # selectInput('i6',
      #             label = "Please choose the algorithm",
      #             choices = c("CMA-ES", "DE", "MLSL", "PSO"), selected = c("CMA-ES", "DE"), width = '50%', multiple = T)),
      # radioButtons("i2", label = "Select the type of data to get", choices = c("Fixed-Target", "Fixed-Budget", "Fixed-Probability"), selected = "Fixed-Budget"),

      # checkboxInput("c7", "Use Specific Targets", value = FALSE),
      # conditionalPanel(condition = 'input["c7"] == true',
      # numericInput("i7", label = "Select the Target Value", value = 500)),

      actionButton('Ontology.Load', 'Load Data')
      
      
    )
  )
}