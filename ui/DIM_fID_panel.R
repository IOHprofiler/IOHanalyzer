DIM_fID_panel <- function() {
  conditionalPanel(
    "input.tabs!='upload' && input.tabs!='readme' && input.tabs!='about'",
    fluidRow(
      column(
        width = 11,
        selectInput(
          "FUNCID_INPUT", 
          label = HTML('<p style="font-size:120%;">Please select the function ID</p>'), 
          choices = NULL, selected = NULL)
      )
    ),
    
    fluidRow(
      column(
        width = 11,
        selectInput(
          "DIM_INPUT",
          label = HTML('<p style="font-size:120%;">Please select the dimension</p>'), 
          choices = NULL, selected = NULL)
      )
    )
  )
}