DIM_fID_panel <- function() {
  conditionalPanel(
    "input.tabs!='upload' && input.tabs!='readme' && input.tabs!='about' && input.tabs!='Settings'",
    fluidRow(
      column(
        width = 11,
        selectInput(
          "Overall.Funcid",
          label = HTML('<p style="font-size:120%;">Please select the function ID</p>'),
          choices = NULL, selected = NULL)
      )
    ),

    fluidRow(
      column(
        width = 11,
        selectInput(
          "Overall.Dim",
          label = HTML('<p style="font-size:120%;">Please select the dimension</p>'),
          choices = NULL, selected = NULL)
      )
    )
  )
}
