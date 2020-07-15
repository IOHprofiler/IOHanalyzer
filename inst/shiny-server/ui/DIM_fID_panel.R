DIM_fID_panel <- function() {
  conditionalPanel(
    "input.tabs!='Report' && input.tabs!='upload' && input.tabs!='readme' && input.tabs!='about' && input.tabs!='Settings'",
    column(12, offset = 0,
      div(
        style = "padding: 0px 0px; margin-top:-5em; margin:0%", 
        fluidRow(
          column(
            width = 11,
            selectInput(
              "Overall.Funcid",
              label = HTML('<p style="font-size:90%;">Select the function ID</p>'),
              choices = NULL, selected = NULL)
          )
        )
      ),
      div(
        style = "padding: 0px 0px; margin-top:-50em;", 
        fluidRow(
          column(
            width = 11,
            selectInput(
              "Overall.Dim",
              label = HTML('<p style="font-size:90%;">Select the dimension</p>'),
              choices = NULL, selected = NULL)
          )
        )
      )
    )
  )
}
