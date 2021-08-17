DIM_fID_panel <- function() {
  conditionalPanel(
    "input.tabs!='Report' && input.tabs!='upload' && input.tabs!='readme' && input.tabs!='about' && input.tabs!='Settings'",
    column(12, offset = 0,
      div(id = 'overall_funcid_box',
        # style = "padding: 0px 0px; margin-top:-5em; margin:0%", 
        fluidRow(
          column(
            width = 11,
            selectInput(
              "Overall.Funcid",
              label = HTML('<p style="font-size:90%;">Function ID:</p>'),
              choices = NULL, selected = NULL)
          )
        )
      ),
      div(id = 'overall_funcname_box',
        # style = "padding: 0px 0px; margin-top:-5em; margin:0%", 
        fluidRow(
          column(
            width = 11,
            selectInput(
              "Overall.Funcname",
              label = HTML('<p style="font-size:90%;">Function Name:</p>'),
              choices = NULL, selected = NULL)
          )
        )
      ),
      div(
        # style = "padding: 0px 0px; margin-top:-50em;", 
        fluidRow(
          column(
            width = 11,
            selectInput(
              "Overall.Dim",
              label = HTML('<p style="font-size:90%;">Dimension:</p>'),
              choices = NULL, selected = NULL)
          )
        )
      ),
      div(id = 'overall_algid_box',
        fluidRow(
          column(
            width = 11,
            selectInput(
              "Overall.ID",
              label = HTML('<p style="font-size:90%;">Algorithm:</p>'),
              choices = NULL, selected = NULL)
          )
        )
      )#,
    )
  )
}
