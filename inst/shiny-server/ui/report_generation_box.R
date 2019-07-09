main_report_box <- function(width = 6, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Report Content</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
        HTML("Select which components should be in the report"),
        checkboxInput("Report.Introduction", "Introduction", T),
        fixed_target_report_box(),
        downloadButton("Report.Generate", "Generate report")
      )
}

fixed_target_report_box <- function(){
  box(title = "Fixed-target", collapsible = T, collapsed = T, width = 11, status = "primary",
      box(title = "Data Summary", collapsible = T, collapsed = T, width = 11, status = "primary",
          checkboxInput("Report.FV.Overview", "Data Overview", F),
          conditionalSelection("Report.FV.Overview",'input["Report.FV.Overview"]'),
          checkboxInput("Report.FV.Statistics", "Runtime Statistics", F) %>%
            shinyInput_label_embed(
              custom_icon("info") %>%
                bs_embed_tooltip(
                  title = "If enabled, the bounds and granularity will be taken from those entered in the corresponding tab."
                )
            ),
          conditionalSelection("Report.FV.Statistics",'input["Report.FV.Statistics"]'),
          checkboxInput("Report.FV.Samples", "Runtime Samples", F) %>%
            shinyInput_label_embed(
              custom_icon("info") %>%
                bs_embed_tooltip(
                  title = "If enabled, the bounds and granularity will be taken from those entered in the corresponding tab."
                )
            ),
          conditionalSelection("Report.FV.Samples",'input["Report.FV.Samples"]')
      ),
      box(title = "Expected runtime", collapsible = T, collapsed = T, width = 11, status = "primary",
          checkboxInput("Report.FV.Single_ERT", "Runtime plot (per function)", F),
          conditionalSelection("Report.FV.Single_ERT",'input["Report.FV.Single_ERT"]', scalex = T, scaley = T),
          checkboxInput("Report.FV.Multi_ERT", "Runtime plot (all functions)", F),
          conditionalSelection("Report.FV.Multi_ERT",'input["Report.FV.Multi_ERT"]', fid=F, scalex = T, scaley = T),
          checkboxInput("Report.FV.Rank", "Runtime ranking", F),
          conditionalSelection("Report.FV.Rank",'input["Report.FV.Rank"]', fid=F)
      )
  )
}

conditionalSelection <- function(id, condition, fid=T, dim=T, alg=T, scalex=F, scaley=F){
  ns <- NS(id)
  conditionalPanel(condition = condition, 
                   if (fid) selectInput(ns("FuncId"), label = "Function ids", choices = NULL, selected = NULL, multiple = T),
                   if (dim) selectInput(ns("DIM"), label = "Dimensions", choices = NULL, selected = NULL, multiple = T),
                   if (alg) selectInput(ns("Alg"), label = "Algorithms", choices = NULL, selected = NULL, multiple = T),
                   if (scalex) checkboxInput(ns("Scalex"), label = "Scale x-axis logarithmically", value = F),
                   if (scaley) checkboxInput(ns("Scaley"), label = "Scale y-axis logarithmically", value = F)
                   )
}