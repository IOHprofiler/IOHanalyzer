main_report_box <- function(width = 6, collapsible = T, collapsed = F) {
  box(title = HTML('<p style="font-size:120%;">Report Content</p>'),
      width = width, solidHeader = T, status = "primary",
      collapsible = collapsible, collapsed = collapsed,
        HTML("Select which components should be in the report. \n All relevant settings (bounds, targets, scaling...) 
             will be taken from the relevant tab if available, or set to a default value."),
      checkboxInput("Report.Introduction", "Introduction", T),
      fixed_target_report_box(),
      fixed_budget_report_box(),
      parameter_box(),
      downloadButton("Report.Generate", "Generate report")
      )
}

fixed_target_report_box <- function(){
  box(title = "Fixed-Target", collapsible = T, collapsed = T, width = 11, status = "primary",
      background = "blue",
      box(title = "Data Summary", collapsible = T, collapsed = T, width = 11, status = "primary",
          background = "navy",
          checkboxInput("Report.RT.Overview", "Data Overview", F),
          conditionalSelection("Report.RT.Overview",'input["Report.RT.Overview"]'),
          checkboxInput("Report.RT.Statistics", "Runtime Statistics", F),
          conditionalSelection("Report.RT.Statistics",'input["Report.RT.Statistics"]'),
          checkboxInput("Report.RT.Samples", "Runtime Samples", F),
          conditionalSelection("Report.RT.Samples",'input["Report.RT.Samples"]')
      ),
      box(title = "Expected Runtime", collapsible = T, collapsed = T, width = 11, status = "primary",
          background = "navy",
          checkboxInput("Report.RT.Single_ERT", "Runtime plot (per function)", F),
          conditionalSelection("Report.RT.Single_ERT",'input["Report.RT.Single_ERT"]'),
          checkboxInput("Report.RT.Multi_ERT", "Runtime plot (all functions)", F),
          conditionalSelection("Report.RT.Multi_ERT",'input["Report.RT.Multi_ERT"]', fid=F),
          checkboxInput("Report.RT.Rank", "Runtime ranking", F),
          conditionalSelection("Report.RT.Rank",'input["Report.RT.Rank"]', fid=F) #, plottype=c("radar", "line"))
      ),
      box(title = "Probability Mass Function", collapsible = T, collapsed = T, width = 11, status = "primary",
          background = "navy",
          checkboxInput("Report.RT.Histogram", "Histogram of hitting times", F),
          conditionalSelection("Report.RT.Histogram",'input["Report.RT.Histogram"]'), # plottype = c("subplot","overlay")),
          checkboxInput("Report.RT.PMF", "Probability Mass Function", F),
          conditionalSelection("Report.RT.PMF",'input["Report.RT.PMF"]')
      ),
      box(title = "Cumulative Distirbution", collapsible = T, collapsed = T, width = 11, status = "primary",
          background = "navy",
          checkboxInput("Report.RT.ECDF_Single_Target", "Single-target ECDF", F),
          conditionalSelection("Report.RT.ECDF_Single_Target",'input["Report.RT.ECDF_Single_Target"]'),
          checkboxInput("Report.RT.ECDF_Single_Function", "Single-function ECDF", F),
          conditionalSelection("Report.RT.ECDF_Single_Function",'input["Report.RT.ECDF_Single_Function"]'),
          checkboxInput("Report.RT.ECDF_Aggregated", "Aggregated ECDF", F),
          conditionalSelection("Report.RT.ECDF_Aggregated",'input["Report.RT.ECDF_Aggregated"]', F, F),
          checkboxInput("Report.RT.ECDF_AUC", "AUC of ECDF", F),
          conditionalSelection("Report.RT.ECDF_AUC",'input["Report.RT.ECDF_AUC"]')
      )
  )
}

fixed_budget_report_box <- function(){
  box(title = "Fixed-Budget", collapsible = T, collapsed = T, width = 11, status = "primary",
      background = "blue",
      box(title = "Data Summary", collapsible = T, collapsed = T, width = 11, status = "primary",
          background = "navy",
          checkboxInput("Report.FV.Overview", "Data Overview", F),
          conditionalSelection("Report.FV.Overview",'input["Report.FV.Overview"]'),
          checkboxInput("Report.FV.Statistics", "Runtime Statistics", F),
          conditionalSelection("Report.FV.Statistics",'input["Report.FV.Statistics"]'),
          checkboxInput("Report.FV.Samples", "Runtime Samples", F),
          conditionalSelection("Report.FV.Samples",'input["Report.FV.Samples"]')
      ),
      box(title = "Expected Runtime", collapsible = T, collapsed = T, width = 11, status = "primary",
          background = "navy",
          checkboxInput("Report.FV.Single_FCE", "Runtime plot (per function)", F),
          conditionalSelection("Report.FV.Single_FCE",'input["Report.FV.Single_FCE"]'),
          checkboxInput("Report.FV.Multi_FCE", "Runtime plot (all functions)", F),
          conditionalSelection("Report.FV.Multi_FCE",'input["Report.FV.Multi_FCE"]', fid=F),
          checkboxInput("Report.FV.Rank", "Runtime ranking", F),
          conditionalSelection("Report.FV.Rank",'input["Report.FV.Rank"]', fid=F) #, plottype=c("radar", "line"))
      ),
      box(title = "Probability Mass Function", collapsible = T, collapsed = T, width = 11, status = "primary",
          background = "navy",
          checkboxInput("Report.FV.Histogram", "Histogram of hitting times", F),
          conditionalSelection("Report.FV.Histogram",'input["Report.FV.Histogram"]'), # plottype = c("subplot","overlay")),
          checkboxInput("Report.FV.PMF", "Probability Mass Function", F),
          conditionalSelection("Report.FV.PMF",'input["Report.FV.PMF"]')
      ),
      box(title = "Cumulative Distirbution", collapsible = T, collapsed = T, width = 11, status = "primary", 
          background = "navy",
          checkboxInput("Report.FV.ECDF_Single_Target", "Single-target ECDF", F),
          conditionalSelection("Report.FV.ECDF_Single_Target",'input["Report.FV.ECDF_Single_Target"]'),
          checkboxInput("Report.FV.ECDF_Single_Function", "Single-function ECDF", F),
          conditionalSelection("Report.FV.ECDF_Single_Function",'input["Report.FV.ECDF_Single_Function"]'),
          checkboxInput("Report.FV.ECDF_AUC", "AUC of ECDF", F),
          conditionalSelection("Report.FV.ECDF_AUC",'input["Report.FV.ECDF_AUC"]')
      )
  )
}

parameter_box <- function(){
  box(title = "Parameters", collapsible = T, collapsed = T, width = 11, status = "primary",
    background = "blue",
    checkboxInput("Report.Param.Plot", "Parameter plot", F),
    conditionalSelection("Report.Param.Plot",'input["Report.Param.Plot"]'),
    checkboxInput("Report.Param.Statistics", "Parameter statistics", F),
    conditionalSelection("Report.Param.Statistics",'input["Report.Param.Statistics"]')
  )
}

conditionalSelection <- function(id, condition, fid=T, dim=T, alg=T, scalex=F, scaley=F, plottype=NULL){
  ns <- NS(id)
  conditionalPanel(condition = condition, 
                   if (fid) selectInput(ns("FuncId"), label = "Function ids", choices = NULL, selected = NULL, multiple = T),
                   if (dim) selectInput(ns("DIM"), label = "Dimensions", choices = NULL, selected = NULL, multiple = T),
                   if (alg) selectInput(ns("Alg"), label = "Algorithms", choices = NULL, selected = NULL, multiple = T),
                   if (scalex) checkboxInput(ns("Scalex"), label = "Scale x-axis logarithmically", value = F),
                   if (scaley) checkboxInput(ns("Scaley"), label = "Scale y-axis logarithmically", value = F),
                   if (!is.null(plottype)) selectInput(ns("Type"), label = "Plot type", choices = plottype, selected = plottype[[1]])
                   )
}