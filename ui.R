#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(rCharts)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Pseudo-Boolean Post-Processing"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      width = 2,
      selectInput("fct", label = "fID",
                  choices = seq(4),
                  selected = 1),
      
       sliderInput("target",
                   "target",
                   min = 0,
                   max = 1000,
                   value = 200)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       fluidRow(
         column(12, align = "center",
                tabsetPanel(type = "tabs", 
                            tabPanel("raw data", plotOutput("convergence.raw", height = "800px")),
                            tabPanel("mean",
                              fluidRow(
                                column(7, align = "center",
                                  p('ERT against fvalues and beanplot'),
                                  plotOutput("mean.convergence", height = "800px")),
                                column(5, align = "left",
                                       p('histogram of the bean'),
                                       showOutput("histogram", "highcharts"))
                            )),
                            tabPanel("ECDF", showOutput("ecdf", "nvd3")))
         )
       )
    )
  )
))
