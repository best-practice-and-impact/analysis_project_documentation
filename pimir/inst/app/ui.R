#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  # Application title
  titlePanel("Perpetual Inventory Method"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
        fileInput("gfcf", "GFCF single timeseries"),
        fileInput("lifeLenghts", "Life Length etc."),
        selectInput("profileType", "Profile Type", choices=c("age-efficiency",
                                                             "age-price")),
        selectInput("profileFunction", "Profile Function", choices=c("lin", "geom", "const", "db", "hyp")),
        selectInput("retDist", "Retirement Distribution", choices=c("pnorm", "pweibull", "plnorm", "pgamma")),
        selectInput("rightTruncate", "Right Truncate", choices=c(FALSE, TRUE)),
        selectInput("combinationMethod", "Combination Method", choices=c("1", "2")),
        numericInput("discount", "Discount Rate", value=0, min=0, max=1),
        numericInput("inflation", "Inflation Rate", value=0, min=0, max=1),
        numericInput("offSet", "Offset", value=0, min=0, max=1),
        numericInput("profileParam", "Additional Parameters to Profile", value=0),
        submitButton("Apply PIM", icon("bolt"))
    ),

    # Show a plot of the generated distribution
    mainPanel(
       tabsetPanel(
         tabPanel("Capital Stock", plotOutput("capstock")),
         tabPanel("Retirement", plotOutput("retirement")),
         tabPanel("Data", tableOutput("tableData"))
       )
    )
  )
))
