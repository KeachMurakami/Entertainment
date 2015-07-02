library(shiny)
library(rCharts)
library(shinydashboard)
library(shinyga)
library(googleVis)

shinyUI(pageWithSidebar(

  headerPanel("Diet Derby"),
  sidebarPanel(
    selectInput("Vars", label = "Choose the Parameter", selected = "Weight",
                choices = c("Weight" = "Weight", "Body Fat" = "BodyFat")),
    tags$hr(),
    submitButton()
  ),
  mainPanel(
    htmlOutput("scat")
    #    showOutput("scat", "polycharts")
  )
))
