library(shiny)
library(rCharts)
library(shinydashboard)
library(shinyga)
library(googleVis)

shinyUI(pageWithSidebar(

  headerPanel("Diet Derby in 環境研"),
  sidebarPanel(
    selectInput("Vars", label = "パラメータ", selected = "Weight",
                choices = c("体重" = "Weight", "体脂肪率" = "BodyFat")),
    selectInput("Labels", label = "ラベルを表示", selected = "PLUSorMINUS",
                choices = c("がんばったorさぼった" = "PLUSorMINUS", "勝因or敗因" = "Event")),
     sliderInput("Targets", label = "目標体重", value = 63,
                 min = 60, max = 70, step = 0.1),
    sliderInput("Smoothing", label = "スムージングします", value = 5,
                min = 1, max = 100),
    tags$hr()
#    submitButton()
  ),
  mainPanel(
    plotOutput("scat"),
    p("線形回帰するとこんな感じです"),
    tableOutput("message")
  )
))
