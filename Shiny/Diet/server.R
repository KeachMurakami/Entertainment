library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(magrittr)
library(tidyr)
library(stringr)
library(rCharts)
library(googleVis)
library(RCurl)
library(shinydashboard)
library(shinyga)
library(googlesheets)

shinyServer(function(input, output) {
    Rawdata <- 
      gs_url("https://docs.google.com/spreadsheets/d/1X1kVbYNHdjueJIxVqmMCOmB231i6aLcP7ADAt7-Wdbw") %>%
      gs_read
  output$scat <- renderPlot({
    handling <-
      Rawdata %>%
      melt(id.vars = c("Date", "Official")) %>%
      separate(col = variable, into = c("variable", "Person"))
    
    variable_ <- input$Vars 
    y_pos <- ifelse(variable_ == "Weight", 71, 25)

    labeldata <-
      handling %>%
      filter(variable == input$Labels) %>%
      na.omit %>%
      mutate(Date = as.Date(paste0("20", Date)),
             y_pos = y_pos)
    
    handling %>%
      filter(variable == variable_) %>%
      mutate(Date = as.Date(paste0("20", Date)),
             value = as.numeric(value)) %>%
      ggplot(aes(x = Date, y = value, col = Person)) +
      stat_smooth(aes(fill = Person), alpha = .3, n = input$smoothing) +
      geom_point(size = 5) +
      geom_text(data = labeldata, aes(y = y_pos, label = value), angle = 90)
  })
})