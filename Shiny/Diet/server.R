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
    
    handled <-
      handling %>%
      filter(variable == variable_) %>%
      mutate(Date = as.Date(paste0("20", Date)),
             value = as.numeric(value))
    
    handled %>%
      ggplot(aes(x = Date, y = value, col = Person)) +
      stat_smooth(aes(fill = Person), alpha = .3, n = input$Smoothing) +
      stat_smooth(aes(fill = Person), fullrange = T, method = "lm", alpha = 0, linetype="dashed") +
      geom_hline(yintercept = input$Targets, colour = "blue", linetype="dashed") +
      geom_point(size = 5) +
      geom_text(data = labeldata, aes(y = y_pos, label = value), angle = 90)
  })
  
  output$message <- renderTable({
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
    
    handled <-
      handling %>%
      filter(variable == variable_) %>%
      mutate(Date = as.Date(paste0("20", Date)),
             value = as.numeric(value))
  
    start_day <-
      handled$Date %>%
      as.numeric %>%
      min      
    
    lapply(1:2, function(i){
      reg <-
        handled %>%
        filter(Person == c("O", "J")[i]) %>%
        lm(value ~ Date, data = .) %>%
        .$coefficients
      
      if(reg[2] < 0){
        the_day <-
          (input$Targets > (reg[1] + reg[2] * (start_day:(start_day + 10000)))) %>%
#          (65 > (reg[1] + reg[2] * (start_day:(start_day + 10000)))) %>%
          data.frame(TF = ., ID = 1:10001) %>%
          filter(TF == TRUE) %>%
          slice(1) %>%
          .[, 2] 
        
        (the_day + start_day) %>%
          as.Date(., origin = "1970-01-01") %>%
          as.character %>%
          data.frame(Person = c("O", "J")[i], reg = reg[2], message = .) %>%
          return
      } else return(data.frame(Person = c("O", "J")[i], reg = reg[2], message = "Never comes."))
    }) %>%
    rbind_all %>%
    data.table::setnames(c("出走者", "1日の増減", "目標達成日！"))
  })
})