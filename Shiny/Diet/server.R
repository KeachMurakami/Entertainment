library(shiny)
library(ggplot2)
library(plyr)
library(dplyr)
library(data.table)
library(magrittr)
library(tidyr)
library(stringr)
library(data.table)
library(rCharts)
library(googleVis)
library(RCurl)
library(shinydashboard)
library(shinyga)
library(googlesheets)

shinyServer(function(input, output) {
  
  output$scat <- renderGvis({

  columns <- c("Date", "Official", "Weight", "BodyFat", "Event", "PlusMinus")
  BodyData <-
    gs_url("https://docs.google.com/spreadsheets/d/1X1kVbYNHdjueJIxVqmMCOmB231i6aLcP7ADAt7-Wdbw") %>%
    gs_read
  dataJ <-
    BodyData %>%
    select(1:6) %>%
    set_names(columns) %>%
    mutate(Person = "J")
  dataO <-
    BodyData %>%
    select(1:2, 7:10) %>%
    set_names(columns) %>%
    mutate(Person = "O")
  
#   FigData <-
#     BodyData %>%
#     filter(variable == as.character(input$Vars)) %>%
#     na.omit %>%
#     mutate(value = as.numeric(value)) %>%
#     dcast(data = ., formula = Date + Official + variable ~ Person)
#   
#   FigData %>%
#     googleVis::gvisLineChart(data = ., xvar = "Date", yvar = c("J", "O"),
#                              options=list(gvis.editor="Edit me!")) %>% plot

com <- function(x, y, z){
  if(!is.na(x)) temp <- str_join(y, z, sep = ": ")
  else temp <- NA
  return(temp)
}

bind_rows(dataJ, dataO) %>%
  mutate(PlusMinus = Vectorize(com)(x = Event, y = Person, z = PlusMinus)) %>%
  gvisAnnotationChart(., 
                      datevar="Date",
                      numvar= as.character(input$Vars), 
                      idvar="Person",
                      titlevar="Event", 
                      annotationvar="PlusMinus",
                      options=list(
                        gvis.editor = "Change type of the graph",
                        width=1000, height=500,
                        fill=1, displayExactValues=TRUE,
                        colors="['#0000ff','#00ff00']")
                      ) %>%
    return
  
#   scat <- rPlot(x = FigData$Date, y = FigData$value, data = FigData, color = FigData$Person, type = "point")
#   scat$addParams(with = 600, height = 450, dom = "scat")
#   return(scat)

#     ggplot(aes(x = Date, y = value, col = Person, shape = Official, group = Person)) +
#     geom_point() +
#     geom_line() %>%
#     return
  })
})