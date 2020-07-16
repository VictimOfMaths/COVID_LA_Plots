library(shiny)
library(ggplot2)

#A clever person would make use of global.R instead of reading this in here as well as
#in server.R
excess <- read.csv("LAExcessSummary.csv")

ui <- fluidPage(
  
  titlePanel("Visualising Local Authority COVID-19 deaths/cases data"),
  
  sidebarPanel(
    
    selectInput('LA', 'Select Nation or Local Authority', c("England", "Scotland", "Wales", 
                                     as.character(unique(excess$name))), multiple=FALSE),
    selectInput('plottype', 'Select plot', c("Total excess deaths"=1,"Excess deaths by cause"=2,"Excess deaths by location"=3,
                                             "Cases vs deaths (Cases E&W only)"=4, "Case numbers (E&W only)"=5, 
                                             "Cases by pillar (England only)"=6))
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)
