library(shiny)
library(ggplot2)

#A clever person would make use of global.R instead of reading this in here as well as
#in server.R
cases <- read.csv("LACases.csv")

ui <- fluidPage(
  
  titlePanel("Visualising Local Authority COVID-19 deaths/cases data"),
  
  sidebarPanel(
    
    selectInput('LA', 'Select Nation or Local Authority', c("England", "Scotland", "Wales", "Northern Ireland",
                                                            sort(as.character(unique(cases$name))), multiple=FALSE,
                selected="England")),
    selectInput('plottype', 'Select plot', c("Total excess deaths (GB only)"=1,"Excess deaths by cause (GB only)"=2,"Excess deaths by location (GB only)"=3,
                                             "Cases vs deaths (GB only)"=4, "Case numbers"=5, 
                                             "Compare case rates"=6, "Compare case numbers"=7,
                                             "Hospital admission/death numbers (Eng only)"=8, 
                                             "Compare Hospital admission rates (Eng only)"=9,
                                             "Compare Hospital death rates (Eng only)"=10)),
    checkboxInput('censoring', "Censor incomplete case data\n(remove most recent 3 days' figures, which are heavily underreported)",
                  TRUE),
    radioButtons('scale', "Select y-axis scale for case plots", choices=c("Linear", "Log"), inline=TRUE)
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)