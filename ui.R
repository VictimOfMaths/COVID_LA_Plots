library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  titlePanel("COVID19 excess deaths by LA"),
  
  sidebarPanel(
    
    selectInput('LA', 'Select LA', choices=names),
    selectInput('plottype', 'Select plot', c("All deaths"=1,"By cause"=2,"By location"=3,
                                             "Cases vs deaths (Cases E&W only)"=4, "Case numbers (E&W only)"=5, 
                                             "Cases by pillar (England only)"=6))
  ),
  
  mainPanel(
    plotOutput('plot')
  )
)
