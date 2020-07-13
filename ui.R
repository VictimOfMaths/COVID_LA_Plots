library(shiny)
library(ggplot2)

fluidPage(

  titlePanel("COVID19 excess deaths by LA"),

  sidebarPanel(

    selectInput('LA', 'Select LA', sort(data_summary$name)),
    selectInput('plottype', 'Select plot', c("All deaths"=1,"By cause"=2,"By location"=3,"Cases vs deaths"=4))
  ),

  mainPanel(
    plotOutput('plot')
  )
)
