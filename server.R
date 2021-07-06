library(shiny)
library(tidyverse)
library(curl)
library(readxl)
library(RcppRoll)
library(paletteer)
library(ggstream)
library(lubridate)
library(extrafont)

server <- function(input, output) {
  
  output$plot <- renderPlot({
    LA <- input$LA
    LAType <- unique(subset(data, areaName==LA)$areaType)
    StartDate <- input$StartDate
    maxcases <- max(subset(data, areaType==LAType & 
                             date>=StartDate)$casesroll, na.rm=TRUE)
    maxrate <- max(subset(data, areaType %in% c("ltla", "nation", "region") & 
                            date>=StartDate)$caserateroll, na.rm=TRUE)
    minrate <- min(subset(data, areaType %in% c("ltla", "nation", "region") & 
                            date>=StartDate)$caserateroll, na.rm=TRUE)
    
    maxLArate <- max(subset(data, areaName==LA & date>=StartDate)$caserateroll, na.rm=TRUE)
    minLArate <- min(subset(data, areaName==LA & date>=StartDate)$caserateroll, na.rm=TRUE)
    
    if (input$plottype == 1){
      caselimit=if_else(input$fix==TRUE, maxcases, NA_real_)
      p <- data %>% 
        filter(areaType %in% c("ltla", "nation", "region") & areaName==LA & !is.na(casesroll) & 
                 date>=as.Date(StartDate)) %>% 
        ggplot()+
        geom_tile(aes(x=date, y=age, fill=casesroll))+
        scale_fill_paletteer_c("viridis::inferno", name="Daily cases", limits=c(0,caselimit))+
        scale_x_date(name="")+
        scale_y_discrete(name="Age")+
        theme_classic()+
        theme(plot.title=element_text(face="bold", size=rel(1.8)),
              text=element_text(family="Lato"))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122")
    }
    
    if (input$plottype == 2){
      ratelimit=if_else(input$fix==TRUE, maxrate, NA_real_)
      p <- data %>% 
        filter(areaType %in% c("ltla", "nation", "region") & areaName==LA & !is.na(caserateroll) & 
                 date>=as.Date(StartDate)) %>% 
        ggplot()+
        geom_tile(aes(x=date, y=age, fill=caserateroll))+
        scale_fill_paletteer_c("viridis::inferno", name="Daily cases\nper 100,000",
                               limits=c(0,ratelimit))+
        scale_x_date(name="")+
        scale_y_discrete(name="Age")+
        theme_classic()+
        theme(plot.title=element_text(face="bold", size=rel(1.8)),
              text=element_text(family="Lato"))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average rate per 100,000 of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122")
    }
    
    if (input$plottype == 3){
      ratelimit.upper=if_else(input$fix==TRUE, maxrate, maxLArate)
      ratelimit.lower=case_when(
        input$fix==TRUE & input$scale=="Log" ~ minrate+0.01,
        input$scale=="Log" ~ minLArate+0.01,
        TRUE ~ 0)
      plotlabel<- if_else(input$scale=="Log", "Daily cases per 100,000 (log scale)",
                          "Daily cases per 100,000")
      scaletype <- if_else(input$scale=="Log", "log2", "identity")
      p <- data %>% 
        filter(areaType %in% c("ltla", "nation", "region") & areaName==LA & !is.na(caserateroll) & 
                 date>=as.Date(StartDate)) %>% 
        ggplot()+
        geom_line(aes(x=date, y=caserateroll, colour=age))+
        scale_colour_paletteer_d("pals::stepped", name="Age")+
        scale_x_date(name="")+
        scale_y_continuous(name=plotlabel, position="right", trans=scaletype,
                           limits=c(ratelimit.lower,ratelimit.upper))+
        theme_classic()+
        theme(plot.title=element_text(face="bold", size=rel(1.8)),
              text=element_text(family="Lato"))+
        guides(colour=guide_legend(ncol=2, byrow=TRUE))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average rate per 100,000 of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122")
    }
    
    if (input$plottype == 4){
      ratelimit.upper=if_else(input$fix==TRUE, maxrate, maxLArate)
      ratelimit.lower=case_when(
        input$fix==TRUE & input$scale=="Log" ~ minrate+0.01,
        input$scale=="Log" ~ minLArate+0.01,
        TRUE ~ 0)
      plotlabel<- if_else(input$scale=="Log", "Daily cases per 100,000 (log scale)",
                          "Daily cases per 100,000")
      scaletype <- if_else(input$scale=="Log", "log2", "identity")
      p <-shortdata %>% 
        filter(areaType %in% c("ltla", "nation", "region") & areaName==LA & !is.na(caserateroll) & 
                 date>=as.Date(StartDate)) %>% 
        ggplot()+
        geom_line(aes(x=date, y=caserateroll, colour=ageband))+
        scale_colour_paletteer_d("awtools::a_palette", name="Age")+
        scale_x_date(name="")+
        scale_y_continuous(name=plotlabel, position="right", trans=scaletype,
                           limits=c(ratelimit.lower,ratelimit.upper))+
        theme_classic()+
        theme(plot.title=element_text(face="bold", size=rel(1.8)),
              text=element_text(family="Lato"))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average rate per 100,000 of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122") 
    }
    
    if (input$plottype == 5){
      caselimit <- if_else(input$fix==TRUE, maxcases, NA_real_)
      datelength <- as.integer(max(shortdata$date)-StartDate)
      bw <- case_when(
        datelength>180 ~ 0.2,
        datelength>80 ~ 0.4, 
        TRUE ~ 0.6)
      
      p <- shortdata %>% 
        filter(areaType %in% c("ltla", "nation", "region") & areaName==LA & !is.na(casesroll) & 
                 date>=as.Date(StartDate)) %>% 
        ggplot()+
        geom_stream(aes(x=date, y=casesroll, fill=ageband), bw=bw)+
        scale_fill_paletteer_d("awtools::a_palette", name="Age")+
        scale_x_date(name="",)+
        scale_y_continuous(name="Daily cases", labels=abs, position="right",
                           limits=c(-caselimit, caselimit))+
        theme_classic()+
        theme(plot.title=element_text(face="bold", size=rel(1.8)),
              text=element_text(family="Lato"))+
        labs(title=paste("Age patterns in COVID-19 cases in", LA),
             subtitle="Rolling 7-day average of confirmed new cases by age group",
             caption="Data from Public Health England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.13158122")
    }
    p     
  })
  
}