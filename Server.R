library(shiny)
library(rsconnect)
library(tidyverse)
library(ggtext)
library(paletteer)
library(lubridate)
library(forcats)

data <- read.csv("LAExcess.csv")
excess <- read.csv("LAExcessSummary.csv")
daydata <- read.csv("LACases.csv")

maxweek.ew <- max(subset(data, country!="Scotland" & !is.na(COVID.20))$week)
enddate.ew <- as.Date("2020-01-03")+weeks(maxweek.ew-1)
maxweek.s <- max(subset(data, country=="Scotland" & !is.na(COVID.20))$week)
enddate.s <- as.Date("2020-01-04")+weeks(maxweek.s-1)

daydata$date <- as.Date(daydata$date)

temp <- unique(as.character(data$name))
names <- as.list(temp)
names(names) <- unique(as.character(data$name))

###################
#LA-specific plots#
###################

server <- function(input, output) {
  
  output$plot <- renderPlot({
    LA <- input$LA
    
    LAdata <- data %>% filter(name==LA) 
    LAexcess <- excess %>% filter(name==LA)
    LAv1 <- as.numeric(LAexcess[3])
    LAv2 <- as.numeric(LAexcess[5])
    
    enddate <- if_else(LAdata$country[1]=="Scotland", enddate.s, enddate.ew)
    source <- if_else(LAdata$country[1]=="Scotland", "NRS", "ONS")
    
    maxweek <- week(enddate)
    
    labpos <- max(sum(LAdata$AllCause.20[LAdata$week==maxweek]),
                  sum(LAdata$deaths.1519[LAdata$week==maxweek]))
    
    lab <- if_else(LAv1<0, 
                   paste0(round(LAv1,0), " (",round(LAv2*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"),
                   paste0("+", round(LAv1,0), " (+",round(LAv2*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"))
    
    #Excess deaths graph
    if (input$plottype == 1){
      p <- LAdata %>% 
        group_by(week) %>% 
        summarise(deaths.1519=sum(deaths.1519), AllCause.20=sum(AllCause.20)) %>% 
        ggplot()+
        geom_line(aes(x=week, y=deaths.1519), colour="skyblue4")+
        geom_line(aes(x=week, y=AllCause.20), colour="red")+
        scale_x_continuous(name="Week")+
        scale_y_continuous(name="Deaths", limits=c(0,NA))+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(), plot.title.position="plot")+
        annotate("text", x=week(enddate)-2, y=max(labpos*1.5, labpos+20), 
                 label=lab,
                 hjust=0, colour="red", size=rel(5))+
        labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
             subtitle=paste0("Weekly deaths in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average in 2015-19</span> by date of occurence<br>Data up to ", enddate, ". Data for recent weeks is likely to be an undercount due to deaths<br>not yet having been fully processed."),
             caption=paste0("Data from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
    }
    
    #Excess deaths by cause
    if (input$plottype == 2){
      p <- LAdata %>% 
        gather(cause, excess, c(8,15)) %>% 
        group_by(week, cause) %>% 
        summarise(excess=sum(excess)) %>% 
        ggplot(aes(x=week, y=excess, fill=cause))+
        geom_bar(stat="identity")+
        geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
        scale_x_continuous(name="Week")+
        scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
        scale_fill_paletteer_d("LaCroixColoR::PinaFraise", name="Cause", labels=c("COVID-19", "Other causes"))+
        theme_classic(base_size=16)+
        theme(plot.title.position="plot")+
        labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
             subtitle=paste0("Excess deaths by date of occurence in 2020 vs. 2015-19 average by cause.\nData up to ", enddate, ". Data for recent weeks is likely to be an undercount due to deaths\nnot yet having been fully processed."),
             caption=paste0("Data from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
    }
    
    #Excess deaths by location
    
    if (input$plottype == 3){
      p <- ggplot(LAdata, aes(x=week, y=allexcess, fill=location))+
        geom_col()+
        geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
        scale_x_continuous(name="Week")+
        scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
        scale_fill_paletteer_d("ggsci::planetexpress_futurama", name="Place of death")+
        theme_classic(base_size=16)+
        theme(plot.title.position="plot")+
        labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
             subtitle=paste0("Excess deaths by occurence in 2020 vs. 2015-19 average by location.\nData up to ", enddate, ". Data for recent weeks is likely to be an undercount due to deaths\nnot yet having been fully processed."),
             caption=paste0("Data from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
    }
    
    #Cases vs. deaths
    if (input$plottype == 4){
      p <- LAdata %>% 
        group_by(week) %>% 
        summarise(excess=sum(COVID.20), cases=unique(cases)) %>% 
        ggplot()+
        geom_segment(aes(x=0.5, xend=maxweek+0.5, y=0, yend=0), colour="Grey30")+
        geom_line(aes(x=week, y=cases), colour="#B25D91")+
        geom_line(aes(x=week, y=excess), colour="#1BB6AF")+
        scale_x_continuous(name="Week", limits=c(0,maxweek+1))+
        scale_y_continuous(name="")+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(), plot.title.position="plot")+
        labs(title=paste0("Timeline of COVID-19 in ", LA),
             subtitle=paste0("Confirmed new COVID-19 <span style='color:#B25D91;'>cases</span> compared to confirmed COVID-19 <span style='color:#1BB6AF;'>deaths</span> by week of occurence.<br>Data up to ", enddate),
             caption=paste0("Data from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
    }
    
    #cases plot
    #England & Wales only
    if (input$plottype == 5){
      p <- daydata %>% 
        filter(name==LA) %>% 
        ggplot()+
        geom_col(aes(x=date, y=cases), fill="skyblue2")+
        geom_line(aes(x=date, y=casesroll_avg), colour="red")+
        scale_x_date(name="Date")+
        scale_y_continuous(name="Daily confirmed new cases")+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(), plot.title.position="plot")+
        labs(title=paste0("Confirmed new COVID cases in ",LA),
             subtitle="Confirmed new COVID-19 cases identified through combined pillar 1 & 2 testing<br>and the <span style='color:Red;'>7-day rolling average",
             caption="Data from PHE | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    
    #Experimental pillar 1 vs. 2 tests numbers
    #ENGLAND ONLY
    if (input$plottype == 6){
      p <- daydata %>% 
        filter(name==LA) %>% 
        ggplot()+
        geom_line(aes(x=date, y=p1cases), colour="#FF4E86")+
        geom_line(aes(x=date, y=p2cases), colour="#FF9E44")+
        geom_line(aes(x=date, y=casesroll_avg), colour="navyblue")+
        scale_x_date(name="Date")+
        scale_y_continuous(name="Daily confirmed new cases")+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(), plot.title.position="plot")+
        labs(title=paste0("Confirmed new COVID cases in ",LA),
             subtitle="Confirmed new COVID-19 cases identified through <span style='color:#FF4E86;'>Pillar 1</span> and <span style='color:#FF9E44;'>Pillar 2</span> testing and the <span style='color:navyblue;'>7-day rolling average</span>.<br>PHE changed their methodology on 1st July and so pillar-specific data is not available since then.<br>Rolling average based on new approach.<br>Pillar-specific figures are estimated from the old approach and may be subject to some double-counting",
             caption="Data from PHE | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    
    #Comparison of case rates with other LAs
    #ENGLAND & WALES ONLY
    if (input$plottype == 7){
      p <- ggplot()+
        geom_line(data=subset(daydata, !name %in% c("England", "Wales")), aes(x=date, y=caserate_avg, group=name), colour="Grey80")+
        geom_line(data=subset(daydata, name==LA), aes(x=date, y=caserate_avg), colour="#FF4E86")+
        scale_x_date(name="Date")+
        scale_y_continuous(name="Daily confirmed new cases per 100,000")+
        theme_classic()+
        theme(plot.subtitle=element_markdown())+
        labs(title=paste0("Rates of confirmed new COVID-19 cases in ", LA, " vs. the rest of the country"),
             subtitle=paste0("Rolling 7-day average of confirmed new COVID-19 cases per 100,000 inhabitants in <span style='color:#FF4E86;'>", LA, " </span><br>compared to other Local Authorities in England & Wales"),
             caption="Data from PHE & PHW | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    p     
  }, height=600)
  
}