library(shiny)
library(rsconnect)
library(tidyverse)
library(ggtext)
library(paletteer)
library(lubridate)
library(forcats)
library(Cairo)

options(scipen=9999, shiny.useragg=TRUE)

load("Alldata.RData")
#data <- read.csv("LAExcess.csv")
#excess <- read.csv("LAExcessSummary.csv")
#daydata <- read.csv("LACases.csv")

maxweek.ew <- max(subset(data, country!="Scotland" & !is.na(COVID.20))$week)
enddate.ew <- as.Date("2020-01-03")+weeks(maxweek.ew-1)
maxweek.s <- max(subset(data, country=="Scotland" & !is.na(COVID.20))$week)
enddate.s <- as.Date("2020-01-05")+weeks(maxweek.s-1)

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
    lag <- if_else(input$censoring==TRUE, 3, 0)

    LAdata <- data %>% 
      filter(name==LA & measure==input$measure) %>% 
      mutate(date=as.Date(date))
    LAexcess <- excess %>% filter(name==LA & measure==if_else(
      input$measure=="Registrations", "Registrations", "Occurrences"))
    LAdaydata <- daydata %>% filter(name==LA)
    LAv1 <- as.numeric(LAexcess[3])
    LAv2 <- as.numeric(LAexcess[5])
    
    #Set up flag for LAs heavily affected by trust mergers in NHS data
    merger <- case_when(
      LA %in% c("Bath and North East Somerset", "Bristol, City of", "North Somerset",
                "South Gloucestershire", "Cornwall", "Wiltshire", "Stroud",
                "Sedgemoor", "South Somerset", "Southend-on-Sea", "Thurrock",
                "Basildon", "Castle Point", "Rochford", "Luton", "Milton Keynes",
                "Bedford", "Buckinghamshire", "Dacorum", "St Albans") ~ 1,
      TRUE ~ 0
    )
    
    enddate <- if_else(LAdata$country[1]=="Scotland", enddate.s, enddate.ew)
    source <- if_else(LAdata$country[1]=="Scotland", "NRS", "ONS")
    source2 <- case_when(
      LAdaydata$country[1]=="England" ~ "PHE",
      LAdaydata$Region[1]=="Region" ~ "PHE",
      LAdaydata$country[1]=="Scotland" ~ "Scottish Government",
      LAdaydata$country[1]=="Wales" ~ "PHW",
      LAdaydata$country[1]=="Northern Ireland" ~ "DoHNI"
    )
    
    maxweek <- week(enddate)
    
    labpos <- max(sum(LAdata$AllCause.20[LAdata$week==maxweek]),
                  sum(LAdata$deaths.1519[LAdata$week==maxweek]))
    
    lab <- if_else(LAv1<0, 
                   paste0(round(LAv1,0), " (",round(LAv2*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"),
                   paste0("+", round(LAv1,0), " (+",round(LAv2*100,0), "%) deaths in 2020\ncompared to the average in 2015-19"))
    
    #Excess deaths graph
    if (input$plottype == 1){
      if (LA %in% c("England", "Wales") & input$measure=="Occurrences") {
        lab <- ""
      }
      subtitle=case_when(input$measure=="Registrations" ~ paste0("Weekly deaths (by date of registration) in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average in 2015-19</span><br>Data up to ", enddate),
                         input$measure=="Occurrences" & LA %in% c("England", "Wales") ~ paste0("Weekly deaths (by date of death) in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average in 2015-19</span><br>Data up to ", enddate, ". Date of occurrence data can have substantial reporting delays, particularly in recent weeks,<br>e.g. around 12% of deaths that have happened will be missing from the most recent 2 weeks of data."),
                         input$measure=="Occurrences" & LAdaydata$country[1]=="Scotland" ~ paste0("Weekly deaths (by date of death) in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average in 2015-19</span><br>Data up to ", enddate, ". Date of occurrence data can have substantial reporting delays, particularly in recent weeks,<br>when a significant proportion of the deaths that have happened will be missing from the most recent weeks of data."),
                         TRUE ~ paste0("Weekly deaths (by date of death) in <span style='color:red;'>2020</span> compared to <span style='color:Skyblue4;'>the average in 2015-19</span><br>Data up to ", enddate, ". Date of occurrence data can have substantial reporting delays, particularly in recent weeks,<br>e.g. around 12% of deaths that have happened will be missing from the most recent 2 weeks of data. <br> 2015-19 data by date of occurrence is not available at subnational level for England & Wales,<br>so 2015-19 registrations data are used here as the comparator. The impact of this is likely to be very small."))
      
      p <- LAdata %>% 
        group_by(week, date) %>% 
        summarise(deaths.1519=case_when(
          LA %in% c("England", "Wales") & input$measure=="Occurrences" ~ sum(death.1519v2, na.rm=TRUE),
          TRUE ~ sum(deaths.1519)), 
          AllCause.20=sum(AllCause.20)) %>% 
        ungroup() %>% 
        ggplot()+
        geom_line(aes(x=date, y=deaths.1519), colour="skyblue4")+
        geom_line(aes(x=date, y=AllCause.20), colour="red")+
        scale_x_date(name="")+
        scale_y_continuous(name="Deaths", limits=c(0,NA))+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(), plot.title.position="plot",
              plot.title=element_text(face="bold", size=rel(1.5)))+
        annotate("text", x=as.Date("2020-07-01"), y=max(labpos*1.5, labpos+20), 
                 label=lab,
                 hjust=0, colour="red", size=rel(5))+
        labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
             subtitle=subtitle,
             caption=paste0("Data from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
     
    }
    
    #Excess deaths by cause
    if (input$plottype == 2){
      subtitle=if_else(input$measure=="Registrations",
                       paste0("Excess deaths by date of registration in 2020 vs. 2015-19 average by cause.\nData up to ", enddate),
                       paste0("Excess deaths by date of occurence in 2020 vs. 2015-19 average by cause.\nData up to ", enddate, ". Date of occurrence data can have substantial reporting delays, particularly in recent weeks,\ne.g. around 12% of deaths that have happened will be missing from the most recent 2 weeks of data."))
      
      caption <- if_else(LAdata$country=="Scotland", paste0("Data from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"),
                      paste0("* Historical data is not available for week 53 to allow the calculation of excess other cause deaths\nData from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
      
      temp <- LAdata %>% 
        gather(cause, excess, c(8,16)) %>% 
        group_by(week, cause, date) %>% 
        summarise(excess=sum(excess))
      
      p <-  temp %>% 
        ggplot(aes(x=date, y=excess, fill=cause))+
        geom_bar(stat="identity")+
        geom_hline(yintercept=0, colour="Grey30")+
        scale_x_date(name="")+
        scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
        scale_fill_paletteer_d("LaCroixColoR::PinaFraise", name="Cause", labels=c("COVID-19", "Other causes"))+
        theme_classic(base_size=16)+
        theme(plot.title.position="plot",
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
             subtitle=subtitle,
             caption=caption)
      
      if (LAdata$country %in% c("England", "Wales")){
        p <- p+annotate("text", x=as.Date("2021-01-01"), 
                        y=0-(min(temp$excess, na.rm=TRUE)+max(temp$excess, na.rm=TRUE))/30, label="*")
      }
    }
    
    #Excess deaths by location
    
    if (input$plottype == 3){
      subtitle=if_else(input$measure=="Registrations",
                       paste0("Excess deaths by date of registration in 2020 vs. 2015-19 average by location\nData up to ", enddate),
                       paste0("Excess deaths by date of occurence in 2020 vs. 2015-19 average by location\nData up to ", enddate, ". Date of occurrence data can have substantial reporting delays, particularly in recent weeks,\ne.g. around 12% of deaths that have happened will be missing from the most recent 2 weeks of data."))
      
      caption <- if_else(LAdata$country=="Scotland", paste0("Data from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"),
                         paste0("* Historical data is not available for week 53 to allow the calculation of excess deaths in 2020\nData from ", source," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
      
      
      p <- ggplot(LAdata, aes(x=date, y=allexcess, fill=location))+
        geom_col()+
        geom_hline(yintercept=0, colour="Grey30")+
        scale_x_date(name="")+
        scale_y_continuous(name="Excess deaths vs. 2015-19 average")+
        scale_fill_paletteer_d("ggsci::planetexpress_futurama", name="Place of death")+
        theme_classic(base_size=16)+
        theme(plot.title.position="plot",
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("Excess deaths in ", LA, " during the pandemic"),
             subtitle=subtitle,
             caption=caption)
    
      
      if (LAdata$country %in% c("England", "Wales")){
        p <- p+annotate("text", x=as.Date("2021-01-01"), 
                        y=0+0-(min(LAdata$allexcess, na.rm=TRUE)+max(LAdata$allexcess, na.rm=TRUE))/30, 
                        label="*")
      }
    }
    
    #Cases vs. deaths
    if (input$plottype == 4){
      subtitle=if_else(input$measure=="Registrations",
                       paste0("Confirmed new COVID-19 <span style='color:#B25D91;'>cases</span> compared to confirmed COVID-19 <span style='color:#1BB6AF;'>deaths</span> by week of registration<br>Data up to ", enddate),
                       paste0("Confirmed new COVID-19 <span style='color:#B25D91;'>cases</span> compared to confirmed COVID-19 <span style='color:#1BB6AF;'>deaths</span> by week of occurence.<br>Data up to ", enddate))
      p <- LAdata %>% 
        group_by(week, date) %>% 
        summarise(excess=sum(COVID.20), cases=unique(cases)) %>% 
        ggplot()+
        geom_hline(yintercept=0, colour="Grey30")+
        geom_line(aes(x=date, y=cases), colour="#B25D91")+
        geom_line(aes(x=date, y=excess), colour="#1BB6AF")+
        #scale_x_date(name="Week", limits=c(0,maxweek+1))+
        scale_y_continuous(name="")+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(), plot.title.position="plot",
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("Timeline of COVID-19 in ", LA),
             subtitle=subtitle,
             caption=paste0("Data from ", source," & ", source2 ," | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
    }
    
    #cases plot
    if (input$plottype == 5){
      plotlabel <- if_else(input$scale=="Log", "Daily confirmed new cases (log scale)",
                       "Daily confirmed new cases")
      scaletype <- if_else(input$scale=="Log", "log2", "identity")
      p <- daydata %>% 
        filter(name==LA & date<max(date)-days(lag)) %>% 
        mutate(casesroll_avg=if_else(date>max(date)-days(lag), NA_real_, casesroll_avg)) %>% 
        ggplot()+
        geom_col(aes(x=date, y=cases), fill="skyblue2")+
        geom_line(aes(x=date, y=casesroll_avg), colour="red")+
        scale_x_date(name="Date")+
        scale_y_continuous(name=plotlabel, position="right", trans=scaletype)+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(), plot.title.position="plot",
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("Confirmed new COVID cases in ",LA),
             subtitle="Confirmed new COVID-19 cases identified through combined pillar 1 & 2 testing<br>and the <span style='color:Red;'>7-day rolling average",
             caption=paste0("Data from ", source2, " | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088"))
    }
    
    
    #Comparison of case rates with other LAs
    if (input$plottype == 6){
      plotlabel <- if_else(input$scale=="Log", "Daily confirmed new cases per 100,000\n(log scale)",
                           "Daily confirmed new cases per 100,000")
      scaletype <- if_else(input$scale=="Log", "log2", "identity")
      p <- ggplot()+
        geom_line(data=subset(daydata, !name %in% c("England", "Wales", "Scotland", "Northern Ireland") & 
                                !Region=="Region" & date<max(date)-days(lag)), 
                  aes(x=date, y=caserate_avg, group=name), colour="Grey80")+
        geom_line(data=subset(daydata, name==LA & date<max(date)-days(lag)), 
                  aes(x=date, y=caserate_avg), colour="#FF4E86")+
        scale_x_date(name="Date")+
        scale_y_continuous(name=plotlabel, position="right", trans=scaletype)+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(),
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("Rates of confirmed new COVID-19 cases in ", LA, " vs. the rest of the country"),
             subtitle=paste0("Rolling 7-day average of confirmed new COVID-19 cases per 100,000 inhabitants in <span style='color:#FF4E86;'>", LA, " </span><br>compared to other Local Authorities in England, Wales, Scotland & Northern Ireland"),
             caption="Data from PHE, PHW, ScotGov & DoHNI | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    
    #Comparison of case numbers with other LAs
    if (input$plottype == 7){
      plotlabel <- if_else(input$scale=="Log", "Daily confirmed new cases\n(log scale)",
                           "Daily confirmed new cases")
      scaletype <- if_else(input$scale=="Log", "log2", "identity")
      p <- ggplot()+
        geom_line(data=subset(daydata, !name %in% c("England", "Wales", "Scotland", "Northern Ireland") & 
                                !Region=="Region" & date<max(date)-days(lag)), 
                  aes(x=date, y=casesroll_avg, group=name), colour="Grey80")+
        geom_line(data=subset(daydata, name==LA & date<max(date)-days(lag)),
                  aes(x=date, y=casesroll_avg), colour="#FF4E86")+
        scale_x_date(name="Date")+
        scale_y_continuous(name=plotlabel, position="right", trans=scaletype)+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(),
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("Number of confirmed new COVID-19 cases in ", LA, " vs. the rest of the country"),
             subtitle=paste0("Rolling 7-day average of confirmed new COVID-19 cases in <span style='color:#FF4E86;'>", LA, " </span><br>compared to other Local Authorities in England, Wales, Scotland & Northern Ireland"),
             caption="Data from PHE, PHW, ScotGov & DoHNI | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    
    #Hospital admissions and deaths data
    if (input$plottype == 8){
      subtitle=case_when(
        merger==0 ~ "Daily number of confirmed new COVID-19 hospital <span style='color:#0361AA;'>admissions</span> and <span style='color:#BE0094;'>deaths</span> with 7-day rolling averages.<br>Data is published at NHS Trust level, so these figures are apportioned between Local Authorities<br>using data on the proportion of admissions to each trust originating from each LA in 2016-18.<br>A small number of deaths and admissions from mental health specialist trusts are excluded from these plots.<br> Admissions data is published weekly, so may by missing for more recent days.<br> Data for the most recent days for both measures may be an undercount due to delays in processing tests.",
        TRUE ~  "Daily number of confirmed new COVID-19 hospital <span style='color:#0361AA;'>admissions</span> and <span style='color:#BE0094;'>deaths</span> with 7-day rolling averages.<br>Data is published at NHS Trust level, so these figures are apportioned between Local Authorities<br>using data on the proportion of admissions to each trust originating from each LA in 2016-18.<br>This LA has been affected by a recent NHS trust merger, which NHS Digital reflect in admissions, but not deaths data.<br>As a result, the process for estimating the two series is slightly different and they should be compared with caution.<br>A small number of deaths and admissions from mental health specialist trusts are excluded from these plots.<br> Admissions data is published weekly, so may by missing for more recent days."
        )
      p <- ggplot()+
        geom_col(data=subset(daydata, name==LA), 
                  aes(x=date, y=admissions), fill="#6EA5CF")+
        geom_line(data=subset(daydata, name==LA), 
                  aes(x=date, y=admroll_avg), colour="#05375D")+
        geom_col(data=subset(daydata, name==LA), 
                 aes(x=date, y=-deaths), fill="#DB6DC2")+
        geom_line(data=subset(daydata, name==LA), 
                  aes(x=date, y=-deathsroll_avg), colour="#690052")+
        geom_hline(yintercept=0)+
        scale_x_date(name="Date", limits=c(as.Date("2020-08-01"), NA))+
        scale_y_continuous(position="right", labels=abs, name="")+
        annotate("text", x=as.Date("2020-08-20"), y=max(daydata$admissions[daydata$name==LA], na.rm=TRUE)/2,
                 label="Daily admissions", size=rel(5))+
        annotate("text", x=as.Date("2020-08-20"), y=-max(daydata$deaths[daydata$name==LA], na.rm=TRUE),
                 label="Daily hospital deaths", size=rel(5))+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(),
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("NHS England data on COVID-19 in hospitals in ", LA),
             subtitle=subtitle,
             caption="Data from NHS England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    
    #Comparison of admission rates with other LAs
    if (input$plottype == 9){
      p <- ggplot()+
        geom_line(data=subset(daydata, name!="England" & Region!="Region"), 
                  aes(x=date, y=admrate_avg, group=name), colour="Grey80")+
        geom_line(data=subset(daydata, name==LA), 
                  aes(x=date, y=admrate_avg), colour="#FF4E86")+
        scale_x_date(name="Date", limits=c(as.Date("2020-08-01"), NA))+
        scale_y_continuous(name="Daily confirmed new hospital admissions per 100,000", 
                           position="right")+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(),
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("COVID-19 cases in hospitals in ", LA, " vs. the rest of England"),
             subtitle=paste0("Rolling 7-day average of confirmed new COVID-19 admissions per 100,000 inhabitants in <span style='color:#FF4E86;'>", LA, " </span><br>compared to other Local Authorities in England.<br>Data is published at NHS Trust level, so these figures are apportioned between Local Authorities<br>using data on the proportion of admissions to each trust originating from each LA in 2016-18.<br>A small number of admissions from mental health specialist trusts are excluded from these plots.<br> Admissions data is published weekly, so may by missing for more recent days."),
             caption="Data from NHS England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    
    #Comparison of hospital death rates with other LAs
    if (input$plottype == 10){
      p <- ggplot()+
        geom_line(data=subset(daydata, name!="England" & Region!="Region"), 
                  aes(x=date, y=deathrate_avg, group=name), colour="Grey80")+
        geom_line(data=subset(daydata, name==LA), 
                  aes(x=date, y=deathrate_avg), colour="#FF4E86")+
        scale_x_date(name="Date", limits=c(as.Date("2020-08-01"), NA))+
        scale_y_continuous(name="Daily confirmed hospital deaths per 100,000", position="right")+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(),
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("COVID-19 hospitals deaths in ", LA, " vs. the rest of England"),
             subtitle=paste0("Rolling 7-day average of deaths in hospital of patients with a positive COVID-19 diagnosis per 100,000 inhabitants in <span style='color:#FF4E86;'>", LA, " </span><br>compared to other Local Authorities in England.<br>Data is published at NHS Trust level, so these figures are apportioned between Local Authorities<br>using data on the proportion of admissions to each trust originating from each LA in 2016-18.<br>A small number of deaths from mental health specialist trusts are excluded from these plots."),
             caption="Data from NHS England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    
    if (input$plottype == 11) {
      p <- daydata %>% 
        filter(name==LA & !is.na(COVID.Beds)) %>% 
        select(date, COVID.Beds, Other.Beds, Unocc.Beds, pop) %>% 
        gather(cause, count, c(2:4)) %>% 
        ggplot()+
        geom_area(aes(x=date, y=count*100000/pop, fill=cause), show.legend=FALSE)+
        scale_x_date(name="")+
        scale_y_continuous(name="Beds per 100,000 population")+
        scale_fill_manual(values=c("#FD625E", "#374649", "#00B8AA"), name="Occupied by", 
                          labels=c("Patient with COVID-19", "Other patient", "Unoccupied"))+
        theme_classic(base_size=16)+
        theme(plot.title=element_text(face="bold", size=rel(1.5)), plot.subtitle=element_markdown())+
        labs(title=paste0("Hospital bed occupancy in ", LA),
             subtitle=paste0("<span style='color:Grey60;'>Bed occupancy rate in England for <span style='color:#FD625E;'>COVID-19 patients</span>, <span style='color:#374649;'>non-COVID patients</span> and <span style='color:#00B8AA;'>unoccupied beds</span>.<br>Data is published at NHS Trust level, so these figures are apportioned between Local Authorities<br>using data on the proportion of admissions to each trust originating from each LA in 2016-18.<br>A small number of deaths from mental health specialist trusts are excluded from these plots."),
             caption="Data from NHS England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    
    if (input$plottype == 12) {
      p <- ggplot()+
        geom_line(data=subset(daydata, name!="England" & Region!="Region"),
                  aes(x=date, y=COVID.Beds/(COVID.Beds+Other.Beds+Unocc.Beds), 
                      group=name), colour="Grey80")+
        geom_line(data=subset(daydata, name==LA),
                  aes(x=date, y=COVID.Beds/(COVID.Beds+Other.Beds+Unocc.Beds)),
                  colour="#FF4E86")+
        scale_x_date(name="Date", limits=c(as.Date("2020-11-17"), NA))+
        scale_y_continuous(name="Proportion of beds occupied by patients with COVID-19", 
                           position="right", labels=scales::label_percent(accuracy=1))+
        theme_classic(base_size=16)+
        theme(plot.subtitle=element_markdown(),
              plot.title=element_text(face="bold", size=rel(1.5)))+
        labs(title=paste0("COVID-19 hospitals bed occupancy in ", LA, " vs. the rest of England"),
             subtitle=paste0("Proportion of hospital beds in <span style='color:#FF4E86;'>", LA, " </span>which are occupied by patients with a positive COVID-19 test<br>compared to other Local Authorities in England.<br>Data is published at NHS Trust level, so these figures are apportioned between Local Authorities<br>using data on the proportion of admissions to each trust originating from each LA in 2016-18.<br>A small number of deaths from mental health specialist trusts are excluded from these plots."),
             caption="Data from NHS England | Plot by @VictimOfMaths\nDOI: 10.15131/shef.data.12658088")
    }
    p     
  }, height=600)
  
}