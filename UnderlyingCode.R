rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ukcovid19) #remotes::install_github("publichealthengland/coronavirus-dashboard-api-R-sdk")
library(ggtext)
library(paletteer)
library(lubridate)
library(forcats)
library(RcppRoll)
library(gt)

###################
#Things to updated#
###################

#England mortality data - updated on Tuesday mornings
EngMortUrl <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/healthandsocialcare/causesofdeath/datasets/deathregistrationsandoccurrencesbylocalauthorityandhealthboard/2021/lahbtables2021week18.xlsx"
#Scottish mortality data - updated on Wednesday lunchtime
ScotMortUrl <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-council-area-location.xlsx"
ScotMortRange <- 9748
ScotMortUrl2 <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-health-board-council-area-2020-2021.xlsx"
ScotMortRange2 <- "BV"
#Admissions data which is published weekly on a Thursday (next update on 27th May)
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-hospital-activity/
admurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/Weekly-covid-admissions-and-beds-publication-210520-1.xlsx"
#Hospital deaths data which is published daily
#https://www.england.nhs.uk/statistics/statistical-work-areas/covid-19-daily-deaths/
deathurl <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/05/COVID-19-total-announced-deaths-24-May-2021.xlsx"
#Increment by 7 when each new report is published
admrange <- "AR"
occrange <- "AT"
#Increment by 1 each day
deathrange <- "QH"
#Set latest date of admissions data
admdate <- as.Date("2021-05-16")

###################################################################################
#Weekly data
#2021 Data

#Read in 2021 data for England
temp <- tempfile()
temp <- curl_download(url=EngMortUrl, destfile=temp, quiet=FALSE, mode="wb")

#Occurrences
data21 <- read_excel(temp, sheet=6, col_names=FALSE)[-c(1:4),]
colnames(data21) <- c("code", "type", "name", "cause", "week", "location", "deaths.20")
data21 <- subset(data21, type=="Local Authority")[,-c(2)]

data21$deaths.20 <- as.numeric(data21$deaths.20)
data21$week <- as.numeric(data21$week)+53
data21$measure <- "Occurrences"

maxweek.ew <- max(data21$week)
enddate.ew <- as.Date("2020-01-03")+weeks(maxweek.ew-1)

#Registrations
data21.2 <- read_excel(temp, sheet=4, col_names=FALSE)[-c(1:4),]
colnames(data21.2) <- c("code", "type", "name", "cause", "week", "location", "deaths.20")
data21.2 <- subset(data21.2, type=="Local Authority")[,-c(2)]

data21.2$deaths.20 <- as.numeric(data21.2$deaths.20)
data21.2$week <- as.numeric(data21.2$week)+53
data21.2$measure <- "Registrations"

data21 <- bind_rows(data21, data21.2)

#Spread causes
data21 <- pivot_wider(data21, names_from="cause", values_from="deaths.20")

#Read in 2020 data for England
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek53.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
#Occurrences
data20 <- read_excel(temp, sheet=6, col_names=FALSE)[-c(1:4),]
colnames(data20) <- c("code", "type", "name", "cause", "week", "location", "deaths.20")
data20 <- subset(data20, type=="Local Authority")[,-c(2)]

data20$deaths.20 <- as.numeric(data20$deaths.20)
data20$week <- as.numeric(data20$week)
data20$measure <- "Occurrences"

maxweek.ew <- max(data21$week)
enddate.ew <- as.Date("2020-01-03")+weeks(maxweek.ew-1)

#Registrations
data20.2 <- read_excel(temp, sheet=4, col_names=FALSE)[-c(1:4),]
colnames(data20.2) <- c("code", "type", "name", "cause", "week", "location", "deaths.20")
data20.2 <- subset(data20.2, type=="Local Authority")[,-c(2)]

data20.2$deaths.20 <- as.numeric(data20.2$deaths.20)
data20.2$week <- as.numeric(data20.2$week)
data20.2$measure <- "Registrations"

data20 <- bind_rows(data20, data20.2)

#Spread causes
data20 <- pivot_wider(data20, names_from="cause", values_from="deaths.20")

#Merge with 2021 data
data2021 <- bind_rows(data20, data21)

#Read in 2015-19 historic data for England & Wales
#Historic data is only available for registrations at subnational level
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/adhocs/11826fiveyearaverageweeklydeathsbylocalauthorityandplaceofoccurrenceenglandandwalesdeathsregistered2015to2019/weeklyfiveyearaveragesbylaandplaceofoccurrence20152019.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519 <- read_excel(temp, sheet=2, col_names=FALSE)[-c(1:3),]
colnames(data1519) <- c("code", "name", "week", "location", "deaths.1519")

data1519$deaths.1519 <- as.numeric(data1519$deaths.1519)
data1519$week <- as.numeric(data1519$week)
data1519 <- data1519 %>% drop_na(name)

#Address merging of Aylesbury Vale, Chiltern and South Bucks into Bucks
data1519$name <- if_else(data1519$name %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"), 
                         "Buckinghamshire", data1519$name)
data1519$code <- if_else(data1519$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"), 
                         "E06000060", data1519$code)

data1519 <- data1519 %>% 
  group_by(week, location, name, code) %>% 
  summarise(deaths.1519=sum(deaths.1519)) %>% 
  ungroup()

#Duplicate for reg/occ split
temp <- data1519 %>% 
  mutate(measure="Registrations")

data1519 <- data1519 %>% 
  mutate(measure="Occurrences") %>% 
  bind_rows(temp)

#Duplicate first weeks to align with 2021 data
data1519 <- data1519 %>% 
  filter(week<=(max(data21$week)-53)) %>% 
  mutate(week=week+53) %>% 
  bind_rows(data1519)

data.ew <- merge(data1519, data2021, all=TRUE)

#Combine Cornwall & Isles of Scilly
data.ew$code <- if_else(data.ew$code=="E06000053", "E06000052", data.ew$code)
data.ew$name <- if_else(data.ew$name=="Isles of Scilly", "Cornwall", data.ew$name)

#Combine Hackney & City of London
data.ew$code <- if_else(data.ew$code=="E09000001", "E09000012", data.ew$code)
data.ew$name <- if_else(data.ew$name=="City of London", "Hackney and City of London", data.ew$name)
data.ew$name <- if_else(data.ew$name=="Hackney", "Hackney and City of London", data.ew$name)

#Compress locations
data.ew$location <- case_when(
  data.ew$location %in% c("Elsewhere", "Home", "Hospice", "Other communal establishment") ~ "Home/Other",
  TRUE ~ data.ew$location)

data.ew <- data.ew %>% 
  group_by(code, name, location, week, measure) %>% 
  summarise(deaths.1519=sum(deaths.1519), AllCause.20=sum(`All causes`), COVID.20=sum(`COVID 19`)) %>% 
  mutate(Other.20=AllCause.20-COVID.20) %>% 
  ungroup()

#Bring in Scottish deaths data 
#2020/21 data

#Occurrences
temp <- tempfile()
source <- ScotMortUrl
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20.s <- read_excel(temp, sheet=2, range=paste0("A5:E", ScotMortRange), col_names=FALSE)
colnames(data20.s) <- c("week", "name", "location", "cause", "deaths")
data20.s$week <- case_when(
  substr(data20.s$week, 1, 2)=="20" ~ as.numeric(substr(data20.s$week, 4,6)),
  TRUE ~ as.numeric(substr(data20.s$week, 4,6))+53
)

maxweek.s <- max(data20.s$week)
enddate.s <- as.Date("2020-01-04")+weeks(maxweek.s-1)

data20.s$cause <- if_else(data20.s$cause=="Non-COVID-19", "Other.20", "COVID.20")

data20.s <- data20.s %>% 
  group_by(week, name, location, cause) %>% 
  summarise(deaths=sum(deaths)) %>% 
  ungroup() %>% 
  spread(cause, deaths)
data20.s$COVID.20 <- replace_na(data20.s$COVID.20, 0)
data20.s$Other.20 <- replace_na(data20.s$Other.20, 0)
data20.s$measure <- "Occurrences"

#Registrations
temp <- tempfile()
source <- ScotMortUrl2
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20.s.2 <- read_excel(temp, sheet=4, range=paste0("A5:", ScotMortRange2,"132"), col_names=FALSE)
colnames(data20.s.2) <- c("name", "location", 1:maxweek.s)

data20.s.2 <- data20.s.2 %>% 
  fill(name, .direction="down") %>% 
  gather(week, COVID.20, c(3:ncol(.)))

data20.s.3 <- read_excel(temp, sheet=5, range=paste0("A5:", ScotMortRange2,"132"), col_names=FALSE)
colnames(data20.s.3) <- c("name", "location", 1:maxweek.s)

data20.s.3 <- data20.s.3 %>% 
  fill(name, .direction="down") %>% 
  gather(week, All.20, c(3:ncol(.))) 

data20.s.2 <- data20.s.2 %>% 
  merge(data20.s.3) %>% 
  mutate(Other.20=All.20-COVID.20, week=as.numeric(week), 
         measure="Registrations") %>% 
  select(name, location, week, COVID.20, Other.20, measure)

data20.s <- bind_rows(data20.s, data20.s.2)

#2015-19 data
#Occurrences
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-council-area-location-15-19.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519.s <- read_excel(temp, sheet=2, range="A5:E25207", col_names=FALSE)
colnames(data1519.s) <- c("week", "name", "location", "year", "deaths")
data1519.s$week <- as.numeric(data1519.s$week)
data1519.s$measure <- "Occurrences"

#Registrations
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-council-areas.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519.s.2 <- read_excel(temp, sheet=2, range="A4:E25193", col_names=FALSE)
colnames(data1519.s.2) <- c("week", "name", "year", "location", "deaths")
data1519.s.2$week <- as.numeric(data1519.s.2$week)
data1519.s.2$measure <- "Registrations"

data1519.s <- bind_rows(data1519.s, data1519.s.2)

#Take 5 year averages
data1519.s <- data1519.s %>% 
  group_by(week, name, location, measure) %>% 
  summarise(deaths.1519=mean(deaths)) %>% 
  ungroup()

#Add 15-19 averages as comparators for 2021 data
data1519.s <- data1519.s %>% 
  filter(week<=maxweek.s-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(data1519.s)

#Merge years
data.s <- merge(data1519.s, data20.s, all=TRUE)

data.s$deaths.1519 <- replace_na(data.s$deaths.1519)
data.s$COVID.20 <- if_else(is.na(data.s$COVID.20) & data.s$week<=maxweek.s, 0, data.s$COVID.20)
data.s$Other.20 <- if_else(is.na(data.s$Other.20) & data.s$week<=maxweek.s, 0, data.s$Other.20)

#Compress locations to match EW
data.s$location <- case_when(
  data.s$location=="Care Home" ~ "Care home",
  data.s$location %in% c("Home / Non-institution", "Other institution") ~ "Home/Other",
  TRUE ~ "Hospital"
)

data.s <- data.s %>% 
  group_by(week, name, location, measure) %>% 
  summarise(deaths.1519=sum(deaths.1519, na.rm=TRUE), 
            across(c("COVID.20", "Other.20"), sum)) %>% 
  mutate(AllCause.20=COVID.20+Other.20) %>% 
  ungroup()

#Bring in Scottish LA codes
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/35de30c6778b463a8305939216656132_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
codelookup <- read.csv(temp)[,c(2,3)]
colnames(codelookup) <- c("code", "name")
data.s <- merge(data.s, codelookup, all.x=TRUE)

#Merge countries
data <- bind_rows(data.ew, data.s)

data$country <- case_when(
  substr(data$code,1,1)=="E" ~ "England",
  substr(data$code,1,1)=="W" ~ "Wales",
  substr(data$code,1,1)=="S" ~ "Scotland")

#Bring in LA populations
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fpopulationandmigration%2fpopulationestimates%2fdatasets%2fpopulationestimatesforukenglandandwalesscotlandandnorthernireland%2fmid20182019laboundaries/ukmidyearestimates20182019ladcodes.xls"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LApop <- read_excel(temp, sheet="MYE2-All", range="A5:D435", col_names=TRUE)
colnames(LApop) <- c("code", "name", "geography", "pop")

#Merge isles of Scilly in with Cornwall
LApop$code <- if_else(LApop$code=="E06000053", "E06000052", LApop$code)
LApop$name <- if_else(LApop$name=="Isles of Scilly", "Cornwall", LApop$name)

#Address merging of Aylesbury Vale, Chiltern and South Bucks into Bucks
LApop$name <- if_else(LApop$name %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"), 
                      "Buckinghamshire", LApop$name)
LApop$code <- if_else(LApop$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"), 
                      "E06000060", LApop$code)

#Merge City of London & Hackney
LApop$code <- if_else(LApop$code=="E09000001", "E09000012", LApop$code)
LApop$name <- if_else(LApop$name=="City of London", "Hackney and City of London", LApop$name)
LApop$name <- if_else(LApop$name=="Hackney", "Hackney and City of London", LApop$name)

LApop <- LApop %>% 
  group_by(name, code) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

data <- merge(data, LApop, all.x=TRUE)

#Bring in Regions
temp <- tempfile()
source <- "https://opendata.arcgis.com/datasets/0c3a9643cc7c4015bb80751aad1d2594_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
LADtoRegion <- read.csv(temp)[,c(1,4)]
colnames(LADtoRegion) <- c("code", "Region")

data <- merge(data, LADtoRegion,all.x=TRUE)

data$Region <- case_when(
  is.na(data$Region) & data$country=="Scotland" ~ "Scotland",
  is.na(data$Region) & data$country=="Wales" ~ "Wales",
  is.na(data$Region) & data$code %in% c("E06000058", "E06000059", "E07000246") ~ "South West",
  is.na(data$Region) & data$code %in% c("E07000244", "E07000245") ~ "East of England",
  is.na(data$Region) & data$code=="E06000060" ~ "South East",
  TRUE ~ as.character(data$Region))

#Generate regional summaries
data.reg <- data %>% 
  filter(country=="England") %>% 
  group_by(week, Region, location, measure) %>% 
  summarise(across(c("deaths.1519", "AllCause.20", "COVID.20", "Other.20", "pop"), sum)) %>% 
  mutate(name=Region, Region="Region", country="England") %>% 
  ungroup()

#Generate national summaries
data.nat <- data %>% 
  group_by(week, country, location, measure) %>% 
  summarise(across(c("deaths.1519", "AllCause.20", "COVID.20", "Other.20", "pop"), sum), country=unique(country)) %>% 
  mutate(name=country, Region="Nation") %>% 
  ungroup()

data <- bind_rows(data, data.reg, data.nat)

#Replace overall occurrences data for England & Wales with actual data from monthly mortality analysis file

#(rather than using registrations as a proxy)
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fbirthsdeathsandmarriages%2fdeaths%2fdatasets%2fmonthlymortalityanalysisenglandandwales%2fdecember2020/monthlymortalityanalysisdecember15012021164301.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

Eng.occ <- read_excel(temp, sheet="Table 12", range=paste0("A13:B", 379))
colnames(Eng.occ) <- c("date", "deaths.1519")
Eng.occ$name <- "England"

Wal.occ <- read_excel(temp, sheet="Table 12", range=paste0("A13:G", 379))[,c(1,7)]
colnames(Wal.occ) <- c("date", "deaths.1519")
Wal.occ$name <- "Wales"

Occ.data <- bind_rows(Eng.occ, Wal.occ) %>% 
  mutate(week=floor((as.Date(date)-as.Date("2020-01-04"))/7)+2) %>% 
  group_by(name, week) %>% 
  summarise(death.1519v2=sum(deaths.1519)) %>% 
  ungroup() %>% 
#Add in a dummy location (since we're only going to use this data in the aggregate graph)
  mutate(location="Home/Other", measure="Occurrences",
         #Adjust for wraparound of weeks at beginning/end of year
         death.1519v2=case_when(
           week==1 ~ death.1519v2*7/3,
           week==53 ~ death.1519v2*7/6,
           TRUE~death.1519v2
         ))

#Duplicate weeks to match early 2021
Occ.data <- Occ.data %>% 
  filter(week<=maxweek.ew-53) %>% 
  mutate(week=week+53) %>% 
  bind_rows(Occ.data)

data <- merge(data, Occ.data, by=c("week", "name", "location", "measure"), all.x=TRUE)

#Calculate excesses
data$allexcess <- case_when(
  data$country=="Scotland" & data$week<=maxweek.s ~ data$AllCause.20-data$deaths.1519,
  data$country!="Scotland" & data$week<=maxweek.ew ~ data$AllCause.20-data$deaths.1519)
data$excessrate <- data$allexcess*100000/data$pop
data$othexcess <- case_when(
  data$country=="Scotland" & data$week<=maxweek.s ~ data$Other.20-data$deaths.1519,
  data$country!="Scotland" & data$week<=maxweek.ew ~ data$Other.20-data$deaths.1519)
data$COVIDrate <- data$COVID.20*100000/data$pop

#############################################################
#Daily data from PHE API
APIdata <- get_data(filters="areaType=ltla", structure=list(date="date",
                                                             name="areaName",
                                                             code="areaCode",
                                                             cases="newCasesBySpecimenDate"))

casedata <- APIdata %>% 
  mutate(date=as.Date(date)) %>% 
  filter(date>as.Date("2020-03-01")) %>% 
  mutate(cases=if_else(is.na(cases), 0, as.double(cases)))

mindate <- min(as.Date(casedata$date))
maxdate <- max(as.Date(casedata$date))

#Address merging of Aylesbury Vale, Chiltern and South Bucks into Bucks
casedata$name <- if_else(casedata$name %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"), 
                           "Buckinghamshire", as.character(casedata$name))
casedata$code <- if_else(casedata$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"), 
                           "E06000060", as.character(casedata$code))

#Align names
casedata$name <- if_else(casedata$name=="Cornwall and Isles of Scilly", "Cornwall", 
                         casedata$name)

casedata$name <- if_else(casedata$name=="Comhairle nan Eilean Siar", 
                         "Na h-Eileanan Siar", casedata$name)

casedata <- casedata %>% 
  group_by(name, code, date) %>% 
  summarise(cases=sum(cases)) %>% 
  ungroup()

#Set up skeleton dataframe, merging City of London and Hackney
daydata <- data.frame(code=rep(unique(subset(data, !Region %in% c("Region", "Nation"))$code),
                               each=maxdate-mindate+1),
                      name=rep(unique(subset(data, !Region %in% c("Region", "Nation"))$name),
                               each=maxdate-mindate+1),
                      date=rep(seq.Date(from=mindate, to=maxdate, by="day"), 
                               times=length(unique(subset(data, !Region %in% c("Region", "Nation"))$code))))

#Add in NI LTLAs which are missing from deaths data
NIdaydata <- data.frame(code=rep(unique(subset(casedata, substr(code,1,1)=="N")$code),
                                 each=maxdate-mindate+1),
                        name=rep(unique(subset(casedata, substr(code,1,1)=="N")$name),
                                 each=maxdate-mindate+1),
                        date=rep(seq.Date(from=mindate, to=maxdate, by="day"), 
                                 times=length(unique(subset(casedata, substr(code,1,1)=="N")$code))))

daydata <- bind_rows(daydata, NIdaydata)

#merge in cases
daydata <- merge(daydata, casedata, by=c("name", "code", "date"), all.x=TRUE) %>% 
  mutate(cases=if_else(is.na(cases), 0, as.double(cases)))

daydata$country <- case_when(
  substr(daydata$code,1,1)=="E" ~ "England",
  substr(daydata$code,1,1)=="W" ~ "Wales",
  substr(daydata$code,1,1)=="S" ~ "Scotland",
  substr(daydata$code,1,1)=="N" ~ "Northern Ireland"
  )

daydata$date <- as.Date(daydata$date)

###################################################
#Hospital admissions, occupancy and deaths in hospitals

#Read in admissions
#First data up to 6th April
admurl.old <- "https://www.england.nhs.uk/statistics/wp-content/uploads/sites/2/2021/04/Weekly-covid-admissions-and-beds-publication-210429-up-to-210406.xlsx"

temp1 <- tempfile()
temp1 <- curl_download(url=admurl.old, destfile=temp1, quiet=FALSE, mode="wb")
raw.adm.old <- read_excel(temp1, sheet="Hosp ads & diag", range=paste0("B25:IS508"), col_names=FALSE)

#Read in more recent data
temp2 <- tempfile()
temp2 <- curl_download(url=admurl, destfile=temp2, quiet=FALSE, mode="wb")
raw.adm <- read_excel(temp2, sheet="Hosp ads & diag", range=paste0("B25:",admrange,"508"), col_names=FALSE)

#Tidy up data
admissions.old <- raw.adm.old %>% 
  gather(date, admissions, c(4:ncol(raw.adm.old))) %>% 
  mutate(date=as.Date("2020-08-01")+days(as.integer(substr(date, 4, 6))-4)) %>% 
  rename(Region=...1, code=...2, name=...3)

admissions <- raw.adm %>% 
  gather(date, admissions, c(4:ncol(raw.adm))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.integer(substr(date, 4, 6))-4)) %>% 
  rename(Region=...1, code=...2, name=...3) %>% 
  bind_rows(admissions.old)

#Read in occupancy data
#First COVID-19
raw.occ.CV.old <- read_excel(temp1, sheet="Adult G&A Beds Occupied COVID", 
                         range=paste0("C25:EO167"), col_names=FALSE)

raw.occ.CV <- read_excel(temp2, sheet="Adult G&A Beds Occupied COVID", 
                         range=paste0("C25:", occrange, "167"), col_names=FALSE)

#Tidy up data
occ.cv.old <- raw.occ.CV.old %>% 
  gather(date, COVID, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2020-11-17")+days(as.numeric(substr(date, 4,7))-3)) %>% 
  rename(code=`...1`, name=`...2`)

occ.cv <- raw.occ.CV %>% 
  gather(date, COVID, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-3)) %>% 
  rename(code=`...1`, name=`...2`) %>% 
  bind_rows(occ.cv.old)

#Second non-COVID-19
raw.occ.oth.old <- read_excel(temp1, sheet="Adult G&A Bed Occupied NonCOVID", 
                          range=paste0("C25:EO167"), col_names=FALSE)

raw.occ.oth <- read_excel(temp2, sheet="Adult G&A Bed Occupied NonCOVID", 
                         range=paste0("C25:", occrange, "167"), col_names=FALSE)

#Tidy up data
occ.oth.old <- raw.occ.oth.old %>% 
  gather(date, Other, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2020-11-17")+days(as.numeric(substr(date, 4,7))-3)) %>% 
  rename(code=`...1`, name=`...2`)

occ.oth <- raw.occ.oth %>% 
  gather(date, Other, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-3)) %>% 
  rename(code=`...1`, name=`...2`)%>% 
  bind_rows(occ.oth.old)

#Third Unoccupied beds
raw.unocc.old <- read_excel(temp1, sheet="Adult G&A Beds Unoccupied", 
                        range=paste0("C25:EO167"), col_names=FALSE)

raw.unocc <- read_excel(temp2, sheet="Adult G&A Beds Unoccupied", 
                         range=paste0("C25:", occrange, "167"), col_names=FALSE)

#Tidy up data
unocc.old <- raw.unocc.old %>% 
  gather(date, Unoccupied, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2020-11-17")+days(as.numeric(substr(date, 4,7))-3)) %>% 
  rename(code=`...1`, name=`...2`)

unocc <- raw.unocc %>% 
  gather(date, Unoccupied, c(3:ncol(.))) %>% 
  mutate(date=as.Date("2021-04-07")+days(as.numeric(substr(date, 4,7))-3)) %>% 
  rename(code=`...1`, name=`...2`) %>% 
  bind_rows(unocc.old)

occupancy <- merge(occ.cv, occ.oth) %>% 
  merge(unocc)

#Read in deaths
temp <- tempfile()
temp <- curl_download(url=deathurl, destfile=temp, quiet=FALSE, mode="wb")
raw.deaths <- read_excel(temp, sheet="Tab4 Deaths by trust", range=paste0("B19:",deathrange,"240"), col_names=FALSE)[,-c(2,5)]

#Tidy up data
deaths <- raw.deaths %>% 
  gather(date, deaths, c(4:ncol(raw.deaths))) %>% 
  mutate(date=as.Date("2020-03-01")+days(as.integer(substr(date, 4, 6))-6)) %>% 
  rename(Region=...1, code=...3, name=...4)

#Address mergers which happened in April 2020 (so are included in the admissions data, but not deaths)
deaths <- deaths %>% 
  mutate(code=case_when(
    code %in% c("RDD", "RQ8") ~ "RAJ", 
    code=="RA3" ~ "RA7",
    code=="RC1" ~ "RC9",
    code=="RBA" ~ "RH5",
    TRUE ~ code)) %>% 
  group_by(code, Region, name, date) %>% 
  summarise(deaths=sum(deaths, na.rm=TRUE)) %>% 
  ungroup()

#Merge together from 1st August 2020 onwards
data.deaths.adm <- deaths %>% 
  filter(date>=as.Date("2020-08-01")) %>% 
  merge(admissions, all.x=TRUE, all.y=TRUE, by=c("code", "date", "Region")) %>% 
  mutate(admissions=if_else(is.na(admissions) & date<=admdate, 0, admissions),
         deaths=if_else(is.na(deaths), 0, deaths),
         name=coalesce(name.x, name.y)) %>% 
  select(-c("name.x", "name.y")) %>% 
  merge(occupancy, all.x=TRUE, all.y=TRUE, by=c("code", "date")) %>% 
  mutate(name=coalesce(name.x, name.y)) %>% 
  select(-c("name.x", "name.y"))

#Bring in PHE data summarising admissions in HES to each trust by MSOA
MSOA.adm <- read.csv("COVID_LA_Plots/Trust to MSOA HES data.csv")

#Address changes in trust codes - data from https://digital.nhs.uk/services/organisation-data-service/organisation-changes
#1st lookup for deaths data which doesn't include any of the 2020 trust changes
MSOA.adm1 <- MSOA.adm %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode=="RBA" ~ "RH5",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(CatchmentYear, msoa, TrustCode) %>% 
  summarise(msoa_total_catchment1=sum(msoa_total_catchment)) %>% 
  ungroup() 


#2nd lookup for admissions up to 4th October, when RD3 and RDZ merged to form R0D in the admissions (but not deaths) data
MSOA.adm2 <- MSOA.adm %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode %in% c("RQ8", "RDD") ~ "RAJ",
    TrustCode=="RA3" ~ "RA7",
    TrustCode=="RC1" ~ "RC9",
    TrustCode=="RBA" ~ "RH5",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(CatchmentYear, msoa, TrustCode) %>% 
  summarise(msoa_total_catchment2=sum(msoa_total_catchment)) %>% 
  ungroup() 

#2nd lookup for after 4th October
MSOA.adm3 <- MSOA.adm %>% 
  mutate(TrustCode=case_when(
    TrustCode %in% c("RE9", "RLN") ~ "R0B",
    TrustCode=="R1J" ~ "RTQ",
    TrustCode=="RQ6" ~ "REM",
    TrustCode=="RNL" ~ "RNN",
    TrustCode %in% c("RQ8", "RDD") ~ "RAJ",
    TrustCode=="RA3" ~ "RA7",
    TrustCode=="RC1" ~ "RC9",
    TrustCode=="RBA" ~ "RH5",
    TrustCode %in% c("RDZ", "RD3") ~ "R0D",
    TRUE ~ as.character(TrustCode))) %>% 
  group_by(CatchmentYear, msoa, TrustCode) %>% 
  summarise(msoa_total_catchment3=sum(msoa_total_catchment)) %>% 
  ungroup()

#get a list of *all* trust codes 
temp1 <- data.frame(TrustCode=unique(MSOA.adm1$TrustCode))
temp2 <- data.frame(TrustCode=unique(MSOA.adm2$TrustCode))
temp3 <- data.frame(TrustCode=unique(MSOA.adm3$TrustCode))
temp <- bind_rows(temp1, temp2, temp3) %>%
  unique()

MSOA.adm <- merge(temp, MSOA.adm1, by="TrustCode", all=TRUE) %>% 
  merge(., MSOA.adm2, all=TRUE) %>% 
  merge(., MSOA.adm3, all=TRUE)

#Bring in MSOA to LTLA lookup
temp <- tempfile()
source <- "http://geoportal1-ons.opendata.arcgis.com/datasets/0b3c76d1eb5e4ffd98a3679ab8dea605_0.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
MSOA.lookup <- read.csv(temp) %>% 
  select(MSOA11CD, LAD19CD) %>% 
  unique() %>% 
  rename(msoa=MSOA11CD)

#Merge into PHE lookup
MSOA.adm <- merge(MSOA.adm, MSOA.lookup, by="msoa", all.x=TRUE)

#Convert to the lookup we want (trust to LTLA), averaging across last 3 years in data (2016-18)
trust.lookup <- MSOA.adm %>% 
  filter(CatchmentYear>=2016) %>% 
  group_by(TrustCode, LAD19CD) %>% 
  summarise(catchment1=sum(msoa_total_catchment1, na.rm=TRUE),
            catchment2=sum(msoa_total_catchment2, na.rm=TRUE),
            catchment3=sum(msoa_total_catchment3, na.rm=TRUE)) %>% 
  ungroup() %>% 
  group_by(TrustCode) %>% 
  mutate(pop1=sum(catchment1, na.rm=TRUE), popprop1=catchment1/pop1,
         pop2=sum(catchment2, na.rm=TRUE), popprop2=catchment2/pop2,
         pop3=sum(catchment3, na.rm=TRUE), popprop3=catchment3/pop3) %>% 
  ungroup() %>% 
  rename(code=TrustCode)

#Bring lookup into admissions and deaths data
data.deaths.adm <-
  merge(data.deaths.adm, trust.lookup, by.x="code", by.y="code", all=TRUE)

#Allocate admissions and deaths to LTLA based on population proportions and 
#aggregate up to LTLA level
data.deaths.adm <- data.deaths.adm %>% 
  mutate(LA.deaths=deaths*popprop1, 
         LA.admissions=case_when(
           date<=as.Date("2020-10-04") ~ admissions*popprop2,
           TRUE ~ admissions*popprop3),
         LA.COVID.Beds=COVID*popprop3,
         LA.Other.Beds=Other*popprop3,
         LA.Unocc.Beds=Unoccupied*popprop3) %>% 
  group_by(LAD19CD, date) %>% 
  summarise(deaths=sum(LA.deaths, na.rm=TRUE), admissions=sum(LA.admissions, na.rm=TRUE),
            COVID.Beds=sum(LA.COVID.Beds, na.rm=TRUE),
            Other.Beds=sum(LA.Other.Beds, na.rm=TRUE),
            Unocc.Beds=sum(LA.Unocc.Beds, na.rm=TRUE)) %>% 
  ungroup() %>% 
  filter(!is.na(LAD19CD)) %>% 
  #merge City of London into Hackney and Isles of Scilly into Cornwall
  mutate(LAD19CD=case_when(LAD19CD=="E09000001" ~ "E09000012",
                           LAD19CD=="E06000053" ~ "E06000052",
                           LAD19CD %in% c("E07000004", "E07000005", "E07000006", 
                                          "E07000007") ~ "E06000060",
                           TRUE ~ as.character(LAD19CD))) %>%
  group_by(LAD19CD, date) %>% 
  summarise(deaths=sum(deaths), admissions=sum(admissions),
            COVID.Beds=sum(COVID.Beds), Other.Beds=sum(Other.Beds),
            Unocc.Beds=sum(Unocc.Beds)) %>% 
  ungroup() %>% 
  mutate(COVID.Beds=if_else(date<as.Date("2020-11-17") | date>admdate+days(2), NA_real_, COVID.Beds),
         Other.Beds=if_else(date<as.Date("2020-11-17") | date>admdate+days(2), NA_real_, Other.Beds),
         Unocc.Beds=if_else(date<as.Date("2020-11-17") | date>admdate+days(2), NA_real_, Unocc.Beds))

#Merge into case data
daydata <- merge(daydata, data.deaths.adm, by.x=c("date", "code"), by.y=c("date", "LAD19CD"), 
                 all.x=TRUE) %>% 
  #Set admissions data for missing dates to missing
  mutate(admissions=if_else(date>admdate, NA_real_, admissions))

#Bring in regions
daydata <- merge(daydata, LADtoRegion, all.x=TRUE)

daydata$Region <- case_when(
  is.na(daydata$Region) & daydata$code %in% c("E06000058", "E06000059", "E07000246") ~ "South West",
  is.na(daydata$Region) & daydata$code %in% c("E07000244", "E07000245") ~ "East of England",
  is.na(daydata$Region) & daydata$code=="E06000060" ~ "South East",
  TRUE ~ as.character(daydata$Region))

#Regional summary (E&W only)
daydata.reg <- daydata %>% 
  filter(!is.na(Region)) %>% 
  group_by(date, Region) %>% 
  summarise(cases=sum(cases), admissions=sum(admissions), deaths=sum(deaths),
            COVID.Beds=sum(COVID.Beds), Other.Beds=sum(Other.Beds),
            Unocc.Beds=sum(Unocc.Beds)) %>% 
  mutate(name=Region, Region="Region") %>% 
  ungroup()

#National summary (E&W only)
daydata.nat <- daydata %>% 
  group_by(date, country) %>% 
  summarise(cases=sum(cases), admissions=sum(admissions), deaths=sum(deaths),
            COVID.Beds=sum(COVID.Beds), Other.Beds=sum(Other.Beds),
            Unocc.Beds=sum(Unocc.Beds)) %>% 
  mutate(name=country, Region="Nation") %>% 
  ungroup()

daydata <- bind_rows(daydata, daydata.reg, daydata.nat)

daydata <- daydata %>% 
  group_by(name) %>% 
  arrange(date) %>% 
  mutate(casesroll_avg=roll_mean(cases, 7, align="center", fill=NA),
         admroll_avg=roll_mean(admissions, 7, align="center", fill=NA),
         deathsroll_avg=roll_mean(deaths, 7, align="center", fill=NA)) %>% 
  ungroup()

daydata$date <- as.Date(daydata$date)

#Calculate weekly cases
daydata$week <- case_when(
  daydata$date<as.Date("2021-01-08") ~ week(as.Date(daydata$date)-days(4)),
  TRUE ~ week(as.Date(daydata$date)-days(4))+53)

daydata.week <- daydata %>% 
  group_by(name, week) %>% 
  summarise(cases=sum(cases)) %>% 
  ungroup()

data <- merge(data, daydata.week, all.x=TRUE) %>% 
  mutate(measure=as.character(measure))

#Calculate total excess deaths
excess.ew <- data %>% 
  filter(country!="Scotland" & week<=maxweek.ew) %>% 
  group_by(name, measure) %>% 
  summarise(excess=sum(allexcess, na.rm=TRUE), hist=sum(deaths.1519, na.rm=TRUE), 
            excessprop=excess/hist) %>% 
  ungroup()

excess.s <-  data %>% 
  filter(country=="Scotland" & week<=maxweek.s) %>% 
  group_by(name, measure) %>% 
  summarise(excess=sum(allexcess, na.rm=TRUE), hist=sum(deaths.1519, na.rm=TRUE), excessprop=excess/hist) %>% 
  ungroup()

excess <- bind_rows(excess.ew, excess.s)

#Bring in LA populations
daydata <- merge(daydata, LApop, all.x=TRUE)

#Sort out pops for regions
regpop <- daydata %>% 
  filter(!Region %in% c("Region", "Nation") & country=="England") %>% 
  filter(date=="2020-04-01") %>% 
  group_by(Region) %>% 
  summarise(pop=sum(pop)) %>% 
  ungroup()

#Sort out pops for nations
natpop <- daydata %>% 
  filter(!Region %in% c("Region", "Nation")) %>% 
  filter(date=="2020-04-01") %>% 
  group_by(country) %>% 
  summarise(pop=sum(pop))

daydata <- merge(daydata, natpop, by="country", all.x=TRUE)
daydata$pop <- if_else(is.na(daydata$pop.x), daydata$pop.y, daydata$pop.x)
daydata <- daydata %>% select(-c(pop.x, pop.y))

daydata <- merge(daydata, regpop, by.x="name", by.y="Region", all.x=TRUE)
daydata$pop <- if_else(is.na(daydata$pop.x), daydata$pop.y, daydata$pop.x)
daydata <- daydata %>% select(-c(pop.x, pop.y))

daydata$caserate <- daydata$cases*100000/daydata$pop
daydata$caserate_avg <- daydata$casesroll_avg*100000/daydata$pop
daydata$admrate <- daydata$admissions*100000/daydata$pop
daydata$admrate_avg <- daydata$admroll_avg*100000/daydata$pop
daydata$deathrate <- daydata$deaths*100000/daydata$pop
daydata$deathrate_avg <- daydata$deathsroll_avg*100000/daydata$pop

#Add actual dates for tidier plotting
data <- data %>% mutate(date=case_when(
  country=="Scotland" ~ as.Date("2020-01-05")+weeks(week-1),
  TRUE ~ as.Date("2020-01-03")+weeks(week-1)
))

#Save master data
write.csv(data, "COVID_LA_Plots/LAExcess.csv")
write.csv(excess, "COVID_LA_Plots/LAExcessSummary.csv")
write.csv(daydata, "COVID_LA_Plots/LACases.csv")

save(data, excess, daydata, file="COVID_LA_Plots/Alldata.RData")

setwd("C:/data projects/colin_misc/COVID_LA_Plots")
setwd("C:/data projects/colin_misc")