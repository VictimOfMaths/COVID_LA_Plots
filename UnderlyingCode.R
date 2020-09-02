rm(list=ls())

library(tidyverse)
library(curl)
library(readxl)
library(ggtext)
library(paletteer)
library(lubridate)
library(forcats)
library(RcppRoll)

###################################################################################
#Weekly data

#Read in 2020 data for England
temp <- tempfile()
source <- "https://www.ons.gov.uk/file?uri=%2fpeoplepopulationandcommunity%2fhealthandsocialcare%2fcausesofdeath%2fdatasets%2fdeathregistrationsandoccurrencesbylocalauthorityandhealthboard%2f2020/lahbtablesweek34.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20 <- read_excel(temp, sheet=6, col_names=FALSE)[-c(1:4),]
colnames(data20) <- c("code", "type", "name", "cause", "week", "location", "deaths.20")
data20 <- subset(data20, type=="Local Authority")[,-c(2)]

data20$deaths.20 <- as.numeric(data20$deaths.20)
data20$week <- as.numeric(data20$week)

maxweek.ew <- max(data20$week)
enddate.ew <- as.Date("2020-01-03")+weeks(maxweek.ew-1)

#Spread causes
data20 <- pivot_wider(data20, names_from="cause", values_from="deaths.20")

#Read in 2015-19 historic data for England & Wales
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

data.ew <- merge(data1519, data20, all.x=TRUE)

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
  group_by(code, name, location, week) %>% 
  summarise(deaths.1519=sum(deaths.1519), AllCause.20=sum(`All causes`), COVID.20=sum(`COVID 19`)) %>% 
  mutate(Other.20=AllCause.20-COVID.20) %>% 
  ungroup()

#Bring in Scottish deaths data (released by NRS on a Wednesday)
#2020 data

#Need to update link and range each week
#https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/general-publications/weekly-and-monthly-data-on-births-and-deaths/deaths-involving-coronavirus-covid-19-in-scotland/related-statistics
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-date-council-area-location.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data20.s <- read_excel(temp, sheet=2, range="A5:E3903", col_names=FALSE)
colnames(data20.s) <- c("week", "name", "location", "cause", "deaths")
data20.s$week <- as.numeric(data20.s$week)

maxweek.s <- max(data20.s$week)
enddate.s <- as.Date("2020-01-04")+weeks(maxweek.s-1)

data20.s$cause <- if_else(data20.s$cause=="Non-COVID-19", "Other.20", "COVID.20")

data20.s <- spread(data20.s, cause, deaths)
data20.s$COVID.20 <- replace_na(data20.s$COVID.20, 0)
data20.s$Other.20 <- replace_na(data20.s$Other.20, 0)

#2015-19 data
temp <- tempfile()
source <- "https://www.nrscotland.gov.uk/files//statistics/covid19/weekly-deaths-by-location-council-areas.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
data1519.s <- read_excel(temp, sheet=2, range="A5:E25193", col_names=FALSE)
colnames(data1519.s) <- c("week", "name", "location", "year", "deaths")
data1519.s$week <- as.numeric(data1519.s$week)

#Take 5 year averages
data1519.s <- data1519.s %>% 
  group_by(week, name, location) %>% 
  summarise(deaths.1519=mean(deaths)) %>% 
  ungroup()

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
  group_by(week, name, location) %>% 
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

#Generate national summaries
data.nat <- data %>% 
  group_by(week, country, location) %>% 
  summarise(across(c("deaths.1519", "AllCause.20", "COVID.20", "Other.20"), sum)) %>% 
  mutate(name=country, Region="Nation") %>% 
  ungroup()

data <- bind_rows(data, data.nat)

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
#Daily data

#Set up daily dataframe
#Bring in case data
#Read in cases data for England
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/coronavirus-cases_latest.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")

casedata.E <- read.csv(temp)[,c(1:5)]
colnames(casedata.E) <- c("name", "code", "geography", "date", "cases")
casedata.E <- casedata.E %>% filter(geography=="ltla")

mindate <- min(as.Date(casedata.E$date))
maxdate <- max(as.Date(casedata.E$date))

#Address merging of Aylesbury Vale, Chiltern and South Bucks into Bucks
casedata.E$name <- if_else(casedata.E$name %in% c("Aylesbury Vale", "Chiltern", "South Bucks", "Wycombe"), 
                           "Buckinghamshire", as.character(casedata.E$name))
casedata.E$code <- if_else(casedata.E$code %in% c("E07000004", "E07000005", "E07000006", "E07000007"), 
                           "E06000060", as.character(casedata.E$code))

casedata.E <- casedata.E %>% 
  group_by(name, code, date) %>% 
  summarise(cases=sum(cases)) %>% 
  ungroup()


#Set up skeleton dataframe, merging City of London and Hackney
daydata <- data.frame(code=rep(unique(subset(data, !name %in% c("England", "Scotland", "Wales"))$code),
                               each=maxdate-mindate+1),
                      name=rep(unique(subset(data, !name %in% c("England", "Scotland", "Wales"))$name),
                               each=maxdate-mindate+1),
                      date=rep(seq.Date(from=mindate, to=maxdate, by="day"), 
                               times=length(unique(subset(data, !name %in% c("England", "Scotland", "Wales"))$code))))

#merge in English cases
daydata <- merge(daydata, casedata.E, by=c("name", "code", "date"), all.x=TRUE)

#Bring in Welsh case data
temp <- tempfile()
source <- "http://www2.nphs.wales.nhs.uk:8080/CommunitySurveillanceDocs.nsf/3dc04669c9e1eaa880257062003b246b/77fdb9a33544aee88025855100300cab/$FILE/Rapid%20COVID-19%20surveillance%20data.xlsx"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
casedata.W <- read_excel(temp, sheet=3)[,c(1:3)]

colnames(casedata.W) <- c("name", "date", "cases")

daydata <- merge(daydata, casedata.W, by=c("name", "date"), all.x=TRUE)

#Fill in blanks
daydata$cases <- coalesce(daydata$cases.x, daydata$cases.y)
daydata <- daydata[,-c(4:5)]
daydata$cases <- if_else(is.na(daydata$cases) & !substr(daydata$code, 1,1)=="S", 0, daydata$cases)

#Bring in Scottish case data
#Need to update this link each day from:
#https://www.opendata.nhs.scot/dataset/covid-19-in-scotland
temp <- tempfile()
source <- "https://www.opendata.nhs.scot/dataset/b318bddf-a4dc-4262-971f-0ba329e09b87/resource/427f9a25-db22-4014-a3bc-893b68243055/download/trend_ca_20200901.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
casedata.S <- read.csv(temp)[,c(1:3)]
colnames(casedata.S) <- c("date", "code", "cases")

casedata.S$date <- as.Date(as.character(casedata.S$date), "%Y%m%d")

daydata <- merge(daydata, casedata.S, by=c("date", "code"), all.x=TRUE)

#Fill in blanks
daydata$cases <- coalesce(daydata$cases.x, daydata$cases.y)
daydata <- daydata[,-c(4:5)]
daydata$cases <- if_else(is.na(daydata$cases), 0, daydata$cases)

#Bring in NI case data
#Need to update this link daily from 
#https://www.health-ni.gov.uk/publications/daily-dashboard-updates-covid-19-september-2020
temp <- tempfile()
source <- "https://www.health-ni.gov.uk/sites/default/files/publications/health/doh-dd-010920.XLSX"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
#Need to update the range here too:
casedata.NI <- read_excel(temp, sheet=3, range="A2:E2382", col_names=FALSE)
colnames(casedata.NI) <- c("date", "name", "tests", "inds", "cases")
casedata.NI$date <- as.Date(casedata.NI$date)

#Set up skeleton dataframe
daydata.NI <- data.frame(date=rep(seq.Date(from=mindate, to=maxdate, by="day"), 
                                  times=length(unique(casedata.NI$name))),
                         name=rep(unique(casedata.NI$name), each=maxdate-mindate+1))

daydata.NI <- merge(daydata.NI, casedata.NI, all.x=TRUE)

#Fill in blanks and remove missing postcode/NA data
daydata.NI$cases <- replace_na(daydata.NI$cases, 0)
daydata.NI <- daydata.NI %>% filter(!name %in% c("Missing Postcode", "NA"))

daydata.NI$code <- case_when(
  daydata.NI$name=="Antrim and Newtownabbey" ~ "N09000001",
  daydata.NI$name=="Ards and North Down" ~ "N09000011",
  daydata.NI$name=="Armagh City, Banbridge and Craigavon" ~ "N09000002",
  daydata.NI$name=="Belfast" ~ "N09000003",
  daydata.NI$name=="Causeway Coast and Glens" ~ "N09000004",
  daydata.NI$name=="Derry City and Strabane" ~ "N09000005",
  daydata.NI$name=="Fermanagh and Omagh" ~ "N09000006",
  daydata.NI$name=="Lisburn and Castlereagh" ~ "N09000007",
  daydata.NI$name=="Mid and East Antrim" ~ "N09000008",
  daydata.NI$name=="Mid Ulster" ~ "N09000009",
  daydata.NI$name=="Newry, Mourne and Down" ~ "N09000010"
)

daydata <- bind_rows(daydata, daydata.NI[,c(2,1,6,5)])

daydata$country <- case_when(
  substr(daydata$code,1,1)=="E" ~ "England",
  substr(daydata$code,1,1)=="W" ~ "Wales",
  substr(daydata$code,1,1)=="S" ~ "Scotland",
  substr(daydata$code,1,1)=="N" ~ "Northern Ireland"
  )

#Experimental pillar 1 & 2 separation - England only and only up to end of June
#Archive files from 1st & 2nd July - either side of pillar 2 addition to data
#Available from https://coronavirus.data.gov.uk/archive
#Pillar 1 data
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/dated/coronavirus-cases_202007011400.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
p1data <- read.csv(temp)[,c(1:5)]
colnames(p1data) <- c("name", "code", "geography", "date", "p1cases")
p1data$date <- as.Date(p1data$date)
p1data <- subset(p1data, geography=="Lower tier local authority" & date<"2020-07-01")
p1data$code <- if_else(p1data$code %in% c("E09000001", "E09000012"), 
                       "E09000012", as.character(p1data$code))
p1data$code <- case_when(
  p1data$code %in% c("E09000001", "E09000012") ~ "E09000012",
  p1data$code %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
  TRUE ~ p1data$code
)

p1data <- p1data %>% 
  group_by(code, date) %>% 
  summarise(p1cases=sum(p1cases)) %>% 
  ungroup()

#Pillar 1 & 2 combined
temp <- tempfile()
source <- "https://coronavirus.data.gov.uk/downloads/csv/dated/coronavirus-cases_202007021618.csv"
temp <- curl_download(url=source, destfile=temp, quiet=FALSE, mode="wb")
p12data <- read.csv(temp)[,c(1:5)]
colnames(p12data) <- c("name", "code", "geography", "date", "p12cases")
p12data$date <- as.Date(p12data$date)
p12data <- subset(p12data, geography=="Lower tier local authority" & date<"2020-07-01")
p12data$code <- as.character(p12data$code)
p12data$code <- case_when(
  p12data$code %in% c("E09000001", "E09000012") ~ "E09000012",
  p12data$code %in% c("E07000004", "E07000005", "E07000006", "E07000007") ~ "E06000060",
  TRUE ~ p12data$code
)
p12data <- p12data %>% 
  group_by(code, date) %>% 
  summarise(p12cases=sum(p12cases)) %>% 
  ungroup()

daydata <- merge(daydata, p1data, by=c("date", "code"), all.x=TRUE)
daydata <- merge(daydata, p12data, by=c("date", "code"), all.x=TRUE)

daydata$p1cases <- if_else(is.na(daydata$p1cases) & daydata$country=="England" & daydata$date<as.Date("2020-07-01"),
                           0, daydata$p1cases)
daydata$p12cases <- if_else(is.na(daydata$p12cases) & daydata$country=="England" & daydata$date<as.Date("2020-07-01"), 
                            0, daydata$p12cases)

#Estimate Pillar 2 cases
daydata$p2cases <- daydata$p12cases-daydata$p1cases
daydata$p2cases <- if_else(daydata$p2cases<0, 0, daydata$p2cases)

daydata$date <- as.Date(daydata$date)

#National summary (E&W only)
daydata.nat <- daydata %>% 
  group_by(date, country) %>% 
  summarise(across(c("cases", "p1cases", "p12cases", "p2cases"), sum)) %>% 
  mutate(name=country) %>% 
  ungroup()

daydata <- bind_rows(daydata, daydata.nat)

daydata <- daydata %>% 
  group_by(name) %>% 
  arrange(date) %>% 
  mutate(casesroll_avg=roll_mean(cases, 7, align="right", fill=0)) %>% 
  ungroup()

daydata$date <- as.Date(daydata$date)

#Calculate weekly cases
daydata$week <- week(as.Date(daydata$date)-days(4))

daydata.week <- daydata %>% 
  group_by(name, week) %>% 
  summarise(cases=sum(cases), p1cases=sum(p1cases), p2cases=sum(p2cases)) %>% 
  ungroup()

data <- merge(data, daydata.week, all.x=TRUE)

#Calculate total excess deaths
excess.ew <- data %>% 
  filter(country!="Scotland" & week<=maxweek.ew) %>% 
  group_by(name) %>% 
  summarise(excess=sum(allexcess, na.rm=TRUE), hist=sum(deaths.1519), excessprop=excess/hist) %>% 
  ungroup()

excess.s <-  data %>% 
  filter(country=="Scotland" & week<=maxweek.s) %>% 
  group_by(name) %>% 
  summarise(excess=sum(allexcess, na.rm=TRUE), hist=sum(deaths.1519), excessprop=excess/hist) %>% 
  ungroup()

excess <- bind_rows(excess.ew, excess.s)

#Bring in LA populations
daydata <- merge(daydata, LApop, all.x=TRUE)

#Sort out pops for nations
natpop <- daydata %>% 
  filter(!name %in% c("England", "Wales", "Scotland", "Northern Ireland")) %>% 
  filter(date=="2020-04-01") %>% 
  group_by(country) %>% 
  summarise(pop=sum(pop))

daydata <- merge(daydata, natpop, by="country", all.x=TRUE)
daydata$pop <- if_else(is.na(daydata$pop.x), daydata$pop.y, daydata$pop.x)
daydata <- daydata[,-c(11,12)]

daydata$caserate <- daydata$cases*100000/daydata$pop
daydata$caserate_avg <- daydata$casesroll_avg*100000/daydata$pop

#Save master data
write.csv(data, "COVID_LA_Plots/LAExcess.csv")
write.csv(excess, "COVID_LA_Plots/LAExcessSummary.csv")
write.csv(daydata, "COVID_LA_Plots/LACases.csv")

#Highlight areas

library(gt)

shortcases <- daydata %>% 
  as.data.frame() %>% 
  filter(!name %in% c("England", "Wales", "Scotland", "Northern Ireland")) %>% 
  select(name, date, country, casesroll_avg, caserate_avg) %>% 
  arrange(name, date) %>% 
  group_by(name) %>% 
  mutate(cases_change=casesroll_avg-lag(casesroll_avg, 7),
         caserate_change=caserate_avg-lag(caserate_avg, 7)) %>% 
  #Take data from day before most recent to allow for incomplete data in most recent
  slice_tail(n=2) %>% 
  slice_head(n=1) %>% 
  as.data.frame()

up_arrow <- "<span style=\"color:red\">&#9650;</span>"
down_arrow <- "<span style=\"color:green\">&#9660;</span>"

#Highest case numbers
casetable <- shortcases %>% 
  slice_max(casesroll_avg, n=10) %>% 
  gt(rowname_col="name") %>% 
  tab_header(title="Local Authorities in the UK with the highest number of new COVID cases",
             subtitle="Based on 7-day rolling average of Pillar 1 and 2 tests combined") %>% 
  tab_source_note("Data from PHE, PHW, DoHNI and ScotGov | Analysis by @VictimOfMaths") %>% 
  tab_stubhead(label="Local Authority") %>% 
  tab_spanner(label="Current cases per day", columns=vars(casesroll_avg, caserate_avg)) %>% 
  tab_spanner(label="Change in last 7 days", columns=vars(cases_change, caserate_change)) %>% 
  cols_label(date="Data up to", country="Country", casesroll_avg="Total",
             caserate_avg="Rate per 100,000", cases_change="Total", 
             caserate_change="Rate per 100,000") %>% 
  fmt_date(columns=vars(date), date_style=9) %>% 
  fmt_number(columns=vars(casesroll_avg, caserate_avg, cases_change, caserate_change),
             decimals=1) %>% 
  text_transform(locations=cells_body(columns="cases_change", rows=cases_change<0),
                 fn = function(x) paste(x, down_arrow)) %>% 
  text_transform(locations=cells_body(columns="cases_change", rows=cases_change>0),
                 fn = function(x) paste(x, up_arrow)) %>% 
  cols_width(vars(casesroll_avg, caserate_avg, cases_change, caserate_change)
             ~ px(100)) %>% 
  cols_align(vars(casesroll_avg, caserate_avg, cases_change, caserate_change),
             align="center")

#Highest case rates
ratetable <- shortcases %>% 
  slice_max(caserate_avg, n=10) %>% 
  gt(rowname_col="name") %>% 
  tab_header(title="Local Authorities in the UK with the highest rate of new COVID cases",
             subtitle="Based on 7-day rolling average of Pillar 1 and 2 tests combined") %>% 
  tab_source_note("Data from PHE, PHW, DoHNI and ScotGov | Analysis by @VictimOfMaths") %>% 
  tab_stubhead(label="Local Authority") %>% 
  tab_spanner(label="Current cases per day", columns=vars(caserate_avg, casesroll_avg)) %>% 
  tab_spanner(label="Change in last 7 days", columns=vars(caserate_change, cases_change)) %>% 
  cols_label(date="Data up to", country="Country", casesroll_avg="Total",
             caserate_avg="Rate per 100,000", cases_change="Total", 
             caserate_change="Rate per 100,000") %>% 
  fmt_date(columns=vars(date), date_style=9) %>% 
  fmt_number(columns=vars(casesroll_avg, caserate_avg, cases_change, caserate_change),
             decimals=1) %>% 
  text_transform(locations=cells_body(columns="caserate_change", rows=cases_change<0),
                 fn = function(x) paste(x, down_arrow)) %>% 
  text_transform(locations=cells_body(columns="caserate_change", rows=cases_change>0),
                 fn = function(x) paste(x, up_arrow)) %>% 
  cols_width(vars(casesroll_avg, caserate_avg, cases_change, caserate_change)
             ~ px(100)) %>% 
  cols_align(vars(casesroll_avg, caserate_avg, cases_change, caserate_change),
             align="center")

#Biggest rise in case numbers
casechangetable <- shortcases %>% 
  slice_max(cases_change, n=10) %>% 
  gt(rowname_col="name") %>% 
  tab_header(title="Local Authorities in the UK with biggest rise in new COVID-19 case numbers in the last week",
             subtitle="Based on 7-day rolling average of Pillar 1 and 2 tests combined") %>% 
  tab_source_note("Data from PHE, PHW, DoHNI and ScotGov | Analysis by @VictimOfMaths") %>% 
  tab_stubhead(label="Local Authority") %>% 
  cols_move(columns=vars(cases_change, caserate_change), after="country") %>% 
  tab_spanner(label="Change in last 7 days", columns=vars(cases_change, caserate_change)) %>% 
  tab_spanner(label="Current cases per day", columns=vars(casesroll_avg, caserate_avg)) %>% 
  cols_label(date="Data up to", country="Country", casesroll_avg="Total",
             caserate_avg="Rate per 100,000", cases_change="Total", 
             caserate_change="Rate per 100,000") %>% 
  fmt_date(columns=vars(date), date_style=9) %>% 
  fmt_number(columns=vars(casesroll_avg, caserate_avg, cases_change, caserate_change),
             decimals=1) %>% 
  text_transform(locations=cells_body(columns="cases_change", rows=cases_change<0),
                 fn = function(x) paste(x, down_arrow)) %>% 
  text_transform(locations=cells_body(columns="cases_change", rows=cases_change>0),
                 fn = function(x) paste(x, up_arrow)) %>% 
  cols_width(vars(casesroll_avg, caserate_avg, cases_change, caserate_change)
             ~ px(100)) %>% 
  cols_align(vars(casesroll_avg, caserate_avg, cases_change, caserate_change),
             align="center")

#Biggest rise in case rates
ratechangetable <- shortcases %>% 
  slice_max(caserate_change, n=10) %>% 
  gt(rowname_col="name") %>% 
  tab_header(title="Local Authorities in the UK with biggest rise in new COVID-19 case rates in the last week",
             subtitle="Based on 7-day rolling average of Pillar 1 and 2 tests combined") %>% 
  tab_source_note("Data from PHE, PHW, DoHNI and ScotGov | Analysis by @VictimOfMaths") %>% 
  tab_stubhead(label="Local Authority") %>% 
  cols_move(columns=vars(cases_change, caserate_change), after="country") %>% 
  tab_spanner(label="Current cases per day", columns=vars(caserate_avg, casesroll_avg)) %>% 
  tab_spanner(label="Change in last 7 days", columns=vars(caserate_change, cases_change)) %>% 
  cols_label(date="Data up to", country="Country", casesroll_avg="Total",
             caserate_avg="Rate per 100,000", cases_change="Total", 
             caserate_change="Rate per 100,000") %>% 
  fmt_date(columns=vars(date), date_style=9) %>% 
  fmt_number(columns=vars(casesroll_avg, caserate_avg, cases_change, caserate_change),
             decimals=1) %>% 
  text_transform(locations=cells_body(columns="caserate_change", rows=cases_change<0),
                 fn = function(x) paste(x, down_arrow)) %>% 
  text_transform(locations=cells_body(columns="caserate_change", rows=cases_change>0),
                 fn = function(x) paste(x, up_arrow)) %>% 
  cols_width(vars(casesroll_avg, caserate_avg, cases_change, caserate_change)
             ~ px(100)) %>% 
  cols_align(vars(casesroll_avg, caserate_avg, cases_change, caserate_change),
             align="center")

gtsave(casetable, filename="casetable.png", path="C:/data projects/colin_misc/COVID_LA_Plots")
gtsave(ratetable, filename="ratetable.png", path="C:/data projects/colin_misc/COVID_LA_Plots")
gtsave(casechangetable, filename="casechangetable.png", path="C:/data projects/colin_misc/COVID_LA_Plots")
gtsave(ratechangetable, filename="ratechangetable.png", path="C:/data projects/colin_misc/COVID_LA_Plots")

daydata$flag <- case_when(
  daydata$name %in% c("Bolton", "Bury", "Manchester", "Oldham", "Rochdale", "Salford", 
                      "Stockport", "Tameside", "Trafford", "Wigan")~ 1,
  TRUE ~ 0)

tiff("Outputs/COVIDManchesterLAs2.tiff", units="in", width=8, height=6, res=500)
ggplot()+
 geom_line(data=subset(daydata, date>as.Date("2020-07-01")), 
           aes(x=date, y=caserate_avg, group=name), colour="Grey80")+
  geom_line(data=subset(daydata, flag==1 & date>as.Date("2020-07-01")), 
            aes(x=date, y=caserate_avg, colour=name))+
  geom_segment(aes(x=as.Date("2020-08-01"), xend=as.Date("2020-08-01"), y=0, yend=20), 
               colour="Red", linetype=2)+
  scale_x_date(name="Date")+
  scale_y_continuous(name="New cases per 100,000 population per day")+
  scale_colour_paletteer_d("LaCroixColoR::paired", name="")+
  theme_classic()+
  labs(title="The case for relaxing local restrictions in Manchester looks questionable",
       subtitle="Rolling 7-day average rates of new confirmed COVID-19 cases",
       caption="Data from PHE | Plot by @VictimOfMaths")
dev.off()

daydata %>% 
  filter(date %in% c(as.Date("2020-08-01"), as.Date("2020-08-31")) & flag==1) %>% 
  select(name, date, casesroll_avg) %>% 
  spread(date, casesroll_avg) %>% 
  mutate(change=`2020-08-31`-`2020-08-01`, changeperc=change*100/`2020-08-01`) %>% 
  gt() %>%
  tab_spanner(label="7-day average new cases",columns=vars(`2020-08-01`, `2020-08-31`)) %>% 
  tab_spanner(label="Change from start of restrictions", columns=vars(change, changeperc)) %>% 
  cols_label(name="", `2020-08-01`="1st August", `2020-08-31`="31st August",
              change="Absolute", changeperc="Relative") %>% 
  fmt_number(columns=vars(`2020-08-01`, `2020-08-31`, change), decimals=1) %>% 
  fmt_number(columns=vars(changeperc), decimals=0) %>% 
  text_transform(locations=cells_body(columns="change", rows=change<0),
                 fn = function(x) paste(x, down_arrow)) %>% 
  text_transform(locations=cells_body(columns="change", rows=change>0),
                 fn = function(x) paste(x, up_arrow)) %>% 
  text_transform(locations=cells_body(columns="changeperc"),
                 fn = function(x) paste0(x, "%"))

daydata$flag2 <- if_else(daydata$name %in% c("Manchester", "Bury", "Tameside", "Rochdale",
                                             "Salford", "Oldham", "Preston", "Blackburn with Darwen",
                                             "Pendle", "Bradford", "Calderdale", "Kirklees",
                                             "Glasgow City", "West Dunbartonshire",
                                             "East Renfrewshire"),
                         1,0)

tiff("Outputs/COVIDRestrictions.tiff", units="in", width=8, height=25, res=500)
daydata %>% 
  group_by(name) %>% 
  mutate(max=max(date)) %>% 
  filter(date==max) %>% 
ggplot()+
  geom_col(aes(x=caserate_avg*7, y=fct_reorder(name, caserate_avg), fill=as.factor(flag2)),
           show.legend = FALSE)+
  scale_x_continuous(name="New COVID-19 cases per 100,000 in the last 7 days")+
  scale_y_discrete(name="")+
  scale_fill_manual(values=c("Grey70", "Red"))+
  theme_classic()+
  labs(title="",
       caption="Data from ONS, NRS, DoHNI, PHE, PHW & PHS | Plot by @VictimOfMaths")
dev.off()