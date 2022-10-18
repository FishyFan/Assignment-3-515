#Getting Working Directory
getwd()

#Setting Working Directory
setwd("/Users/harshsanghvi/Downloads")

#Import Library
library(tidyverse)
library(dplyr)
library(stringr)
library(tidyr)

#Importing Data

data <- read.csv("StormEvents_details-ftp_v1.0_d1992_c20220425.csv")

#Limiting to Specific Variables
usevariables <- c("BEGIN_YEARMONTH","BEGIN_DAY","BEGIN_TIME",
                 "END_YEARMONTH","END_DAY","END_TIME",
                 "EPISODE_ID",
                 "EVENT_ID",
                 "STATE","STATE_FIPS",
                 "CZ_NAME",
                 "CZ_TYPE",
                 "CZ_FIPS",
                 "EVENT_TYPE",
                 "SOURCE",
                 "BEGIN_LAT","BEGIN_LON",
                 "END_LAT","END_LON")
str(usevariables)

#Creating new dataframe with limited variables
storm_1992 <- data[usevariables]
head(storm_1992)

#Arranging data by Beginning year and month
dataarrange <- arrange(storm_1992, BEGIN_YEARMONTH)
head(dataarrange)

#Changing case for state and county
dataarrange$casestate <- str_to_title(string = dataarrange$STATE)
dataarrange$casecounty <- str_to_title(string = dataarrange$CZ_NAME)
head(dataarrange$casestate)
head(dataarrange$casecounty)

#Limit County by C only
ccountydata <- filter(dataarrange, CZ_TYPE=='C')
ccountydata <- select(dataarrange, - c(CZ_TYPE))
head(ccountydata)

#Add 0 to State FIPS
ccountydata1 <- str_pad(storm_1992$STATE_FIPS, width=3, side= "left", pad= "0")
view(ccountydata1)



#Add 0 to county FIPS

str_pad(storm_1992$CZ_FIPS, width=3, side= "left", pad= "0")
view(ccountydata1)

#Unite both State and county FIPS
library(tidyr)
library(dplyr)

unite(ccountydata, testmerge, STATE_FIPS, CZ_FIPS, sep ="00")

#Changing column names to lower case
rename_all(storm_1992, tolower)


#New DataFrame and renaming
data("state")
renamestate <- data.frame(state=state.name, area=state.area, region=state.region)
newset <- data.frame(table(storm_1992$STATE))
head(newset)

newerset<-rename(newset, c("state"="Var1"))
mergeddata <- merge(x=newerset, y=renamestate, by.x = "state", by.y = "state")
head(mergeddata)

renamestateinfo  <- mutate_all(renamestate, toupper)
mergeddata <- merge(x=newerset, y=renamestateinfo, by.x = "state", by.y = "state")
head(mergeddata)


#Scatter Plot with GG
library(ggplot2)
eventplot <- ggplot(mergeddata, aes(x = area, y = Freq)) +
  geom_point(aes(color = region)) +
  labs(x = "Land area(square miles)" , y = "# of storm events in 1992") +
  theme(axis.text.x = element_text(angle = 90, size = 8))
eventplot

