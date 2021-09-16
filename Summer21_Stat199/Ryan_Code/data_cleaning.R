# SOURCE(DATACLEAN.R) 
# THIS IS USED TO CALL R FILES FROM OTHER CODE 

rm(list = ls())

library(ggplot2)
library(dplyr)
library(DT)
library(rmapshaper)
library(leaflet)
library(htmltools)
library(tigris)
library(totalcensus)
library(rpart)
library(rpart.plot)
library(pROC)
library(randomForest)
library(pscl)
library(boot)
library(reshape2)
library(RColorBrewer)

# I WILL PUT THE CLEANING AND SET UP DATA HERE
cps = read.csv("Ryan_Data/cps(clean).csv")

cps$urban_c <- cps$urban

cps$urban_c <- ifelse(cps$urban_c == 1, "Large Central Metro",
                      ifelse(cps$urban_c == 2, "Large Fringe Metro",
                             ifelse(cps$urban_c == 3, "Medium Metro", 
                                    ifelse(cps$urban_c == 4, "Small Metro",
                                           ifelse(cps$urban_c == 5, "Micropolitan", "Non-Core/Possibly Rural")))))

cps$urban_c[is.na(cps$urban_c)] <- c("Non-Core/Possibly Rural")

count(cps, var = urban)
#LRF: order is not what you wanted: turn this into an "ordered factor" then r realizes
#there is an inherent ordering, and it will respect that in plots, tables, etc...
#this code should do that: (attribute will follow variable urban_c through to new datasets)

cps$urban_c <- factor(cps$urban_c, levels = c("Large Central Metro", "Large Fringe Metro",  "Medium Metro",
                                              "Small Metro","Micropolitan", "Non-Core/Possibly Rural" ))


# THIS IS THE ACS SECTION TO ADD THE URBANICITY METRIC AND SET IT UP FOR PREDICTIONS
county_codes = read.csv("Ryan_Data/NCHSURCodes2013.csv")

acs = read.csv("Ryan_data/acs(clean).csv")

county_codes = rename(county_codes, "county_name" = "County.name")

acs = rename(acs, "location" = "X")

acs$county = acs$location

acs$county = gsub("[[:digit:]]", "", acs$county)

acs$county = gsub("Block Group ,", "", acs$county)

acs$county = gsub("Census Tract ,", "", acs$county)

acs$county = gsub("Census Tract .,", "", acs$county)

#acs$county = gsub("(.*),.*", "\\1", acs$county)

acs$county = gsub("  ", "", acs$county)

acs = rename(acs, "state_and_county" = "county") 

county_codes$state_name = county_codes$State.Abr.

county_codes$state_name <- ifelse(county_codes$state_name == "AL", "Alabama",
                           ifelse(county_codes$state_name == "AK", "Alaska",
                           ifelse(county_codes$state_name == "AZ", "Arizona",
                           ifelse(county_codes$state_name == "AR", "Arkansas",
                           ifelse(county_codes$state_name == "CA", "California",
                           ifelse(county_codes$state_name == "CO", "Colorado",
                           ifelse(county_codes$state_name == "CT", "Connecticut",
                           ifelse(county_codes$state_name == "DE", "Delaware",
                           ifelse(county_codes$state_name == "FL", "Florida",
                           ifelse(county_codes$state_name == "GA", "Georgia",
                           ifelse(county_codes$state_name == "HI", "Hawaii",
                           ifelse(county_codes$state_name == "ID", "Idaho",
                           ifelse(county_codes$state_name == "IL", "Illinois",
                           ifelse(county_codes$state_name == "IN", "Indiana",
                           ifelse(county_codes$state_name == "IA", "Iowa",
                           ifelse(county_codes$state_name == "KS", "Kansas",
                           ifelse(county_codes$state_name == "KY", "Kentucky",
                           ifelse(county_codes$state_name == "LA", "Louisiana",
                           ifelse(county_codes$state_name == "ME", "Maine",
                           ifelse(county_codes$state_name == "MD", "Maryland",
                           ifelse(county_codes$state_name == "MA", "Massachusetts",
                           ifelse(county_codes$state_name == "MI", "Michigan",
                           ifelse(county_codes$state_name == "MN", "Minnesota",
                           ifelse(county_codes$state_name == "MS", "Mississippi",
                           ifelse(county_codes$state_name == "MO", "Missouri",
                           ifelse(county_codes$state_name == "MT", "Montana",
                           ifelse(county_codes$state_name == "NE", "Nebraska",
                           ifelse(county_codes$state_name == "NV", "Nevada",
                           ifelse(county_codes$state_name == "NH", "New Hampshire",
                           ifelse(county_codes$state_name == "NJ", "New Jersey",
                           ifelse(county_codes$state_name == "NM", "New Mexico",
                           ifelse(county_codes$state_name == "NY", "New York",
                           ifelse(county_codes$state_name == "NC", "North Carolina",
                           ifelse(county_codes$state_name == "ND", "North Dakota",
                           ifelse(county_codes$state_name == "OH", "Ohio",
                           ifelse(county_codes$state_name == "OK", "Oklahoma",
                           ifelse(county_codes$state_name == "OR", "Oregon",
                           ifelse(county_codes$state_name == "PA", "Pennsylvania",
                           ifelse(county_codes$state_name == "RI", "Rhode Island",
                           ifelse(county_codes$state_name == "SC", "South Carolina",
                           ifelse(county_codes$state_name == "SD", "South Dakota",
                           ifelse(county_codes$state_name == "TN", "Tennessee",
                           ifelse(county_codes$state_name == "TX", "Texas",
                           ifelse(county_codes$state_name == "UT", "Utah",
                           ifelse(county_codes$state_name == "VT", "Vermont",
                           ifelse(county_codes$state_name == "VA", "Virginia",
                           ifelse(county_codes$state_name == "WA", "Washington",
                           ifelse(county_codes$state_name == "WV", "West Virginia",
                           ifelse(county_codes$state_name == "WI", "Wisconsin", 
                           ifelse(county_codes$state_name == "WY", "Wyoming", "NA"))))))))))))))))))))))))))))))))))))))))))))))))))

county_codes = county_codes[-c(324),]

county_codes$state_and_county <- paste(county_codes$county_name,",",county_codes$state_name)

county_codes$state_and_county <- gsub(" ,", ",", county_codes$state_and_county)

county_codes$state_and_county = as.factor(county_codes$state_and_county)

acs$state_and_county = as.factor(acs$state_and_county)

acs$state_and_county <- tolower(acs$state_and_county)

county_codes$state_and_county <- tolower(county_codes$state_and_county)

# BEFORE THIS WILL WORK I NEED TO CHANGE LASALLE ILLINOIS INTO LA SALLE IN THE ACS DATABASE, I NEED TO CHANGE DONA ANA IN THE ACS DATABASE
# TO DONA ANA, AND I NEED TO CHANGE PETERSBURG BOROUGH INTO PETERSBURG CENSUS AREA OR JUST CHANGE IT TO 6.

acs$state_and_county <- gsub("lasalle county, illinois", "la salle county, illinois", acs$state_and_county)

acs$state_and_county <- gsub("doã±a ana county, new mexico", "dona ana county, new mexico", acs$state_and_county)

acs$state_and_county <- gsub("petersburg borough, alaska", "petersburg census area, alaska", acs$state_and_county)

acs = merge(x = acs, y = county_codes, by = "state_and_county", all.x = TRUE)

# IT WORKS!! NOW I NEED TO REMOVE EVERYTHING I DON'T NEED AND KEEP THE 2013 CODE

acs <- acs[, c("GEOID","location","avg_hhsize", "hispanic","elderly","black","kids","education","employed","married","disability","households",
               "X2013.code", "female" )]

acs = rename(acs, "urban_code" = "X2013.code")

acs$urban_c <- acs$urban_code

acs$urban_c <- ifelse(acs$urban_c == 1, "Large Central Metro",
                      ifelse(acs$urban_c == 2, "Large Fringe Metro",
                             ifelse(acs$urban_c == 3, "Medium Metro", 
                                    ifelse(acs$urban_c == 4, "Small Metro",
                                           ifelse(acs$urban_c == 5, "Micropolitan", "Non-Core/Possibly Rural")))))

count(acs, var = urban_code)

acs$urban_c[is.na(acs$urban_c)] <- c("Non-Core/Possibly Rural")

acs$urban_c <- factor(acs$urban_c, levels = c("Large Central Metro", "Large Fringe Metro",  "Medium Metro",
                                              "Small Metro","Micropolitan", "Non-Core/Possibly Rural" ))

# THESE ARE CAUSING ISSUES, I NEED TO ONLY DIVIDE WHERE THERE ARE ACTUAL VALUES
# NA.OMIT CAUSES PROBLEMS DUE TO HOW IT ACTUALLY WORKS WITHIN R, BETTER TO USE OTHER METHOD.
#acs = na.omit(acs)

acs = subset(acs, households != 0)

# I COULD USE MUTATE_AT IN ORDER TO ROUND ALL OF THE VALUES, BUT THIS WORKS FOR NOW, ALBEIT BEING REPETITIVE 

acs$hispanic <- round(acs$hispanic/acs$households, digits = 3)

acs$elderly <- round(acs$elderly/acs$households, digits = 3)

acs$black <- round(acs$black/acs$households, digits = 3)

acs$kids <- round(acs$kids/acs$households, digits = 3)

acs$education <- round(acs$education/acs$households, digits = 3)

acs$employed <- round(acs$employed/acs$households, digits = 3)

acs$married <- round(acs$married/acs$households, digits = 3)

acs$disability <- round(acs$disability/acs$households, digits = 3)

acs$female <- round(acs$female/acs$households, digits = 3)

acs = rename(acs, "hhsize" = "avg_hhsize")

#acs = subset(acs, select = -c(urban_code))

