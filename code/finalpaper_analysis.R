#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 3 October 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: merging cleaned data sets
##########################

#Set working directory
try(setwd("/Users/Paulo/GitHub/FinalPaper/code"),silent=TRUE)
try(setwd("/Users/djm113/Documents/GitHub/FinalPaper/code"),silent=TRUE)

#Dynamical link
source('merge.R')

#Drop observations for 2015 and further (research focus lies on time period between 2010 and 2014)
data_2010_2014 <- analysis_data[which(analysis_data$year < 2015),]
data_2010_2014$apt_new[is.na(data_2010_2014$apt_new)] <- 0

#Cumulative sum of new appartments/month per district
data_2010_2014$AB_supply <- ave(data_2010_2014$apt_new, data_2010_2014$NID, FUN=cumsum)

#Dropping variables not needed for further analysis
analysis_simple <- data_2010_2014[, c("NID", "neighbourhood", "year_month", "year", "month", "occup_rate", "AB_supply", "avg_inc", "ue_rate", "guests" )]

#Log Airbnb apt supply and hotel occupancy rate
analysis_simple$log_ABsupply <- log(analysis_simple$AB_supply)
analysis_simple$occup_log <- log(analysis_simple$occup_rate)

#Create binary variable for Airbnb's official market entry in June 2011
analysis_simple$marketentry <- ifelse ((analysis_simple$year_month < "Juni 2011"), 0, 1)


