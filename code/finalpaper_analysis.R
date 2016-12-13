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

##### Data Prep and descriptive analysis using the static supply #####

#Drop observations for 2015 and further (research focus lies on time period between 2010 and 2014)
data_2010_2014 <- analysis_data[which(analysis_data$year < 2015),]
data_2010_2014$apt_new[is.na(data_2010_2014$apt_new)] <- 0

#Cumulative sum of new appartments/month per district
data_2010_2014 <- data_2010_2014[order(data_2010_2014$NID,data_2010_2014$year_month),]
data_2010_2014$AB_supply <- ave(data_2010_2014$apt_new, data_2010_2014$NID, FUN=cumsum)

#Dropping variables not needed for further analysis
analysis_simple <- data_2010_2014[, c("NID", "neighbourhood", "year_month", "year", "month", "occup_rate", "AB_supply", "avg_inc", "ue_rate", "guests", "beds", "nights", "arrivals" )]
analysis_simple$NID <- as.factor(analysis_simple$NID)
analysis_simple$factor_ym <- as.factor(analysis_simple$year_month)
analysis_simple$neighbourhood <- substring(analysis_simple$neighbourhood, 3)

#Log Airbnb apt supply and hotel occupancy rate
analysis_simple$log_ABsupply <- log(analysis_simple$AB_supply)
analysis_simple$log_ABsupply[analysis_simple$log_ABsupply=="-Inf"] <- 0

#logging nights & average income per district
analysis_simple$log_nights <- log(analysis_simple$nights)
analysis_simple$log_inc <- log(analysis_simple$avg_inc)

#Create binary variable for Airbnb's official market entry in June 2011
analysis_simple$seqym <- ave(analysis_simple$AB_supply, analysis_simple$NID, FUN = seq_along)
analysis_simple$marketentry <- ifelse((analysis_simple$seqym < 18), 0, 1)

#interactionterm
analysis_simple$logABAB <- analysis_simple$AB_supply*analysis_simple$log_ABsupply
analysis_simple$AB_supply_2 <- analysis_simple$AB_supply*analysis_simple$AB_supply

#dummy for district specific market entry 
analysis_simple$dmarketentry <- ifelse((analysis_simple$AB_supply < 10), 0, 1)

#Create binary variable for passed Zweckentfremdungsverbot in May 2014
analysis_simple$ZEV <- ifelse((analysis_simple$seqym > 52), 1, 0)

###### Data Prep and descriptive analysis using the dynmic supply ######

#Drop observations for 2015 and further (research focus lies on time period between 2010 and 2014)
ddata_2010_2014 <- life_listings1[which(life_listings1$year < 2015),]

#Dropping variables not needed for further analysis
danalysis_simple <- ddata_2010_2014[, c("NID", "neighbourhood", "year_month", "year", "month", "occup_rate", "AB_supply", "avg_inc", "ue_rate", "guests", "beds", "nights", "arrivals" )]
danalysis_simple$factor_ym <- as.factor(danalysis_simple$year_month)
danalysis_simple$neighbourhood <- substring(danalysis_simple$neighbourhood, 3)

#Log Airbnb apt supply and hotel occupancy rate
danalysis_simple$log_ABsupply <- log(danalysis_simple$AB_supply)
danalysis_simple$log_ABsupply[danalysis_simple$log_ABsupply=="-Inf"] <- 0

#Logging nights & average income per district
danalysis_simple$log_nights <- log(danalysis_simple$nights)
danalysis_simple$log_inc <- log(danalysis_simple$avg_inc)

#Create binary variable for Airbnb's official market entry in June 2011
danalysis_simple$ym <- as.yearmon(danalysis_simple$year_month)
danalysis_simple$marketentry <- ifelse((danalysis_simple$ym < "Juni 2011"), 0, 1)

#Interactionterm
danalysis_simple$logABAB <- danalysis_simple$AB_supply*danalysis_simple$log_ABsupply

#Quadratic AB supply
danalysis_simple$AB_supply_2 <- danalysis_simple$AB_supply*danalysis_simple$AB_supply

#Dummy for district specific market entry 
danalysis_simple$dmarketentry <- ifelse((danalysis_simple$AB_supply < 10), 0, 1)

#Subset for observations where Airbnb Supply was at least 10 apts
marketentry_static <- subset(analysis_simple, analysis_simple$dmarketentry==1)
marketentry_dynamic <- subset(danalysis_simple, analysis_simple$dmarketentry==1)

#Create binary variable for passed Zweckentfremdungsverbot in May 2014
danalysis_simple$ZEV <- ifelse((danalysis_simple$ym > "Mai 2014"), 0, 1)