#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 3 October 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: merging cleaned data sets
##########################

# Set working directory
try(setwd("/Users/Paulo/GitHub/FinalPaper/code"),silent=TRUE)
try(setwd("/Users/djm113/Documents/GitHub/FinalPaper/code"),silent=TRUE)

#dynamical link
source('merge.R')

#drop observations for 2015 and further
data_2010_2014 <- analysis_data[which(analysis_data$year < 2015),]

data_2010_2014$apt_new[is.na(data_2010_2014$apt_new)] <- 0

#cumulative sum of new appartments/month per district
data_2010_2014$AB_supply <- ave(data_2010_2014$apt_new, data_2010_2014$NID, FUN=cumsum)

#dropping variables not needed
analysis_simple <- data_2010_2014[, c("NID", "neighbourhood", "year_month", "year", "month", "occup_rate", "AB_supply", "avg_inc", "ue_rate", "guests" )]

#log Airbnb Apt Supply (simple model)
analysis_simple$log_ABsupply <- log(analysis_simple$AB_supply)

#creating subset