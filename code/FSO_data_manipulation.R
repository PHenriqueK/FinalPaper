#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 11 November 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: Cleans and Merges different sub-data sets from the German Federal Statistics Office (FSO)
##########################

# Dynamical Link to Data/Packages R script file
source('data_airbnb.R')

##### Cleaning & merging GENESIS DATA ######

#Renaming columns
names(GENESIS_2010) <- c("NID", "neighbourhood", "hotels", "beds", "overnight", "visitors")
GENESIS_2010 <- GENESIS_2010[-1,]

names(GENESIS_2011) <- c("NID", "neighbourhood", "hotels", "beds", "overnight", "visitors")
GENESIS_2011 <- GENESIS_2011[-1,]

names(GENESIS_2012) <- c("NID", "neighbourhood", "hotels", "beds", "overnight", "visitors")
GENESIS_2012 <- GENESIS_2012[-1,]

names(GENESIS_2013) <- c("NID", "neighbourhood", "hotels", "beds", "overnight", "visitors") 
GENESIS_2013 <- GENESIS_2013[-1,]

names(GENESIS_2014) <- c("NID", "neighbourhood", "hotels", "beds", "overnight", "visitors") 
GENESIS_2014 <- GENESIS_2014[-1,]

#Adding a time variable (year) 
GENESIS_2010$year <- 2010
GENESIS_2011$year <- 2011
GENESIS_2012$year <- 2012
GENESIS_2013$year <- 2013
GENESIS_2014$year <- 2014

#Assigning values to Neighbourhood ID's (NID's)
GENESIS_2010$NID <- c(1:12)
GENESIS_2011$NID <- c(1:12)
GENESIS_2012$NID <- c(1:12)
GENESIS_2013$NID <- c(1:12)
GENESIS_2014$NID <- c(1:12)

#Joining data frames per genre
GENESIS <- rbind(GENESIS_2010, GENESIS_2011, GENESIS_2012, GENESIS_2013, GENESIS_2014) 
GENESIS <- GENESIS[order(GENESIS$NID, decreasing = FALSE), ] # order for NID
GENESIS <- GENESIS[, c(1,2,7,3,4,5,6)]

#Removing unnecessary data subsets
remove(GENESIS_2010, GENESIS_2011, GENESIS_2012, GENESIS_2013, GENESIS_2014) 
GENESIS_merge <- GENESIS[, c("NID", "year", "beds", "hotels")]
