#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 11 November 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: Merges cleaned data sets
##########################

#Dynamical link
source('airbnb_data_manipulation.R')

#Merge data GENE
FSO_SBB1 <- merge(SBB_2010_2015_merge, GENESIS_merge, by=c("NID", "year"), all.x = TRUE, all.y = TRUE)
FSO_SBB2 <- merge(FSO_SBB1, SBB_HH_Income_merge, by=c("NID", "year"), all.x = TRUE)
FSO_SBB <- merge(FSO_SBB2, SBB_unemployment_merge, by=c("NID", "year"), all.x = TRUE)

#Putting Neighborhood ID's (NID's) in order
FSO_SBB <- FSO_SBB[order(FSO_SBB$NID, decreasing = FALSE), ]

#Calculating hotel occupancy rate per month/year and district
FSO_SBB$occup_rate <- FSO_SBB$nights/(FSO_SBB$beds*30)

#Final merge, creates our comprehensive data frame
analysis_data <- merge(FSO_SBB, agg_listings_merge, by=c("NID", "year_month"), all.x = TRUE)