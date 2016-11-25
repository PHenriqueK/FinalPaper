#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 11 November 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: Merges cleaned data sets
##########################

#Dynamical link
source('airbnb_data_manipulation_review.R')

#Merge data GENE
FSO_SBB1 <- merge(SBB_2010_2015_merge, GENESIS_merge, by=c("NID", "year"), all.x = TRUE, all.y = TRUE)
FSO_SBB2 <- merge(FSO_SBB1, SBB_HH_Income_merge, by=c("NID", "year"), all.x = TRUE)
FSO_SBB <- merge(FSO_SBB2, SBB_unemployment_merge, by=c("NID", "year"), all.x = TRUE)

#Putting Neighborhood ID's (NID's) in order
FSO_SBB <- FSO_SBB[order(FSO_SBB$NID, decreasing = FALSE), ]
FSO_SBB <- FSO_SBB[which(FSO_SBB$year < 2015),]
FSO_SBB$NID <- as.numeric(FSO_SBB$NID)
FSO_SBB$year_month <- as.factor(FSO_SBB$year_month)

#Calculating hotel occupancy rate per month/year and district
FSO_SBB$month <- as.factor(FSO_SBB$month)
FSO_SBB$occup_rate <- FSO_SBB$nights/(FSO_SBB$beds*30)
FSO_SBB$occup_rate[FSO_SBB$month == 1 | 3 | 5 | 7 | 8 | 10 | 12] <- FSO_SBB$nights[FSO_SBB$month == 1 | 3 | 5 | 7 | 8 | 10 | 12]/(FSO_SBB$beds[FSO_SBB$month == 1 | 3 | 5 | 7 | 8 | 10 | 12]*31)
FSO_SBB$occup_rate[FSO_SBB$month == 4 | 6 | 9 | 11] <- FSO_SBB$nights[FSO_SBB$month == 4 | 6 | 9 | 11]/(FSO_SBB$beds[FSO_SBB$month == 4 | 6 | 9 | 11]*30)
FSO_SBB$occup_rate[FSO_SBB$month == 2 ] <- FSO_SBB$nights[FSO_SBB$month == 2 ]/(FSO_SBB$beds[FSO_SBB$month == 2 ]*28)

#Final merge, creates our comprehensive data frame
agg_listings_merge$year_month <- as.factor(agg_listings_merge$year_month)
analysis_data <- merge(FSO_SBB, agg_listings_merge, by=c("NID", "year_month"), all.x = TRUE)

life_listings1 <- merge(FSO_SBB, life_listings, by=c("NID", "year_month"), all.x = TRUE)
