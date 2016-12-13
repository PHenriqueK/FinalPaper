#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 3 October 2016
## Last update: 11 November 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: clean inside Airbnb data
## What it does: Cleans Airbnb data
##########################

# Dynamical Link to Data/Packages R script file
source('code/SBB_data_manipulation.R')

listings <- Detailed_Listings 

listings <- listings[, c("id", "neighbourhood_group_cleansed", "first_review", "last_review", "host_since", "number_of_reviews", "room_type")]

#Creating a unique neighbourhood ID (NID)
listings$NID [listings$neighbourhood_group_cleansed == "Mitte"] <- 1
listings$NID [listings$neighbourhood_group_cleansed == "Friedrichshain-Kreuzberg"] <- 2
listings$NID [listings$neighbourhood_group_cleansed == "Pankow"] <- 3
listings$NID [listings$neighbourhood_group_cleansed == "Charlottenburg-Wilm."] <- 4
listings$NID [listings$neighbourhood_group_cleansed == "Spandau"] <- 5
listings$NID [listings$neighbourhood_group_cleansed == "Steglitz - Zehlendorf"] <- 6
listings$NID [listings$neighbourhood_group_cleansed == "Tempelhof - Schöneberg"] <- 7
listings$NID [listings$neighbourhood_group_cleansed == "Neukölln"] <- 8
listings$NID [listings$neighbourhood_group_cleansed == "Treptow - Köpenick"] <- 9
listings$NID [listings$neighbourhood_group_cleansed == "Marzahn - Hellersdorf"] <- 10
listings$NID [listings$neighbourhood_group_cleansed == "Lichtenberg"] <- 11
listings$NID [listings$neighbourhood_group_cleansed == "Reinickendorf"] <- 12

#This deletes 5 Airbnb listings for which there are no dates at all
listings <- listings[!(listings$host_since == "" | is.na(listings$host_since)), ] 

#generate year-month variable
listings$host_since <- as.yearmon(listings$host_since,"%Y-%m")
listings$first_review <- as.yearmon(listings$first_review,"%Y-%m")

#Computing listing date (6 months prior to first review)
listings$listingdate <- listings$first_review - .5

#If apartment does not have a first review date, use host since date instead 
listings$listingdate[is.na(listings$listingdate)] <- listings$host_since[is.na(listings$listingdate)]

#Creating a new yy-mm variable
listings$year_month <- listings$listingdate

#Creating a counting variable
listings$count <- 1

#Making aggregate data by district and month by counting the number of new apts per month in each district
agg_listings <- tally(group_by(listings, NID, year_month))

#Names "agg_listings" columns
names(agg_listings) <- c("NID", "year_month", "apt_new")

#Ready to merge
agg_listings_merge <- agg_listings

##### Computing dynamic Airbnb supply #####
reviews <- Airbnb_Reviews

#Creating a new yy-mm variable
reviews$rev_year_month <- as.yearmon(reviews$date,"%Y-%m")

reviews$count <- 1

agg_reviews <- tally(group_by(reviews, listing_id, rev_year_month))

names(agg_reviews) <- c("id", "rev_year_month", "new_reviews")

agg_reviews <- agg_reviews[which(agg_reviews$rev_year_month < "Jan 2015"),]

agg_reviews$new_reviews[agg_reviews$new_reviews >= "1"] <- 1

agg_reviews_time <- expand.grid(id = unique(agg_reviews$id),
                                rev_year_month = unique(agg_reviews$rev_year_month))

lifecycle <- merge(agg_reviews, agg_reviews_time, all = TRUE)

id_NID <- listings[, c("id", "host_since", "first_review", "NID")]

lifecycle_NID <- merge(lifecycle, id_NID, by = c("id"), all = TRUE)

lifecycle_NID$new_reviews[is.na(lifecycle_NID$new_reviews)] <- 0

#Computing listing date (6 months prior to first review)
lifecycle_NID$year_month <- lifecycle_NID$first_review

lifecourse <- lifecycle_NID[, c("id", "NID", "rev_year_month", "year_month", "new_reviews")]

lifecourse <- lifecourse[which(lifecourse$year_month < "Jan 2015"),]

lifecourse$life <- rollmean(lifecourse$new_reviews, 7, na.pad = TRUE, align = "left")

lifecourse$count [lifecourse$life > 0] <- 1
lifecourse$count [is.na(lifecourse$count)] <- 0

life_reshape <- lifecourse

life_reshape$year_month <- NULL
life_reshape$listing_year <- NULL
life_reshape$new_reviews <- NULL
life_reshape$life <- NULL

life_long <- cast(life_reshape, id ~ rev_year_month)
life_long_NID <- merge(life_long, id_NID, by = c("id"))
life_long_NID <- life_long_NID[, -(2:5)]
life_long_NID$host_since <- NULL
life_long_NID$first_review <- NULL
life_long_NID$id <- NULL

melt.df <- melt(life_long_NID, id.vars=c("NID"))

life_listings <- aggregate(melt.df$value, by=list(NID=melt.df$NID, year_month=melt.df$variable), FUN=sum)

#Ready to merge
names(life_listings) <- c("NID", "year_month", "AB_supply")
life_listings$NID <- as.factor(life_listings$NID)