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
source('SBB_data_manipulation.R')

listings <- Detailed_Listings 

listings <- listings[, c("id", "neighbourhood_group_cleansed", "first_review", "last_review", "number_of_reviews", "room_type")]

#This deletes all Airbnb listings for which there exists no review data and which 
listings <- listings[!(listings$number_of_reviews == 0), ] 

#Computing listing date (6 months prior to first review)
listings$listingdate <- as.Date(as.yearmon(as.Date(listings$first_review)) - .5)

#Splitting listing date up into its elements
listings$listingdate <- as.Date(listings$listingdate,"%Y-%m-%d")
listings <- mutate(listings, date = ymd(listings$listingdate), listing_day = day(date), 
                   listing_month = month(date), listing_year = year(date))

#Creating a new yy-mm variable
listings$year_month <- as.yearmon(listings$listingdate, "%Y-%m")

#Creating a counting variable
listings$count <- 1

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

############ WIP ###########
reviews <- Airbnb_Reviews

reviews$date <- as.Date(reviews$date,"%Y-%m-%d")

#Splitting reviews date up into its elements
reviews <- mutate(reviews, date = ymd(reviews$date), rev_day = day(date), rey_month = month(date), rev_year = year(date))

#Creating a new yy-mm variable
reviews$rev_year_month <- as.yearmon(reviews$date, "%Y-%m")

reviews$count <- 1

agg_reviews <- tally(group_by(reviews, listing_id, rev_year_month, rev_year))

names(agg_reviews) <- c("id", "rev_year_month", "rev_year", "new_reviews")

agg_reviews <- agg_reviews[which(agg_reviews$rev_year < 2015),]

#Merge reviews and listings data 
birthdate <- listings[, c("id", "year_month")]
lifecourse <- merge(agg_reviews, birthdate, by=c("id"), all.x = TRUE)
names(lifecourse) <- c("id", "rev_year_month", "rev_year", "new_reviews", "listingdate")


########################################

#Making aggregate data by district and month by counting the number of new apts per month in each district
agg_listings <- tally(group_by(listings, NID, year_month))

#Names "agg_listings" columns
names(agg_listings) <- c("NID", "year_month", "apt_new")

#Ready to merge
agg_listings_merge <- agg_listings