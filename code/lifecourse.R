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

agg_reviews$new_reviews[agg_reviews$new_reviews >= "1"] <- 1

agg_reviews_time <- expand.grid(id = unique(agg_reviews$id),
                           rev_year_month = unique(agg_reviews$rev_year_month))

lifecycle <- merge(agg_reviews, agg_reviews_time, all = TRUE)

id_NID <- listings[, c("id", "host_since", "first_review", "NID")]

lifecycle_NID <- merge(lifecycle, id_NID, by = c("id"), all = TRUE)

lifecycle_NID$new_reviews[is.na(lifecycle_NID$new_reviews)] <- 0

#Computing listing date (6 months prior to first review)
#lifecycle_NID$listingdate <- as.Date(as.yearmon(as.Date(lifecycle_NID$first_review)) - .5)

lifecycle_NID$listingdate <- lifecycle_NID$first_review

#Splitting listing date up into its elements
lifecycle_NID$listingdate <- as.Date(lifecycle_NID$listingdate,"%Y-%m-%d")
lifecycle_NID <- mutate(lifecycle_NID, date = ymd(lifecycle_NID$listingdate), listing_day = day(date), 
                   listing_month = month(date), listing_year = year(date))

#Creating a new yy-mm variable
lifecycle_NID$year_month <- as.yearmon(lifecycle_NID$listingdate, "%Y-%m")

lifecourse <- lifecycle_NID[, c("id", "NID", "rev_year_month", "year_month", "listing_year", "new_reviews")]

lifecourse <- lifecourse[which(lifecourse$listing_year < 2015),]

get.mav <- function(bp,n=7){
  require(zoo)
  if(is.na(bp[1])) bp[7] <- sum(bp,na.rm=TRUE)
  bp <- na.locf(bp,na.rm=FALSE)
  if(length(bp)<n) return(bp)
  c(bp[1:(n-7)],rollapply(bp,width=n,sum,align="right"))  
}

library(data.table)
setDT(lifecourse)     # converts test to a data.table in place
setkey(lifecourse,id,rev_year_month)
lifecourse[,life:=as.numeric(get.mav(new_reviews,7)),by=id]

lifecourse$life [lifecourse$new_reviews == 1] <- 1
lifecourse$new_reviews [lifecourse$rev_year_month == lifecourse$year_month] <- 1

lifecourse$count [lifecourse$life >= 1] <- 1
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
names(life_listings) <- c("NID", "year_month", "AB_supply")
life_listings$NID <- as.factor(life_listings$NID)
