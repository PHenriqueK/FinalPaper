#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 11 November 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: Loads Airbnb Listings Data
##########################

#Dynamical link
source('data.R')

#Loading "listings.csv.gz" dataset from InsideAirbnb.com
#Listings_URL <- "http://data.insideairbnb.com/germany/be/berlin/2015-10-03/data/listings.csv.gz"
#temp <- tempfile()
#download.file(Listings_URL, temp)
#Detailed_Listings <- read.csv(gzfile(temp, "listings.csv"))
#unlink(temp)

#Import "reviews.csv" from insideairbnb.com
#Airbnb_Reviews <- read.csv("http://data.insideairbnb.com/germany/be/berlin/2015-10-03/visualisations/reviews.csv")

Airbnb_Reviews <- read.csv("http://data.insideairbnb.com/germany/be/berlin/2015-10-03/visualisations/reviews.csv")

Detailed_Listings <- read.csv("data/listings.csv")