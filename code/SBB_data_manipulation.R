#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 11 November 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: merging and cleaning different sub-data sets from the Statistical Office Berlin/Brandenberg
##########################

# Dynamical Link to Data/Packages R script file
source('FSO_data_manipulation.R')

##### Cleaning SBB DATA on unemployment #####

#Creating a blank filling function
Fill <- function(x,missing="")
{
  Log <- x != missing
  y <- x[Log]
  y[cumsum(Log)]
}


names(SBB_unemployment) <- c("neighbourhood", "year", "totalworkforce", "V2", "jobseeker", "V3")

SBB_unemployment$neighbourhood <- Fill(as.character(SBB_unemployment$neighbourhood))

SBB_unemployment <- SBB_unemployment[, c("neighbourhood", "year", "totalworkforce", "jobseeker")]

##Calculating the unemployment rate 
SBB_unemployment$ue_rate <- SBB_unemployment$jobseeker/SBB_unemployment$totalworkforce

##Creating a unique neighbourhood ID (NID)
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Mitte"] <- 1
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Friedrichshain-Kreuzberg"] <- 2
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Pankow"] <- 3
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Charlottenburg-Wilmersdorf"] <- 4
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Spandau"] <- 5
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Steglitz-Zehlendorf"] <- 6
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Tempelhof-Schoeneberg"] <- 7
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Neukoelln"] <- 8
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Treptow-Koepenick"] <- 9
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Marzahn-Hellersdorf"] <- 10
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Lichtenberg"] <- 11
SBB_unemployment$NID [SBB_unemployment$neighbourhood == "Reinickendorf"] <- 12

#Creating ready-to-merge dataset
SBB_unemployment_merge <- SBB_unemployment[, c("NID", "year", "ue_rate")]

##### Cleaning SBB DATA household income #####

SBB_HH_Income <- SBB_HH_Income[-1,]

SBB_HH_Income <- SBB_HH_Income[, c("V1", "V2", "V5", "V7", "V9", "V11", 
                                   "V13", "V15", "V17")]

names(SBB_HH_Income) <- c("neighbourhood", "year", "inc_500_899", "inc_900_1299", "inc_1300_1499", "inc_1500_1999", "inc_2000_2599", "inc_2600_3199", "inc_3200plus")

SBB_HH_Income$neighbourhood <- Fill(as.character(SBB_HH_Income$neighbourhood))

##Creating a unique neighbourhood ID (NID)
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Mitte"] <- 1
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Friedrichshain-Kreuzberg"] <- 2
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Pankow"] <- 3
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Charlottenburg-Wilmersdorf"] <- 4
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Spandau"] <- 5
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Steglitz-Zehlendorf"] <- 6
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Tempelhof-Schoeneberg"] <- 7
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Neukoelln"] <- 8
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Treptow-Koepenick"] <- 9
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Marzahn-Hellersdorf"] <- 10
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Lichtenberg"] <- 11
SBB_HH_Income$NID [SBB_HH_Income$neighbourhood == "Reinickendorf"] <- 12

#Cleaning up income data
SBB_HH_Income$inc_500_899 <- as.numeric(SBB_HH_Income$inc_500_899)
SBB_HH_Income$inc_900_1299 <- as.numeric(SBB_HH_Income$inc_900_1299)
SBB_HH_Income$inc_1300_1499 <- as.numeric(SBB_HH_Income$inc_1300_1499)
SBB_HH_Income$inc_1500_1999 <- as.numeric(SBB_HH_Income$inc_1500_1999)
SBB_HH_Income$inc_2000_2599 <- as.numeric(SBB_HH_Income$inc_2000_2599)
SBB_HH_Income$inc_2600_3199 <- as.numeric(SBB_HH_Income$inc_2600_3199)
SBB_HH_Income$inc_3200plus <- as.numeric(SBB_HH_Income$inc_3200plus)

#Creating average income per district 
#Number of people per income group*average income per group according to column names
SBB_HH_Income$sum <- SBB_HH_Income$inc_500_899 + SBB_HH_Income$inc_900_1299 + SBB_HH_Income$inc_1300_1499 + SBB_HH_Income$inc_1500_1999 + SBB_HH_Income$inc_2000_2599 + SBB_HH_Income$inc_2600_3199 + SBB_HH_Income$inc_3200plus
SBB_HH_Income$value1 <- SBB_HH_Income$inc_500_899/SBB_HH_Income$sum*((500+899)/2) + SBB_HH_Income$inc_900_1299*((900+1299)/2) + SBB_HH_Income$inc_1300_1499*((1300+1499)/2) + SBB_HH_Income$inc_1500_1999*((1500+1999)/2) + SBB_HH_Income$inc_2000_2599*((2000+2599)/2) + SBB_HH_Income$inc_2600_3199*((2600+3199)/2) + SBB_HH_Income$inc_3200plus*3200
SBB_HH_Income$avg_inc <- SBB_HH_Income$value1/SBB_HH_Income$sum

#Creating ready to merge dataset
SBB_HH_Income_merge <- SBB_HH_Income[, c("NID", "year", "avg_inc")]

##### cleaning SBB DATA on tourism #####
#drop rows with no information, remove columns that contain entirely NA values, rename columns
SBB_2010_2015 <- SBB_2010_2015[-(865:871),]
SBB_2010_2015 <- SBB_2010_2015 [, !apply(is.na(SBB_2010_2015), 2, all)]
names(SBB_2010_2015) <- c("neighbourhood", "year", "month", "guests", "nights")
SBB_2010_2015$month [SBB_2010_2015$month == "Januar"] <- 1
SBB_2010_2015$month [SBB_2010_2015$month == "Februar"] <- 2
SBB_2010_2015$month [SBB_2010_2015$month == "Maerz"] <- 3
SBB_2010_2015$month [SBB_2010_2015$month == "April"] <- 4
SBB_2010_2015$month [SBB_2010_2015$month == "Mai"] <- 5
SBB_2010_2015$month [SBB_2010_2015$month == "Juni"] <- 6
SBB_2010_2015$month [SBB_2010_2015$month == "Juli"] <- 7
SBB_2010_2015$month [SBB_2010_2015$month == "August"] <- 8
SBB_2010_2015$month [SBB_2010_2015$month == "September"] <- 9
SBB_2010_2015$month [SBB_2010_2015$month == "Oktober"] <- 10
SBB_2010_2015$month [SBB_2010_2015$month == "November"] <- 11
SBB_2010_2015$month [SBB_2010_2015$month == "Dezember"] <- 12

##Filling in blanks
SBB_2010_2015$neighbourhood <- Fill(as.character(SBB_2010_2015$neighbourhood))
SBB_2010_2015$year <- Fill(as.character(SBB_2010_2015$year))

#Creating a yy-mm time variable
SBB_2010_2015$year_month <- as.yearmon(paste(SBB_2010_2015$year, SBB_2010_2015$month, sep = "-"))

#Creating a unique neighbourhood ID
SBB_2010_2015$NID <- sapply(strsplit(as.character(SBB_2010_2015$neighbourhood),' '), "[", 1)
SBB_2010_2015$NID [SBB_2010_2015$NID == "01"] <- 1
SBB_2010_2015$NID [SBB_2010_2015$NID == "02"] <- 2
SBB_2010_2015$NID [SBB_2010_2015$NID == "03"] <- 3
SBB_2010_2015$NID [SBB_2010_2015$NID == "04"] <- 4
SBB_2010_2015$NID [SBB_2010_2015$NID == "05"] <- 5
SBB_2010_2015$NID [SBB_2010_2015$NID == "06"] <- 6
SBB_2010_2015$NID [SBB_2010_2015$NID == "07"] <- 7
SBB_2010_2015$NID [SBB_2010_2015$NID == "08"] <- 8
SBB_2010_2015$NID [SBB_2010_2015$NID == "09"] <- 9

SBB_2010_2015 <- SBB_2010_2015[, c(7,6,1,2,3,4,5)]
SBB_2010_2015 <- SBB_2010_2015[order(SBB_2010_2015$year_month, decreasing = FALSE), ] # order for NID

#Creating a ready-to-merge dataset
SBB_2010_2015_merge <- SBB_2010_2015

#Removing unnecessary data subsets
remove(SBB_HH_Income, SBB_unemployment) 

