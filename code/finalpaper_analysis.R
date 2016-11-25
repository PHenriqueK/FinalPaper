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

#Drop observations for 2015 and further (research focus lies on time period between 2010 and 2014)
data_2010_2014 <- analysis_data[which(analysis_data$year < 2015),]
data_2010_2014$apt_new[is.na(data_2010_2014$apt_new)] <- 0

#Cumulative sum of new appartments/month per district
data_2010_2014$AB_supply <- ave(data_2010_2014$apt_new, data_2010_2014$NID, FUN=cumsum)

#Dropping variables not needed for further analysis
analysis_simple <- data_2010_2014[, c("NID", "neighbourhood", "year_month", "year", "month", "occup_rate", "AB_supply", "avg_inc", "ue_rate", "guests", "beds", "nights" )]
analysis_simple$NID <- as.factor(analysis_simple$NID)
analysis_simple$factor_ym <- as.factor(analysis_simple$year_month)
analysis_simple$neighbourhood <- substring(analysis_simple$neighbourhood, 3)

#Group data
#analysis_simple <- group_by(analysis_simple, NID, year, year_month)

#Log Airbnb apt supply and hotel occupancy rate
analysis_simple$log_ABsupply <- log(analysis_simple$AB_supply)
analysis_simple$log_ABsupply[analysis_simple$log_ABsupply=="-Inf"] <- 0

#logging nights & average income per district
analysis_simple$log_nights <- log(analysis_simple$nights)
analysis_simple$log_inc <- log(analysis_simple$avg_inc)

#Create binary variable for Airbnb's official market entry in June 2011
analysis_simple$year <- as.numeric(analysis_simple$year)
analysis_simple$marketentry <- ifelse ((analysis_simple$year_month < "Juni 2011"), 0, 1)

#interactionterm
analysis_simple$logABAB <- analysis_simple$AB_supply*analysis_simple$log_ABsupply

######


#Drop observations for 2015 and further (research focus lies on time period between 2010 and 2014)
ddata_2010_2014 <- life_listings1[which(life_listings1$year < 2015),]
#ddata_2010_2014$apt_new[is.na(ddata_2010_2014$apt_new)] <- 0

#Dropping variables not needed for further analysis
danalysis_simple <- ddata_2010_2014[, c("NID", "neighbourhood", "year_month", "year", "month", "occup_rate", "AB_supply", "avg_inc", "ue_rate", "guests", "beds", "nights" )]
danalysis_simple$NID <- as.factor(danalysis_simple$NID)
danalysis_simple$factor_ym <- as.factor(danalysis_simple$year_month)

#Group data
#danalysis_simple <- group_by(danalysis_simple, NID, year, year_month)

#Log Airbnb apt supply and hotel occupancy rate
danalysis_simple$log_ABsupply <- log(danalysis_simple$AB_supply)
danalysis_simple$log_ABsupply[danalysis_simple$log_ABsupply=="-Inf"] <- 0

#logging nights & average income per district
danalysis_simple$log_nights <- log(danalysis_simple$nights)
danalysis_simple$log_inc <- log(danalysis_simple$avg_inc)

#Create binary variable for Airbnb's official market entry in June 2011
danalysis_simple$year <- as.numeric(danalysis_simple$year)
danalysis_simple$marketentry <- ifelse ((danalysis_simple$year_month < "Juni 2011"), 0, 1)

#interactionterm
danalysis_simple$logABAB <- danalysis_simple$AB_supply*danalysis_simple$log_ABsupply

ModelDI <- plm(log_nights ~ log_ABsupply  + AB_supply + logABAB + log_inc + ue_rate, data=danalysis_simple, index=c("NID", "factor_ym"), model="within")
summary(ModelDI)


#Calculate bed growth

##Inferent Statistics

#Fixed or Random Effects? Hausmann Test
random <- plm(occup_rate ~ log_ABsupply + as.factor(year_month) + as.factor(NID), data=analysis_simple, index=c("NID", "year_month"), model="random")
fixed <- plm(occup_rate ~ log_ABsupply + as.factor(year_month) + as.factor(NID), data=analysis_simple, index=c("NID", "year_month"), model="within")

phtest(fixed, random) #Hausmann Test -> random effects model

#Testing for random effects: 
pool <- plm(occup_rate ~ log_ABsupply + as.factor(year_month) + as.factor(NID), data=analysis_simple, index=c("NID", "year_month"), model="pooling")
plmtest(pool, type=c("bp"))

#Testing time-fixed effects. The null is that no time-fixed effects needed
fixed.time <- plm(occup_rate ~ log_ABsupply + as.factor(year_month), data=analysis_simple, index=c("NID", "year_month"), model="within")
pFtest(fixed.time, fixed) #p-value <.05 -> time fixed effects

#Lagrange Multiplier Test - time effects (Breusch-Pagan)
plmtest(fixed, c("time"), type=("bp")) #p-value <.05 -> time fixed effects

#Augmented Dickey-Fuller Test
#The Dickey-Fuller test to check for stochastic trends. The null hypothesis is that the series 
#has a unit root (i.e. non-stationary). If unit root is present you can take the first difference 
#of the variable.
library(tseries)
Panel.set <- plm.data(analysis_simple, index = c("NID", "year_month"))
adf.test(Panel.set$log_ABsupply, k=2) #p-value <.05 -> no unit roots present

detect_lin_dep(analysis_simple)

## y=log_nights

#ModelIa: 
ModelI <- felm(analysis_simple$log_nights ~ analysis_simple$log_ABsupply | as.factor(analysis_simple$NID) + as.factor(analysis_simple$year_month))
summary(ModelIa)

#Model Ib: 
ModelIa <- plm(log_nights ~ log_ABsupply, data=analysis_simple, index=c("NID", "factor_ym"), model="within")
summary(ModelIb)

#LM
ModelIa <- lm(log_nights ~ log_ABsupply + NID + factor_ym, data=analysis_simple)
summary(ModelIb)

#Model II: 
ModelII <- plm(log_nights ~ log_ABsupply + AB_supply + logABAB + log_inc + ue_rate + marketentry, data=analysis_simple, index=c("NID", "factor_ym"), model="within")
summary(ModelII)

ModelIILM <- lm(log_nights ~ log_ABsupply + AB_supply + logABAB + log_inc + ue_rate + marketentry + NID + factor_ym, data=analysis_simple)
summary(ModelIILM)

#Testing whether fixed effects is needed. The null is that no time-fixed effects needed
pFtest(ModelII, ModelIILM) #p-value <.05 -> fixed effects

#Model A: includes economic control variables and district dummy
ModelA <- plm(occup_rate ~ log_ABsupply, data=analysis_simple, index=c("NID", "factor_ym"), model="within")
summary(ModelA)

#Model C: introduces an interaction between log_airbnb supply and the absolute level of airbnb supply
ModelC <- plm(occup_rate ~ log_ABsupply + AB_supply + logABAB + log_inc + ue_rate, data=analysis_simple, index=c("NID", "factor_ym"), model="within")
summary(ModelC)

#Model D: introduces a binary variable for Airbnb's official market entry
ModelD <- plm(occup_rate ~ log_ABsupply + AB_supply + logABAB + log_inc + ue_rate + marketentry, data=analysis_simple, index=c("NID", "factor_ym"), model="within")
summary(ModelD)

#Model D (LM): introduces a binary variable for Airbnb's official market entry
ModelDLM <- lm(occup_rate ~ log_ABsupply + AB_supply + logABAB + log_inc + ue_rate + marketentry  + NID + factor_ym, data=analysis_simple)
summary(ModelDLM)

# F-test for joint significance (p-value < .01 -> highly joint significance)
linearHypothesis(ModelD, c("log_ABsupply", "logABAB = 0"), test="F")

linearHypothesis(ModelD, c("AB_supply", "logABAB = 0"), test="F") 

ModelI <- lm(log_nights ~ log_ABsupply + NID + year_month, data=analysis_simple)
summary(ModelI)
