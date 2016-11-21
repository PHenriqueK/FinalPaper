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
analysis_simple <- data_2010_2014[, c("NID", "neighbourhood", "year_month", "year", "month", "occup_rate", "AB_supply", "avg_inc", "ue_rate", "guests" )]
analysis_simple$NID <- as.factor(analysis_simple$NID)

#Group data
analysis_simple <- group_by(analysis_simple, NID, year, year_month)

#Log Airbnb apt supply and hotel occupancy rate
analysis_simple$log_ABsupply <- log(analysis_simple$AB_supply)
analysis_simple$log_ABsupply[which(!is.finite(analysis_simple$log_ABsupply))] <- 0
analysis_simple$occup_log <- log(analysis_simple$occup_rate)

#Create binary variable for Airbnb's official market entry in June 2011
analysis_simple$marketentry <- ifelse ((analysis_simple$year_month < "Juni 2011"), 0, 1)

##Descriptice Statistics
coplot(AB_supply ~ year_month|NID, data=analysis_simple)
plot(AB_supply ~ year_month|NID, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=analysis_simple)

#Fixed effects: Regular OLS regression does not consider heterogeneity across groups or time

#Heterogeneity across years and neighbourhoods/ plotmeans draw a 95% confidence interval around the means
library(foreign)
library(gplots)
plotmeans(occup_log ~ NID, main="Heterogeineity across neighbourhoods", data=analysis_simple)
plotmeans(occup_log ~ year, main="Heterogeineity across years", data=analysis_simple)


##Inferent Statistics

#Fixed or Random Effects? Hausmann Test
#To decide between fixed or random effects you can run a Hausman test where the null hypothesis is that the preferred model 
#is random effects vs. the alternative the fixed effects (see Green, 2008, chapter 9). It basically tests whether the unique 
#errors (ui) are correlated with the regressors, the null hypothesis is they are not. Run a fixed effects model and save the 
#estimates, then run a random model and save the estimates, then perform the test. If the p-value is significant 
#(for example <0.05) then use fixed effects, if not use random effects.

random <- plm(occup_log ~ log_ABsupply, data=analysis_simple, index=c("NID", "year_month"), model="random")
fixed <- plm(occup_log ~ log_ABsupply, data=analysis_simple, index=c("NID", "year_month"), model="within")

phtest(fixed, random) #Hausmann Test -> random effects model

#Testing for random effects: Breusch-Pagan Lagrange multiplier (LM)
#The null hypothesis in the LM test is that variances across entities is zero. 
#This is, no significant difference across units (i.e. no panel effect).
#Here we do not reject the null hypothesis and conclude that random effects is appropriate. 
#This is, there is evidence of significant differences across neighbourhoods, therefore we run a random effects model
pool <- plm(occup_log ~ log_ABsupply , data=analysis_simple, index=c("NID", "year_month"), model="pooling")
plmtest(pool, type=c("bp"))

#Testing time-fixed effects. The null is that no time-fixed effects needed
fixed.time <- plm(occup_log ~ log_ABsupply + as.factor(year_month), data=analysis_simple, index=c("NID", "year_month"), model="within")
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

#Model A: includes economic control variables and district dummy
ModelA <- plm(occup_log ~ log_ABsupply + as.factor(NID) + as.factor(year_month), data=analysis_simple, index=c("NID", "year_month"), model="random")
summary(ModelA)

#Model B: introduces year_month and neighbourhood dummy
ModelB <- plm(occup_log ~ log_ABsupply + avg_inc + ue_rate+ as.factor(NID) + as.factor(year_month), data=analysis_simple, index=c("NID", "year_month"), model="random")
ModelB2 <- plm(occup_log ~ log_ABsupply + avg_inc + ue_rate+ as.factor(NID) + as.factor(year), data=analysis_simple, index=c("NID", "year_month"), model="random")
summary(ModelB)
summary(ModelB2)

#Model C: introduces an interaction between log_airbnb supply and the absolute level of airbnb supply
ModelC <- plm(occup_log ~ log_ABsupply + log_ABsupply*AB_supply + avg_inc + ue_rate+ as.factor(NID) + as.factor(year_month), data=analysis_simple, index=c("NID", "year_month"), model="random")
summary(ModelC)

#Model D: introduces a binary variable for Airbnb's official market entry
ModelD <- plm(occup_log ~ log_ABsupply + log_ABsupply*AB_supply + marketentry + avg_inc + ue_rate + as.factor(NID) + as.factor(year), data=analysis_simple, index=c("NID", "year_month"), model="random")
summary(ModelD)

#Model E: Model Mitte 
subset_mitte <- subset(analysis_simple, analysis_simple$NID==1)
ModelE <- plm(occup_log ~ log_ABsupply + log_ABsupply*AB_supply + marketentry + avg_inc + ue_rate + as.factor(year_month), data=subset_mitte, index="year_month", model="random")
ModelE <- lm(occup_log ~ log_ABsupply + log_ABsupply*AB_supply + marketentry + avg_inc + ue_rate + as.factor(year), data=subset_mitte)
summary(ModelE)

# F-test for joint significance (p-value < .01 -> highly joint significance)
linearHypothesis(ModelD, c("log_ABsupply", "log_ABsupply:AB_supply = 0"), test="F")

linearHypothesis(ModelD, c("AB_supply", "log_ABsupply:AB_supply = 0"), test="F") 

#Create clean labels
labels <- c('Log Airbnb supply', 'Average income/district',
            'Unemployment rate', 'NID',
            '4th Ranked School', '(Intercept)')

