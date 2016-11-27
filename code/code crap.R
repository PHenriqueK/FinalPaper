ModelI <- LMI(danalysis_simple$log_nights, danalysis_simple)
summary(ModelI)

# F-test for joint significance (p-value < .01 -> highly joint significance)
linearHypothesis(ModelD, c("log_ABsupply", "logABAB = 0"), test="F")

linearHypothesis(ModelD, c("AB_supply", "logABAB = 0"), test="F") 

##### Further statistical tests #####
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

#Testing whether fixed effects is needed. The null is that no time-fixed effects needed
pFtest(ModelII, ModelIILM) #p-value <.05 -> fixed effects