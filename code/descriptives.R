##Descriptive & Graphical analysis
#Heterogeneity across years and neighbourhoods/ plotmeans draw a 95% confidence interval around the means

plotmeans(occup_log ~ NID, main="Heterogeineity across neighbourhoods", data=analysis_simple)
plotmeans(occup_log ~ year, main="Heterogeineity across years", data=analysis_simple)

##Descriptice Statistics
coplot(AB_supply ~ year_month|NID, data=analysis_simple)
plot(AB_supply ~ year_month, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=analysis_simple)

library(ggplot2)

plot1 <- ggplot(analysis_simple, aes(x = year_month, 
                                     y = log_ABsuppy,
                                     colour = marketentry) + 
                                     geom_point())
plot2 <- plot1 + stat_smooth(method = loess)
plot2

library(coefplot)
multiplot(ModelA,ModelD)

##### Further statistical tests #####
#Fixed or Random Effects? Hausmann Test
random <- plm(occup_rate ~ log_ABsupply + log_inc + ue_rate + arrivals, data=analysis_simple, index=c("NID", "year_month"), model="random")
fixed <- plm(occup_rate ~ log_ABsupply + log_inc + ue_rate + arrivals, data=analysis_simple, index=c("NID", "year_month"), model="within")

phtest(fixed, random) #Hausmann Test -> fixed effects model

#Testing time-fixed effects. The null is that no time-fixed effects needed
fixed.time <- plm(occup_rate ~ log_ABsupply + log_inc + ue_rate + arrivals, data=analysis_simple, index=c("NID", "year_month"), model="within")
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