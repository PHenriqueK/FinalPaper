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

