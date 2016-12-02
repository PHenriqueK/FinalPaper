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

##Estimation Results (2) ##
``` {r, echo = FALSE, include=TRUE, message = FALSE, error = FALSE, results = "asis"}
ModelIV <- FEV(analysis_simple$occup_rate, analysis_simple)

stargazer(ModelIV, 
          digits = 3, 
          covariate.labels = c("Airbnb Listings", "Sqrd. Airbnb Listings", "Average HH Income (Log)", "Unemployment Rate", "Incoming Passengers", "Market Entry"), 
          omit = c("NID", "factor_ym"),           
          omit.stat = c('f', 'ser'), # to nicely fits on the page
          omit.labels = c("Neighbourhood-specific trend", "Time trend"), 
          dep.var.labels = c("Occupancy Rate"),
          out.header = FALSE, 
          model.names = FALSE,
          header = FALSE,
          add.lines = list(c('District & Time FE?', rep('Yes', 4))),
          align = FALSE,
          type = "html")

```



## Estimation Results (2)

```{r, echo = FALSE}

ModelIIIframe <- data.frame(Variable = rownames(summary(ModelIII)$coef), Coefficient = summary(ModelIII)$coef[,1], SE = summary(ModelIII)$coef[,2], modelName = "district Model")

ModelIIIframe <- ModelIIIframe[-c(1,2,3,4),]

ModelIIIframe$Variable <- c("Mitte", "Friedrichshain-Kreuzberg", "Pankow", "Charlottenburg-Wilm.", "Spandau", "Steglitz - Zehlendorf", "Tempelhof - Schöneberg", "Neukölln", "Treptow - Köpenick", "Marzahn - Hellersdorf", "Lichtenberg", "Reinickendorf")

# Specify the width of confidence intervals
interval <- -qnorm((1-0.95)/2)  # 95% multiplier

# Plot

zp1 <- ggplot(ModelIIIframe)

zp1 <- zp1 + geom_linerange(aes(x = Variable, ymin = Coefficient - SE*interval1,
                                ymax = Coefficient + SE*interval1),
                            lwd = 1, position = position_dodge(width = 1/2))
zp1 <- zp1 + geom_pointrange(aes(x = Variable, y = Coefficient, ymin = Coefficient - SE*interval2,
                                 ymax = Coefficient + SE*interval2),
                             lwd = 1/2, position = position_dodge(width = 1/2),
                             shape = 21, fill = "WHITE")
zp1 <- zp1 + ggtitle("Comparing Various Country Coefficients") + 
  theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5), 
        axis.text.x = element_text(face = "bold"), 
        axis.title.y = element_text(face = "bold")) + 
  labs(x = "Countries") 
print(zp1)

```

<div class="centered">
  Remember, a lower number (i.e., closer to 1) equates to a higher political interest. 
</div>