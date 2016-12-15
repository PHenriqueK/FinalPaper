library(ggmap)
berlinmap <- qmap("Berlin", zoom = 12)

FinalMap <- berlinmap +
  geom_point(aes(x = longitude, y = latitude),
             data = Detailed_Listings,
             alpha=0.5, color="#c0392b", size = .7) +
  #geom_point(aes(x = stations$coords.x1, y = stations$coords.x2), data = stations) +
  xlab('') + ylab('') +
  theme(axis.ticks = element_blank(), 
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

print(FinalMap)

cor(analysis_simple$AB_supply, analysis_simple$occup_rate) 



ModelI <- LMI(danalysis_simple$log_nights, danalysis_simple)
summary(ModelI)

# F-test for joint significance (p-value < .01 -> highly joint significance)
linearHypothesis(ModelD, c("log_ABsupply", "logABAB = 0"), test="F")

linearHypothesis(ModelD, c("AB_supply", "logABAB = 0"), test="F") 



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

#creating growth variable
analysis_simple$growth <- ddply(analysis_simple,"NID",transform, Growth=c(NA,exp(diff(log(AB_supply)))-1))
analysis_simple$growth[analysis_simple$growth=="NaN"] <- 0
analysis_simple$growth[analysis_simple$growth=="-Inf"] <- 0
analysis_simple$growth[analysis_simple$growth=="NA"] <- 0
analysis_simple$growth[analysis_simple$growth=="Inf"] <- 0
analysis_simple$growth[is.na(analysis_simple$growth)] <- 0

analysis_simple$ymym <- as.yearmon(analysis_simple$year_month)

ggplot(analysis_simple, aes(x=year_month, y=growth.Growth, group=neighbourhood, colour=neighbourhood)) +
  geom_line(alpha=0.7) + 
  labs(x="Months", y="Number of Airbnb Listings") + 
  scale_colour_discrete(name ="Neighbourhood") +
  scale_x_yearmon(as.yearmon(analysis_simple$ymym), format = "%Y-%m", n = 6)
</div>
  
  
  ##### Inferent Statistics #####
LMI <- function(x, y) { 
  (lm(x ~ log_ABsupply + NID + factor_ym, data=y))
}

LMII <- function(x, y) { 
  (lm(x ~ 0 + log_ABsupply + log_inc + ue_rate + arrivals + as.factor(NID) + factor_ym, data=y))
}

LMIII <- function(x, y) { 
  (lm(x ~ 0 +log_ABsupply + log_inc + ue_rate +  arrivals + marketentry + as.factor(NID) + factor_ym, data=y))
}

LMIV <- function(x, y) { 
  (lm(x ~ 0 + log_ABsupply + AB_supply + logABAB + log_inc + ue_rate + arrivals + marketentry + NID + factor_ym, data=y))
}

LMV <- function(x, y) { 
  (lm(x ~ 0 + AB_supply + AB_supply_2 + log_inc + ue_rate + dmarketentry + arrivals + NID + factor_ym, data=y))
}

FEI <- function(x, y) { 
  plm(x ~ log_ABsupply + log_inc + ue_rate + arrivals, data=y, index=c("NID", "factor_ym"), model="within")
}

FEII <- function(x, y) {
  plm(x ~ log_ABsupply + log_inc + ue_rate + arrivals + dmarketentry, data=y, index=c("NID", "factor_ym"), model="within")
}

FEIIm <- function(x, y) {
  plm(x ~ log_ABsupply + log_inc + ue_rate + arrivals + marketentry, data=y, index=c("NID", "factor_ym"), model="within")
}

FEIIz <- function(x, y) {
  plm(x ~ log_ABsupply + log_inc + ue_rate + arrivals + ZEV, data=y, index=c("NID", "factor_ym"), model="within")
}

FEIIdm <- function(x, y) {
  plm(x ~ log_ABsupply + log_inc + ue_rate + arrivals + marketentry + dmarketentry, data=y, index=c("NID", "factor_ym"), model="within")
}


FEIIa <- function(x, y) {
  plm(x ~ AB_supply + log_inc + ue_rate + arrivals, data=y, index=c("NID", "factor_ym"), model="within")
}

FEIII <- function(x, y) {
  plm(x ~ log_ABsupply + AB_supply + logABAB + log_inc + ue_rate + arrivals, data=y, index=c("NID", "factor_ym"), model="within")
}

#Introduces a binary variable for Airbnb's official market entry
FEIV <- function(x, y) {
  plm(x ~ log_ABsupply + AB_supply + logABAB + log_inc + ue_rate + arrivals, data=y, index=c("NID", "factor_ym"), model="within")
}

#Introduces a binary variable for Airbnb's official market entry + nonlinear
FEV <- function(x, y) {
  plm(x ~ AB_supply + AB_supply_2 + log_inc + ue_rate + arrivals + marketentry, data=y, index=c("NID", "factor_ym"), model="within")
}
