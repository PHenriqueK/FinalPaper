#####################
## R source to first pair assignment
## Dan Murphy & Paulo Kalkhake
## Last update: 11 November 2016
## R version 3.3.1 (2016-06-21) -- "Bug in Your Hair"
## Platform: x86_64-apple-darwin13.4.0 (64-bit)
## What it does: Sets working directory & loads packages and datasets
##########################

# remove objects from the specified environment
rm(list=ls())  

# Set working directory
try(setwd("/Users/Paulo/GitHub/FinalPaper/"),silent=TRUE)
try(setwd("/Users/djm113/Documents/GitHub/FinalPaper/"),silent=TRUE)

# Load Packages: credits to Wickham Hadley, Xie Yihui, Frank Harrell Jr, Philippe Grosejan & Frederic Ibanez and others
# (install when needed)

library(ggplot2)
library(car)
library(plm)
library(lubridate)
library(knitr)
library(pastecs)
library(Hmisc)
library(curl)
library(rio)
library(stringr)
library(plyr)
library(xml2)
library(DataCombine)
library(tidyr)
library(data.table)
library(dplyr)
library(zoo)
library(xtable)
library(texreg)
library(stargazer)
library(foreign)
library(gplots)

#Loading Data from "Regionaldatenbank Deutschland"
GENESIS_2010 <- read.csv("data/GENESIS_2010.csv", sep=";", quote="\"", stringsAsFactors = FALSE)
GENESIS_2011 <- read.csv("data/GENESIS_2011.csv", sep=";", quote="\"", stringsAsFactors = FALSE)
GENESIS_2012 <- read.csv("data/GENESIS_2012.csv", sep=";", quote="\"", stringsAsFactors = FALSE)
GENESIS_2013 <- read.csv("data/GENESIS_2013.csv", sep=";", quote="\"", stringsAsFactors = FALSE)
GENESIS_2014 <- read.csv("data/GENESIS_2014.csv", sep=";", quote="\"", stringsAsFactors = FALSE)

#Loading data from the Statistical Office Berlin/Brandenburg
SBB_2010_2015 <- read.csv("data/SBB_2010_2015.csv", sep=";", stringsAsFactors = FALSE)
SBB_unemployment <- read.csv2("data/SBB_unemployment.csv", stringsAsFactors = FALSE)
SBB_HH_Income <- read.csv2("data/SBB_HH_Income.csv", header = FALSE, stringsAsFactors = FALSE)
