#FinalPaper for Introduction to Collaborative Social Science Data Analysis (MPP-E1180)

Dan Murphy & Paulo Kalkhake

Latest Update: December 16, 2016

##The Effect of Airbnb on Hotels: Evidence from Berlin

###Project Background

This repository contains the material for the collaborative research project in 'Introduction to Collaborative Social Science Data Analysis' (MPP-E1180) @HSoG Berlin. 

###Abstract

#TO DO 
- USE POOLED OLS
- MAP WITH EVERY DISTRICT AND DENSITY COMPARING 2010 and 2014

###Repository Files

In addition to this readme file, there are several folders and files in this repository.

| File/Folder                    | Description |
| ----------------------- | --------------------- | 
| code    | Includes all relevant code to clean, to merge, and to run relevant statistical analysis | 
| data    | Includes relevant data | 
| presentation  | Includes the necessary files; and the presentation html-file  |
| website  | Includes the necessary files; and the website html-file  |
| Bibliography.bib | Relevant references in BibTeX format |

### Contact
Comments and feedback are encouraged, and can be sent to [Paulo Kalkhake](mailto: p.kalkhake@mpp.hertie-school.org(p.kalkhake@mpp.hertie-school.org)) or [Dan Murphy](mailto:danieljmurphy01@gmail.com)).


________

##To Do

#Rsults in a positive correlation, tell why (tourism, etc). What to do?!?!? 
1. Model A: log_occup_rate = log_supply * econ_control * neighborhood_dummy

# Here, we also introduce time FE to mitigate the above effect
2. Model B: log_occup_rate = log_supply * econ_control * neighborhood_dummy * year_dummy 
#Year must be as.factor()

# The effect of AIRBNB is likely best observed once it has reached a critical mass. So, here we introduce an interaction between log_airbnb supply and the absolute level of airbnb supply.
3. Model C: log_occup_rate = log_supply * econ_control * neighborhood_dummy * year_dummy * interaction variable