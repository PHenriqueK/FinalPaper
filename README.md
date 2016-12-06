# FinalPaper for Introduction to Collaborative Social Science Data Analysis (MPP-E1180)

Dan Murphy & Paulo Kalkhake

Latest Update: November 18, 2016

##The Effect of Airbnb's Market Entry on Hotels in Berlin: A Regression Discontinuity Approach.

###Project Background

This repository contains the material for the Collaborative Research Project in Introduction to Collaborative Social Science Data Analysis (MPP-E1180) @HSoG Berlin. We seek to illustrate the magnitude of the "Airbnb effect" on hotels in Berlin, using a Regression Discontinuity Model (RDM).

#TO DO 
- USE POOLED OLS
- MAP WITH EVERY DISTRICT AND DENSITY COMPARING 2010 and 2014

###Repository Files

In addition to this readme file, there are 11 files in this repository.

| File                    | Description |
| ----------------------- | --------------------- | 
| Assignment_3_Markdown.Rmd    | Markdown document that contains the assignment in raw form | 
| Assignment_3_Markdown.pdf  | Assignment in pdf format  |
| assignment03_analysis.R  | Statistical analysis of cleaned and merged data  |
| merge.R  | Merges all of the cleaned data sets  |
| data.R | Loads packages and Berlin/Brandenburg data  |
| data_airbnb.R | Loads Airbnb data  |
| SBB_data_manipulation.R | Cleans Statistical Office of Berlin/Brandenburg data  |
| FSO_data_manipulation.R | Cleans Federal Statistics Office data  |
| airbnb_data_manipulation.R | Cleans Airbnb data  |
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

### MAYBE. 


**Deadline**:

- Presentation: In-class 2 December

- Website/Paper: 16 December 2016

## Collaborative Research Project (2)

The project can be thought of as a 'dry run' for your thesis with multiple 
presentation outputs.

Presentation: 10 minutes **maximum**. **Engagingly** present your research
question and key findings to a general academic audience (fellow students).

Paper: 5,000 words maximum. **Standard academic paper**, properly cited laying out
your research question, literature review, data, methods, and findings.

Website: An engaging website designed to convey your research to **a general
audience**.