#Third Pair Assignment for Introduction to Collaborative Social Science Data Analysis (MPP-E1180)

Dan Murphy & Paulo Kalkhake

Latest Update: November 11, 2016

##The Effect of Airbnb's Market Entry on Hotels in Berlin: A Regression Discontinuity Approach.

###Project Background

This repository contains the material for the Third Pair Assignment in Introduction to Collaborative Social Science Data Analysis (MPP-E1180) @HSoG Berlin. Here, we gather, clean, and merge all of the relevant data for our final project. We also illustrate the data with descriptive statistics and a basic inferential model. In our paper, we will seek to illustrate the magnitude of the "Airbnb effect" on hotels in Berlin, using a Regression Discontinuity Model (RDM).

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

In order to successfully run the code, either run the *assignment03_analysis.R* file with the source code, or run the files in the following order. 

> *1. data.R*

> *2. data_airbnb.R*

> *3. FSO_data_manipulation.R*

> *4. SBB_data_manipulation.R*

> *5. airbnb_data_manipulation.R*

> *6. merge.R*

> *7. assignment03_analysis.R*

### Contact
Comments and feedback are encouraged, and can be sent to [Paulo Kalkhake](mailto: p.kalkhake@mpp.hertie-school.org(p.kalkhake@mpp.hertie-school.org)) or [Dan Murphy](mailto:d.murphy@mpp.hertie-school.org(d.murphy@mpp.hertie-school.org)).
