# FinalPaper for Introduction to Collaborative Social Science Data Analysis (MPP-E1180)

Dan Murphy & Paulo Kalkhake

Latest Update: November 18, 2016

##The Effect of Airbnb's Market Entry on Hotels in Berlin: A Regression Discontinuity Approach.

###Project Background

This repository contains the material for the Collaborative Research Project in Introduction to Collaborative Social Science Data Analysis (MPP-E1180) @HSoG Berlin. We seek to illustrate the magnitude of the "Airbnb effect" on hotels in Berlin, using a Regression Discontinuity Model (RDM).

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
Comments and feedback are encouraged, and can be sent to [Paulo Kalkhake](mailto: p.kalkhake@mpp.hertie-school.org(p.kalkhake@mpp.hertie-school.org)) or [Dan Murphy](mailto:danieljmurphy01@gmail.com)).






__________________________________________
For the Collaborative Research Project you will pose an interesting social science question and attempt to answer it using standard academic practices including original data collection and statistical analysis. The project should be considered a ‘dry run’ for your thesis. The project has three presentation outputs designed to present your research to multiple audiences. The first is a oral presentation (10 minute maximum) given in the final class. The second is a standard academic paper (5,000 words maximum) that is fully reproducible and dynamically generated. The third is a website designed to present key aspects of your research in an engaging way to a general audience. The paper and website are due in the Final Exam Week. The presentation and website are each worth 10% of your final mark. The paper is worth 30%.

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

## Collaborative Research Project (3)

As always, you should **submit one GitHub repository** with all of the
materials needed to **completely reproduce** your data gathering, analysis, and
presentation documents.

**Note**: Because you've had two assignments already to work on parts of the
project, I expect **high quality work**.

## Collaborative Research Project (4)

Find one other group to be a **discussant** for your presentation.

The discussants will provide a quick (max 2 minute) critique of your 
presentation--ideas for things you can improve on your paper.


________________
# Gandrud criticsms of third pair assignment

## Gathered/Cleaned/Merged 2 Data Sets (if applicable)

Good discussion of data sources, gathering, cleaning, and merging.

## Descriptive Statistics for Evaluating Research Question

Generally nice plots for describing the data. Though somewhat scant exploration of the data. You could have explored the data in much more detail.

Unclear from the plot what the lines in the first figure show.

## Inferential Statistics for Evaluating Research Question (attempted, but not required)

What you are really interested in is not the level of listings vs hotel occupancy, but instead how the change in the former affects the change in the latter.

## Dynamically Generated Tables/Figures

Dynamically generated plots, but no tables.



**Total Mark**:  8.5/10