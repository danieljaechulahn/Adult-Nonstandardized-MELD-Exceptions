# Association of Non-Standardized MELD Score Exceptions with Waitlist Mortality in Adult Liver Transplant Candidates

This repository contains all code for data preparation and anlysis done for this manuscript.

Data source was the Q1 2024 Scientific Registry of Transplant Recipients (SRTR) Standard Analysis Files (SAF)

## Data Preparation

The Data Preparation Notebook RMD creates a dataset with information on demographics and clinical characteristics, listing center ID, outcome on the waitlist (death, transplantation). Code used for exclusion criteria (multiple registrations, Status 1 at initial listing, received a standardized MELD exception at the time of listing, already listed on another solid organ waiting list) are all provided. The final datasets used for analysis are labeled as "finaldataset" (one observation per candidate) and "timeseriesdata," which is a longitudinal version of "finaldataset," that includes every observation recorded per candidate on the waitlist. The "timeseriesdata" dataset was used for the mixed-effects Cox models and estimation of generalized c-indices. 

## Data Analysis

The code for Table 1 and figures in the main text of the manuscript is shown in the "Paper Figures", "Table 1", and "Figure 5 Map Code" RMDs. 

Information on the construction of the mixed effects Cox proportional hazards models is provided in "Mixed Effects Models" RMD. Our code for calculating the generalized c-indices of allocation and laboratory MELD using the risksetROC R package is in "Generalized C Indices" RMD. Code for our sensitivity analyses are in "Sensitivity Analysis" RMD, and information on how we censored the pre-NLRB cohort is in the "NLRBCensoring" document.
