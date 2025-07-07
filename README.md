# Twin Transition in European SMEs

This repository contains all materials related to the final project for the course *Multivariate analysis for social scientists* at Università degli Studi di Milano. The analysis investigates the relationship between digitalisation and environmental practices among European SMEs, using data from the Flash Eurobarometer 486 (2020).


# Data source:  
Flash Eurobarometer 486 (2020) – https://search.gesis.org/research_data/ZA7637 


# Analysis:

- Model: Multivariate Probit  
- Software: R    
- Dependent variables: five binary indicators of environmental practices  
- Main predictors: binary indicators of digital technology adoption  
- Controls: firm size, sector (NACE), family ownership, financial capacity, skill shortages, urban/industrial location, countries 

Data transformations and modelling steps are reported and documented in the scripts.

# Dependencies:

- library(haven) 
- library(tidyverse)
- library(dplyr)
- library(readr)
- library(tibble)
- library(mvProbit)
- library(forcats)
- library(magrittr)



