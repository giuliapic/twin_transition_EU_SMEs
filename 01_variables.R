#libraries 

library(haven)
library(tidyverse)
library(dplyr)
library(sjPlot)


#uploading the datasets for 486 (green developement)

eb498_green <- read_dta("C:/data_giulia/data/eurobarometer/data_489_green.dta")


table(eb498_green$isocntry)


#selecting the country "italy" 

eb498_green_it <- eb498_green[eb498$isocntry == "IT", ]
eb498_green_it



#test

#prova2
#prova provata








