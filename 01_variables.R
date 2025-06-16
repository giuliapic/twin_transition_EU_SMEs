#libraries 

library(haven)
library(tidyverse)
library(dplyr)
library(sjPlot)


#uploading the datasets: 489 (green) + 4 86 (digital)

eb_digital_raw <- read_dta("C:/data_giulia/data/eurobarometer/data_486_digital.dta")


#which countries are in the survey 
table(eb_digital_raw$isocntry)

#how many observation do we have? 
#population: 16.365


#selecting the variables for EB_digital 



#DEPENDENT VARIABLE OF GREEN SUSTAINABILITY

# eco innovation Q19_5 --> DUMMY 

attr(eb_digital_raw$q19_5, "labels")

eb_digital <- eb_digital_raw %>%
  mutate(eco_innovation = ifelse(q19_5 == 1, 1, 0))


# recycling 
eb_digital <- eb_digital_raw %>%
   mutate(recycling = ifelse(q24_1 == 1, 1, 0))

 
 #resource reduction 
eb_digital <- eb_digital_raw %>%
  mutate(res_red = ifelse(q24_2 == 1, 1, 0))


# energy saving 
eb_digital <- eb_digital_raw %>%
  mutate(energy_saving = ifelse(q24_2 == 1, 1, 0))


# sustainable products 
eb_digital <- eb_digital_raw %>%
  mutate(sust_prod = ifelse(q24_2 == 1, 1, 0))



#---------------



#INDEPENDENT VARIABLE 

#AI
eb_digital <- eb_digital_raw %>% 
  mutate(AI_var = ifelse(q23_1 == 1, 1, 0))

#bigdata
eb_digital <- eb_digital_raw %>% 
  mutate(bigdata = ifelse(q23_5 == 1, 1, 0))

#cloud
eb_digital <- eb_digital_raw %>% 
  mutate(cloud = ifelse(q23_2 == 1, 1, 0))

#highspeed
eb_digital <- eb_digital_raw %>% 
  mutate(highspeed = ifelse(q23_6 == 1, 1, 0))

#robot
eb_digital <- eb_digital_raw %>% 
  mutate(robot = ifelse(q23_3 == 1, 1, 0))

#smart devices
eb_digital <- eb_digital_raw %>% 
  mutate(smart = ifelse(q23_4 == 1, 1, 0))





#modifica dataset originale con eb_digital 

eb_digital_raw$q23_1

select(eb_digital_raw, q23_1:q23_7)

eb_digital <- mutate(eb_digital_raw, digital_index = q23_1 + q23_2 + q23_3 + q23_4 + q23_5 + q23_6 + q23_7)

#aggiungiamo tutte le altre variabili + digital index 


















