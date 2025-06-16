#libraries 

library(haven)
library(tidyverse)
library(dplyr)
library(sjPlot)


#uploading the datasets: 489 (green) + 4 86 (digital)

eb_digital_raw <- read_dta("C:/data_giulia/data/eurobarometer/data_486_digital.dta")

eb_digital <- eb_digital_raw

#which countries are in the survey 
table(eb_digital$isocntry)

#how many observation do we have? 
#population: 16.365


#N.B. 
# variables imported with labelled numeric values (e.g., 0 = "Not mentioned", 1 = "Yes") 
# do not contain actual NA values unless explicitly coded as such in the dataset. 
# Therefore, missing values are not expected and are not filtered out during binary recoding. 
# This applies to all similar variables with labelled 0/1 structures across the dataset.



#DEPENDENT VARIABLE - green - recoded in dummies


# eco innovation Q19_5

attr(eb_digital$q19_5, "labels")

eb_digital <- eb_digital %>%
  mutate(eco_innovation = ifelse(q19_5 == 1, 1, 0))


# recycling 
eb_digital <- eb_digital %>%
   mutate(recycling = ifelse(q24_1 == 1, 1, 0))

 
 #resource reduction 
eb_digital <- eb_digital %>%
  mutate(res_red = ifelse(q24_2 == 1, 1, 0))


# energy saving 
eb_digital <- eb_digital %>%
  mutate(energy_saving = ifelse(q24_2 == 1, 1, 0))


# sustainable products 
eb_digital <- eb_digital %>%
  mutate(sust_prod = ifelse(q24_2 == 1, 1, 0))



#---------------



#INDEPENDENT VARIABLE - digital - recoded in dummies

#AI
eb_digital <- eb_digital %>% 
  mutate(AI_var = ifelse(q23_1 == 1, 1, 0))

#bigdata
eb_digital <- eb_digital %>% 
  mutate(bigdata = ifelse(q23_5 == 1, 1, 0))

#cloud
eb_digital <- eb_digital %>% 
  mutate(cloud = ifelse(q23_2 == 1, 1, 0))

#highspeed
eb_digital <- eb_digital %>% 
  mutate(highspeed = ifelse(q23_6 == 1, 1, 0))

#robot
eb_digital <- eb_digital %>% 
  mutate(robot = ifelse(q23_3 == 1, 1, 0))

#smart devices
eb_digital <- eb_digital %>% 
  mutate(smart = ifelse(q23_4 == 1, 1, 0))


#------------------


#CONTROL VARIABLES - firm level - recoded in dummies


#Export
#the variable has value 1 if it export less than 25%, 2 if it exports between 25% and 50%, 3 if it exports more than 50%, 4 = don't know/no answer, 5= NA
#the purpose is to create a variable that has value 1 if the firm answer 1,2 or 3. 
#first i recode each answer in binary form, than i create the new variable that contains at least 1 of the 3 answer recoded. 


attr(eb_digital_raw$q12b, "labels")

eb_digital <- eb_digital %>%
  mutate(
    export = ifelse(q12b %in% 1:3, 1, ifelse(q12b == 4,0,NA))
  )

table(eb_digital$export, useNA = "always") #NAs are +90%, maybe we should check/create a variable for "non export"



#family owned _ 1= mostly or entirely family owned 
eb_digital <- eb_digital %>%
  mutate(fam_owned = ifelse(q13_7 == 1, 1, 0))


#financecap _ if it is high capability 
eb_digital <- eb_digital %>%
  mutate(
    financecap = ifelse(q4a %in% 7:8, 1,
                        ifelse(q4a %in% 1:6, 0, NA)))
    

table(eb_digital$financecap, useNA = "always")


#highgrowth 
eb_digital <- eb_digital %>%
  mutate(
    highgrowth = ifelse(q5_1 == 4 & q5_2 ==4, 1,
                        ifelse(q5_1 %in% 1:3 & q5_2 %in% 1:3, 0, NA)) #the answer 5 is treated as NA, following the questionnaire
  )

table(eb_digital$highgrowth, useNA = "always")



#localisation industrial 
eb_digital <- eb_digital %>%
  mutate(
    indstrl_area = ifelse(q8_4 == 1, 1, 0))

table(eb_digital$indstrl_area)

#localisation urban 
eb_digital <- eb_digital %>% 
  mutate( 
    urban_area = ifelse(q8_1 == 1, 1, 0))

table(eb_digital$urban_area)


#Old firms (founded before 2000 = 1)
attr(eb_digital$q1, "labels")

eb_digital <- eb_digital %>%
  mutate(
    old_firm = ifelse(q1 == 4, 1, 0))

table(eb_digital$old_firm)


#skillshortage 
eb_digital <- eb_digital %>%
  mutate(
    skillshortage = ifelse(q26_4 == 1,1,0)
  )


#Size 





#modifica dataset originale con eb_digital 

eb_digital_raw$q23_1

select(eb_digital_raw, q23_1:q23_7)

eb_digital <- mutate(eb_digital_raw, digital_index = q23_1 + q23_2 + q23_3 + q23_4 + q23_5 + q23_6 + q23_7)

#aggiungiamo tutte le altre variabili + digital index 


















