
# ZOTEO -->CHECK IT, IT'S A TOOL FOR THE LITERATURE 

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
table(eb_digital$eco_innovation)


# recycling 
table(eb_digital$recycling)
 
 #resource reduction 
table(eb_digital$res_red)


# energy saving 
table(eb_digital$energy_saving)


# sustainable products 
table(eb_digital$sust_prod)


#---------------



#INDEPENDENT VARIABLE - digital - recoded in dummies

#AI
attr(eb_digital$q23_1, "labels")
table(eb_digital$sust_prod)

#bigdata
attr(eb_digital$q23_5, "labels")
table(eb_digital$bigdata)

#cloud
attr(eb_digital$q23_2, "labels")
table(eb_digital$cloud)

#highspeed
attr(eb_digital$q23_6, "labels")
table(eb_digital$highspeed)

#robot
attr(eb_digital$q23_3, "labels")
table(eb_digital$robot)

#smart devices
attr(eb_digital$q23_4, "labels")
table(eb_digital$smart)

#------------------


#CONTROL VARIABLES - firm level - recoded in dummies


#Export
#the variable has value 1 if it export less than 25%, 2 if it exports between 25% and 50%, 3 if it exports more than 50%, 4 = don't know/no answer, 5= NA
#the purpose is to create a variable that has value 1 if the firm answer 1,2 or 3. 
#first i recode each answer in binary form, than i create the new variable that contains at least 1 of the 3 answer recoded. 


#attr(eb_digital_raw$q12b, "labels")

eb_digital <- eb_digital %>%
  mutate(
    export = ifelse(q12b %in% 1:3, 1, ifelse(q12b == 4,0,NA))
  )

table(eb_digital$export, useNA = "always") #NAs are +90%, maybe we should check/create a variable for "non export"
#la eliminiamo momentaneamente 


#family owned _ 1= mostly or entirely family owned 
eb_digital <- eb_digital %>%
  mutate(fam_owned = ifelse(q13_7 == 1, 1, 0))


#financecap _ if it is high capability 
attr(eb_digital_raw$q4a, "labels")

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

eb_digital <- mutate(eb_digital, ln_size = log(size))


#NACE 

nace_labels <- c(
  "1"  = "Mining and quarrying",
  "2"  = "Manufacturing",
  "3"  = "Electricity, gas, steam and air conditioning supply",
  "4"  = "Water supply; sewerage, waste management and remediation",
  "5"  = "Construction",
  "6"  = "Wholesale and retail trade; motor vehicle repair",
  "7"  = "Transportation and storage",
  "8"  = "Accommodation and food service activities",
  "9"  = "Information and communication",
  "10" = "Financial and insurance activities",
  "12" = "Real estate activities",
  "13" = "Professional, scientific and technical activities",
  "14" = "Education",
  "16" = "Human health, arts, and other services"  # aggregato residuale (R, Q, P)
)


eb_digital <- eb_digital %>%
  mutate(
    nace_a = as.character(nace_a),                     # converte da haven_labelled
    sector_label = recode(nace_a, !!!nace_labels),     
    sector_label = as.factor(sector_label),
    sector_label = droplevels(sector_label)
  )

#isocntry 

table(eb_digital$isocntry)

country_labels <- c(
  "AT" = "Austria",
  "BA" = "Bosnia and Herzegovina",
  "BE" = "Belgium",
  "BG" = "Bulgaria",
  "BR" = "Brazil",
  "CA" = "Canada",
  "CY" = "Cyprus",
  "CZ" = "Czech Republic",
  "DE" = "Germany",
  "DK" = "Denmark",
  "EE" = "Estonia",
  "ES" = "Spain",
  "FI" = "Finland",
  "FR" = "France",
  "GB" = "United Kingdom",
  "GR" = "Greece",
  "HR" = "Croatia",
  "HU" = "Hungary",
  "IE" = "Ireland",
  "IS" = "Iceland",
  "IT" = "Italy",
  "JP" = "Japan",
  "LT" = "Lithuania",
  "LU" = "Luxembourg",
  "LV" = "Latvia",
  "MK" = "North Macedonia",
  "MT" = "Malta",
  "NL" = "Netherlands",
  "NO" = "Norway",
  "PL" = "Poland",
  "PT" = "Portugal",
  "RO" = "Romania",
  "RS" = "Serbia",
  "RS-KM" = "Kosovo (UNSCR 1244)",
  "SE" = "Sweden",
  "SI" = "Slovenia",
  "SK" = "Slovakia",
  "TR" = "Turkey",
  "US" = "United States"
)

eb_digital <- eb_digital %>%
  mutate(country_name = recode(isocntry, !!!country_labels)) %>%
  mutate(country_name = as.factor(country_name)) %>%
  mutate(country_name = droplevels(country_name))



#----------------------------------

#Creato dataset a parte 

dat <- eb_digital %>% select(eco_innovation:skillshortage,nace_a,ln_size,isocntry, -export, -highgrowth)

summary(dat)

dat <- dat %>%
  filter(ln_size != -Inf) %>%     
  na.omit() %>%                   
  mutate(
    nace_a = as.factor(nace_a),
    isocntry = as.factor(isocntry)
  ) %>%
  mutate(
    nace_a = droplevels(nace_a),
    isocntry = droplevels(isocntry)
  )



#vediamo 
dat_clean <- dat_clean %>%
  mutate(sector_label = recode(nace_a, !!!nace_labels))


#-------------------------



#rispetto al settore e rispetto al paese, dobbiamo scegliere quale variabile prendere di riferimento. 
#è complesso, quindi al momento li escludo dal glm 















