#libraries 

library(haven)
library(tidyverse)
library(dplyr)


# HERE there's the new dataset obtained at the end of this file - useful if you don't need to run all the code.
#It contains the variables ready to be fitted in the model you find in the "02_analysis" file.  

dat <- readRDS("03_output/dat.rds") 
dat <- read.csv("03_output/dat.csv") 


#---------------- Settings ---------------------------------------------------------------------------------------------------------------------------------

#uploading the dataset Eurobarometer 486

eb_digital_raw <- read_dta("C:/data_giulia/data/eurobarometer/data_486_digital.dta")

#creating a copy of the dataset

eb_digital <- eb_digital_raw
#total number of observation: 16.365


#N.B. 
# variables imported with labelled numeric values (e.g., 0 = "Not mentioned", 1 = "Yes") 
# do not contain actual NA values unless explicitly coded as such in the dataset. 
# Therefore, missing values are not expected and are not filtered out while recoding. 
# This applies to all similar variables with labelled 0/1 structures across the dataset.



#-----------------DEPENDENT VARIABLE (green)------------------ 


# eco innovation Q19_5
attr(eb_digital$q19_5, "labels")

eb_digital <- eb_digital %>%
  mutate(eco_innovation = ifelse(q19_5 == 1, 1, 0))

table(eb_digital$eco_innovation)


# recycling 
attr(eb_digital$q24_1, "labels")

eb_digital <- eb_digital %>% 
  mutate(recycling = ifelse(q24_1 == 1, 1, 0))

table(eb_digital$recycling)


#resource reduction 
attr(eb_digital$q24_2, "labels")

eb_digital <- eb_digital %>% 
  mutate(resource_reduction = ifelse(q24_2 == 1, 1, 0))

table(eb_digital$resource_reduction)


# energy saving 
attr(eb_digital$q24_3, "labels")

eb_digital <- eb_digital %>% 
  mutate(energy_saving = ifelse(q24_3 == 1, 1, 0))

table(eb_digital$energy_saving)


# sustainable products 
attr(eb_digital$q24_4, "labels")

eb_digital <- eb_digital %>%
  mutate(sust_prod = ifelse(q24_4 == 1, 1, 0))

table(eb_digital$sust_prod)


#--------------- INDEPENDENT VARIABLE (digital)-----------------------------


#AI
attr(eb_digital$q23_1, "labels")

eb_digital <- eb_digital %>%
  mutate(AI = ifelse(q23_1 == 1, 1, 0))

table(eb_digital$AI)

#bigdata
attr(eb_digital$q23_5, "labels")

eb_digital <- eb_digital %>% 
  mutate(bigdata = ifelse(q23_5 == 1, 1, 0))

table(eb_digital$bigdata)

#cloud computing
attr(eb_digital$q23_2, "labels")

eb_digital <- eb_digital %>% 
  mutate(cloud = ifelse(q23_2 == 1, 1, 0))

table(eb_digital$cloud)

#highspeed infrastructure
attr(eb_digital$q23_6, "labels")

eb_digital <- eb_digital %>%
  mutate(highspeed = ifelse(q23_6 ==1, 1, 0))

table(eb_digital$highspeed)

#robot for automation
attr(eb_digital$q23_3, "labels")

eb_digital <- eb_digital %>% 
  mutate(robot = ifelse(q23_3 == 1, 1, 0))

table(eb_digital$robot)

#smart devices (sensors, thermostats etc)
attr(eb_digital$q23_4, "labels")

eb_digital <- eb_digital %>% 
  mutate(smart_devices = ifelse(q23_4 == 1, 1, 0))

table(eb_digital$smart_devices)

#------------------ CONTROL VARIABLES - firm level ---------------------------


#NB. As some of the answers are not binary like the previous ones, they can contain NAs, so checking for each variable the NAs is necessary.


#Export --- NOT USED in the model, because of the NAs (15.632)
attr(eb_digital_raw$q12b, "labels")

eb_digital <- eb_digital %>%
  mutate(
    export = ifelse(q12b %in% 1:3, 1, ifelse(q12b == 4,0,NA))
  )

table(eb_digital$export, useNA = "always") #NAs are >90% --> excluded from models

#family owned  
attr(eb_digital$q13_7, "labels")

eb_digital <- eb_digital %>%
  mutate(fam_owned = ifelse(q13_7 == 1, 1, 0))

table(eb_digital$fam_owned, useNA = "always") #kept for modeling

#financecap _ if it is high capability 
attr(eb_digital_raw$q4a, "labels")

eb_digital <- eb_digital %>%
  mutate(financecap = ifelse(q4a %in% 7:8, 1,
                        ifelse(q4a %in% 1:6, 0, NA)))
    
table(eb_digital$financecap, useNA = "always") #kept for modeling 


#highgrowth --- Excluded from the models
attr(eb_digital$q5_1, "labels")
attr(eb_digital$q5_2, "labels")

eb_digital <- eb_digital %>%
  mutate(
    highgrowth = ifelse(q5_1 == 4 & q5_2 ==4, 1,
                        ifelse(q5_1 %in% 1:3 & q5_2 %in% 1:3, 0, NA)) #the answer 5 is treated as NA, following the questionnaire
  )

table(eb_digital$highgrowth, useNA = "always") #We exclude it



#localisation industrial 
attr(eb_digital$q8_4, "labels") #binary, no NAs

eb_digital <- eb_digital %>%
  mutate(
    indstrl_area = ifelse(q8_4 == 1, 1, 0))

table(eb_digital$indstrl_area)

#localisation urban 
attr(eb_digital$q8_1, "labels") #binary, no NAs

eb_digital <- eb_digital %>% 
  mutate( 
    urban_area = ifelse(q8_1 == 1, 1, 0))

table(eb_digital$urban_area)


#skill shortage 
attr(eb_digital$q26_4, "labels") #NAs check not needed

eb_digital <- eb_digital %>%
  mutate(
    skillshortage = ifelse(q26_4 == 1,1,0)
  )

table(eb_digital$skillshortage)


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
  "16" = "Human health, arts, and other services"  
)


eb_digital <- eb_digital %>%
  mutate(
    nace_a = as.character(nace_a),                    
    sector_label = recode(nace_a, !!!nace_labels),     
    sector_label = as.factor(sector_label),
    sector_label = droplevels(sector_label)
  )

# country 

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

#this script is to adapt the variable "country" for fitting it in the model 

eb_digital <- eb_digital %>%
  mutate(country_name = recode(isocntry, !!!country_labels)) %>%
  mutate(country_name = as.factor(country_name)) %>%
  mutate(country_name = droplevels(country_name))



#--------------- Creating a new dataset with the selection of all previous variables only ----------------------------

dat <- eb_digital %>% select(eco_innovation:skillshortage,nace_a,ln_size,isocntry, -export, -highgrowth)

summary(dat)


# Mutating countries and sectors as factors - to create dummies in the model that can be used to account for fixed errors
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


#Saving the new dataset in the output  
saveRDS(dat, "03_output/dat.rds")
write.csv(dat, "03_output/dat.csv")

#calling "dat" without the need to re-execute all the script: 
dat <- readRDS("03_output/dat.rds")
dat <- read.csv("03_output/dat.csv")








