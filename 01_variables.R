#libraries 

library(haven)
library(tidyverse)
library(dplyr)
library(sjPlot)


#uploading the datasets: 489 (green) + 4 86 (digital)

eb_digital <- read_dta("C:/data_giulia/data/eurobarometer/data_486_digital.dta")


#which countries are in the survey 
table(eb_green$isocntry)

#how many observation do we have? 
#population: 16.365






#selecting the variables for EB_digital 






# "nace" = nace code 


# "size" = dimension of the firm


#Q4B = turnover = "what was the annual turnover of your enterprise in 2019?"(from 1 to 9)


#Q7A = "in terms of growth either in employment or in turnover, does your enterprise 
# 1. have a strategic growth plan 
#2. plan to grow as a result of introducing some kind of innovation 
# the other answer are not correlated to the rq (it's a multiple response question)


#Q8 could be interesting: "in which of the following areas is your enterprise located?" from large town to rural/industrial area 

#Q13: "in term of ownership, is your enterprise..." multiple response from one person to cooperative 


#Q16_6 "how would you rate your business environment in terms of availability of support to help enterprises become more sustainable"
#Q16_8 "how would you rate your business environment in terms of infrastructure for businesses, such as available office space, internet connectivity etc"

#Q17 what are the biggest problems for your enterprise 
# 1= difficulties with innovation 
#3= access to data 
#8= difficulties with digitalisation 
#9= other 
#10 = don't know 

#Q19 "during the past 2 months, has your enterprise introduced any of the following types of innovations? 
#response from 1 to 7 is "yes", response 8 is "no", response 9 "dont know"

#Q20 "which of the following is a barrier to innovation in your enterprise?"
#multiple answer

#Q21 uguale alla 20 

#Q22 which of the following options best describes your enterprise's approach to digital technologies 
#multiple response 

#Q23 which of the following digital technologies has your enterprise adopted to date? 

#q24 in terms of environmental and csr, which of the following actions is your enterprise acrively taken? 

#q25 do you have a strategy or action plan to become a sustainable enterprise? multiple response from 1 "yes it has already been implemented" to 4 "no it will not in the future"

#q26 which of the following are currently preventing your enterprise from becoming sustainable? multiple response 























