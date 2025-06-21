library(tidyverse)
library(dplyr)
library(sjPlot)
library(mvProbit)



#multi 

fit_mvp <- mvProbit(cbind(eco_innovation, recycling, resource_reduction, energy_saving, sust_prod) ~ 
                      AI + bigdata + cloud + highspeed + robot + smart_devices + 
                      fam_owned + financecap + indstrl_area + 
                      urban_area + skillshortage + nace_a + ln_size + isocntry, 
                    data = dat)

summary(fit_mvp)

#cartella output in modo da non fittarli più


#funzione per le probabilità marginali 

marginal_effects_mvprobit <- function(fit, data) {}

