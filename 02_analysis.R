library(tidyverse)
library(dplyr)
library(sjPlot)
library(mvProbit)



#multivatiate probit model

fit_mvp <- mvProbit(cbind(eco_innovation, recycling, resource_reduction, energy_saving, sust_prod) ~ 
                      AI + bigdata + cloud + highspeed + robot + smart_devices + 
                      fam_owned + financecap + indstrl_area + 
                      urban_area + skillshortage + nace_a + ln_size + isocntry, 
                    data = dat)

summary(fit_mvp)


#----- try 1 with a reducted sample: 

dat_sample <- dat[sample(nrow(dat), 600), ]

fit_sample <- mvProbit(
  cbind(eco_innovation, recycling, resource_reduction, energy_saving, sust_prod) ~ 
    AI + bigdata + cloud + smart_devices + ln_size + financecap,
  data = dat_sample,
  nDraws = 100
)

summary(fit_sample)


#cartella output in modo da non fittarli più



