library(tidyverse)
library(dplyr)
library(sjPlot)
library(mvProbit)


#2 Equations, with the same x - excluding NACE & country 


glm(eco_innovation ~ 
      AI_var + bigdata + cloud + highspeed + robot + smart + 
      fam_owned + financecap + indstrl_area + 
      urban_area + skillshortage + ln_size ,
    data = dat,
    family = binomial(link = "logit")) %>% summary()



formula <-  cbind(eco_innovation, recycling, res_red, energy_saving, sust_prod) ~ 
  AI_var + bigdata + cloud + highspeed + robot + smart + 
  fam_owned + financecap + indstrl_area + 
  urban_area + skillshortage + ln_size

fit <- mvProbit(formula, data = dat)

summary(fit)

save.image("wks.RData")


