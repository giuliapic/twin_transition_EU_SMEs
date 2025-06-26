#libraries 

library(tidyverse)
library(dplyr)
library(tibble)
library(mvProbit)
library(forcats)
library(magrittr)


#All the outputs are available in the folder "03_output" of this project. 

#HERE is the code to call directly the objects without running all the code 
#NB: if you try to run the code, BE SURE that you have time and resources for running the Probit model (estimated time: +4h)


#descriptive statistics
write.csv(digital_summary, "03_output/digital_summary.csv", row.names = FALSE)
write.csv(sust_summary, "03_output/sust_summary.csv", row.names = FALSE)

#Probit model object
fit_mvp <- readRDS("03_output/fit_mvp.rds") 

#csv of the probit model
write.csv(fit_mvp_csv, "appendix_mvprobit_output.csv", row.names = FALSE)

#csv of the marginal effects (formatted to be knitted in rmd)
write.csv(table_formatted, "03_output/table_marginal_effects_formatted.csv", row.names = FALSE)






#------------------ Descriptive statistics -------------------------------------

#digital variables 



#green variables 






#------------------ PROBIT MODEL -----------------------------------------------

#HERE 
#there's the output already saved, ready to be called from the "output" file in this project. 
#I RECOMMAND to not run the model as it could take more than 4 hours with a good processor. 

fit_mvp <- readRDS("03_output/fit_mvp.rds") 


#fitting multivatiate probit model

fit_mvp <- mvProbit(cbind(eco_innovation, recycling, resource_reduction, energy_saving, sust_prod) ~ 
                      AI + bigdata + cloud + highspeed + robot + smart_devices + 
                      fam_owned + financecap + indstrl_area + 
                      urban_area + skillshortage + nace_a + ln_size + isocntry, 
                    data = dat)

summary(fit_mvp)



#saving the csv of the output 

fit_mvp_csv <- tidy(fit_mvp)

write.csv(fit_mvp_csv, "appendix_mvprobit_output.csv", row.names = FALSE)

#saving the RDS of the output 




#------------------ Calculating the marginal effects of the model---------------

marginal_effects_mvProbit <- function(fit, data) {

  # calling all estimated coefficients 
  all_coefs <- coef(fit)
  
  # creating the right side of the formula

  formula_rhs <- as.formula(paste("~", paste(deparse(as.formula(fit$call$formula)[[3]]), collapse = " ")))
  
  X <- model.matrix(formula_rhs, data = data)
  X_mean <- colMeans(X)
  var_names <- names(X_mean)
  
  # how many equations?
  eq_ids <- unique(gsub("b_([0-9]+)_.*", "\\1", grep("^b_", names(all_coefs), value = TRUE)))
  eq_ids <- sort(as.integer(eq_ids))
  
  results <- list()
  
  for (eq in eq_ids) { # it extracts coefficients for this equation
    coef_names <- paste0("b_", eq, "_", 0:(length(var_names)-1))
    
    if (!all(coef_names %in% names(all_coefs))) {
      warning(paste("Equazione Y", eq, "skipped per nomi non corrispondenti"))
      next
    }
    
  beta <- all_coefs[coef_names]
  names(beta) <- var_names
    
  eta <- sum(X_mean * beta)
  phi <- dnorm(eta)
  marg_eff <- phi * beta
    
  results[[paste0("Y", eq)]] <- data.frame( 
  Equation = paste0("Y", eq),
  Variable = var_names,
  Marginal_Effect = marg_eff)
  }
  
  do.call(rbind, results)
}



#----------------- FORMATTING THE TABLE ---------------------------------------- 

# MARGINAL EFFECTS 
ME <- marginal_effects_mvProbit(fit_mvp, dat) %>%
  tidyr::spread(key = Equation, value = Marginal_Effect)

colnames(ME) <- c("Variable", "eco_innovations", "recycling", "resource_reduction", "energy_saving", "sustainable_products")


# pvalue
summary = summary(fit_mvp)
p = summary$estimate[1:325, 4]

pvalue <- marginal_effects_mvProbit(fit_mvp, dat) %>%
  mutate(pvalue = round(p, 5)) %>%
  select(-Marginal_Effect) %>%
  tidyr::spread(key = Equation, value = pvalue)

colnames(pvalue) <- c("Variable", "P_value1", "P_value2", "P_value3", "P_value4", "P_value5")


# Join ME and pvalue
tab_me_p <- left_join(ME, pvalue, by = "Variable") %>%
  select(Variable,
         eco_innovations, P_value1,
         recycling, P_value2,
         resource_reduction, P_value3,
         energy_saving, P_value4,
         sustainable_products, P_value5)



# Erasing countries and sector of the firm 
index <- grepl("isocntry", tab_me_p$Variable) | grepl("nace_a", tab_me_p$Variable)

tab_me_p <- tab_me_p[!index, ]

tab_me_p <- tab_me_p[tab_me_p$Variable != "(Intercept)", ]


# renaming variables
tab_me_p$Variable <- forcats::fct_recode(tab_me_p$Variable,
                                         AI = "AI",
                                         bigdata = "bigdata",
                                         cloud = "cloud",
                                         family_owned = "family_owned",
                                         finance_capability = "finance_capability",
                                         industrial_area = "industrial_area",
                                         size = "size",
                                         skills_barrier = "skills_barrier",
                                         smart_devices = "smart_devices",
                                         urban_area = "urban_area")



# Formatting
format_table <- function(coef, pval) {
  if (is.na(coef) | is.na(pval)) return("")
  stars <- ifelse(pval < 0.01, "***",
                  ifelse(pval < 0.05, "**",
                         ifelse(pval < 0.1, "*", "")))
  paste0(round(coef, 3), stars, " (", round(pval, 3), ")")
}

# table creation
table_formatted <- tab_me_p %>%
  mutate(
    `Eco-Innovation` = mapply(format_table, eco_innovations, P_value1),
    `Recycling` = mapply(format_table, recycling, P_value2),
    `Resource Reduction` = mapply(format_table, resource_reduction, P_value3),
    `Energy Saving` = mapply(format_table, energy_saving, P_value4),
    `Sustainable Products` = mapply(format_table, sustainable_products, P_value5)
  ) %>%
  select(Variable, `Eco-Innovation`, Recycling, `Resource Reduction`, `Energy Saving`, `Sustainable Products`)

table_formatted

# Saving output
write.csv(table_formatted, "03_output/table_marginal_effects_formatted.csv", row.names = FALSE)




