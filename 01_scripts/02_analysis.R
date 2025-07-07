#libraries 

library(tidyverse)
library(dplyr)
library(readr)
library(tibble)
library(mvProbit)
library(forcats)
library(magrittr)


#All the outputs are available in the folder "03_output" of this project. 

#HERE is the code to call directly the objects without running all the code 
#NB: if you try to run the code, BE SURE that you have time and resources for running the Probit model (estimated time: >4h)


#descriptive statistics
table_digital <- read_csv("02_output/table_digital.csv")
table_green   <- read_csv("02_output/table_green.csv")
table_controls <- read_csv("02_output/table_controls.csv")

#Probit model object
fit_mvp <- readRDS("02_output/fit_mvp.rds") 

#csv of the probit model
appendix_probit <- read.csv("02_output/appendix_mvprobit_output.csv")

#csv of the marginal effects (formatted to be knitted in rmd)
table_ME_formatted <- read.csv("02_output/table_marginal_effects_formatted.csv")






#------------------ Descriptive statistics -------------------------------------

#digital variables 

label_digital <- c(
  AI = "AI, machine learning, pattern recognition",
  bigdata = "Big data analytics",
  cloud = "Cloud computing",
  highspeed = "High-speed infrastructure",
  robot = "Use of robots, automation of processes",
  smart_devices = "Smart devices, intelligent sensors, thermostats"
)

table_digital <- dat %>%
  select(all_of(names(label_digital))) %>%
  summarise(across(everything(), ~ round(mean(.x) * 100, 1))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "percent") %>%
  mutate(label = label_digital[variable]) %>%
  select(label, percent)

table_digital

write_csv(table_digital, "02_output/table_digital.csv")


#green variables 

label_green <- c(
  eco_innovation = "Eco-innovation",
  recycling = "Recycling or reuse of materials",
  resource_reduction = "Reduction of material use and resources",
  energy_saving = "Energy saving measures or renewables",
  sust_prod = "Development of sustainable products/services"
)

table_green <- dat %>%
  select(all_of(names(label_green))) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE) * 100, 1))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "percent") %>%
  mutate(label = label_green[variable]) %>%
  select(label, percent)

table_green

write_csv(table_green, "02_output/table_green.csv")


#control variables 

label_controls <- c( 
  fam_owned = "Family owned",
  financecap = "High financial capability",
  indstrl_area = "Located in an industrial area",
  urban_area = "Located in a big city", 
  skillshortage = "Skills barrier", 
  ln_size = "Size")


binary_controls <- setdiff(names(label_controls), "ln_size")

table_controls <- dat %>%
  select(all_of(binary_controls)) %>%
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE) * 100, 1))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "percent") %>%
  mutate(label = label_controls[variable]) %>%
  select(label, percent)

table_controls

write_csv(table_controls, "02_output/table_controls.csv")


#descriptive statistics of all the variables 


#------------------ PROBIT MODEL -----------------------------------------------

#HERE 
#you find the final output of the model already saved, ready to be called from the "output" file in this project. 
#I RECOMMEND to not run the model as it could take more than 4 hours with a good processor. 

fit_mvp <- readRDS("02_output/fit_mvp.rds") 


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
saveRDS(fit_mvp, "02_output/fit_mvp.rds")
fit_mvp <- readRDS("02_output/fit_mvp.rds") 



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
  
  #extracting coefficients for the equation:
  for (eq in eq_ids) { 
    coef_names <- paste0("b_", eq, "_", 0:(length(var_names)-1))
    
    if (!all(coef_names %in% names(all_coefs))) {
      warning(paste("Equation Y", eq, "skipped for non corresponding names"))
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

# Marginal effects
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

tab_me_p$Variable

# renaming variables
tab_me_p$Variable <- forcats::fct_recode(tab_me_p$Variable,
                                         "AI, machine learning, pattern recognition" = "AI, machine learning, pattern recognition",
                                         "Big data analytics" = "Big data analytics",
                                         "Cloud computing" = "Cloud computing",
                                         "Family owned" = "Family owned",
                                         "High finance capability" = "High finance capability",
                                         "High speed infrastructure" = "High speed infrastructure",
                                         "Industrial area" = "Industrial area",
                                         "Size" = "Size",
                                         "Use of robots, automation of processes" = "Use of robots, automation of processes",
                                         "Skills barrier" = "Skills barrier",
                                         "Smart devices, intelligent sensors, thermostats" = "Smart devices, intelligent sensors, thermostats",
                                         "Urban area" = "Urban area")



# Formatting
format_table <- function(coef, pval) {
  if (is.na(coef) | is.na(pval)) return("")
  stars <- ifelse(pval < 0.01, "***",
                  ifelse(pval < 0.05, "**",
                         ifelse(pval < 0.1, "*", "")))
  paste0(round(coef, 3), stars, " (", round(pval, 3), ")")
}

# final table creation
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
write.csv(table_formatted, "02_output/table_marginal_effects_formatted.csv", row.names = FALSE)



