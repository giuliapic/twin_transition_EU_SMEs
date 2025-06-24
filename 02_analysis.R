library(tidyverse)
library(dplyr)
library(sjPlot)
library(mvProbit)
library(broom)



#multivatiate probit model

fit_mvp <- mvProbit(cbind(eco_innovation, recycling, resource_reduction, energy_saving, sust_prod) ~ 
                      AI + bigdata + cloud + highspeed + robot + smart_devices + 
                      fam_owned + financecap + indstrl_area + 
                      urban_area + skillshortage + nace_a + ln_size + isocntry, 
                    data = dat)

summary(fit_mvp)


#csv creation of the output 

fit_mvp_csv <- tidy(fit_mvp)

write.csv(fit_mvp_csv, "appendix_mvprobit_output.csv", row.names = FALSE)


#--------------Calculating the marginal effects of the model------------------

marginal_effects_mvProbit <- function(fit, data) {

  # calling all estimated coefficients 
  all_coefs <- coef(fit)
  
  # creating the right side of the formula

  formula_rhs <- as.formula(paste("~", deparse(as.formula(fit$call$formula)[[3]])))
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


#creating an object with marginal effects 
ME_mvp <- marginal_effects_mvProbit(fit = fit_mvp, data = dat)

ME_mvp

#substitution of the "y" label with the variable label 

ME_labelled <- ME_mvp |> 
  dplyr::mutate(Equation = dplyr::recode(Equation,
                                         Y1 = "eco_innovation",
                                         Y2 = "recycling",
                                         Y3 = "resource_reduction",
                                         Y4 = "energy_saving",
                                         Y5 = "sustnb_prod"
  ))


#saving the output as CSV 
write.csv(ME_labelled, "marginal_effects_mvProbit_.csv", row.names = FALSE)



