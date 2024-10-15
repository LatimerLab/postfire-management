#### Do model selection for effect of planting and planting timing on seedling density ####
# May 10, 2024 

#### Setup ####

library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)
library(sjPlot)
library(MuMIn)
library(Hmisc)
library(car)
library(DHARMa) 
library(flextable)

# Load the functions needed for this script
source("./scripts/field-data-analysis/field_data_analysis_functions.R")

# Load R object: plot_dhm_long
load("output/plotSeedlingData.RData") 
#old not downscaled data here: plotSeedlingData_old_not_downscaled.RData

#### Model Information #### 

# First cut at variables to include 

vars_to_test <-c("tmean", "tmin", "tmax", "normal_annual_precip", 
                  "rad_summer", "Forbs", "Grasses", "Shrubs", "LiveOverstory", 
                  "ShrubHt", "log10SeedWallConifer", "tpi2000", "elev", "twi", 
                  "CWD_sound", "facts.planting.first.year", "fsplanted")

plot_dhm_for_model <- dplyr::select(plot_dhm, ln.dens.planted, paste(vars_to_test))

# Check correlations
cor(plot_dhm_for_model[,1:16]) # Problems: all temp variables highly correlated. Elev highly correlated with tmean and tmax. We should keep either only tmean, or tmin plus elevation. For now let's go with elevation and tmin. 

# I'm also removing LiveOverstory because it's almost always zero 

#### Set up the data set to be used for model selection ####

# Final set of variables to include 
vars_to_test <- c("tmin", "normal_annual_precip", 
                  "rad_summer", "Forbs", "Grasses", "Shrubs",
                  "ShrubHt", "log10SeedWallConifer", "tpi2000", "elev", "twi", 
                  "CWD_sound", "facts.planting.first.year", "fsplanted")

# Scale the continuous explanatory variables
plot_dhm_for_model <- dplyr::select(plot_dhm, ln.dens.planted, paste(vars_to_test), Fire, PairID, PlotID)
plot_dhm_for_model[, 2:13] <- scale(plot_dhm_for_model[, 2:13])

# Reorder planted variable so unplanted is the base
plot_dhm_for_model <- mutate(plot_dhm_for_model, fsplanted = relevel(fsplanted, "unplanted"))

# Check for missing values 
apply(plot_dhm_for_model, 2, f<-function(x) {return(sum(is.na(x)))}) # OK

# Which interactions to include: 
two_way_interacs_to_test <- c("tmin:fsplanted", "normal_annual_precip:fsplanted",
    "rad_summer:fsplanted", "Forbs:fsplanted", "Grasses:fsplanted", 
    "Shrubs:fsplanted", "ShrubHt:fsplanted", "log10SeedWallConifer:fsplanted", 
    "tpi2000:fsplanted", "elev:fsplanted", "twi:fsplanted", 
    "CWD_sound:fsplanted", "tmin:normal_annual_precip", 
    "normal_annual_precip:rad_summer", "twi:normal_annual_precip", 
    "twi:rad_summer", "twi:elev", "tpi2000:normal_annual_precip", 
    "tpi2000:tmin", "tpi2000:elev", "fsplanted:facts.planting.first.year")

# Testing only one 3-way-interaction: Shrubs:fsplanted:facts.planting.first.year
three_way_interacs_to_test <- "Shrubs:fsplanted:facts.planting.first.year"

#### Create list of vectors fixed effects for all models (essentially a ragged array but in list form). Each item in the list contains a character vector of fixed effects for one model. Each of these character vectors contains all the candidate main effect, plus either no interactions, or one 2-way interaction to test, or one 2-way and 3-way interaction to test. 

# Create a list of fixed effects for all models with 2-way interactions 
model_fixed_effects <- list()
for (i in 1:length(two_way_interacs_to_test)) model_fixed_effects[[i]] <- c(vars_to_test, two_way_interacs_to_test[i]) 


#### Fit all models with 2-way interactions and get AIC #### 

response_variable <- "ln.dens.planted"
groups <- "Fire"

# Fit base model with only main effects
base_model <- fit_lmer_model(x = vars_to_test, response = response_variable, groups = groups, data = plot_dhm_for_model) 

# Fit all the models with interactions to test
model_list <- lapply(model_fixed_effects, FUN = fit_lmer_model, response = response_variable, groups = groups, data = plot_dhm_for_model)

AIC_vals <- unlist(lapply(model_list, AIC))

which((AIC(base_model) - AIC_vals) >= 2) # Models 13, 20, and 21 have lower AIC than the base model
model_list[[13]] # tmin:normal_annual_precip
model_list[[20]]# tpi2000:elev
model_list[[21]] # fsplanted:facts.planting.first.year

#### Start with a base model including the chosen 2-way interactions, then test adding the 3-way interactions 

vars_to_test_3way <- c(vars_to_test, "tmin:normal_annual_precip", "tpi2000:elev", "fsplanted:facts.planting.first.year")

# Fit base model with 2-way interactions, but without any 3-way interactions 
base_model_2way <- fit_lmer_model(x = vars_to_test_3way, response = response_variable, groups = groups, data = plot_dhm_for_model) 

# Create a list of candidate 3-way interactions 
#model_fixed_effects_3way <- list()
#for (i in 1:length(three_way_interacs_to_test)) {
#  model_fixed_effects_3way[[i]] <- c(vars_to_test_3way, two_way_interacs_to_test[i], # three_way_interacs_to_test[i])
#}

# Fit all the models with 3-way interactions to test
#model_list_3way <- lapply(model_fixed_effects_3way, FUN = fit_lmer_model, response = response_variable, groups = groups, data = plot_dhm_for_model)

## Directly compare of models with different interactions involving shrubs 
  # base_model_2way has no shrub interactions
  # base_model_2way_shrubs has a 2-way interaction with shrubs and planting
  # base_model_3way_shrubs has a 3-way interaction with shrubs, planting, and planting timing. 
base_model_2way_shrubs <- fit_lmer_model(x = c(vars_to_test_3way, "Shrubs:fsplanted"), response = response_variable, groups = groups, data = plot_dhm_for_model)
base_model_3way_shrubs <- fit_lmer_model(x = c(vars_to_test_3way, "Shrubs:fsplanted", "Shrubs:fsplanted:facts.planting.first.year"), response = response_variable, groups = groups, data = plot_dhm_for_model)
AIC(base_model_2way, base_model_2way_shrubs, base_model_3way_shrubs) # 3-way interaction improves AIC

#### Next and final step: Backward stepwise elimination of variables using AIC ####

# Set up full model from which to test dropping terms
full_model_formula <- make_formula(response = response_variable, predictors = c(vars_to_test_3way, two_way_interacs_to_test[6], three_way_interacs_to_test[1]), groups = groups)
full_model_3way <- lmer(full_model_formula, data = plot_dhm_for_model, na.action = na.pass, REML=FALSE)

# Variables not to include in the backwards stepwise elimination 
# These are ones involved in interactions that were already tested and added 
fixed_vars <- c("tmin", "normal_annual_precip", "tpi2000", "elev", "Shrubs", "fsplanted", "facts.planting.first.year")

m2 <- backwards_eliminate(full_model_3way, fixed = fixed_vars) # Remove twi
m2

m3 <- backwards_eliminate(m2$model_list[[6]], fixed = fixed_vars)
m3 # remove forbs

m4 <- backwards_eliminate(m3$model_list[[2]], fixed = fixed_vars)
m4 # remove rad_summer

m5 <- backwards_eliminate(m4$model_list[[1]], fixed = fixed_vars)
m5 # remove CWD_sound 

m6 <- backwards_eliminate(m5$model_list[[4]], fixed = fixed_vars)
m6 # remove Grasses

m7 <- backwards_eliminate(m6$model_list[[1]], fixed = fixed_vars)
m7 # remove ShrubHt


m8 <- backwards_eliminate(m7$model_list[[1]], fixed = fixed_vars)
# STOP -- all variables retained. 

#### FINAL MODEL ##### 
seedling_density_final_model <- m7$model_list[[1]]

r.squaredGLMM(seedling_density_final_model) # R2 seems decent at 0.53 marginal
summary(seedling_density_final_model)

# Check model residuals using DHARMa residuals
simulationOutput <- simulateResiduals(fittedModel = seedling_density_final_model)
plot(simulationOutput) # Residuals qqplot and distribution are fine; quantile deviations detected at the lower quantile level. 

save(seedling_density_final_model, file = "./output/seedling_density_final_model.Rdata")


#### Make table of model selection procedure, including AIC for all models considered

#First create a row for the base model 
base_model_info <- data.frame(model = as.character(formula(base_model))[3], AIC = AIC(base_model), delta_AIC = 0)

# Add the models testing 2-way interactions
model_selection_table <- data.frame(model = as.character(unlist(lapply(model_list, formula))), AIC = AIC_vals, delta_AIC = AIC(base_model) - AIC_vals)
model_selection_table <- rbind(base_model_info, model_selection_table)

# Add the model with the 3-way interaction 
model_selection_table <- rbind(model_selection_table, data.frame(model = as.character(formula(base_model_3way_shrubs))[3], AIC = AIC(base_model_3way_shrubs), delta_AIC = AIC(base_model) - AIC(base_model_3way_shrubs)))

# Add models tested for backwards variable elimination 
model_list_backwards <- list(m2$model_list[[6]], m3$model_list[[2]], m4$model_list[[1]], m5$model_list[[4]], m6$model_list[[1]], m7$model_list[[1]])
model_selection_table <- rbind(model_selection_table, data.frame(model = as.character(unlist(lapply(model_list_backwards, formula))), AIC = unlist(lapply(model_list_backwards, AIC)), delta_AIC = AIC(base_model) - unlist(lapply(model_list_backwards, AIC))))

##### Convert model selection table to tabular format for Word and export 
model_selection_table <- mutate(model_selection_table,  AIC = round(AIC, 2), delta_AIC = round(delta_AIC, 2))
flextable(model_selection_table) %>% 
  # make columns of the flextable wider
  flextable::width(j = 1:3, width = 1) %>%
  save_as_docx(path = "./figures/exploratory/seedling_density_model_comparison_table.docx")
