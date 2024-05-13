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

# Load the functions needed for this script
source("./scripts/field-data-analysis/field_data_analysis_functions.R")

# Load R object: plot_dhm_long
load("output/plotSeedlingData.RData") 
#old not downscaled data here: plotSeedlingData_old_not_downscaled.RData

#### Model Information #### 

# First cut at variables to include 

vars_to_test <- c("tmean", "tmin", "tmax", "normal_annual_precip", 
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
    "tpi2000:tmin", "tpi2000:elev")
three_way_interacs_to_test <- c("tmin:fsplanted:facts.planting.first.year", 
    "normal_annual_precip:fsplanted:facts.planting.first.year", 
    "rad_summer:fsplanted:facts.planting.first.year", 
    "Forbs:fsplanted:facts.planting.first.year", 
    "Grasses:fsplanted:facts.planting.first.year", 
    "Shrubs:fsplanted:facts.planting.first.year", 
    "ShrubHt:fsplanted:facts.planting.first.year", 
    "log10SeedWallConifer:fsplanted:facts.planting.first.year", 
    "tpi2000:fsplanted:facts.planting.first.year", 
    "elev:fsplanted:facts.planting.first.year", 
    "twi:fsplanted:facts.planting.first.year", 
    "CWD_sound:fsplanted:facts.planting.first.year")

#### Create list of vectors fixed effects for all models (essentially a ragged array but in list form). Each item in the list contains a character vector of fixed effects for one model. Each of these character vectors contains all the candidate main effect, plus either no interactions, or one 2-way interaction to test, or one 2-way and 3-way interaction to test. 

# Create a list of fixed effects for all models with 2-way interactions 
model_fixed_effects <- list()
for (i in 1:length(two_way_interacs_to_test)) model_fixed_effects[[i]] <- c(vars_to_test, two_way_interacs_to_test[i]) 


#### Fit all models with 2-way interactions and get AIC #### 

response_variable <- "ln.dens.planted"
groups <- "Fire"

# Fit base model with only main effects
base_model <- fit_lmer_model(x = vars_to_test, response = response_variable, groups = groups, dataset = plot_dhm_for_model) 

# Fit all the models with interactions to test
model_list <- lapply(model_fixed_effects, FUN = fit_lmer_model, response = response_variable, groups = groups, dataset = plot_dhm_for_model)

AIC_vals <- unlist(lapply(model_list, AIC))

which((AIC(base_model) - AIC_vals) >= 2) # Models 13 and 20 have lower AIC than the base model
model_list[[13]] # tmin:normal_annual_precip
model_list[[20]]# tpi2000:elev

#### Start with a base model including the chosen 2-way interactions, then test adding the 3-way interactions 

vars_to_test_3way <- c(vars_to_test, "tmin:normal_annual_precip", "tpi2000:elev")

# Fit base model with 2-way interactions, but without any 3-way interactions 
base_model_2way <- fit_lmer_model(x = vars_to_test_3way, response = response_variable, groups = groups, dataset = plot_dhm_for_model) 

# Create a list of candidate 3-way interactions 
model_fixed_effects_3way <- list()
for (i in 1:length(three_way_interacs_to_test)) {
  model_fixed_effects_3way[[i]] <- c(vars_to_test_3way, two_way_interacs_to_test[i],  three_way_interacs_to_test[i])
}

# Fit all the models with 3-way interactions to test
model_list_3way <- lapply(model_fixed_effects_3way, FUN = fit_lmer_model, response = response_variable, groups = groups, dataset = plot_dhm_for_model)

AIC_vals_3way <- unlist(lapply(model_list_3way, AIC))

which((AIC(base_model_2way) - AIC_vals_3way) >= 2) # Shrub interaction improves AIC. Maybe so does precip, but that model won't converge. Ditch it? 

# QUESTION what happened to the shrubs effects? 
summary(model_list_3way[[2]])
summary(model_list_3way[[6]])


#### Next and final step: Backward stepwise elimination of variables using AIC ####

full_model_3way <- fit_lmer_model(x = c(vars_to_test_3way, two_way_interacs_to_test[6], three_way_interacs_to_test[6]), response = response_variable, groups = groups, data = plot_dhm_for_model) 

step(full_model_3way, direction = "backward") # This is throwing an error for some reason?? 
dropterm(full_model_3way, direction = "backward") # This is throwing an error for some reason?? 


drop_terms <- drop1(full_model_3way)
names(drop_terms)

stepcAIC(full_model_3way, direction = "backward", data = plot_dhm_for_model)
