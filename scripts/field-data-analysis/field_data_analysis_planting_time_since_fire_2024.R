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

load("output/plotSeedlingData.RData") #load R object: plot_dhm_long
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
                              "rad_summer:fsplanted", "Forbs:fsplanted", "Grasses:fsplanted", "Shrubs:fsplanted",
                              "ShrubHt:fsplanted", "log10SeedWallConifer:fsplanted", "tpi2000:fsplanted", "elev:fsplanted", "twi:fsplanted", 
                              "CWD_sound:fsplanted", "tmin:normal_annual_precip", "normal_annual_precip:rad_summer", "twi:normal_annual_precip", "twi:rad_summer", "twi:elev", "tpi2000:normal_annual_precip", "tpi2000:tmin", "tpi2000:elev")

# Starting model
base_model_formula <- paste0("ln.dens.planted ~ ", paste(vars_to_test, collapse = "+"), paste(" + (1|Fire)"))

pltd <- lmer(formula = base_model_formula, data = plot_dhm_for_model, REML = TRUE)

# Test 2-way interactions 

model_list <- list() 
for (i in two_way_interacs_to_test) {
  formula_2way <- paste0("ln.dens.planted ~ ", paste(vars_to_test, collapse = "+"), paste0(" + ", i), paste0(" + (1|Fire)"))
  mod_2way <- lmer(formula = formula_2way, data = plot_dhm_for_model, REML = TRUE)
  model_list <- c(model_list, mod_2way)
}

AIC_2way <- unlist(lapply(model_list, AIC))
AIC_base <- AIC(pltd)
two_way_interacs_to_keep <- which((AIC_base-AIC_2way)>=2)

# Model with 2-way interactions 
formula_2way_final <- paste0("ln.dens.planted ~ ", paste(vars_to_test, collapse = "+"), paste0(" + ", two_way_interacs_to_test[two_way_interacs_to_keep[1]], " + ", two_way_interacs_to_test[two_way_interacs_to_keep[2]]), paste0(" + (1|Fire)"))

# Test 3-way interaction
three_way_interacs_to_test <- c("Shrubs:fsplanted:facts.planting.first.year")

# Model with 3-way interaction 
formula_3way <- paste0("ln.dens.planted ~ ", paste(vars_to_test, collapse = "+"), paste0(" + ", two_way_interacs_to_test[two_way_interacs_to_keep[1]], " + ", two_way_interacs_to_test[two_way_interacs_to_keep[2]], " + ",  "Shrubs:fsplanted", " + ", three_way_interacs_to_test), paste0(" + (1|Fire)"))

mod_3way <- lmer(formula = formula_3way, data = plot_dhm_for_model, REML = TRUE)
summary(mod_3way)
AIC(mod_3way, mod_2way)                 

# Drop variables that don't help model 
step(mod_3way, direction = "backward")

# Result: "ln.dens.planted ~ tmin + normal_annual_precip + Shrubs + log10SeedWallConifer + tpi2000 + elev + facts.planting.first.year + fsplanted + (1 | Fire) + tmin:normal_annual_precip + tpi2000:elev + Shrubs:fsplanted + Shrubs:facts.planting.first.year:fsplanted"
 
formula_final <- "ln.dens.planted ~ tmin + normal_annual_precip + Shrubs + log10SeedWallConifer + tpi2000 + elev + facts.planting.first.year + fsplanted +  tmin:normal_annual_precip + tpi2000:elev + Shrubs:fsplanted + Shrubs:facts.planting.first.year:fsplanted + (1 | Fire)"

pltd_final <- lmer(formula_final, data = plot_dhm_for_model, REML = TRUE)
summary(pltd_final)
