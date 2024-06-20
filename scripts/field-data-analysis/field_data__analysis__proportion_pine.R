#### Script to set up data and do forward model selection for proportion pine ###
# This approach treats the data as binomial with successes = count of pines, and failures = count of non-pine conifers. 

library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(sjPlot)
library(MuMIn)
library(Hmisc)
library(car)

# Load data 
load("./output/plotSeedlingData.RData") 

# Load the functions needed for this script
source("./scripts/field-data-analysis/field_data_analysis_functions.R")

# Calculate mean seedling densities and other characteristics for each fire 
plot_dhm %>% group_by(Fire) %>% 
  summarise(dens.all.mean = mean(dens.all), shrubs_mean = mean(Shrubs))

# Make proportion pine data set for analysis 
plot_dhm_pine <- plot_dhm
#plot_dhm_pine$p.pine <- cbind(round(plot_dhm$dens.conif/24.94098, 0), round(plot_dhm$dens.pine/24.94098, 0)) 
plot_dhm_pine <- plot_dhm_pine %>% 
  filter(dens.conif != 0) %>% # look only at plots that contain conifers
  mutate(ForShr = Shrubs + Forbs) # combine shrubs and forbs

nrow(plot_dhm_pine) # 119 rows -- this is substantially smaller, so may need to refit a simpler model
nrow(plot_dhm) # 182 rows 

vars_to_test_continuous <- c("tmax", "tmean", "tmin", "normal_annual_precip", "rad_summer", "Forbs", "Shrubs", "Grasses", "ShrubHt", "LiveOverstory", "log10SeedWallConifer", "twi", "tpi2000", "elev", "CWD_sound")
vars_to_test_factor <- c("fsplanted", "facts.planting.first.year")

# Check for missing values by variable 
check_missing_values(plot_dhm_pine[,vars_to_test_continuous]) # one missing in LiveOverstory
hist(plot_dhm$LiveOverstory)
sum(plot_dhm$LiveOverstory==0, na.rm=T)/length(plot_dhm_pine) # This variable is 88% zeros -- leave it out. 

# Check variable correlations 
cor(plot_dhm_pine[,vars_to_test_continuous], use = "na.or.complete")
# Temp variables too strongly correlated, only tmin is less than 0.7 correlated with elevation. Everything else looks ok. 

# Update continuous variables to test in light of correlations and heavy zero-domination of live overstory
vars_to_test_continuous <- c("tmin", "normal_annual_precip", "rad_summer", "Forbs", "Shrubs", "Grasses", "ShrubHt", "log10SeedWallConifer", "twi", "tpi2000", "elev", "CWD_sound")

# scale continuous variables 
plot_dhm_pine_std <- stdize(plot_dhm_pine[ , vars_to_test_continuous], prefix = FALSE)

# add factor variables
plot_dhm_pine_std <- cbind(plot_dhm_pine_std, plot_dhm_pine[, vars_to_test_factor])

# Add the binomial response 
# this step converts the density of tree per hectare to a count in the 401m2 plot
plot_dhm_pine_std <- mutate(plot_dhm_pine_std, 
      seedling_count = round(plot_dhm_pine$dens.conif/24.94098, 0), 
      pine_count = round(plot_dhm_pine$dens.pine/24.94098, 0))

# Add the grouping variables 
plot_dhm_pine_std <- cbind(plot_dhm_pine_std, plot_dhm_pine[, c("Fire", "PairID")])

# Reorder planted variable so unplanted is the base
plot_dhm_pine_std <- mutate(plot_dhm_pine_std, fsplanted = relevel(fsplanted, "unplanted"))

# remove NAs so model comparison is on par for all variable combinations 
apply(plot_dhm_pine_std, 2, f <- function(x) {return(sum(is.na(x)))}) # only 1 row has missing data
plot_dhm_pine_std <- plot_dhm_pine_std[complete.cases(plot_dhm_pine_std),]

#### Model selection ####

# Set up and fit the base model 
# Note in binomial glm, we supply response as: cbind(successes, failures)
prop_pine_formula <- as.formula("cbind(pine_count, seedling_count-pine_count) ~  fsplanted * facts.planting.first.year + (1|Fire)")

pines_base_model <- glmer(formula = prop_pine_formula, data = plot_dhm_pine_std, family = "binomial")
summary(pines_base_model)
AIC(pines_base_model)

#### Sequentially add individual terms to model  ####

# Do this by testing all the variables individually, finding which gives lowest AIC (if any improve AIC by more than 2), then adding that variable, then repeat. 

vars_to_try_adding <- vars_to_test_continuous
m2 <- forward_select(pines_base_model, terms_to_add = vars_to_try_adding)

m3 <- forward_select(m2$model, terms_to_add = m2$variables_to_test)
m4 <- forward_select(m3$model, terms_to_add = m3$variables_to_test)
m5 <- forward_select(m4$model, terms_to_add = m4$variables_to_test, glmerControl(optimizer = "bobqa", optCtrl = list(maxfun = 2e5, check.conv.grad = .makeCC("warning", tol = 5e-3, relTol = NULL))))
# Model failed to converge here. Stop with m4. 

#### Test adding interactions ####

pines_main_effects_model <- m4$model

model_terms <- extract_model_terms(pines_main_effects_model)

two_way_interacs_to_test <- c("Shrubs:fsplanted", "ShrubHt:fsplanted", "twi:fsplanted")

# Create a list of fixed effects for all models with 2-way interactions 
model_fixed_effects <- list()
for (i in 1:length(two_way_interacs_to_test)) model_fixed_effects[[i]] <- c(model_terms$main_effects_vector, model_terms$interaction_vector, two_way_interacs_to_test[i]) 


#### Fit all models with 2-way interactions and get AIC #### 

response_variable <- model_terms$response
base_model_variables <- c(model_terms$main_effects_vector, model_terms$interaction_vector)
groups <- model_terms$groups

# Fit base model with only main effects
base_model <- fit_binomial_glmer_model(x = base_model_variables, response = response_variable, groups = groups, data = plot_dhm_pine_std) 

# Fit all the models with interactions to test
model_list <- lapply(model_fixed_effects, FUN = fit_binomial_glmer_model, response = response_variable, groups = groups, data = plot_dhm_pine_std)

AIC_vals <- unlist(lapply(model_list, AIC))

which((AIC(base_model) - AIC_vals) >= 2) # Model 1 has lower AIC than the base model
model_list[[1]] # fsplanted:Shrubs 
model_list[[2]] # fsplanted:ShrubHt 

AIC(fit_binomial_glmer_model(c(model_fixed_effects[[1]], "ShrubHt:fsplanted"), response = response_variable, groups = groups, data = plot_dhm_pine_std)) # Including interactions with ShrubHt improves model by >2 AIC points. 

AIC(fit_binomial_glmer_model(c(model_fixed_effects[[1]], "ShrubHt:fsplanted", "twi:fsplanted"), response = response_variable, groups = groups, data = plot_dhm_pine_std)) # Including interactions with twi doesn't improve model by more than 2 AIC points. 


#### Now test 3-way interaction with Shrubs #####

pines_model_2way <- model_list[[1]]

pines_model_3way <-fit_binomial_glmer_model(x = c(model_fixed_effects[[1]], "Shrubs:fsplanted:facts.planting.first.year"), response = response_variable, groups = groups, data = plot_dhm_pine_std)
AIC(pines_model_2way, pines_model_3way) # Convergence problems with the 3-way interaction 
# Checking AIC, the 3-way interaction doesn't improve model by 2 points. 

# Stop here and leave out 3-way interaction. 

pines_final_model <- pines_model_2way

save(pines_final_model, file = "./output/proportion_pines_final_model.Rdata")
