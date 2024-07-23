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
library(flextable)

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
sum(plot_dhm$LiveOverstory==0, na.rm=T)/length(plot_dhm_pine) # This variable is 88% zeros, and the rest low values. So we will exclude it. 

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
m5 <- forward_select(m4$model, terms_to_add = m4$variables_to_test)
# Model failed to converge here. Stop with m4. 

#### Test adding interactions ####

pines_main_effects_model <- m4$model

model_terms <- extract_model_terms(pines_main_effects_model)

two_way_interacs_to_test <- c("Shrubs:fsplanted", "ShrubHt:fsplanted", 
                              "twi:fsplanted")

# Create a list of fixed effects for all models with 2-way interactions 
model_fixed_effects <- list()
for (i in 1:length(two_way_interacs_to_test)) model_fixed_effects[[i]] <- c(model_terms$main_effects_vector, model_terms$interaction_vector, two_way_interacs_to_test[i]) 


#### Fit all models with 2-way interactions and get AIC #### 

response_variable <- "cbind(pine_count, seedling_count-pine_count)"
base_model_variables <- c(model_terms$main_effects_vector, model_terms$interaction_vector)
groups <- "Fire"

# Fit base model with only main effects
base_model <- fit_binomial_glmer_model(x = base_model_variables, response = response_variable, groups = groups, data = plot_dhm_pine_std) 

# Fit all the models with interactions to test
model_list_2way <- lapply(model_fixed_effects, FUN = fit_binomial_glmer_model, response = response_variable, groups = groups, data = plot_dhm_pine_std)

AIC_vals_2way <- unlist(lapply(model_list_2way, AIC))

which((AIC(base_model) - AIC_vals_2way) >= 2) # Model 1 has lower AIC than the base model
model_list_2way[[1]] # fsplanted:Shrubs interaction is significant
model_list_2way[[2]] # ShrubHt:fsplanted interaction is significant

#### Now test 3-way interaction with Shrubs #####

pines_model_2way <- fit_binomial_glmer_model(x = c(model_fixed_effects[[1]], "ShrubHt:fsplanted"), response = response_variable, groups = groups, data = plot_dhm_pine_std)

pines_model_3way <-fit_binomial_glmer_model(x = c(model_fixed_effects[[1]],"Shrubs:fsplanted:facts.planting.first.year"), response = response_variable, groups = groups, data = plot_dhm_pine_std)

pines_model_3way_ShrubHt <-fit_binomial_glmer_model(x = c(model_fixed_effects[[1]], "ShrubHt:fsplanted", "Shrubs:fsplanted:facts.planting.first.year"), response = response_variable, groups = groups, data = plot_dhm_pine_std)

AIC(pines_model_2way, pines_model_3way, pines_model_3way_ShrubHt) # Convergence problems with the 3-way interaction 
# Checking AIC, the 3-way interaction doesn't improve model by 2 points. 

# Also when both 3-way interaction and ShrubHt are included, model won't converge. 
# So we will stop here and use the 2-way interaction model.

pines_final_model <- glmer(formula(pines_model_2way), data = plot_dhm_pine_std, family = "binomial")

r.squaredGLMM(pines_final_model) 
#                 R2m      R2c
#theoretical 0.7843631 0.935610
#delta       0.7603270 0.906939

save(pines_final_model, file = "./output/proportion_pines_final_model.Rdata")



#### Make table of model selection procedure, including AIC for all models considered

#First create a row for the base model 
base_model_info <- data.frame(model = as.character(formula(pines_base_model))[3], AIC = AIC(pines_base_model), delta_AIC = 0)

# Add the models that test individual variables 
model_selection_table <- rbind(base_model_info, data.frame(model = as.character(unlist(lapply(list(m2$model, m3$model, m4$model), formula))), AIC = unlist(lapply(list(m2$model, m3$model, m4$model), AIC)), delta_AIC = AIC(pines_base_model) - unlist(lapply(list(m2$model, m3$model, m4$model), AIC))))

# Add the models testing 2-way interactions
model_selection_table <- rbind(model_selection_table, data.frame(model = as.character(unlist(lapply(model_list_2way, formula))), AIC = AIC_vals_2way, delta_AIC = AIC(pines_base_model) - AIC_vals_2way))

# Add the model with both important 2-way interactions 
model_selection_table <- rbind(model_selection_table, data.frame(model = as.character(formula(pines_model_2way))[3], AIC = AIC(pines_model_2way), delta_AIC = AIC(pines_base_model) - AIC(pines_model_2way)))

# Add the model with the 3-way interaction 
model_selection_table <- rbind(model_selection_table, data.frame(model = as.character(formula(pines_model_3way))[3], AIC = AIC(pines_model_3way), delta_AIC = AIC(pines_base_model) - AIC(pines_model_3way)))

##### Convert model selection table to tabular format for Word and export 
model_selection_table <- mutate(model_selection_table,  AIC = round(AIC, 2), delta_AIC = round(delta_AIC, 2))
flextable(model_selection_table) %>% 
  # make columns of the flextable wider
  flextable::width(j = 1:3, width = 1) %>%
  save_as_docx(path = "./figures/exploratory/prop_pines_model_selection_table.docx")
