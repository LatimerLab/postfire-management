library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(sjPlot)
library(MuMIn)
library(Hmisc)
library(car)
library(brms)
#library(BiodiversityR)

# Load data 
load("./output/plotSeedlingData.RData") 

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
sum(plot_dhm$LiveOverstory==0, na.rm=T)/length(plot_dhm_pine) # This variable is 88% zeros -- can we leave it out? Will leave out for now. 

# Check variable correlations 
cor(plot_dhm_pine[,vars_to_test_continuous], use = "na.or.complete")
# Temp variables too strongly correlated, only tmin is less than 0.7 correlated with elevation. Everything else looks ok. 

# Update continuous variables to test in light of correlations and heavy zero-domination of live overstory
vars_to_test_continuous <- c("tmin", "normal_annual_precip", "rad_summer", "Forbs", "Shrubs", "Grasses", "ShrubHt", "log10SeedWallConifer", "twi", "tpi2000", "elev", "CWD_sound")

# scale continuous variables 
plot_dhm_pine_std <- stdize(plot_dhm_pine[ , vars_to_test_continuous], prefix = FALSE)

# add factor variables
plot_dhm_pine_std <- cbind(plot_dhm_pine_std, plot_dhm_pine[, vars_to_test_factor])

# Sdd the response 

#Binomial version: 
plot_dhm_pine_std <- mutate(plot_dhm_pine_std, 
      seedling_count = round(plot_dhm_pine$dens.conif/24.94098, 0), 
      pine_count = round(plot_dhm_pine$dens.pine/24.94098, 0))

# Continuous [0,1] version for beta regression 
plot_dhm_pine_std <- mutate(plot_dhm_pine_std, 
        raw_prop = plot_dhm_pine$dens.pine / plot_dhm_pine$dens.conif)
# Create new prop variable where 0s and 1s are replaced by halfway between min/max value and 0/1
min_prop <- min(plot_dhm_pine_std$raw_prop[plot_dhm_pine_std$raw_prop>0])
max_prop <- max(plot_dhm_pine_std$raw_prop[plot_dhm_pine_std$raw_prop<1])
plot_dhm_pine_std$prop_response <- plot_dhm_pine_std$raw_prop
plot_dhm_pine_std$prop_response[plot_dhm_pine_std$raw_prop == 0] <- min_prop
plot_dhm_pine_std$prop_response[plot_dhm_pine_std$raw_prop == 1] <- max_prop

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



