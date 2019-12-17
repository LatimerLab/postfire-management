#setwd("~/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(brms)
library(lme4)
library(effects)

### Load the data
d = read.csv("existing_regen/regen_plots_w_gis_data.csv",stringsAsFactors = FALSE) %>%
  mutate(Fire_and_Age = paste(Fire,survey_years_post,sep="_"))


###!!! Temporary: remove the shrub plots with exactly 0% and 100% cover. TODO: Transform 0 and 100 to be off the boundary.
#d = d %>%
 # filter(SHRUB != 0 & SHRUB != 100) %>%
  #mutate(SHRUB = SHRUB/100)
d = d %>%
  filter(!is.na(SHRUB)) %>%
  mutate(SHRUB = ifelse(SHRUB == 0, 1, SHRUB)) %>%
  mutate(SHRUB = ifelse(SHRUB == 100, 99, SHRUB)) %>%
  mutate(SHRUB = SHRUB/100)
dl = d %>%
  filter(!is.na(SHRUB)) %>%
  mutate(SHRUB = SHRUB/100)

### Run beta regression in BRMS
delta_val = 0.95
treedepth = 12
m = brm(SHRUB ~ scale(normal_annual_precip) * scale(tmax) * scale(rad_winter) + scale(tpi5000) + (1|Fire_and_Age) , data=d, family ="beta",chains=3,cores=3,control=list(adapt_delta=delta_val,max_treedepth=treedepth),seed=5, inits=0)
m.5 = brm(SHRUB ~ scale(normal_annual_precip) * scale(tmax) + scale(tpi5000) + (1|Fire_and_Age) , data=d, family ="beta",chains=3,cores=3,control=list(adapt_delta=delta_val,max_treedepth=treedepth),seed=5, inits=0)
m2 = brm(SHRUB ~ scale(rad_winter)+scale(normal_annual_precip) + 
          #I(scale(normal_annual_precip)^2) +
          scale(rad_winter):I(scale(normal_annual_precip)^2) +
          #I(scale(rad_winter)^2) +
          #scale(normal_annual_precip)*I(scale(rad_winter)^2) +
          #I(scale(rad_winter)^2)*I(scale(normal_annual_precip)^2) +
          scale(tmean) + 
          #poly(scale(elev), 2) + #does not improve
          #scale(tmin) +
          #scale(tmax) +
          #scale(twi) + 
          #I(scale(twi)^2) + 
          (1|Fire_and_Age), data=d, family ="beta",chains=3,cores=3,control=list(adapt_delta=delta_val,max_treedepth=treedepth),seed=5, inits=0)


summary(m)

# Compare fitted vs. observed

fitted = fitted(m)[,"Estimate"]
observed = d$SHRUB

plot(observed,fitted) # < not great looking

cor(observed,fitted)^2  # rough approximate R-sq of 0.22

### Run using lme4

m1 <- lmer(asin(sqrt(SHRUB)) ~ scale(normal_annual_precip) * scale(tmax) + scale(tpi5000) + (1|Fire_and_Age) + scale(rad_winter), data=dl)
summary(m1)
plot(m1)

m3 <- lmer(asin(sqrt(SHRUB)) ~  scale(normal_annual_precip)*scale(tmax)*scale(rad_winter) + 
             #I(scale(normal_annual_precip)^2) +
             #scale(tmax):I(scale(normal_annual_precip)^2) +
             #I(scale(tmax)^2) +
             #scale(normal_annual_precip)*I(scale(tmax)^2) +
             #I(scale(tmax)^2)*I(scale(normal_annual_precip)^2) +
             #poly(scale(elev), 2) + #does not improve
             #scale(tmin) +
             #scale(tmax) +
             #scale(twi) + 
             #I(scale(twi)^2) + 
             #scale(tpi5000) +
             (1|Fire_and_Age), data=dl)
summary(m3)
plot(allEffects(m3))
plot(m3)


