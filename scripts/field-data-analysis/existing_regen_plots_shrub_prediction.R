setwd("~/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(brms)

### Load the data
d = read.csv("existing_regen/regen_plots_w_gis_data.csv",stringsAsFactors = FALSE) %>%
  mutate(Fire_and_Age = paste(Fire,survey_years_post,sep="_"))


###!!! Temporary: remove the shrub plots with exactly 0% and 100% cover. TODO: Transform 0 and 100 to be off the boundary.
d = d %>%
  filter(SHRUB != 0 & SHRUB != 100) %>%
  mutate(SHRUB = SHRUB/100)

### Run beta regression in BRMS
delta_val = 0.95
treedepth = 12
m = brm(SHRUB ~ scale(normal_annual_precip) * scale(tmax) + scale(tpi5000) + (1|Fire_and_Age) + scale(rad_winter), data=d, family ="beta",chains=3,cores=3,control=list(adapt_delta=delta_val,max_treedepth=treedepth),seed=5, inits=0)

summary(m)

# Compare fitted vs. observed

fitted = fitted(m)[,"Estimate"]
observed = d$SHRUB

plot(observed,fitted) # < not great looking

cor(observed,fitted)^2  # rough approximate R-sq of 0.22




