setwd("~/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(betareg)
library(brms)

d = read.csv("existing_regen/regen_plots_w_gis_data.csv",stringsAsFactors = FALSE) %>%
  mutate(Fire_and_Age = paste(Fire,survey_years_post,sep="_"))


##!! Temporary: remove the shrub plots with exactly 0% and 100% cover. TODO: Transform 0 and 100 to be off the boundary.
d = d %>%
  filter(SHRUB != 0 & SHRUB != 100) %>%
  mutate(SHRUB = SHRUB/100)



m = brm(SHRUB ~ normal_annual_precip + (1|Fire_and_Age), data=d, family ="beta",chains=3,cores=3)

