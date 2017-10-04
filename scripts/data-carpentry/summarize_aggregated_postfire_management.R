setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(dplyr)

d <- read.csv("data/output-exploratory/aggregated-management-history/aggregated_management_history.csv",header=TRUE,stringsAsFactors = FALSE)

d$salvaged <- d$salvage.nyears > 0
d$planted <- d$planting.nyears > 0



### Compute total area of salvage x planted

salvage.x.plant <- d %>%
  group_by(fire.name,salvaged,planted) %>%
  summarise(area.tot = sum(area.sqm)) %>%
  ungroup %>%
  group_by(fire.name) %>%
  mutate(area.prop = area.tot/sum(area.tot)) %>%
  mutate(area.prop = round(area.prop,2))
  
  
