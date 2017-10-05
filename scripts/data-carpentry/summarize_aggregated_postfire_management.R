setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(dplyr)
library(stringr)
library(tidyr)

d <- read.csv("data/output-exploratory/aggregated-management-history/aggregated_management_history.csv",header=TRUE,stringsAsFactors = FALSE)

d$salvaged <- d$salvage.nyears > 0
d$planted <- d$planting.nyears > 0

## Compute year first planted
d <- d %>%
  separate(planting.years.post,"first.pltd.yr",sep=", ",remove=FALSE,extra="drop")


## Compute T/F columns for first planted in first, second, third, fourth, or later years; site prep; released 0, 1, 2, or more times; replanted 0, 1, or more times; thinned 0, 1, or more times; pruned 0 or more times
d <- d %>%
  mutate(
         pltd.yr0 = (first.pltd.yr == 0),
         pltd.yr1 = (first.pltd.yr == 1),
         pltd.yr2 = (first.pltd.yr == 2),
         pltd.yr3 = (first.pltd.yr == 3),
         pltd.yr4 = (first.pltd.yr == 4),
         pltd.later = (first.pltd.yr > 4),
         site.prepped = prep.nyears > 0,
         released.0x = release.nyears == 0,
         released.1x = release.nyears == 1,
         released.2x = release.nyears == 2,
         released.morex = release.nyears > 2,
         replanted.0x = replant.nyears == 0,
         replanted.1x = replant.nyears == 1,
         replanted.morex = replant.nyears == 2,
         thinned.0x = thin.nyears == 0,
         thinned.1x = thin.nyears == 1,
         thinned.morex = thin.nyears > 1,
         pruned.0x = prune.nyears == 0,
         pruned.morex = prune.nyears > 0)

#! why do prunes not add to 1?
#! freds was pruned too much 



## Convert the T/F columns into area that applies (just the area of the slice if true, and 0 if false)
d <- d %>%
  mutate_each(funs(area=.*area.sqm),pltd.yr0:pruned.morex)



### Compute total area of salvage x planted
## with percentages for each year planted and managed in different categories

salvage.x.plant <- d %>%
  group_by(fire.name,salvaged,planted) %>%
  summarise_at(vars(area.sqm,pltd.yr0_area:pruned.morex_area),funs(tot=sum(.))) %>%
  ungroup

salvage.x.plant <- salvage.x.plant %>%
  group_by(fire.name) %>%
  mutate_at(vars(area.sqm_tot,pltd.yr0_area_tot:pruned.morex_area_tot),funs(prop=round(./sum(area.sqm_tot),2))) %>%
  ungroup
    
salvage.x.plant.simp <- salvage.x.plant %>%
  mutate(area.sqm_tot = area.sqm_tot/10000) %>%
  select(fire.name,salvaged,planted,area.ha=area.sqm_tot,area_prop=area.sqm_tot_prop,contains("area_tot_prop")) %>%
  filter(area.ha > 1) # the resulting fire X salvage (t/f) X planted (t/f) area must be > 1 ha this mostly removes the not-salvaged, not-planted factor level which on most fires is a tiny area: the area that happened to have been planted the same year as the fire, just before it)

  
## shorten names
names(salvage.x.plant.simp) <- gsub("_area_tot","",names(salvage.x.plant.simp))
  
 