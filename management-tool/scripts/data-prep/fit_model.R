setwd("~/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(lme4)

load("output/plotSeedlingData.RData") #load R object: plot_dhm_long
plot_dhm <- plot_dhm %>% 
  mutate(ln.dens.planted = log(dens.planted+24.99)) %>%
  filter(Type != "internal") %>% 
  mutate(ln.dens.conif = log(dens.conif+24.99)) %>%
  mutate(fsplanted = as.factor(fsplanted)) %>%
  mutate(facts.released = as.factor(facts.released)) %>%
  mutate(GrassHt = ifelse(is.na(GrassHt), 0, GrassHt)) %>%
  mutate(ShrubHt = ifelse(is.na(ShrubHt), 0, ShrubHt)) %>%
  mutate(ForbHt = ifelse(is.na(ForbHt), 0, ForbHt)) %>%
  mutate(SeedWallConifer = ifelse(is.na(SeedWallConifer), 500, SeedWallConifer)) %>%
  mutate(neglog5SeedWallConifer = -logb(SeedWallConifer, base = exp(5))) %>%
  mutate(totalCov = Shrubs + Grasses + Forbs) %>%
  mutate(totalCovxHt = (Shrubs*ShrubHt + Grasses*GrassHt + Forbs*ForbHt))

test <- plot_dhm %>% select(Fire, facts.planting.first.year)
unique(test)

##### final models for planted species and conifers --------------------------------------------------------------------------------

#USE THIS MODEL FOR THE TOOL
pltd <- lmer(ln.dens.planted ~ scale(tpi2000)*facts.planting.first.year + 
               scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
               scale(tmean)*scale(normal_annual_precip) +
               neglog5SeedWallConifer + scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)

#### histograms of the predictors ####

hist(plot_dhm$neglog5SeedWallConifer,nclass = 20)


