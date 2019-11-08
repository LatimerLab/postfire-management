library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)

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
  mutate(totalCov = Shrubs + Grasses + Forbs) %>%
  mutate(totalCovxHt = (Shrubs*ShrubHt + Grasses*GrassHt + Forbs*ForbHt))

##### Model for Derek ----------------------------------------------------------------------------------

Derek <- lmer(ln.dens.planted ~ scale(Shrubs)*facts.planting.first.year*fsplanted+scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)

AIC(Derek)
summary(Derek)
plot(allEffects(Derek))



m1 <- lmer(ln.dens.planted ~ scale(slope_dem) + scale(elev) + 
             fsplanted*scale(normal_annual_precip) + (fsplanted*scale(normal_annual_precip)|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(m1)
summary(m1)
plot(m1)
plot(allEffects(m1))

m2 <- lmer(log(dens.planted+25) ~ scale(elev)*scale(slope_dem) + fsplanted + scale(normal_annual_precip) + (scale(elev)*scale(slope_dem)|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(m2)
plot(m2)
plot(allEffects(m2))

m2 <- lmer(log(dens.planted+24.9) ~ scale(twi)*fsplanted + scale(tpi2000)*fsplanted + scale(elev) + scale(slope_dem) + scale(normal_annual_precip) + (scale(tpi2000)|Fire) + (scale(normal_annual_precip)|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(m2)
AIC(m2)
plot(m1)
plot(allEffects(m1))

m2 <- lmer(log(dens.conif+24.9) ~ scale(tpi2000)+fsplanted + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(m2)
AIC(m2)
plot(m2)

m2 <- lmer(log(dens.conif+24.9) ~ scale(twi)*fsplanted + (scale(twi)|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(m2)
AIC(m2)
plot(m2)

##### Time since fire ---------------------------------------------------------------------------------

plot_dhm$facts.planting.first.year <- as.numeric(plot_dhm$facts.planting.first.year)
tsf1 <- lmer(ln.dens.planted ~ fsplanted*facts.planting.first.year + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(tsf1)
summary(tsf1)
plot(allEffects(tsf1))
#added in every combination of random slopes. Not better models.
                
plot_dhm$facts.planting.first.year <- as.numeric(plot_dhm$facts.planting.first.year)
tsf1 <- lmer(ln.dens.planted ~ fsplanted*facts.planting.first.year + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(tsf1)
summary(tsf1)
plot(allEffects(tsf1))
#added in every combination of random slopes. Not better models.

##### Time since fire ---------------------------------------------------------------------------------

#plot_dhm$facts.planting.first.year <- as.numeric(plot_dhm$facts.planting.first.year)
tsf1 <- lmer(ln.dens.planted ~ fsplanted*facts.planting.first.year + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(tsf1)
summary(tsf1)
plot(allEffects(tsf1))
#added in every combination of random slopes. Not better models.

#plot_dhm$facts.planting.first.year <- as.numeric(plot_dhm$facts.planting.first.year)
tsf1 <- lmer(ln.dens.planted ~ fsplanted*facts.planting.first.year + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(tsf1)
summary(tsf1)
plot(allEffects(tsf1))
#added in every combination of random slopes. Not better models.

##### Shrubs in plots --------------------------------------------------------------------------------

shr1 <- lmer(ln.dens.planted ~ scale(Shrubs)*facts.planting.first.year*fsplanted+scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)

AIC(shr1)
summary(shr1)
plot(allEffects(shr1))

shr1 <- lmer(ln.dens.conif ~ facts.planting.first.year*scale(Shrubs)*fsplanted+scale(ShrubHt) + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))

##### Shrubs in plots --------------------------------------------------------------------------------

shr1 <- lmer(ln.dens.planted ~ fsplanted*facts.planting.first.year + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
plot(shr1)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))

shr1 <- lmer(ln.dens.conif ~ fsplanted*facts.planting.first.year + scale(ShrubHt) + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))



