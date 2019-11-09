library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)
library(sjPlot)

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

##### Model for Derek ----------------------------------------------------------------------------------

Derek <- lmer(ln.dens.planted ~ scale(Shrubs)*facts.planting.first.year*fsplanted+scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)

AIC(Derek)
summary(Derek)
plot(allEffects(Derek))



##### biotic environment --------------------------------------------------------------------------------

shr1 <- lmer(ln.dens.planted ~ scale(Shrubs)*facts.planting.first.year*fsplanted + neglog5SeedWallConifer + scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
step(shr1)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))
tab_model(shr1)

shr1 <- lmer(ln.dens.conif ~ facts.planting.first.year*scale(Shrubs)*fsplanted + neglog5SeedWallConifer + scale(ShrubHt) + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
step(shr1)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))

##### biotic environment --------------------------------------------------------------------------------

shr1 <- lmer(ln.dens.planted ~ scale(tpi2000)*facts.planting.first.year +
               scale(Shrubs)*facts.planting.first.year*fsplanted +
               scale(tmean)*scale(normal_annual_precip) +
              neglog5SeedWallConifer + scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)

AIC(shr1)
step(shr1)
summary(shr1)
plot(allEffects(shr1))
tab_model(shr1)
anova(shr1, shr2)

shr1 <- lmer(ln.dens.conif ~ scale(rad_summer) + facts.planting.first.year*scale(Shrubs)*fsplanted + neglog5SeedWallConifer + scale(ShrubHt) + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
step(shr1)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))

#twi may be related to shrub cover?

#####################################################################################
#####################################################################################
#  ___/-\___  #                  __                       .__                       #
# |---------| #                _/  |_____________    _____|  |__                    #
#  |   |   |  #                \   __\_  __ \__  \  /  ___/  |  \                   #
#  | | | | |  #                 |  |  |  | \// __ \_\___ \|   Y  \                  #
#  | | | | |  #                 |__|  |__|  (____  /____  >___|  /                  #
#  | | | | |  #                                  \/     \/     \/                   #
#  |_______|  #######################################################################
#####################################################################################
