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
tab_model(shr)

shr1 <- lmer(ln.dens.conif ~ 
               #facts.planting.first.year*scale(Shrubs)*fsplanted + neglog5SeedWallConifer + scale(ShrubHt) + 
               scale(tpi2000)*facts.planting.first.year +
               scale(Shrubs)*scale(tmean) +
               scale(tmean)*scale(normal_annual_precip) +
               neglog5SeedWallConifer + 
               #scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
step(shr1)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))

##### biotic environment --------------------------------------------------------------------------------

shr1 <- lmer(ln.dens.planted ~ scale(tpi2000)*facts.planting.first.year +
               scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
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
shr2 <- lmer(asin(sqrt(Shrubs/100)) ~ fsplanted*facts.released + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
plot(allEffects(shr2))


shr2 <- lmer(scale(asin(sqrt(Shrubs/100))) ~ scale(rad_winter)+scale(normal_annual_precip) + 
               I(scale(normal_annual_precip)^2) +
               scale(rad_winter):I(scale(normal_annual_precip)^2) +
               #I(scale(rad_winter)^2) +
               #scale(normal_annual_precip)*I(scale(rad_winter)^2) +
               #I(scale(rad_winter)^2)*I(scale(normal_annual_precip)^2) +
               scale(tmean) + 
               #poly(scale(elev), 2) + #does not improve
               #scale(tmin) +
               #scale(tmax) +
               scale(twi) + 
               I(scale(twi)^2) + 
               #scale(slope_dem) + 
               #I(scale(slope_dem)^2) + 
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(shr2)
summary(shr2)
plot(shr2)
hist(resid(shr2))
step(shr2)
plot(allEffects(shr2))
tab_model(shr2)


plot(allEffects(shr2))

cor(plots %>% dplyr::select(elev, rad_winter, slope_dem, normal_annual_precip, twi, tpi2000, tmax, tmin, tmean),  use = "complete.obs", method = "pearson")

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
