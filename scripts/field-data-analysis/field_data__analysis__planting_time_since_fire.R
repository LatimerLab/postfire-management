library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)
library(sjPlot)
library(MuMIn)

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
  mutate(totalCovxHt = (Shrubs*ShrubHt + Grasses*GrassHt + Forbs*ForbHt)) %>%
  mutate(ShrVol = (Shrubs*ShrubHt))

#plant.yr.per.fire <- plot_dhm %>% select(Fire, facts.planting.first.year)
#unique(plant.yr.per.fire)

##### final models for planted species and conifers --------------------------------------------------------------------------------

#USE THIS MODEL FOR THE TOOL
pltd <- lmer(ln.dens.planted ~ scale(tpi2000)*facts.planting.first.year + 
               scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
               #scale(ShrVol)*facts.planting.first.year*fsplanted +
               scale(tmin_mjj)*scale(normal_annual_precip) +
               scale(neglog5SeedWallConifer) +
               scale(I(DuffDepth+LitterDepth)) +
               scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)

AIC(pltd)
summary(pltd)
plot(pltd)
plot(allEffects(pltd))
r.squaredGLMM(pltd)

#All conifers model
conif <- lmer(ln.dens.conif ~ scale(tpi2000)+facts.planting.first.year +
                #fsplanted +
                facts.planting.first.year*fsplanted*scale(ShrVol) +
                scale(tmean)*scale(normal_annual_precip) +
                scale(I(DuffDepth+LitterDepth)) +
                neglog5SeedWallConifer + #scale(ShrubHt) +
                (1|Fire) + (1|Fire:PairID), data = plot_dhm)

AIC(conif)
summary(conif)
anova(conif)
plot(allEffects(conif))

#summary(lm(tpi500~ twi + I(twi^2), , data = plot_dhm))
#plot(tpi500 ~ twi, data = plot_dhm)

#Find the messed up shrub heights.
plot_dhm %>% filter(ShrubHt < 1) %>% select(PlotID, Shrubs, ShrubHt) %>% arrange(desc(Shrubs))
#Shrub model
shr <- lmer(scale(asin(sqrt(Shrubs/100))) ~ scale(normal_annual_precip)*scale(rad_winter) + 
               #I(scale(normal_annual_precip)^2) +
               #scale(rad_winter):I(scale(normal_annual_precip)^2) +
               #I(scale(rad_winter)^2) +
               #scale(normal_annual_precip)*I(scale(rad_winter)^2) +
               #I(scale(rad_winter)^2)*I(scale(normal_annual_precip)^2) +
               #scale(tmean) + 
               #poly(scale(elev), 2) + #does not improve
               scale(tmean) +
               #I(scale(tmin)^2) + Does not improve modl
               #scale(tmax) +
               #scale(slope_dem) + 
               #I(scale(slope_dem)^2) + 
               scale(twi) + 
               I(scale(twi)^2) +
               #scale(tpi500) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(shr)
summary(shr)
plot(shr)
hist(resid(shr))
plot(allEffects(shr))
r.squaredGLMM(shr)

#Shrub Height model, NOT COMPLETED
shrvol <- lmer(ShrCovxHt ~ scale(normal_annual_precip)*scale(rad_winter) + 
                #I(scale(normal_annual_precip)^2) +
                #scale(rad_winter):I(scale(normal_annual_precip)^2) +
                #I(scale(rad_winter)^2) +
                #scale(normal_annual_precip)*I(scale(rad_winter)^2) +
                #I(scale(rad_winter)^2)*I(scale(normal_annual_precip)^2) +
                #scale(normal_annual_precip)*scale(tmean) + 
                #I(scale(normal_annual_precip)^2) +
                #scale(tmax):I(scale(normal_annual_precip)^2) +
                #I(scale(rad_winter)^2) +
                #scale(normal_annual_precip)*I(scale(tmax)^2) +
                #I(scale(tmax)^2)*I(scale(normal_annual_precip)^2) +
                #scale(tmax) + 
                #scale(tmax):scale(normal_annual_precip) + 
                #I(scale(tmax)^2) +
                #scale(elev) +
                #I(scale(elev)^2) +
                #scale(tmin) +
                #I(scale(tmin)^2) + 
                #scale(tmax) +
                #scale(slope_dem) + 
                #I(scale(slope_dem)^2) + 
                #scale(twi) + 
                #I(scale(twi)^2) +
                scale(tpi2000) +
                #scale(tmax) +
                #scale(tpi2000)*scale(tmax) +
                scale(tmax)+
                I(scale(tpi2000)^2) +
                I(scale(tmax)^2) +
                #scale(normal_annual_precip) +
                #I(scale(normal_annual_precip)^2) +
                #scale(normal_annual_precip):scale(tmax) +
                #I(scale(normal_annual_precip)^2):scale(tmax) +
                #scale(normal_annual_precip):I(scale(tmax)^2) +
                #I(scale(normal_annual_precip)^2):I(scale(tmax)^2) +
                #I(scale(tpi2000)^2):scale(tmax) +
                #scale(tpi2000):I(scale(tmax)^2) +
                #I(scale(tpi2000)^2):I(scale(tmax)^2) +
                (1|Fire) + (1|Fire:PairID), data = plot_dhm %>% filter(ShrubHt > 0))

AIC(shrht)
summary(shrht)
plot(shrht)
hist(resid(shrht))
plot(allEffects(shrht))
r.squaredGLMM(shrht)

#save(pltd, shr, file = "output/modelDataForPlots.RData")


cor(plots %>% dplyr::select(elev, rad_winter, slope_dem, normal_annual_precip, twi, tpi100, tpi500, tpi2000, tpi5000, tmax, tmin, tmean),  use = "complete.obs", method = "pearson")
cor(plots %>% dplyr::select(tmax_ndj, tmin_ndj, tmean_ndj, tmax_fma, tmin_fma, tmean_fma, tmax_mjj, 
                            tmin_mjj, tmean_mjj, tmax_aso, tmin_aso, tmean_aso, tmax, tmin, tmean),  use = "complete.obs", method = "pearson")

##### SEM --------------------------------------------------------------------------------------

pltd <- glmer.nb(round(dens.planted, 0 ) ~ scale(tpi2000)*facts.planting.first.year + 
                   scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
                   scale(tmean)*scale(normal_annual_precip) +
                   neglog5SeedWallConifer + scale(ShrubHt) +
                   (1|Fire) + (1|Fire:PairID) ,data = plot_dhm %>% mutate(obs = row_number()))

library(piecewiseSEM)

plot_dhm.sem <- plot_dhm %>%
  mutate(ztpi2000 = scale(tpi2000)) %>%
  mutate(zasShrubs = scale(asin(sqrt(Shrubs/100)))) %>%
  mutate(ztmean = scale(tmean)) %>%
  mutate(znormal_annual_precip = scale(normal_annual_precip)) %>%
  mutate(zShrubHt = scale(ShrubHt)) %>%
  mutate(zrad_winter = scale(rad_winter)) %>%
  mutate(ztmin = scale(tmin)) %>%
  mutate(ztpi500 = scale(tpi500)) %>%
  mutate(fsplantedBin = as.numeric(ifelse(fsplanted == "planted", 1, 0)))

sem.mods <- psem(
  
  lmer(ln.dens.planted ~ ztpi2000*facts.planting.first.year +
         zasShrubs*facts.planting.first.year*fsplantedBin +
         ztmean*znormal_annual_precip +
         neglog5SeedWallConifer + zShrubHt +
         (1|Fire) + (1|Fire:PairID), data = plot_dhm.sem),
  
  lmer(zasShrubs ~ znormal_annual_precip*zrad_winter + 
                 #I(scale(normal_annual_precip)^2) +
                 #scale(rad_winter):I(scale(normal_annual_precip)^2) +
                 #I(scale(rad_winter)^2) +
                 #scale(normal_annual_precip)*I(scale(rad_winter)^2) +
                 #I(scale(rad_winter)^2)*I(scale(normal_annual_precip)^2) +
                 #scale(tmean) + 
                 #poly(scale(elev), 2) + #does not improve
                 ztmin +
                 #scale(tmax) +
                 #scale(slope_dem) + 
                 #I(scale(slope_dem)^2) + 
                 scale(twi) + 
                 I(scale(twi)^2) +
                 ztpi500 + 
                 (1|Fire) + (1|Fire:PairID), data = plot_dhm.sem)
    )
  
  
  
sum.mod.1 <- summary(sem.mods)
print(sum.mod.1)
AIC(sem.mods, aicc = TRUE)


##### Looking into concerns form audience at AFES -----------------------------------------------

### Test for hump shaped relationship between shrub cover and tree dense.

shr1 <- lmer(ln.dens.planted ~ scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
               #I(scale(asin(sqrt(Shrubs/100)))^2)*facts.planting.first.year +
               #I(scale(asin(sqrt(Shrubs/100)))^2)*fsplanted + 
               #I(scale(asin(sqrt(Shrubs/100)))^2)*facts.planting.first.year*fsplanted +
               #I(scale(asin(sqrt(Shrubs/100)))^2)
               scale(tpi2000)*facts.planting.first.year +
               scale(tmean)*scale(normal_annual_precip) +
               neglog5SeedWallConifer + scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(shr1)

# NOPE, NOT QUADRATIC TERM FOR SHRUBS ^^^^^^



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


conif <- lmer(ln.dens.conif ~ scale(rad_summer) + 
                facts.planting.first.year*scale(Shrubs)*fsplanted + 
                neglog5SeedWallConifer + scale(ShrubHt) + (1|Fire) + (1|Fire:PairID), data = plot_dhm)

AIC(conif)
summary(conif)
plot(allEffects(conif))

### Test for hump shaped relationship between shrub cover and tree dense.

shr1 <- lmer(ln.dens.planted ~ scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
               #I(scale(asin(sqrt(Shrubs/100)))^2)*facts.planting.first.year +
               #I(scale(asin(sqrt(Shrubs/100)))^2)*fsplanted + 
               #I(scale(asin(sqrt(Shrubs/100)))^2)*facts.planting.first.year*fsplanted +
               #I(scale(asin(sqrt(Shrubs/100)))^2)
               scale(tpi2000)*facts.planting.first.year +
               scale(tmean)*scale(normal_annual_precip) +
               neglog5SeedWallConifer + scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(shr1)

# NOPE, NOT QUADRATIC TERM FOR SHRUBS ^^^^^^

shr2 <- lmer(scale(asin(sqrt(Shrubs/100))) ~ scale(rad_winter)+scale(normal_annual_precip) + 
               I(scale(normal_annual_precip)^2) +
               scale(rad_winter):I(scale(normal_annual_precip)^2) +
               #I(scale(rad_winter)^2) +
               #scale(normal_annual_precip)*I(scale(rad_winter)^2) +
               #I(scale(rad_winter)^2)*I(scale(normal_annual_precip)^2) +
               scale(tmin) + 
               #poly(scale(elev), 2) + #does not improve
               #scale(tmin) +
               #scale(tmax) +
               #scale(twi) + 
               #I(scale(twi)^2) + 
               #scale(slope_dem) + 
               #I(scale(slope_dem)^2) +
               scale(tpi500) +
               (1|Fire) 
             + (1|Fire:PairID)
             , data = plot_dhm)

#twi may be related to shrub cover?
shr2 <- lmer(asin(sqrt(Shrubs/100)) ~ fsplanted*facts.released + (1|Fire) + (1|Fire:PairID), data = plot_dhm)
plot(allEffects(shr2))

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

