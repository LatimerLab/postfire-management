library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)
library(sjPlot)
library(MuMIn)
library(Hmisc)
library(car)

load("output/plotSeedlingData.RData") #load R object: plot_dhm_long
#old not downscaled data here: plotSeedlingData_old_not_downscaled.RData

#plant.yr.per.fire <- plot_dhm %>% select(Fire, facts.planting.first.year)
#unique(plant.yr.per.fire)



##### final models for planted species and conifers --------------------------------------------------------------------------------

#USE THIS MODEL FOR THE TOOL
pltd <- lmer(ln.dens.planted ~ scale(tpi2000)*scale(elev) +
               scale(Shrubs)*facts.planting.first.year*fsplanted + 
               #scale(log(planted_density_tpa+24.99)) +
               scale(tmin)*scale(normal_annual_precip) +
               scale(log10SeedWallConifer) +
               scale(LitDuff) +
               (1|Fire) +
               (1|Fire:PairID),
              REML = F,
              data = plot_dhm)

summary(pltd)
AIC(pltd)


anova(pltd,pltd2)
ranef(pltd)
plot(pltd)
plot(allEffects(pltd))
r.squaredGLMM(pltd)
leveragePlots(pltd)
qqnorm(residuals(pltd))
# 

#for recording model selection
pltd <- lmer(ln.dens.planted ~ 
               
### climate
               #scale(tmean) +
               scale(tmin)*scale(normal_annual_precip) +
               #scale(tmax) +
               #scale(normal_annual_precip) +
               #scale(rad_summer) +
### competition
               #scale(Forbs) +
               #scale(Grasses) +
               scale(Shrubs)*facts.planting.first.year*fsplanted +
               #scale(LiveOverstory) +
               #scale(ShrubHt) +
### dispersal
               scale(log10SeedWallConifer) +
###Topography
               scale(tpi2000)*scale(elev) +
               #scale(twi) +
               #scale(elev)  +   
               #scale(LitDuff) +
               #scale(CWD_sound) +
###planting  
               # +
               # +
               (1|Fire) +
               (1|Fire:PairID), 
             REML = T,
             data = plot_dhm)

               

summary(pltd)#, correlation=TRUE)
AIC(pltd)
#results.table <- summary(pltd)$coefficients

#mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2) #mod.selc.record2[2:nrow(mod.selc.record2),]
#mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients)

# For revision, load model selection results that were saved earlier 
mod.selc.record2 <- read.csv("output/mod.selc.record2.csv")
load("output/mod.selc.est.list_backup.Rdata")
results.table <- read.csv("output/results.table.csv")


mod.selc.record2.fr <- mod.selc.record2 %>% 
  mutate(V1 = str_replace_all(V1, pattern = "ln.dens.planted ~", replacement = "Seedling Density =")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tpi100\\)", replacement = "Topographic Position Index 100")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tpi500\\)", replacement = "Topographic Position Index 500")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tpi2000\\)", replacement = "Topographic Position Index 2000")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tpi5000\\)", replacement = "Topographic Position Index 5000")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(elev\\)", replacement = "Elevation")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(Forbs\\)", replacement = "%Forb")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(Grasses\\)", replacement = "%Grasses")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(ShrubHt\\)", replacement = "Ave. Shrub Ht")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(Shrubs\\)", replacement = "%Shrubs")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "facts.planting.first.year", replacement = "Year Planted")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "fsplanted", replacement = "Planting")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "facts.released", replacement = "Released")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tmean\\)", replacement = "Mean Annual Temp.")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tmax\\)", replacement = "Max. Annual Temp.")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(rad_winter\\)", replacement = "Winter Solar Insolation")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(rad_winter_spring\\)", replacement = "Winter/Spring Solar Insolation")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(rad_spring\\)", replacement = "Spring Solar Insolation")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(rad_spring_summer\\)", replacement = "Spring/Summer Solar Insolation")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(rad_summer\\)", replacement = "Summer Solar Insolation")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(normal_annual_precip\\)", replacement = "Normal Annual Precipitation")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tmin\\)", replacement = "Min. Annual Temp.")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(bcm_snowpack\\)", replacement = "Snowpack")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(twi\\)", replacement = "Total Water Index")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(log10SeedWallConifer\\)", replacement = "Seed Source")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(LitterDepth\\)", replacement = "Litter Depth")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(DuffDepth\\)", replacement = "Duff Depth")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(LitDuff\\)", replacement = "Litter + Duff Depth")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(CWD_rotten\\)", replacement = "Rotten Coarse Woody Debris")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(CWD_sound\\)", replacement = "Coarse Woody Debris")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(LiveOverstory\\)", replacement = "%Overstory")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(aspect_dem\\)", replacement = "Aspect"))  %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(slope_dem\\)", replacement = "Slope"))  

#write.csv(mod.selc.record2.fr, "output/mod.selc.record2.csv")
#save(mod.selc.est.list2, file = "output/mod.selc.est.list_backup2.RData")

results.table <- as.data.frame(results.table) %>% mutate(Vars = row.names(results.table))
results.table <- results.table %>% 
  mutate(Vars = str_replace(Vars, pattern = "ln.dens.planted ~", replacement = "Seedling Density =")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(tpi100\\)", replacement = "Topographic Position Index 100")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(tpi500\\)", replacement = "Topographic Position Index 500")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(tpi2000\\)", replacement = "Topographic Position Index 2000")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(tpi5000\\)", replacement = "Topographic Position Index 5000")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(elev\\)", replacement = "Elevation")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(Forbs\\)", replacement = "%Forb")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(Grasses\\)", replacement = "%Grasses")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(ShrubHt\\)", replacement = "Ave. Shrub Ht")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(Shrubs\\)", replacement = "%Shrubs")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "facts.planting.first.year", replacement = "Year Planted")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "fsplanted", replacement = "Planting")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "facts.released", replacement = "Released")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(tmean\\)", replacement = "Mean Annual Temp.")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(tmax\\)", replacement = "Max. Annual Temp.")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(rad_winter\\)", replacement = "Winter Solar Insolation")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(rad_winter_spring\\)", replacement = "Winter/Spring Solar Insolation")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(rad_spring\\)", replacement = "Spring Solar Insolation")) %>%  
  mutate(Vars = str_replace(Vars, pattern = "scale\\(rad_spring_summer\\)", replacement = "Spring/Summer Solar Insolation")) %>%  
  mutate(Vars = str_replace(Vars, pattern = "scale\\(rad_summer\\)", replacement = "Summer Solar Insolation")) %>%  
  mutate(Vars = str_replace(Vars, pattern = "scale\\(normal_annual_precip\\)", replacement = "Normal Annual Precipitation")) %>%  
  mutate(Vars = str_replace(Vars, pattern = "scale\\(tmin\\)", replacement = "Min. Annual Temp.")) %>%  
  mutate(Vars = str_replace(Vars, pattern = "scale\\(bcm_snowpack\\)", replacement = "Snowpack")) %>%  
  mutate(Vars = str_replace(Vars, pattern = "scale\\(twi\\)", replacement = "Total Water Index")) %>%  
  mutate(Vars = str_replace(Vars, pattern = "scale\\(log10SeedWallConifer\\)", replacement = "Seed Source")) %>%  
  mutate(Vars = str_replace(Vars, pattern = "scale\\(LitterDepth\\)", replacement = "Litter Depth")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(DuffDepth\\)", replacement = "Duff Depth")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(LitDuff\\)", replacement = "Litter + Duff Depth")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(CWD_rotten\\)", replacement = "Rotten Coarse Woody Debris")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(CWD_sound\\)", replacement = "Coarse Woody Debris")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(LiveOverstory\\)", replacement = "%Overstory")) %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(aspect_dem\\)", replacement = "Aspect"))  %>% 
  mutate(Vars = str_replace(Vars, pattern = "scale\\(slope_dem\\)", replacement = "Slope")) 

#write.csv(results.table, "output/results.table.csv")

ggplot(data.frame(lev=hatvalues(pltd),pearson=residuals(pltd,type="pearson")),
       aes(x=lev,y=pearson)) +
  geom_point() +
  theme_bw()

levId <- which(hatvalues(pltd) >= .23)


#All conifers model
conif <- lmer(ln.dens.conif ~ scale(tpi2000)*scale(elev) +
                #facts.planting.first.year +
                #fsplanted +
                facts.planting.first.year*fsplanted*scale(Shrubs) +
                scale(tmin)*scale(normal_annual_precip) +
                #scale(LitDuff) +
                log10SeedWallConifer + #scale(ShrubHt) +
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
shr <- lmer(#cbind(Shrubs, 100-Shrubs)
            #scale(Shrubs)
            scale(asin(sqrt(Shrubs/100)))
            ~ scale(normal_annual_precip)*scale(rad_winter) + 
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
               #scale(bcm_tmax_july) +
               #scale(bcm_aet)+
               #scale(bcm_cwd) +
               #scale(log10SeedWallConifer) +
               #scale(LitDuff) +
               #scale(bcm_snowpack) +
               #I(scale(twi)^2) +
               #scale(tpi500) +
               #facts.released +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm %>% mutate(facts.released = as.factor(ifelse(fsplanted == "unplanted", 1, facts.released))) )
AIC(shr)
summary(shr)
plot(shr)
hist(resid(shr))
plot(allEffects(shr))

r.squaredGLMM(shr)

#Model select procedure

#Shrub model
shr <- lmer(#cbind(Shrubs, 100-Shrubs)
  #scale(Shrubs)
  scale(asin(sqrt(Shrubs/100))) ~
    scale(tmean) + # tmin, tmax #stay
    scale(normal_annual_precip) + #stay 
    scale(rad_winter) + #stay
    #scale(tpi2000) + #8
    scale(twi) + #stay 
    #facts.released + #7
    scale(tmean)*scale(rad_winter) + #stay
    #scale(tmean)*scale(normal_annual_precip) + #5
    scale(normal_annual_precip)*scale(rad_winter) + #stay 
    #scale(tmean)*scale(normal_annual_precip)*scale(rad_winter) + #1
    #scale(twi)*scale(normal_annual_precip) + #3
    #scale(twi)*scale(tmean) + #4
    #scale(tpi2000)*scale(normal_annual_precip) + #2
    #scale(tpi2000)*scale(tmean) + #6
    (1|Fire) + (1|Fire:PairID), REML = F, data = plot_dhm %>% 
    mutate(facts.released = as.factor(ifelse(fsplanted == "unplanted", 1, facts.released))))
#anova(shr, shr2)
AIC(shr)
summary(shr)
shr.selc.record2 <- rbind(as.data.frame(cbind(as.character(shr@call)[2], AIC(shr))), shr.selc.record2) #mod.selc.record2[2:nrow(mod.selc.record2),]
shr.selc.est.list2 <- list(shr.selc.est.list2, summary(shr)$coefficients)
#shr.selc.record2 <- as.data.frame(cbind(as.character(shr@call)[2], AIC(shr)))
#shr.selc.est.list2 <- summary(shr)$coefficients


mod.selc.record2.shr <- shr.selc.record2 %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale(asin(sqrt(Shrubs/100))) ~", replacement = "Shrub Cover =")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tpi2000\\)", replacement = "Topographic Position Index 2000")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "fsplanted", replacement = "Planting")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "facts.released", replacement = "Released")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tmean\\)", replacement = "Mean Annual Temp.")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(tmax\\)", replacement = "Max. Annual Temp.")) %>% 
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(rad_winter\\)", replacement = "Winter Solar Insolation")) %>%
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(normal_annual_precip\\)", replacement = "Normal Annual Precipitation")) %>%  
  mutate(V1 = str_replace_all(V1, pattern = "scale\\(twi\\)", replacement = "Total Water Index"))

#write.csv(mod.selc.record2.shr, "output/shr.selc.record2.csv")
#save(mod.selc.est.list2, file = "output/mod.selc.est.list_backup2.RData")

r.squaredGLMM(shr)

#save(pltd, shr, file = "output/modelDataForPlots.RData")


cor(plots %>% dplyr::select(elev, rad_winter, slope_dem, normal_annual_precip, twi, tpi100, tpi500, tpi2000, tpi5000, tmax, tmin, tmean),  use = "complete.obs", method = "pearson")
cor(plots %>% dplyr::select(tmax_ndj, tmin_ndj, tmean_ndj, tmax_fma, tmin_fma, tmean_fma, tmax_mjj, 
                            tmin_mjj, tmean_mjj, tmax_aso, tmin_aso, tmean_aso, tmax, tmin, tmean),  use = "complete.obs", method = "pearson")



pltd.nb <- glmer.nb(round(dens.planted/24.94098, 0 ) ~ scale(tpi2000)*scale(elev) + 
                      scale(Shrubs)*facts.planting.first.year*fsplanted +
                      #scale(ShrubHolisticVolume^(2/3))*facts.planting.first.year*fsplanted +
                      scale(tmin)*scale(normal_annual_precip) +
                      scale(log10SeedWallConifer) +
                      #scale(LitDuff) +
                      (1|Fire) +
                      #l(0+scale(normal_annual_precip)|Fire) + 
                      #(0+scale(tmin_mjjas)|Fire) + 
                      #(0+scale(tmin):scale(normal_annual_precip)|Fire) + 
                      #(0+scale(tpi2000)|Fire) +
                      (1|Fire:PairID), data = plot_dhm)
summary(pltd.nb)
plot(allEffects(pltd.nb))

pltd.po <- glmer(round(dens.planted/24.94098, 0 ) ~ scale(tpi2000)*scale(elev) + 
                   scale(Shrubs)*facts.planting.first.year*fsplanted +
                   scale(slope_dem)*scale((1-cos((pi/180)*(aspect_dem)))/2) + 
                   #scale(ShrubHolisticVolume^(2/3))*facts.planting.first.year*fsplanted +
                   scale(tmin)*scale(normal_annual_precip) +
                   scale(log10SeedWallConifer) +
                   scale(LitDuff) +
                   (1|Fire) +
                   #l(0+scale(normal_annual_precip)|Fire) + 
                   #(0+scale(tmin)|Fire) + 
                   #(0+scale(tmin):scale(normal_annual_precip)|Fire) + 
                   #(0+scale(tpi2000)|Fire) +
                   (1|Fire:PairID) + (1|obs), family = poisson, data = plot_dhm %>% mutate(obs = row_number()))
AIC(pltd.po)
summary(pltd.po)

plot(allEffects(pltd.po))


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
                log10SeedWallConifer + scale(ShrubHt) + (1|Fire) + (1|Fire:PairID), data = plot_dhm)

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
               log10SeedWallConifer + scale(ShrubHt) +
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

shr1 <- lmer(ln.dens.planted ~ scale(Shrubs)*facts.planting.first.year*fsplanted + log10SeedWallConifer + scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
step(shr1)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))
tab_model(shr)

shr1 <- lmer(ln.dens.conif ~ 
               #facts.planting.first.year*scale(Shrubs)*fsplanted + log10SeedWallConifer + scale(ShrubHt) + 
               scale(tpi2000)*facts.planting.first.year +
               scale(Shrubs)*scale(tmean) +
               scale(tmean)*scale(normal_annual_precip) +
               log10SeedWallConifer + 
               #scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
step(shr1)
AIC(shr1)
summary(shr1)
plot(allEffects(shr1))


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

##### SEM --------------------------------------------------------------------------------------




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
         log10SeedWallConifer + zShrubHt +
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
               log10SeedWallConifer + scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(shr1)

# NOPE, NOT QUADRATIC TERM FOR SHRUBS ^^^^^^


#####I copies the model selection from the list before and deleted LittDuff from it. It actually worked.

pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))); mod.selc.est.list2 <- summary(pltd)$coefficients;pltd <- lmer(ln.dens.planted ~ scale(tmin) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmax) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + fsplanted * facts.planting.first.year + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) * fsplanted + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) * fsplanted + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) * fsplanted + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) * fsplanted + scale(tpi2000) + scale(twi) + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) * fsplanted + scale(twi) + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) * fsplanted + scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) * fsplanted + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + facts.planting.first.year * fsplanted + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + facts.planting.first.year * fsplanted + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) * facts.planting.first.year * fsplanted + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) * fsplanted + scale(Grasses) + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) * facts.planting.first.year * fsplanted + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) * fsplanted + scale(Shrubs) + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) + scale(ShrubHt) * facts.planting.first.year * fsplanted + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) + scale(ShrubHt) * fsplanted + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(elev) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(rad_summer) + scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(tmean) + scale(normal_annual_precip) + scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(normal_annual_precip) + scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(Forbs) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(LiveOverstory) + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) + scale(twi) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(twi) + scale(CWD_sound) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(twi) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) * scale(rad_summer) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(normal_annual_precip) * scale(rad_summer) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmean) * scale(normal_annual_precip) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmax) * scale(normal_annual_precip) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) * scale(rad_summer) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(twi) * scale(normal_annual_precip) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(twi) * scale(rad_summer) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(twi) * scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) * scale(normal_annual_precip) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Grasses) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) * scale(tmin) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(ShrubHt) + scale(log10SeedWallConifer) + scale(tpi2000) * scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(log10SeedWallConifer) + scale(tpi2000) * scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) * fsplanted + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(log10SeedWallConifer) + scale(tpi2000) * scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(log10SeedWallConifer) + scale(tpi2000) * scale(elev) * fsplanted + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients);
pltd <- lmer(ln.dens.planted ~ scale(tmin) * scale(normal_annual_precip) + scale(Shrubs) * facts.planting.first.year * fsplanted + scale(log10SeedWallConifer) + scale(tpi2000) * scale(elev) + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm); mod.selc.record2 <- rbind(as.data.frame(cbind(as.character(pltd@call)[2], AIC(pltd))), mod.selc.record2); mod.selc.est.list2 <- list(mod.selc.est.list2, summary(pltd)$coefficients)
