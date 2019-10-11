library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)

load("output/plotSeedlingData.RData") #load R object: plot_dhm_long

m1 <- lmer(log(dens.planted+1) ~ fsplanted*slope_dem + fsplanted*elev + fsplanted*rad_spring_summer + fsplanted*normal_annual_precip + (1|Fire/PairID), data = plot_dhm)
summary(m1)

m1 <- lmer(log(dens.conif+1) ~ scale(elev)*scale(slope_dem) + fsplanted + scale(normal_annual_precip) + (1|Fire/PairID), data = plot_dhm)
summary(m1)
plot(m1)
plot(allEffects(m1))

