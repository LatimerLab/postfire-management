library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)

load("output/plotSeedlingData.RData") #load R object: plot_dhm_long
plot_dhm <- plot_dhm %>% mutate(ln.dens.planted = log(dens.planted+1)) %>%
  mutate(fsplanted = as.factor(fsplanted))

m1 <- lmer(ln.dens.planted ~ scale(slope_dem) + scale(elev) + 
             fsplanted*scale(normal_annual_precip) + (fsplanted*scale(normal_annual_precip)|Fire) + (1|Fire:PairID), data = plot_dhm)
AIC(m1)
summary(m1)
plot(m1)
plot(allEffects(m1))

m2 <- lmer(log(dens.planted+1) ~ scale(elev)*scale(slope_dem) + fsplanted + scale(normal_annual_precip) + (scale(elev)*scale(slope_dem)|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(m2)
plot(m2)
plot(allEffects(m2))

m2 <- lmer(log(dens.planted+1) ~ scale(elev)+scale(slope_dem) + fsplanted + scale(normal_annual_precip) + (scale(normal_annual_precip)|Fire) + (1|Fire:PairID), data = plot_dhm)
summary(m2)
AIC(m2)
plot(m1)
plot(allEffects(m1))

m3 <- glmer(