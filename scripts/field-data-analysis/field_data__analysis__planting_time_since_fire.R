library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)
library(merTools)

load("output/plotSeedlingData.RData") #load R object: plot_dhm_long
plot_dhm <- plot_dhm %>% 
  mutate(ln.dens.planted = log(dens.planted+24.99)) %>%
  filter(Type != "internal") %>% 
  mutate(ln.dens.conif = log(dens.conif+24.99)) %>%
  mutate(fsplanted = as.factor(fsplanted)) %>%
  mutate(facts.released = as.factor(facts.released)) %>%
  mutate(GrassHt = ifelse(is.na(GrassHt), 0, GrassHt)) %>%
  mutate(SeedWallConifer = ifelse(is.na(SeedWallConifer), 500, SeedWallConifer)) %>%
  mutate(neglog5SeedWallConifer = -logb(SeedWallConifer, base = exp(5))) %>%
  mutate(ShrubHt = ifelse(is.na(ShrubHt), 0, ShrubHt)) %>%
  mutate(ForbHt = ifelse(is.na(ForbHt), 0, ForbHt)) %>%
  mutate(totalCov = Shrubs + Grasses + Forbs) %>%
  mutate(totalCovxHt = (Shrubs*ShrubHt + Grasses*GrassHt + Forbs*ForbHt))

##### Model for Derek ----------------------------------------------------------------------------------

mod <- lmer(ln.dens.planted ~ scale(Shrubs)*facts.planting.first.year*fsplanted+scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)

AIC(mod)
summary(mod)
plot(allEffects(mod))


### Get predictions + redisuals for observed data points--to plot data on top of predictions

obs = predict(mod, re.form=NA)
obs_resid = (obs + resid(mod,re.form=NA)) %>% exp() 
plot_dhm$obs_resid = obs_resid


### Make hypothetical dataframe for prediction

Shrubs_mean = mean(plot_dhm$Shrubs)
ShrubHt_mean = mean(plot_dhm$ShrubHt)
neglog5SeedWallConifer_mean = -0.7 # mean(plot_dhm$neglog5SeedWallConifer)

Shrubs_levels = seq(from=0,to=100,length.out=100)
facts.planting.first.year_levels = c(1,2,3)
fsplanted_levels = c("planted","unplanted")

newdat = expand.grid(Shrubs = Shrubs_levels,
                     facts.planting.first.year = facts.planting.first.year_levels,
                     fsplanted = fsplanted_levels,
                     ShrubHt = ShrubHt_mean,
                     neglog5SeedWallConifer = neglog5SeedWallConifer_mean,
                     Fire = "hypothetical",
                     PairID = "hypothetical")


pred = predict(mod,newdata = newdat, re.form=NA) %>% exp()

pred_int = predictInterval(mod,newdata=newdat,which="fixed",level=0.95,n.sims=10000,include.resid.var=FALSE) %>% exp()

dat_pred = newdat
dat_pred = cbind(dat_pred,pred_int)


dat_pred$pred = pred


## Make nice names for plotting
dat_pred = dat_pred %>%
  mutate(facts.planting.first.year = recode(facts.planting.first.year,
                                            "1" = "Planted 1 year post-fire",
                                            "2" = "Planted 2 years post-fire",
                                            "3" = "Planted 3 years post-fire"),
         fsplanted = recode(fsplanted,
                            planted = "Planted",
                            unplanted = "Unplanted"))

plot_dhm = plot_dhm %>%
  mutate(facts.planting.first.year = recode(facts.planting.first.year,
                                            "1" = "Planted 1 year post-fire",
                                            "2" = "Planted 2 years post-fire",
                                            "3" = "Planted 3 years post-fire"),
         fsplanted = recode(fsplanted,
                            planted = "Planted",
                            unplanted = "Unplanted"))


p = ggplot(dat_pred, aes(x=Shrubs,y=pred,color=fsplanted)) +
  facet_wrap(~facts.planting.first.year) +
  geom_ribbon(aes(ymin=lwr,ymax=upr,fill=fsplanted),color=NA,alpha=0.1) +
  geom_line(size=0.8) +
  geom_point(data=plot_dhm,aes(y=obs_resid),size=0.5) +
  coord_cartesian(ylim = c(0, 500), xlim =c(0,100)) +
  theme_bw(12) +
  labs(y="Seedlings per hectare",x="Shrub cover (%)") +
  theme(legend.title = element_blank(), panel.grid.minor = element_blank(), panel.grid.major=element_line(size=0.3)) +
  scale_color_manual(values = c("darkcyan","darkorange")) +
  scale_fill_manual(values = c("darkcyan","darkorange"))
  
Cairo("figures/shrub_year_planted_w_data.png",width=1600*2,height=600*2,res=200*2)
p
dev.off()

  
  
  
  
  
  
#####################################










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



