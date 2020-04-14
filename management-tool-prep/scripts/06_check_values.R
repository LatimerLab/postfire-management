## Check that input env. variables and shrub cover predictions are correct at surveyed plots.

setwd("~/repos/postfire-management")

library(sf)
library(tidyverse)
library(raster)
library(lme4)


#### Load env raster ####

region = st_read("management-tool-app/data/power_fire_perimeter.geojson")

env = brick("management-tool-prep/data/non-synced/for-tool/env_raster_stack.tif")
env = crop(env,region %>% st_transform(projection(env)))
env = mask(env,region %>% st_transform(projection(env)))

names(env) = c("tpi","ppt","tmin","shrub")
env$tpi = env$tpi/10
env$tmin = env$tmin/100


#### Load shrub env raster ####

shr_env = brick("management-tool-prep/data/non-synced/intermediate/shrub_env_pred_checking.grd")
shr_env = crop(shr_env,region %>% st_transform(projection(shr_env)))
shr_env = mask(shr_env,region %>% st_transform(projection(shr_env)))



#### Load plot data ####

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
  mutate(LitDuff = LitterDepth + DuffDepth) %>%
  mutate(ShrubHt2 = ifelse(ShrubHt == 0, ShrubErectHt, ShrubHt))


### Add model-based shrub predictions to plot data

shr <- lmer(asin(sqrt(Shrubs/100)) ~ scale(normal_annual_precip)*scale(rad_winter) + 
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
              #I(scale(twi)^2) + 
              #scale(tpi500) +
              (1|Fire) + (1|Fire:PairID), data = plot_dhm)

shrub = predict(shr,newdata=plot_dhm, re.form=NA)

shrub_pred = ((shrub %>% sin())^2)*100

plot_dhm$quinn_shrub_pred = shrub_pred

#### Select only relevant vars

plot_dhm = plot_dhm %>%
  dplyr::select(X,Y,tmin,tpi2000,LitDuff,tmean,twi,normal_annual_precip,rad_winter,quinn_shrub_pred)







## make plot data spatial

plot_sp = st_as_sf(plot_dhm,coords = c("X","Y"), crs = 3310)

#### extract and compare

## seedling predictors

plot_sp$tool_ppt = extract(env$ppt,plot_sp, method = "bilinear")
plot_sp$tool_tpi = extract(env$tpi,plot_sp, method = "bilinear")
plot_sp$tool_tmin = extract(env$tmin,plot_sp, method = "bilinear")
plot_sp$tool_shrub = extract(env$shrub,plot_sp, method = "bilinear")

## shrub predictors
plot_sp$shrub_ppt = extract(shr_env$ppt,plot_sp, method = "bilinear")
plot_sp$shrub_rad = extract(shr_env$rad,plot_sp, method = "bilinear")
plot_sp$shrub_tmean = extract(shr_env$tmean,plot_sp, method = "bilinear")
plot_sp$shrub_twi = extract(shr_env$twi,plot_sp, method = "bilinear")





### Get model-based shrub predictions












env_df = as.data.frame(env,xy=TRUE)
names(env_df) = c("x","y","tpi","ppt","tmin","shrub")
env_df = env_df %>%
  rename(normal_annual_precip = ppt,
         tpi2000 = "tpi") %>%
  mutate(tmin = tmin/100,
         tpi2000 = tpi2000/10)    ###!!! need to add /10 once re-generate the input rasters


