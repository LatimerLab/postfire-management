setwd("~/repos/postfire-management")

library(tidyverse)
library(raster)
library(sf)
library(lme4)

#### Load predictor data ####
ppt = raster("management-tool-prep/data/non-synced/intermediate/ppt.tif")
tmean = raster("management-tool-prep/data/non-synced/intermediate/tmean.tif")
twi = raster("management-tool-prep/data/non-synced/intermediate/twi.tif")
rad = raster("management-tool-prep/data/non-synced/intermediate/rad.tif")

region = st_read("management-tool-prep/data/focal-region/focal-region.geojson")



#### Fit shrub cover model ####

load("output/plotSeedlingData.RData") #load R object: plot_dhm_long
# plot_dhm <- plot_dhm %>% 
#   mutate(ln.dens.planted = log(dens.planted+24.99)) %>%
#   filter(Type != "internal") %>% 
#   mutate(ln.dens.conif = log(dens.conif+24.99)) %>%
#   mutate(fsplanted = as.factor(fsplanted)) %>%
#   mutate(facts.released = as.factor(facts.released)) %>%
#   mutate(GrassHt = ifelse(is.na(GrassHt), 0, GrassHt)) %>%
#   mutate(ShrubHt = ifelse(is.na(ShrubHt), 0, ShrubHt)) %>%
#   mutate(ForbHt = ifelse(is.na(ForbHt), 0, ForbHt)) %>%
#   mutate(SeedWallConifer = ifelse(is.na(SeedWallConifer), 500, SeedWallConifer)) %>%
#   mutate(neglog5SeedWallConifer = -logb(SeedWallConifer, base = exp(5))) %>%
#   mutate(totalCov = Shrubs + Grasses + Forbs) %>%
#   mutate(totalCovxHt = (Shrubs*ShrubHt + Grasses*GrassHt + Forbs*ForbHt)) %>%
#   mutate(LitDuff = LitterDepth + DuffDepth) %>%
#   mutate(ShrubHt2 = ifelse(ShrubHt == 0, ShrubErectHt, ShrubHt))

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


#### Combine input vars into predictor brick ####

shr_pred_env = brick(ppt,rad,tmean,twi)
shr_pred_env = crop(shr_pred_env,region %>% st_transform(projection(shr_pred_env)))
shr_pred_env = mask(shr_pred_env,region %>% st_transform(projection(ppt)))

## write shrub pred raster for checking/debugging
writeRaster(shr_pred_env,"management-tool-prep/data/non-synced/intermediate/shrub_env_pred_checking.grd", overwrite=TRUE)

## Convert predictor brick to DF
shr_env_df = as.data.frame(shr_pred_env,xy=TRUE)
gc()

shr_env_df = shr_env_df %>%
  rename(normal_annual_precip = ppt,
         rad_winter = rad) 

## Make shrub predictions
shr_pred = predict(shr,shr_env_df,re.form=NA)
gc()

shr_env_df = shr_env_df[,c("x","y")]
shr_env_df$shr_pred = shr_pred

saveRDS(shr_env_df,"management-tool-prep/data/non-synced/intermediate/shrub_pred_df.rds")
gc()

