setwd("~/projects/Post-fire management/postfire-management")

library(tidyverse)
library(lme4)
library(sf)
library(raster)
library(ncdf4)


region = st_read("management-tool-prep/data/focal-region/focal-region.geojson")

#### Fit and save the seedling density model ####

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

shr <- lmer(asin(sqrt(Shrubs/100)) ~ scale(normal_annual_precip)*scale(rad_winter) + 
              #I(scale(normal_annual_precip)^2) +
              #scale(rad_winter):I(scale(normal_annual_precip)^2) +
              #I(scale(rad_winter)^2) +
              #scale(normal_annual_precip)*I(scale(rad_winter)^2) +
              #I(scale(rad_winter)^2)*I(scale(normal_annual_precip)^2) +
              #scale(tmean) + 
              #poly(scale(elev), 2) + #does not improve
              scale(tmin) +
              #I(scale(tmin)^2) + Does not improve modl
              #scale(tmax) +
              #scale(slope_dem) + 
              #I(scale(slope_dem)^2) + 
              scale(twi) + 
              I(scale(twi)^2) +
              #scale(tpi500) + 
              (1|Fire) + (1|Fire:PairID), data = plot_dhm)


### Load env raster stack (for precip)

env = brick("management-tool-prep/data/non-synced/for-tool/env_raster_stack.tif")


## Load and sync rasters


### load rad winter
rad_winter = raster("management-tool-prep/data/non-synced/rad_winter.tif")
rad_winter = crop(rad_winter,region %>% st_transform(crs(rad_winter)))
rad_winter = mask(rad_winter,region %>% st_transform(crs(rad_winter)))
rad_winter_resample = projectRaster(rad_winter,env,method="bilinear")
gc()

### load tmin
tmin = stack("data/non-synced/existing-datasets/topowx_temerature/tmin_normal/normals_tmin.nc")
tmin = mean(tmin)
tmin = tmin * 100
writeRaster(tmin,"data/non-synced/existing-datasets/topowx_temerature/tmin_normal/normals_tmin_x100.nc",datatype="INT2S")
tmin2 = raster("data/non-synced/existing-datasets/topowx_temerature/tmin_normal/normals_tmin_x100.nc")
tmin2 = crop(tmin2,region %>% st_transform(crs(tmin2)))
tmin2 = mask(tmin2,region %>% st_transform(crs(tmin2)))
tmin2_resample = projectRaster(tmin2,env,method="bilinear")
gc()

### load twi
twi = raster("management-tool-prep/data/non-synced/twi_merged.tif")
twi = twi * 100
twi = crop(twi,region %>% st_transform(crs(twi)))
twi = mask(twi,region %>% st_transform(crs(twi)))
twi_resample = projectRaster(twi,env,method="bilinear")
gc()

### Write all
rad_tmin_twi = brick(rad_winter_resample, tmin2_resample,twi_resample)
writeRaster(rad_tmin_twi,"management-tool-prep/data/non-synced/rad_tmin100_twi100.tif",datatype="INT2S",overwrite=TRUE)
gc()


#### Makde shrub model predictions
rad_tmin_twi = brick("management-tool-prep/data/non-synced/rad_tmin100_twi100.tif")

env2 = stack(env,rad_tmin_twi)

env_df = as.data.frame(env2,xy=TRUE)
names(env_df) = c("x","y","tpi","ppt","tmean","rad_winter","tmin","twi")
env_df = env_df %>%
  rename(normal_annual_precip = ppt,
         tpi2000 = "tpi") %>%
  mutate(tmean = tmean/100,
         tpi2000 = tpi2000,  ###!!! need to add /10 once re-generate the input rasters
         tmin = tmin/100,
         twi = twi/100)    


#### Add shrub model predictions to raster stack
pred = predict(shr,env_df,re.form=NA)
pred = ((pred %>% sin)^2)*100  # undo arcsin sqrt

env_df$pred = pred

pred_raster = rasterFromXYZ(env_df %>% dplyr::select(x,y,pred))

plot(pred_raster)





