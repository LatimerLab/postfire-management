setwd("~/repos/postfire-management")

library(tidyverse)
library(lme4)
library(sf)
library(raster)


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
  mutate(totalCovxHt = (Shrubs*ShrubHt + Grasses*GrassHt + Forbs*ForbHt)) %>%
  mutate(LitDuff = LitterDepth + DuffDepth) %>%
  mutate(ShrubHt2 = ifelse(ShrubHt == 0, ShrubErectHt, ShrubHt))

pltd <- lmer(ln.dens.planted ~ scale(tpi2000)*facts.planting.first.year + 
               asin(sqrt(Shrubs/100))*facts.planting.first.year*fsplanted +
               #scale(ShrubHolisticVolume^(2/3))*facts.planting.first.year*fsplanted +
               scale(tmin)*scale(normal_annual_precip) +
               scale(neglog5SeedWallConifer) +
               scale(LitDuff) +
               #scale(ShrubHt2) +
               #scale(ShrubHolisticVolume) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)



## Save it
saveRDS(pltd,"management-tool-prep/data/non-synced/for-tool/model.rds")
saveRDS(plot_dhm,"management-tool-prep/data/non-synced/for-tool/data.rds")

#### Load seedling predictor data ####

region = st_read("management-tool-prep/data/focal-region/focal-region.geojson")

tpi = raster("management-tool-prep/data/non-synced/intermediate/tpi2000.tif")
ppt = raster("management-tool-prep/data/non-synced/intermediate/ppt.tif")
#tmean = raster("management-tool-prep/data/non-synced/intermediate/tmean.tif")
tmin = raster("management-tool-prep/data/non-synced/intermediate/tmin.tif")
#twi = raster("management-tool-prep/data/non-synced/intermediate/twi.tif")
#rad = raster("management-tool-prep/data/non-synced/intermediate/rad.tif")
shrub = raster("management-tool-prep/data/non-synced/intermediate/shrub.tif")



#### Stack and save seedl env predictor rasters ####

env = stack(tpi*10,ppt,tmin*100,shrub*100) # mult tmin by 100 so it can be saved as an int to save space
env = crop(env,region %>% st_transform(projection(env)))
env = mask(env,region %>% st_transform(projection(env)))

### TEMPORARY for development, make raster coarser so it's more wieldy
env = aggregate(env,fact=2)

writeRaster(env,"management-tool-prep/data/non-synced/for-tool/env_raster_stack.tif",overwrite=TRUE, datatype="INT2S", options="COMPRESS=LZW")   ##738


