setwd("~/repos/postfire-management")

library(tidyverse)
library(lme4)
library(sf)
library(raster)


#### Fit and save the seedling density model ####

load("output/plotSeedlingData.RData") #load R object: plot_dhm


pltd <- lmer(ln.dens.planted ~ scale(tpi2000)*scale(elev) +
               scale(Shrubs)*facts.planting.first.year*fsplanted + 
               scale(tmean)*scale(normal_annual_precip) +
               scale(log10SeedWallConifer) +
               #scale(LitDuff) +
               (1|Fire) +
               (1|Fire:PairID)
             , REML = T,
             data = plot_dhm) # removed [-c(levId),]

ggpairs(plot_dhm %>% dplyr::select(elev, tmean, normal_annual_precip))

## Save it
saveRDS(pltd,"management-tool-prep/data/non-synced/for-tool/model.rds")
saveRDS(plot_dhm,"management-tool-prep/data/non-synced/for-tool/data.rds")

#### Load seedling predictor data ####

region = st_read("management-tool-prep/data/focal-region/focal-region.geojson")
tpi = raster("management-tool-prep/data/non-synced/intermediate/tpi2000.tif")
ppt = raster("management-tool-prep/data/non-synced/intermediate/ppt.tif")
tmean = raster("management-tool-prep/data/non-synced/intermediate/tmean.tif")
elev = raster("management-tool-prep/data/non-synced/intermediate/elev.tif")
#tmin = raster("management-tool-prep/data/non-synced/intermediate/tmin.tif")
#twi = raster("management-tool-prep/data/non-synced/intermediate/twi.tif")
#rad = raster("management-tool-prep/data/non-synced/intermediate/rad.tif")
shrub = raster("management-tool-prep/data/non-synced/intermediate/shrub.tif")

eveg = raster("management-tool-prep/data/non-synced/intermediate/eveg_focal.tif")

eveg[is.na(eveg)] = 0


#### Stack and save seedl env predictor rasters ####

env = stack(tpi*10,ppt,tmean*100,shrub*100,elev,eveg) # mult tmin by 100 so it can be saved as an int to save space
env = crop(env,region %>% st_transform(projection(env)))
env = mask(env,region %>% st_transform(projection(env)))


writeRaster(env,"management-tool-prep/data/non-synced/for-tool/env_raster_stack.tif",overwrite=TRUE, datatype="INT2S", options="COMPRESS=LZW")   ##738

# Write an alternative smaller (coarser) raster for faster computation
env_coarse = aggregate(env,fact=2,fun=mean)
writeRaster(env_coarse,"management-tool-prep/data/non-synced/for-tool/env_raster_stack_coarse.tif",overwrite=TRUE, datatype="INT2S", options="COMPRESS=LZW")   ##738


#### Save a table of predictor limits, to determine extrapolation ####

limits = plot_dhm %>%
  dplyr::select(tpi = tpi2000,
         ppt = normal_annual_precip,
         tmean = tmean,
         elev = elev) %>%
  summarize_all(list(min = min, max = max))

write.csv(limits,"management-tool-prep/data/non-synced/for-tool/var_lims.csv",row.names=FALSE)
