setwd("C:/Users/DYoung/Documents/Research Projects/Post-fire management/postfire-management")

library(raster)
library(tidyverse)
library(sf)

dem = raster("data/non-synced/existing-datasets/DEM/CAmerged14_albers.tif")

zones = st_read("existing_regen/zones_for_twi.gpkg")

for(i in 1:nrow(zones)) {
  
  
  zone = zones[i,]
  name = zone$name
  dem_crop = crop(dem,zone)
  dem_crop = mask(dem_crop,zone)
  writeRaster(dem_crop,paste0("data/non-synced/dem_for_twi/",name,"_mask.tif"),overwrite=TRUE)
  
  
}