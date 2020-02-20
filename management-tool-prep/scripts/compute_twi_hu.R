setwd("~/projects/Post-fire management/postfire-management")

library(dplyr)
library(sf)
library(raster)
library(dynatopmodel)
library(furrr)

focal_region = st_read("management-tool-prep/data/focal-region/focal-region.geojson") %>% st_transform(3310)

dem = raster("data/non-synced/existing-datasets/DEM/CAmerged14_albers.tif")
dem = crop(dem,focal_region)
dem = mask(dem,focal_region)

basins = st_read("management-tool-prep/data/non-synced/hydrology/WBDHU8.shp") %>% st_transform(3310)

basins_focal = st_intersection(basins,focal_region %>% st_union())

## biggest ones first, so free cores later are tasked with small jobs, also so progress estimation is pessimistic
basins_focal$area = st_area(basins_focal)
basins_focal = basins_focal %>%
  arrange(desc(area))


twi_basin = function(basin_number) {
  
  write("done",   paste0("management-tool-prep/data/non-synced/",basin_number,"_started.txt"))
  
  basin_focal = basins_focal[basin_number,]
  
  buffer_amount = ifelse(basin_number %in% c(33,28), 4000, 3000)
  
  dem_basin = crop(dem, st_buffer(basin_focal,buffer_amount) )
  dem_basin = mask(dem_basin, st_buffer(basin_focal,buffer_amount) )
  
  target_raster = raster(crs = crs(dem_basin), ext = extent(dem_basin), resolution = 30)
  
  dem_resamp = raster::resample(dem_basin,target_raster)
  
  dem_resamp[dem_resamp == 0] = NA
  
  twi_pre = upslope.area(dem_resamp,atb=TRUE)
  twi = twi_pre$atb
  
  twi = crop(twi, st_buffer(basin_focal,100) )
  twi = mask(twi, st_buffer(basin_focal,100) )
  
  write("done",   paste0("management-tool-prep/data/non-synced/",basin_number,"_finished.txt"))
  
  return(twi)
}

basin_numbers = 1:nrow(basins_focal)

plan(multiprocess(workers=3))

twis = future_map(basin_numbers,twi_basin,.progress = TRUE)
