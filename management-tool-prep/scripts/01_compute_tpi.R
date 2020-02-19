setwd("~/projects/Post-fire management/postfire-management")

library(raster)
library(sf)
library(spatialEco)
library(furrr)


dem = raster("C:/Users/DYoung/Documents/GIS/CA abiotic layers/DEM/CA/new ncal/CAmerged14_albers.tif")

focal_area = st_read("C:/Users/DYoung/Documents/projects/Post-fire management/postfire-management/management-tool-prep/data/focal-region/focal-region.geojson") %>% st_transform(3310) %>% st_buffer(1)

dem_focal = raster::crop(dem,focal_area)
dem_focal = mask(dem_focal,focal_area)

tpi_focal = tpi(dem_focal,scale = round(2000/30))

writeRaster(tpi_focal,"management-tool-prep/data/non-synced/intermediate/tpi2000.tif")
