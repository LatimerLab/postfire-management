setwd("~/projects/Post-fire management/postfire-management")

dem = raster("data/non-synced/existing-datasets/DEM/CAmerged14_albers.tif")

target_raster = raster(crs = crs(dem), ext = extent(dem), resolution = 30)

dem_resamp = raster::resample(dem,target_raster)

dem_resamp[dem_resamp == 0] = NA


twi = upslope.area(dem_resamp,atb=TRUE)