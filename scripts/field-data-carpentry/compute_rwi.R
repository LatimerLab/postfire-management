library(raster)
library(dynatopmodel)
library(sf)


dem = raster("C:/Users/DYoung/Documents/GIS/CA abiotic layers/DEM/CA/new ncal/CAmerged14_albers.tif")

focal_area = st_read("C:/Users/DYoung/Documents/Research Projects/Post-fire management/postfire-management/data/existing-datasets/focal_area_twi_test.geojson") %>% st_transform(3310)











dem_focal = crop(dem,focal_area)

# need to resample to a raster with same x and y resolution
target_raster = raster(crs = crs(dem_focal), ext = extent(dem_focal), resolution = c(30,30))
dem_resamp = resample(dem_focal,target_raster)






a = upslope.area(dem_resamp,atb=TRUE)
plot(a$atb)
