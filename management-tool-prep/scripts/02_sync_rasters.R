setwd("~/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(raster)
library(sf)

focal_region = st_read("management-tool-prep/data/focal-region/focal-region.geojson")

tpi = raster("management-tool-prep/data/non-synced/intermediate/tpi2000.tif")

ppt = raster("data/non-synced/existing-datasets/precipitation/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")


## extract TopoWX normal temperature
tmax = raster("data/non-synced/existing-datasets/topowx_temerature/tmax_normal/normals_tmax.nc")
tmin = raster("data/non-synced/existing-datasets/topowx_temerature/tmin_normal/normals_tmin.nc")
tmean = mean(tmax,tmin)



### Sync ppt
ppt_focal = crop(ppt,focal_region)
ppt_resample = projectRaster(ppt_focal,tpi,method="bilinear")
writeRaster(ppt_resample,"management-tool-prep/data/non-synced/intermediate/ppt.tif",overwrite=TRUE)


### Sync tmean
tmean_focal = crop(tmean,focal_region)
tmean_resample = projectRaster(tmean_focal,tpi,method="bilinear")

writeRaster(tmean_resample,"management-tool-prep/data/non-synced/intermediate/tmean.tif",overwrite=TRUE)
