setwd("~/repos/postfire-management")

library(tidyverse)
library(raster)
library(sf)

focal_region = st_read("management-tool-prep/data/focal-region/focal-region.geojson")

tpi = raster("management-tool-prep/data/non-synced/intermediate/tpi2000.tif")
ppt = raster("data/non-synced/existing-datasets/precipitation/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
twi = raster("management-tool-prep/data/non-synced/twi_merged.tif")
rad_winter = raster("management-tool-prep/data/non-synced/rad/rad_winter.tif")
elev = raster("data/non-synced/existing-datasets/DEM/CAmerged15.tif")


## extract TopoWX normal temperature
tmax = brick("data/non-synced/existing-datasets/topowx_temerature/tmax_ds_4km_flint_noxy.tif") %>% mean()
tmin = brick("data/non-synced/existing-datasets/topowx_temerature/tmin_ds_4km_flint_noxy.tif") %>% mean()
tmean = mean(tmax,tmin)



### Sync ppt
ppt_focal = crop(ppt,focal_region)
ppt_resample = projectRaster(ppt_focal,tpi,method="bilinear")
writeRaster(ppt_resample,"management-tool-prep/data/non-synced/intermediate/ppt.tif",overwrite=TRUE)

### Sync elev
elev_focal = crop(elev,focal_region)
elev_resample = projectRaster(elev_focal,tpi,method="bilinear")
writeRaster(elev_resample,"management-tool-prep/data/non-synced/intermediate/elev.tif",overwrite=TRUE)



### Sync tmean
tmean_focal = crop(tmean,focal_region)
tmean_resample = projectRaster(tmean_focal,tpi,method="bilinear")
writeRaster(tmean_resample,"management-tool-prep/data/non-synced/intermediate/tmean.tif",overwrite=TRUE)


# ### Sync tmin
# tmin_focal = crop(tmin,focal_region)
# tmin_resample = projectRaster(tmin_focal,tpi,method="bilinear")
# writeRaster(tmin_resample,"management-tool-prep/data/non-synced/intermediate/tmin.tif",overwrite=TRUE)


### Sync twi
twi_focal = crop(twi,focal_region %>% st_transform(projection(twi)))
twi_resample = projectRaster(twi_focal,tpi,method="bilinear")
writeRaster(twi_resample,"management-tool-prep/data/non-synced/intermediate/twi.tif",overwrite=TRUE)



### Sync rad_winter
rad_focal = crop(rad_winter,focal_region %>% st_transform(projection(twi)))
rad_resample = projectRaster(rad_focal,tpi,method="bilinear")
writeRaster(rad_resample,"management-tool-prep/data/non-synced/intermediate/rad.tif",overwrite=TRUE)


