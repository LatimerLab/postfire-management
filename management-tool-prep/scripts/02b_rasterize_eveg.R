library(tidyverse)
library(raster)
library(sf)
library(fasterize)

focal_region = st_read("management-tool-prep/data/focal-region/focal-region.geojson")

raster_template = raster("management-tool-prep/data/non-synced/intermediate/ppt.tif")


eveg_s = st_read("management-tool-prep/data/non-synced/eveg/S_USA.EVMid_R05_SouthSierra.gdb")
eveg_n = st_read("management-tool-prep/data/non-synced/eveg/S_USA.EVMid_R05_NorthSierra.gdb")

## thin each to just the focal types
whr_focal = c("DFR","EPN","JPN","MHC","PPN","SMC","WFR")

eveg_s = eveg_s %>%
  filter(CWHR_TYPE %in% whr_focal | (COVERTYPE == "CON" & (!(COVERTYPE %in% c("RFR","LPN","SCN")))))
eveg_n = eveg_n %>%
  filter(CWHR_TYPE %in% whr_focal | (COVERTYPE == "CON" & (!(CWHR_TYPE %in% c("RFR","LPN","SCN")))))

eveg = rbind(eveg_s,eveg_n)

# only one category (focal)
eveg$type_cat = 1

eveg_proj = st_transform(eveg,st_crs(raster_template))

eveg_rast = fasterize(eveg_proj,raster_template,field="type_cat",fun="max")
writeRaster(eveg_rast,"management-tool-prep/data/non-synced/intermediate/eveg_focal.tif",overwrite=TRUE)




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


