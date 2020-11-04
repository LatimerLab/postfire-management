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
  filter(CWHR_TYPE %in% whr_focal | (COVERTYPE == "CON" & (!(CWHR_TYPE %in% c("RFR","LPN","SCN")))))
eveg_n = eveg_n %>%
  filter(CWHR_TYPE %in% whr_focal | (COVERTYPE == "CON" & (!(CWHR_TYPE %in% c("RFR","LPN","SCN")))))

eveg = rbind(eveg_s,eveg_n)

# only one category (focal)
eveg$type_cat = 1

eveg_proj = st_transform(eveg,st_crs(raster_template))

eveg_rast = fasterize(eveg_proj,raster_template,field="type_cat",fun="max")
writeRaster(eveg_rast,"management-tool-prep/data/non-synced/intermediate/eveg_focal.tif",overwrite=TRUE)



