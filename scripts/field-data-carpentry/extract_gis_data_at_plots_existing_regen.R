setwd("/home/derek/repos/postfire-management")



library(tidyverse)
library(raster)
library(sf)
library(spatialEco)
library(dynatopmodel)


## load data
plots = read.csv("existing_regen/plots_compiled_filtered.csv",stringsAsFactors = FALSE)
#plots = read.csv("data/field-processed/compiled-processed/plots.csv",stringsAsFactors = FALSE)


### !! For plots in UTM 10N
## make plots spatial

plots = plots %>%
  mutate(Easting = as.numeric(Easting),
         Northing = as.numeric(Northing))

plots_sp <- st_as_sf(plots, coords = c("Easting","Northing"), crs = 26910) %>%
  st_transform(3310)

### For plots in geographic
## make plots spatial
# 
# plots = plots %>%
#   mutate(Long = as.numeric(Long)*-1,
#          Lat = as.numeric(Lat))
# 
# plots_sp <- st_as_sf(plots, coords = c("Long","Lat"), crs = 4326) %>%
#   st_transform(3310)



# get Albers coordinates
plots_sp = cbind(plots_sp,st_coordinates(plots_sp))

## extract PRISM normal precip
ppt = raster("data/non-synced/existing-datasets/precipitation/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
plots_sp$normal_annual_precip = raster::extract(ppt,plots_sp,method="bilinear")

## extract TopoWX normal temperature
tmax = raster("data/non-synced/existing-datasets/topowx_temerature/tmax_ds.tif")
tmin = raster("data/non-synced/existing-datasets/topowx_temerature/tmin_ds.tif")
tmean = (tmax + tmin)/2
plots_sp$tmax = raster::extract(tmax,plots_sp,method="bilinear")
plots_sp$tmin = raster::extract(tmin,plots_sp,method="bilinear")
plots_sp$tmean = raster::extract(tmean,plots_sp,method="bilinear")

## extract elevation
dem = raster("data/non-synced/existing-datasets/DEM/CAmerged14_albers.tif")
plots_sp$elev = raster::extract(dem,plots_sp,method="bilinear")

## get topographic indices
plots_vicinity = st_buffer(plots_sp,50000) %>% st_union()
dem_local = crop(dem,plots_vicinity %>% as("Spatial"))
dem_local = mask(dem_local,plots_vicinity %>% as("Spatial"))
dem_local = reclassify(dem_local,cbind(0,NA)) # set zero to nodata

template = raster(xmn = -350000,xmx = 350000, ymn = -480000, ymx = 480000, resolution = 30,  crs = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")
dem_reg = resample(dem_local,template)
gc()
dem_reg[dem_reg == 0] = NA
gc()

topo_data = data.frame()

## do extractions by fire
fires = unique(plots_sp$Fire)
for(fire in fires) {
  
  cat("Running for fire",fire,"\n")
  
  plots_fire = plots_sp %>%
    filter(Fire == fire)
  
  plots_fire_vicinity = st_buffer(plots_fire,20000) %>% st_union()
  dem_local_fire = crop(dem_local,plots_fire_vicinity %>% as("Spatial"))
  dem_local_fire = mask(dem_local_fire,plots_fire_vicinity %>% as("Spatial"))
  dem_reg_fire = crop(dem_reg,plots_fire_vicinity %>% as("Spatial"))
  dem_reg_fire = mask(dem_reg_fire,plots_fire_vicinity %>% as("Spatial"))
  
  # filename = paste0("../dem_reg_",fire,".tif")
  # writeRaster(dem_reg_fire,filename,overwrite=TRUE)
  # 
  
  tpi500 = tpi(dem_local_fire,scale = round(500/30))
  tpi100 = tpi(dem_local_fire,scale = round(100/30))
  tpi2000 = tpi(dem_local_fire,scale = round(2000/30))
  tpi5000 = tpi(dem_local_fire,scale = round(5000/30)) # this takes hours

  twi = upslope.area(dem_reg_fire,atb=TRUE)

  tpi500_d = extract(tpi500, plots_fire, method="bilinear")
  tpi100_d = extract(tpi100, plots_fire, method="bilinear")
  tpi2000_d = extract(tpi2000, plots_fire, method="bilinear")
  tpi5000_d = extract(tpi5000, plots_fire, method="bilinear")
  twi_d = extract(twi[["atb"]], plots_fire, method="bilinear")
  # 
  topo_data_fire = data.frame(Regen_Plot = plots_fire$Regen_Plot,tpi100 = tpi100_d, tpi500 = tpi500_d, tpi2000 = tpi2000_d, tpi5000 = tpi5000_d, twi = twi_d)
  topo_data = bind_rows(topo_data,topo_data_fire)

  
}

plots_sp = left_join(plots_sp,topo_data)


## compute and extract slope and aspect (takes ~ 15 minutes)
slope = terrain(dem_local,opt=c("slope"),unit="degrees")
aspect = terrain(dem_local,opt=c("aspect"),unit="degrees")

plots_sp$slope_dem = raster::extract(slope,plots_sp,method="bilinear")
plots_sp$aspect_dem = raster::extract(aspect,plots_sp,method="bilinear")

# ## extract solar rad
# ## This section is for planting plot data
# rad_winter = raster("data/non-synced/existing-datasets/solar radiation/rad_winter.tif")
# plots_sp$rad_winter = raster::extract(rad_winter,plots_sp,method="bilinear")
# rad_winter_spring = raster("data/non-synced/existing-datasets/solar radiation/rad_winter_spring.tif")
# plots_sp$rad_winter_spring = raster::extract(rad_winter_spring,plots_sp,method="bilinear")
# rad_spring = raster("data/non-synced/existing-datasets/solar radiation/rad_spring.tif")
# plots_sp$rad_spring = raster::extract(rad_spring,plots_sp,method="bilinear")
# rad_spring_summer = raster("data/non-synced/existing-datasets/solar radiation/rad_spring_summer.tif")
# plots_sp$rad_spring_summer = raster::extract(rad_spring_summer,plots_sp,method="bilinear")
# rad_summer = raster("data/non-synced/existing-datasets/solar radiation/rad_summer.tif")
# plots_sp$rad_summer = raster::extract(rad_summer,plots_sp,method="bilinear")

## This section is for existing regen plot data
rad_winter_socal = raster("data/non-synced/existing-datasets/solar radiation/rad_winter_socal.tif")
rad_winter_norcal = raster("data/non-synced/existing-datasets/solar radiation/rad_winter_norcal.tif")
rad_winter = merge(rad_winter_socal,rad_winter_norcal)

      rad_winter_spring_socal = raster("data/non-synced/existing-datasets/solar radiation/rad_winter_spring_socal.tif")
rad_winter_spring_norcal = raster("data/non-synced/existing-datasets/solar radiation/rad_winter_spring_norcal.tif")
rad_winter_spring = merge(rad_winter_spring_socal,rad_winter_spring_norcal)

rad_spring_socal = raster("data/non-synced/existing-datasets/solar radiation/rad_spring_socal.tif")
rad_spring_norcal = raster("data/non-synced/existing-datasets/solar radiation/rad_spring_norcal.tif")
rad_spring = merge(rad_spring_socal,rad_spring_norcal)

rad_spring_summer_socal = raster("data/non-synced/existing-datasets/solar radiation/rad_spring_summer_socal.tif")
rad_spring_summer_norcal = raster("data/non-synced/existing-datasets/solar radiation/rad_spring_summer_norcal.tif")
rad_spring_summer = merge(rad_spring_summer_socal,rad_spring_summer_norcal)

rad_summer_socal = raster("data/non-synced/existing-datasets/solar radiation/rad_summer_socal.tif")
rad_summer_norcal = raster("data/non-synced/existing-datasets/solar radiation/rad_summer_norcal.tif")
rad_summer = merge(rad_summer_socal,rad_summer_norcal)

plots_sp$rad_winter = raster::extract(rad_winter,plots_sp,method="bilinear")
plots_sp$rad_winter_spring = raster::extract(rad_winter_spring,plots_sp,method="bilinear")
plots_sp$rad_spring = raster::extract(rad_spring,plots_sp,method="bilinear")
plots_sp$rad_spring_summer = raster::extract(rad_spring_summer,plots_sp,method="bilinear")
plots_sp$rad_summer = raster::extract(rad_summer,plots_sp,method="bilinear")



# 
# ## extract twi
# twi = raster("data/non-synced/existing-datasets/twi/twi_merged.tif")
# plots_sp$twi = raster::extract(twi,plots_sp,method="bilinear")

#### Extract and summarize management history ####

## turn plots back to non-spatial for writing to CSV
plots = plots_sp %>% st_set_geometry(NULL)

## write data
write.csv(plots,"existing_regen/regen_plots_w_gis_data.csv",row.names = FALSE)
