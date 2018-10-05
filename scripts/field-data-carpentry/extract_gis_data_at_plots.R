setwd("C:/Users/DYoung/Documents/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(raster)

## load data
plots = read.csv("data/field-processed/compiled-processed/plots.csv",stringsAsFactors = FALSE)

## make plots spatial

plots = plots %>%
  mutate(Long = as.numeric(Long)*-1,
         Lat = as.numeric(Lat))

plots_sp <- st_as_sf(plots, coords = c("Long","Lat"), crs = 4326) %>%
  st_transform(3310)

# get Albers coordinates
plots_sp = cbind(plots_sp,st_coordinates(plots_sp))

## extract PRISM normal precip
ppt = raster("data/non-synced/existing-datasets/precipitation/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
plots_sp$normal_annual_precip = extract(ppt,plots_sp,method="bilinear")

## extract elevation
dem = raster("data/non-synced/existing-datasets/DEM/CAmerged12_albers2.tif")
plots_sp$elev = extract(dem,plots_sp,method="bilinear")

## extract solar rad
rad_march = raster("data/non-synced/existing-datasets/solar radiation/march_rad.tif")
plots_sp$rad_march = extract(rad_march,plots_sp,method="bilinear")

## turn plots back to non-spatial
plots = plots_sp %>% st_set_geometry(NULL)

## load data
write.csv(plots,"data/field-processed/compiled-processed/plots_w_gis_data.csv",row.names = FALSE)
