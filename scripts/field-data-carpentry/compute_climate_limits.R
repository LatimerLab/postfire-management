setwd("~/Research Projects/Post-fire management/postfire-management")

library(raster)
library(sf)
library(tidyverse)

veg = st_read("C:/Users/DYoung/Documents/GIS/CALVEG/shapefiles/simp.shp")

plots = st_read("data/field-processed/spatial/plots_points.gpkg")

ppt = raster("data/non-synced/existing-datasets/precipitation/PRISM_ppt_30yr_normal_800mM2_annual_bil.bil")
dem = raster("data/non-synced/existing-datasets/DEM/CAmerged12.tif")
rad = raster("data/non-synced/existing-datasets/solar radiation/march_rad.tif")
temp = raster("data/non-synced/existing-datasets/temperature/PRISM_tmean_30yr_normal_800mM2_annual_bil.bil")


fires = c("Power","AmRiv","MoonAnt","Piute","Ctwd")

for(fire in fires) {

  
  plots_foc = plots[plots$Fire == fire,]
  
  area = st_buffer(plots_foc,60000) %>% st_union() %>% st_sf()

  points = st_make_grid(area,cellsize=2000,what="centers") %>% st_intersection(tiers) %>% st_sf 
  
  points = st_transform(points,st_crs(veg))
  
  points_veg = st_intersection(points,veg)
  
  ## whr types of interest are:
  focal_types = c("DFR","EPN","JPN","MHC","PPN","WFR","SMC")
  #leaving out red fir, lodgepole pine, subalpine conifer
  
  points_veg = points_veg %>%
    mutate(focal_type = (WHRTYPE %in% focal_types))
  
  # get the climate values for the points
  points_veg$ppt = raster::extract(ppt,points_veg,method="bilinear")
  points_veg$rad = raster::extract(rad,points_veg,method="bilinear")
  points_veg$elev = raster::extract(dem,points_veg,method="bilinear")
  points_veg$temperature = raster::extract(temp,points_veg,method="bilinear")
  
  
  # plot ppt vs temp for this forest type
  points_foc = points_veg %>%
    filter(focal_type == TRUE)
  
  ggplot(points_foc,aes(x=temperature,y=ppt)) +
    geom_point() +
    theme_bw()
  
  
  
  ## compute the limits
  
  points_summ = points_veg %>%
    group_by(tier) %>%
    summarize(min_ppt = quantile(ppt,.001),
              min_elev = quantile(elev,.001),
              min_temp = quantile(temp,.001),
              max_temp = quantile(temp,.999))
  
  points_foc_summ = points_veg %>%
    filter(focal_type == TRUE) %>%
    group_by(tier) %>%
    summarize(min_ppt = quantile(ppt,.1))
  
  
  
  
  
  ggplot(points_summ,aes(x=tier,y=min_ppt)) +
    geom_point() +
    geom_point(data=points_foc_summ,color="red")


}