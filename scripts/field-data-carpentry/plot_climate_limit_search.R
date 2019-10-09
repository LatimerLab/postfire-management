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
  
  
  
  area = st_buffer(plots_foc,5000) %>% st_union() %>% st_sf()
  area$tier = 0
  
  
  
  tiers = area
  
  n_tiers = 12
  tier_increment = 5000
  prev_tier = area
  
  for(i in 1:n_tiers) {
    
    prev_tier_buff = st_buffer(prev_tier,tier_increment)
    new_tier = st_difference(prev_tier_buff,tiers %>% st_union) %>% select()
    new_tier$tier = i
    tiers = rbind(tiers,new_tier)
    prev_tier = new_tier
    
  }
  
  st_write(tiers,"data/intermediate/tiers_temporary.gpkg",delete_dsn=TRUE)
  
  # make a grid of points to sample in the area
  
  points = st_make_grid(tiers,cellsize=2000,what="centers") %>% st_sf %>% st_intersection(tiers)
  
  points = st_transform(points,st_crs(veg))
  
  points_veg = st_intersection(points,veg)
  
  table(points_veg$WHRTYPE)
  
  
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
  
  ggplot(points_veg,aes(x=ppt,y=rad,color=tier,shape=focal_type)) +
    geom_point()
  
  
  ## plot how the minimum precip within the forest type changes by tier
  
  # compute that
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