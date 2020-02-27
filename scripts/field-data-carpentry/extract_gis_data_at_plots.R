setwd("C:/Users/DYoung/Documents/projects/Post-fire management/postfire-management")

library(tidyverse)
library(raster)
library(sf)
library(spatialEco)
library(dynatopmodel)
library(ncdf4)


## load data
plots = read.csv("data/field-processed/compiled-processed/plots.csv",stringsAsFactors = FALSE)


### For plots in geographic
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
plots_sp$normal_annual_precip = raster::extract(ppt,plots_sp,method="bilinear")

## extract TopoWX normal temperature
tmax = stack("data/non-synced/existing-datasets/topowx_temerature/tmax_normal/normals_tmax.nc")
tmax_ndj = mean(tmax[[c(11,12,1)]])
tmax_fma = mean(tmax[[c(2,3,4)]])
tmax_mjj = mean(tmax[[c(5,6,7)]])
tmax_aso = mean(tmax[[c(8,9,10)]])
tmax = mean(tmax)

tmin = stack("data/non-synced/existing-datasets/topowx_temerature/tmin_normal/normals_tmin.nc")
tmin_ndj = mean(tmin[[c(11,12,1)]])
tmin_fma = mean(tmin[[c(2,3,4)]])
tmin_mjj = mean(tmin[[c(5,6,7)]])
tmin_aso = mean(tmin[[c(8,9,10)]])
tmin = mean(tmin)

tmean_ndj = mean(tmax_ndj,tmin_ndj)
tmean_fma = mean(tmax_fma,tmin_fma)
tmean_mjj = mean(tmax_mjj,tmin_mjj)
tmean_aso = mean(tmax_aso,tmin_aso)
tmean = mean(tmax,tmin)

plots_sp$tmax_ndj = raster::extract(tmax_ndj,plots_sp,method="bilinear")
plots_sp$tmin_ndj = raster::extract(tmin_ndj,plots_sp,method="bilinear")
plots_sp$tmean_ndj = raster::extract(tmean_ndj,plots_sp,method="bilinear")

plots_sp$tmax_fma = raster::extract(tmax_fma,plots_sp,method="bilinear")
plots_sp$tmin_fma = raster::extract(tmin_fma,plots_sp,method="bilinear")
plots_sp$tmean_fma = raster::extract(tmean_fma,plots_sp,method="bilinear")

plots_sp$tmax_mjj = raster::extract(tmax_mjj,plots_sp,method="bilinear")
plots_sp$tmin_mjj = raster::extract(tmin_mjj,plots_sp,method="bilinear")
plots_sp$tmean_mjj = raster::extract(tmean_mjj,plots_sp,method="bilinear")

plots_sp$tmax_aso = raster::extract(tmax_aso,plots_sp,method="bilinear")
plots_sp$tmin_aso = raster::extract(tmin_aso,plots_sp,method="bilinear")
plots_sp$tmean_aso = raster::extract(tmean_aso,plots_sp,method="bilinear")

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

gc()

topo_data = data.frame()

## do extractions by fire
fires = unique(plots_sp$Fire)
for(fire in fires) {
  
  cat("Running for fire",fire,"\n")
  
  plots_fire = plots_sp %>%
    filter(Fire == fire)
  
  plots_fire_vicinity = st_buffer(plots_fire,ifelse(fire == "Piute",15000,20000)) %>% st_union()
  dem_local_fire = crop(dem_local,plots_fire_vicinity %>% as("Spatial"))
  dem_local_fire = mask(dem_local_fire,plots_fire_vicinity %>% as("Spatial"))
  # dem_reg_fire = crop(dem_reg,plots_fire_vicinity %>% as("Spatial"))
  # dem_reg_fire = mask(dem_reg_fire,plots_fire_vicinity %>% as("Spatial"))
  
  # filename = paste0("../dem_reg_",fire,".tif")
  # writeRaster(dem_reg_fire,filename,overwrite=TRUE)
  # 
  
  tpi500 = tpi(dem_local_fire,scale = round(500/30))
  tpi100 = tpi(dem_local_fire,scale = round(100/30))
  tpi2000 = tpi(dem_local_fire,scale = round(2000/30))
  tpi5000 = tpi(dem_local_fire,scale = round(5000/30)) # this takes hours

  
  target_raster = raster(crs = crs(dem_local_fire), ext = extent(dem_local_fire), resolution = 30)
  dem_resamp = raster::resample(dem_local_fire,target_raster)
  dem_resamp[dem_resamp == 0] = NA
  
  
  twi = upslope.area(dem_resamp,atb=TRUE)

  tpi500_d = raster::extract(tpi500, plots_fire, method="bilinear")
  tpi100_d = raster::extract(tpi100, plots_fire, method="bilinear")
  tpi2000_d = raster::extract(tpi2000, plots_fire, method="bilinear")
  tpi5000_d = raster::extract(tpi5000, plots_fire, method="bilinear")
  twi_d = raster::extract(twi[["atb"]], plots_fire, method="bilinear")
   
  topo_data_fire = data.frame(PlotID = plots_fire$PlotID,tpi100 = tpi100_d, tpi500 = tpi500_d, tpi2000 = tpi2000_d, tpi5000 = tpi5000_d, twi = twi_d)
  topo_data = bind_rows(topo_data,topo_data_fire)

  gc()
  
}

plots_sp = left_join(plots_sp,topo_data)


## compute and extract slope and aspect (takes ~ 15 minutes)
slope = terrain(dem_local,opt=c("slope"),unit="degrees")
aspect = terrain(dem_local,opt=c("aspect"),unit="degrees")

plots_sp$slope_dem = raster::extract(slope,plots_sp,method="bilinear")
plots_sp$aspect_dem = raster::extract(aspect,plots_sp,method="bilinear")

## extract solar rad
rad_winter = raster("data/non-synced/existing-datasets/solar radiation/rad_winter.tif")
plots_sp$rad_winter = raster::extract(rad_winter,plots_sp,method="bilinear")
rad_winter_spring = raster("data/non-synced/existing-datasets/solar radiation/rad_winter_spring.tif")
plots_sp$rad_winter_spring = raster::extract(rad_winter_spring,plots_sp,method="bilinear")
rad_spring = raster("data/non-synced/existing-datasets/solar radiation/rad_spring.tif")
plots_sp$rad_spring = raster::extract(rad_spring,plots_sp,method="bilinear")
rad_spring_summer = raster("data/non-synced/existing-datasets/solar radiation/rad_spring_summer.tif")
plots_sp$rad_spring_summer = raster::extract(rad_spring_summer,plots_sp,method="bilinear")
rad_summer = raster("data/non-synced/existing-datasets/solar radiation/rad_summer.tif")
plots_sp$rad_summer = raster::extract(rad_summer,plots_sp,method="bilinear")


# ## extract twi
# twi = raster("data/non-synced/existing-datasets/twi/twi_merged.tif")
# plots_sp$twi = raster::extract(twi,plots_sp,method="bilinear")

#### Extract and summarize management history ####

## load and extract management data
facts = st_read("data/site-selection/output/aggregated-management-history/shapefiles/management_history_summarized.gpkg")

facts_simple = facts %>%
  dplyr::select(salvage,prep.nyears,release.years.post,thin.years.post,replant.years.post,planting.years.post,planting.suids.noslivers)

names(facts_simple)[1:(length(names(facts_simple))-1)] = paste0("facts.",names(facts_simple))[1:(length(names(facts_simple))-1)]

plots_sp_facts = st_intersection(plots_sp,facts_simple)

## Add back the plots that did not intersect with FACTS layers (st_intersection drops them)
plots_dropped = setdiff(plots_sp$PlotID,plots_sp_facts$PlotID)
plots_sp_dropped = plots_sp[plots_sp$PlotID %in% plots_dropped,]

cols_needed = names(plots_sp_facts)
missing = setdiff(cols_needed,names(plots_sp_dropped))
plots_sp_dropped[missing] = NA
plots_sp_dropped = plots_sp_dropped[cols_needed]

plots_sp_facts = rbind(plots_sp_facts,plots_sp_dropped)

#### Add fire years ####
plots_sp_facts = plots_sp_facts %>%
  mutate(fire_year = recode(Fire,"Ctnwd" = 1994,
                            "MoonAnt" = 2007,
                            "AmRiv" = 2008,
                            "Power" = 2004,
                            "Piute" = 2008))



#### Pull in species planting records ####

source("scripts/field-data-carpentry/extract_facts_species.R") # this stores the file species_per_plot.csv

species_per_plot = read.csv("data/intermediate/species_per_plot.csv",header=TRUE)

plots_sp_facts = left_join(plots_sp_facts,species_per_plot,by="PlotID")




## Determine whether planted/unplanted pairs are both salvaged, both not, or just one or the other salvaged
# Actually we don't have to do that because at least according to FACTS, none of our unplanted plots were in salvaged land. Want to check this with field-based observations though.

## Test summarizing our plots based on management year breakdowns to see if we need to make the classifications coarser.

plots_sp_facts_planted = plots_sp_facts[!is.na(plots_sp_facts$facts.planting.years.post),]

plots_summary = plots_sp_facts_planted %>%
  mutate(FireID = str_sub(PlotID,1,1)) %>%
  group_by(FireID,facts.salvage,facts.prep.nyears,facts.release.years.post,facts.thin.years.post,facts.replant.years.post,facts.planting.years.post) %>%
  summarize(nplots = n())

# The above summary shows that we need to simplify a few facts variables
# Also it shows that no plots were thinned or prepped!

## Simplify the FACTS variables and rerun the summary
plots_sp_facts = plots_sp_facts %>%
  mutate(facts.released = ifelse(facts.release.years.post != "","YES","no"),
         facts.replanted = ifelse(facts.replant.years.post != "","YES","no")) %>%
  mutate(facts.planting.first.year = str_sub(facts.planting.years.post,1,1))

plots_sp_facts_planted = plots_sp_facts[!is.na(plots_sp_facts$facts.planting.years.post),]

plots_summary = plots_sp_facts_planted %>%
  mutate(FireID = str_sub(PlotID,1,1)) %>%
  group_by(FireID,facts.salvage,facts.released,facts.replanted,facts.planting.first.year) %>%
  summarize(nplots = n())

# Now it looks good


## turn plots back to non-spatial for writing to CSV
plots = plots_sp_facts %>% st_set_geometry(NULL)

## write data
write.csv(plots,"data/field-processed/compiled-processed/plots_w_gis_data_newTWI.csv",row.names = FALSE)