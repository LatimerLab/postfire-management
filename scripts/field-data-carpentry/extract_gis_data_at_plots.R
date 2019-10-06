setwd("C:/Users/DYoung/Documents/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(raster)
library(sf)


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
plots_sp$normal_annual_precip = raster::extract(ppt,plots_sp,method="bilinear")

## extract elevation
dem = raster("data/non-synced/existing-datasets/DEM/CAmerged12.tif")
plots_sp$elev = raster::extract(dem,plots_sp,method="bilinear")

## compute and extract slope and aspect (takes ~ 15 minutes)
slope = terrain(dem,opt=c("slope"),unit="degrees")
aspect = terrain(dem,opt=c("aspect"),unit="degrees")

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
write.csv(plots,"data/field-processed/compiled-processed/plots_w_gis_data.csv",row.names = FALSE)
