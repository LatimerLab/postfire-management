setwd("~/repos/postfire-management")

library(sf)
library(tidyverse)
library(fasterize)
library(raster)


## load power fire perimeter and severity

perims = st_read("management-tool-prep/data/non-synced/burnsev/veg_severity_perimeters18_1.gdb")
sevs = st_read("management-tool-prep/data/non-synced/burnsev/VegBurnSeverityBA18_1.gdb")

perim = perims %>%
  filter(FIRE_NAME == "POWER",
         FIRE_YEAR == "2004")

sev = sevs %>%
  filter(VB_ID == "2004POWER",
         BEST_ASSESS == "YES")

## save the focal perim and sev
st_write(perim %>% st_transform(4326),"management-tool-app/data/power_perim.geojson")
st_write(sev %>% st_transform(4326),"management-tool-app/data/power_sev.geojson")

#### Compute distance to anything other than highest sev (class 7)

## get all the non-high-sev area
sev_nonhigh = sev %>%
  filter(BURNSEV < 7)

## buffer out the fire perim 100 m to use that as non-high-sev
perim_buffer = st_buffer(perim,100)
perim_ring = st_difference(perim_buffer,perim)

## merge the ring with the non-high-sev to get all areas to measure seed distance from
seed_source = st_union(sev_nonhigh,perim_ring) %>% st_buffer(0) %>% st_union

## turn into raster
raster_template = raster(seed_source %>% as("Spatial"),resolution=30,crs=3310)
#seed_source$seed_source = TRUE
seed_rast = fasterize(seed_source %>% st_as_sf,raster_template,fun="count")

## comp distance to non-high sev
seed_dist = distance(seed_rast)

## crop to the fire footprint
seed_dist = crop(seed_dist,perim)
seed_dist = mask(seed_dist,perim)
