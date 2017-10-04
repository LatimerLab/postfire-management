setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)
library(dplyr)

d <- st_read(dsn="data/output-exploratory/aggregated-management-history/shapefiles/management_history.gpkg",stringsAsFactors=FALSE)

d$plant.nosalvage <- ifelse(d$salvage.nyears == 0 & d$planting.nyears > 0,"yes","no")

d <- st_buffer(d,0)

st_write(d,"data/output-exploratory/aggregated-management-history/shapefiles/management_history_plant_nosalvage.gpkg",delete_dsn=TRUE)
