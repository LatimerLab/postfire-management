setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)
library(dplyr)

d <- st_read(dsn="data/site-selection/output/aggregated-management-history/shapefiles/management_history.gpkg",stringsAsFactors=FALSE)

d$plant.nosalvage <- ifelse(d$salvage.nyears == 0 & d$planting.nyears > 0,"yes","no")
d$noplant.nosalvage <- ifelse(d$salvage.nyears == 0 & d$planting.nyears == 0,"yes","no")
d$plant <- ifelse(d$planting.nyears == 0,"no","yes")
d$salvage <- ifelse(d$salvage.nyears == 0,"no","yes")

d <- st_buffer(d,0)

st_write(d,"data/output-exploratory/aggregated-management-history/shapefiles/management_history_summarized.gpkg",delete_dsn=TRUE)
