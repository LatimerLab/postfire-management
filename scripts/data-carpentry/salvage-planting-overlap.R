setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)


## Load FACTS data

facts <- st_read(dsn = "data/non-synced/existing-datasets/CA_Activity_merged.shp", stringsAsFactors = FALSE)
