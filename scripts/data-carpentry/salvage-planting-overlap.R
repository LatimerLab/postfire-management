setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)

## Load FACTS data
facts <- st_read(dsn = "data/non-synced/existing-datasets/CA_Activity_merged.shp", stringsAsFactors = FALSE)
facts$year <- substr(facts$DATE_ACCOM,1,4)
#project to CA Albers
facts <- st_transform(facts,crs=3310)
facts$id <- 1:nrow(facts)


## Make planting-only FACTS layer
planting <- c("Plant Trees")
facts.planting <- facts[facts$ACTIVITY %in% planting,]

## Make salvage-only FACTS layer
salvage <- c("Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (EA/RH/FH)")
facts.salvage <- facts[facts$ACTIVITY %in% salvage,]

## We're only interested in areas planted between 2000 and 2015
planting.years <- 1980:2017
facts.planting <- facts.planting[facts.planting$year %in% planting.years,]

## We're only interested in salvage that happened within 6 years prior to the planting
salvage.prior <- 6
salvage.years <- (min(planting.years)-salvage.prior):(max(planting.years))
facts.salvage <- facts.salvage[facts.salvage$year %in% salvage.years,]

## Optionally thin to a focal region
rd <- st_read(dsn = "data/non-synced/existing-datasets/ranger-districts/S_USA.RangerDistrict.shp", stringsAsFactors = FALSE)
rd <- rd[rd$FORESTNAME == "Plumas National Forest",]
rd <- st_transform(rd,crs=3310)
facts.salvage <- st_intersection(facts.salvage,rd)
facts.planting <- st_intersection(facts.planting,rd)


## For each planted polygon, get all touching salvage polygons from up to 6 years prior to the planting
all.overlap.ids <- NULL #just add first row as an example

for(i in 1:nrow(facts.planting)) {
  
  facts.planting.polygon <- facts.planting[i,]
  planting.year <- as.numeric(facts.planting.polygon$year)
  salvage.year.range <- (planting.year-10):planting.year #this includes salvage that happened in the same year as planting
  facts.salvage.matchyears <- facts.salvage[facts.salvage$year %in% salvage.year.range,]
  
  # see which salvage polygons (rows) at least partially overlap the planted areas
  salvage.overlap <- st_intersects(facts.planting.polygon,facts.salvage.matchyears)
  
  overlap.ids <- facts.salvage.matchyears[unlist(salvage.overlap),]$id
  
  all.overlap.ids <- c(all.overlap.ids,overlap.ids)
  
  cat("\r ",i)
  
  ##! here we could add IDs to the saved salvage polygons to reference the planting polygons they are associated with
  
  
}

all.overlap.ids <- unique(all.overlap.ids)
overlapping.salvage.polygons <- facts.salvage[facts.salvage$id %in% all.overlap.ids,]


## Save the planting and salvage polygons
st_write(facts.planting,dsn="data/output-exploratory/salvage-overlap-planting/planting.shp",delete_dsn=TRUE)
st_write(overlapping.salvage.polygons,dsn="data/output-exploratory/salvage-overlap-planting/salvage_that_overlaps.shp",delete_dsn=TRUE)
#st_write(facts.salvage,dsn="data/output-exploratory/salvage-overlap-planting/salvage_all.shp") # for testing
