setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)


## Load FACTS data
facts <- st_read(dsn = "data/non-synced/existing-datasets/CA_Activity_merged.shp", stringsAsFactors = FALSE)
facts$year <- substr(facts$DATE_COMPL,1,4)


## Make planting-only FACTS layer
planting <- c("Plant Trees")
facts.planting <- facts[facts$ACTIVITY %in% planting,]

## Make salvage-only FACTS layer
salvage <- c("Salvage Cut (intermediate treatment, not regeneration)")
facts.salvage <- facts[facts$ACTIVITY %in% salvage,]

## We're only interested in areas planted between 2000 and 2014
planting.years <- 2000:2014
facts.planting <- facts.planting[facts.planting$year %in% planting.years,]

## We're only interested in salvage that happened within 8 years prior to the planting
salvage.prior <- 8
salvage.years <- (min(planting.years)-salvage.prior):(max(planting.years))


## For each planted polygon, get all touching salvage polygons from up to 10 years prior to the planting
overlapping.polygons <- facts.salvage[1,] #just add first row as an example

for(i in 1:nrow(facts.planting)) {
  
  facts.planting.polygon <- facts.planting[i,]
  planting.year <- as.numeric(facts.planting.polygon$year)
  salvage.year.range <- (planting.year-10):planting.year
  facts.salvage.matchyears <- facts.salvage[facts.salvage$year %in% salvage.year.range,]
  
  # see which salvage polygons (rows) at least partially overlap the planted areas
  salvage.overlap <- st_intersects(facts.planting.polygon,facts.salvage.matchyears)
  
  cat(i)
  
  overlapping.polygons <- rbind(overlapping.polygons,facts.salvage.matchyears[unlist(salvage.overlap),])
  
}

overlapping.salvage.polygons <- overlapping.polygons[-1,] # remove first row, which was just a template for the object
overlapping.salvage.polygons <- unique(overlapping.salvage.polygons)



## Save the planting and salvage polygons



st_write(facts.planting,dsn="data/output-exploratory/salvage-overlap-planting/planting.shp",delete_dsn=TRUE)
st_write(overlapping.salvage.polygons,dsn="data/output-exploratory/salvage-overlap-planting/salvage_that_overlaps.shp",delete_dsn=TRUE)

st_write(facts.salvage,dsn="data/output-exploratory/salvage-overlap-planting/salvage_all.shp")
