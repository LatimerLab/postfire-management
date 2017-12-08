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

focal.rd <- c("Lassen National Forest","Plumas National Forest","Tahoe National Forest","Eldorado National Forest","Stanislaus National Forest","Sierra National Forest","Sequoia National Forest","Shasta-Trinity National Forest","Klamath National Forest","Mendocino National Forest")
rd <- st_read(dsn = "data/non-synced/existing-datasets/ranger-districts/S_USA.RangerDistrict.shp", stringsAsFactors = FALSE)
rd <- rd[rd$FORESTNAME %in% focal.rd,]
rd <- st_transform(rd,crs=3310)
rd <- st_buffer(rd,0)

facts.salvage <- st_intersection(facts.salvage,rd)
facts.planting <- st_intersection(facts.planting,rd)


## For each planted polygon, get all touching salvage polygons from up to 6 years prior to the planting
all.overlap.ids <- NULL #just add first row as an example

for(j in 1:length(focal.rd)) {
  
  rd.focal.name <- focal.rd[j]
  
  cat(paste0("\nRunning for ", rd.focal.name,"\n"))
  rd.focal <- rd[rd$FORESTNAME == rd.focal.name,]
  facts.planting.rd <- st_intersection(facts.planting,rd.focal)
  facts.salvage.rd <- st_intersection(facts.salvage,rd.focal)

  

  for(i in 1:nrow(facts.planting.rd)) {
    
    facts.planting.polygon <- facts.planting.rd[i,]
    planting.year <- as.numeric(facts.planting.polygon$year)
    if(is.na(planting.year)) next()
    salvage.year.range <- (planting.year-7):planting.year #this includes salvage that happened in the same year as planting
    facts.salvage.matchyears <- facts.salvage.rd[facts.salvage.rd$year %in% salvage.year.range,]
    
    if(nrow(facts.salvage.matchyears) == 0) {
      next()
    }
    
    # see which salvage polygons (rows) at least partially overlap the planted areas
    salvage.overlap <- st_intersects(facts.planting.polygon,facts.salvage.matchyears)
    
    overlap.ids <- facts.salvage.matchyears[unlist(salvage.overlap),]$id
    
    all.overlap.ids <- c(all.overlap.ids,overlap.ids)
    
    cat("\r ",i)
    
    ##! here we could add IDs to the saved salvage polygons to reference the planting polygons they are associated with
    
    
  }

}
  
all.overlap.ids <- unique(all.overlap.ids)
overlapping.salvage.polygons <- facts.salvage[facts.salvage$id %in% all.overlap.ids,]


## Save the planting and salvage polygons
st_write(facts.planting,dsn="data/site-selection/output/salvage-overlap-planting/planting.shp",delete_dsn=TRUE)
st_write(overlapping.salvage.polygons,dsn="data/site-selection/output/salvage-overlap-planting/salvage_that_overlaps.shp",delete_dsn=TRUE)
#st_write(facts.salvage,dsn="data/output-exploratory/salvage-overlap-planting/salvage_all.shp") # for testing

## Load them out again

facts.planting <- st_read(dsn="data/site-selection/output/salvage-overlap-planting/planting.shp")

