setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)
library(rgdal)
library(raster)
library(dplyr)
library(sp)

crs <- 3310 # CA albers


#### Load necessary layers ####

# load FACTS planting history slices of focal fires
planting.slices <- readOGR("data/output-exploratory/aggregated-management-history/shapefiles/management_history.gpkg",stringsAsFactors = FALSE)
planting.slices <- as(planting.slices,"sf")
planting.slices <- st_transform(planting.slices,crs=crs)
planting.slices <- planting.slices[planting.slices$planting.nyears > 0,]


# load names of focal fires
focal.fires.input <- read.csv("data/analysis-parameters/focal_fires.csv",stringsAsFactors=FALSE)
fires.focal.names <- unique(focal.fires.input$VB_ID)


# load fire perimeter database and thin to focal fires
fires <- readOGR("data/non-synced/existing-datasets/veg_severity_perimeters16_1.gdb",stringsAsFactors=FALSE)
fires <- as(fires,"sf")
fires <- st_transform(fires,crs=crs)
fires <- fires[fires$FIRE_YEAR > 1983,]
fires.focal <- fires[fires$VB_ID %in% fires.focal.names,]


# load full FACTS (not just planting) and clip to focal fires
#    we need this to avoid putting plots in areas that were managed
facts.all <- st_read("data/non-synced/existing-datasets/pseudo-FACTS/CA clips/CA_Activity_merged.shp",stringsAsFactors=FALSE)
facts.all <- st_transform(facts.all,crs=crs)
facts.all.fires <- st_intersection(facts.all,fires.focal)

# load fire severity layers and thin to focal fires
fire.sev <- st_read("data/non-synced/existing-datasets/VegBurnSeverity_shp/veg_burn_severity.shp",stringsAsFactors=FALSE)
fire.sev <- fire.sev[fire.sev$VB_ID %in% fires.focal.names,]
fire.sev <- fire.sev[fire.sev$BEST_ASSES == "YES",]
fire.sev <- st_transform(fire.sev,crs=crs)


# load DEM and compute slope and aspect
dem <- raster("data/non-synced/existing-datasets/DEM/CAmerged12_albers2.tif")
dem <- crop(dem,as(fires.focal,"Spatial"))
#dem <- mask(dem,as(fires.focal,"Spatial"))
slope.aspect <- terrain(dem,opt=c("slope","aspect"),unit="degrees")

# load USFS ownership
ownership <- st_read("data/non-synced/existing-datasets/CPAD State Federal/CPAD_State_Federal.shp")
ownership <- ownership[ownership$AGNCY_NAME == "United States Forest Service",]
ownership <- st_transform(ownership,crs=crs)




#### Place points ####

# Buffer out the planting units by 0.5 m so they merge back together
planting.zone <- st_buffer(planting.slices,dist=1)
planting.zone <- st_union(planting.zone)



# Buffer in by 20 m and out by 20 m and place points along the resulting perimeters to establish the candidate set of "treated" and "control" plots
treated.plot.perim <- st_buffer(planting.zone,dist=-20)
treated.plot.perim <- st_cast(treated.plot.perim,"MULTILINESTRING")
treated.perim.length <- st_length(treated.plot.perim) %>% sum() %>% as.numeric()
treated.plot.perim <- as(treated.plot.perim,"Spatial")
trt.candidate.plots <- spsample(treated.plot.perim,n=treated.perim.length/100,type="regular")
trt.candidate.plots <- as(trt.candidate.plots,"sf")


control.plot.perim <- st_buffer(planting.zone,dist=20)
control.plot.perim <- st_cast(control.plot.perim,"MULTILINESTRING")
control.perim.length <- st_length(control.plot.perim) %>% sum() %>% as.numeric()
control.plot.perim <- as(control.plot.perim,"Spatial")
ctrl.candidate.plots <- spsample(control.plot.perim,n=control.perim.length/50,type="regular")
ctrl.candidate.plots <- as(ctrl.candidate.plots,"sf")


trt.candidate.plots$type <- "treatment"
ctrl.candidate.plots$type <- "control"

candidate.plots <- rbind(trt.candidate.plots,ctrl.candidate.plots)
candidate.plots$id <- seq(1,nrow(candidate.plots))

# Optional, not implemented: Remove candidate control plots that are in an area with any FACTS management history (beyond planting)

# Find the most recent focal fire that each plot burned in so we can make sure treated and control are from the same fire
fire.intersect <- st_intersects(candidate.plots,fires.focal)
fires.focal.data <- as.data.frame(fires.focal) # remove spatial data to speed up the following function
fires.focal.data <- fires.focal.data %>% select(-geometry)

get_mostrecent_VB_ID <- function(x) { # function to get the fire name and year from the most recent overlapping fire, given row indeces of all fires that overlap (from above)

  overlaps <- x
  years <- fires.focal.data[overlaps,"FIRE_YEAR"]
  max.year <- max(years)
  max.index <- which(years == max.year)
  
  if (length(max.index) > 1) { # burned more than once in same year, so reject
    fire_VB_ID <- "twice burned"
  } else if (length(max.index) == 0) {
    fire_VB_ID <- "no overlapping fire"
  } else {
    fire.index <- overlaps[max.index]
    fire_VB_ID <- fires.focal.data[fire.index,"VB_ID"]
  }
  
  return(fire_VB_ID)

}

most.recent.focal.fire <- lapply(fire.intersect,FUN=get_mostrecent_VB_ID)
candidate.plots$most.recent.focal.fire <- unlist(most.recent.focal.fire)

st_write(candidate.plots,"temp_test/candidate_plots.shp",delete_dsn=TRUE)




# Get slope, aspect, elevation, of all control and treated plots
candidate.plots <- as(candidate.plots,"Spatial") # change the plots to SpatialPointsDF because raster package doesn't work with them yet
candidate.plots$slope <- extract(slope.aspect[["slope"]],candidate.plots)
candidate.plots$aspect <- extract(slope.aspect[["aspect"]],candidate.plots)
candidate.plots$elev <- extract(dem,candidate.plots)


# Get the severity of the most recent overlapping focal fire(s) -- that is the fire that prompted the planting
candidate.plots <- as(candidate.plots,"sf")
sev.intersect <- st_intersects(candidate.plots,fire.sev)

fire.sev.data <- as.data.frame(fire.sev) # remove spatial data to speed up the following function
fire.sev.data <- fire.sev.data %>% select(-geometry)

get_mostrecent_severity <- function(x) { # function to get the severities of the overlapping severity layers. if multiple overlapping, concatenate into a string listing all.
  
  overlaps <- x
  fire.sev.overlap <- fire.sev.data[overlaps,]
  overlap.years <- fire.sev.overlap$FIRE_YEAR
  max.yr.index <- which(overlap.years == max(overlap.years)) # indices of all overlap fires that were the most recent focal fire
  severities <- fire.sev.overlap[max.yr.index,"BURNSEV"]
  severity.mostrecent <- max(severities) # in case there was more than one severity layer (e.g., two fires that burned in the same year)
  
  return(severity.mostrecent)
  
}

most.recent.focal.fire.sev <- lapply(sev.intersect,FUN=get_mostrecent_severity)
candidate.plots$focal.fire.sev <- unlist(most.recent.focal.fire.sev)

st_write(candidate.plots,"temp_test/candidate_plots.shp",delete_dsn=TRUE)


# get fire history (in one alphabetized concatenated string) -- not just of focal fires -- to make sure that treated and control plots have same fire history
fire.perim.intersect <- st_intersects(candidate.plots,fires)

fires.data <- as.data.frame(fires) # remove spatial data to speed up the following function
fires.data <- fires.data %>% select(-geometry)

get_all_VB_IDs <- function(x) { # function to get the VB_ID of all overlapping historical fires (since 1984) to make sure that treated and control plots have the same fire history
  overlaps <- x
  vb_ids <- sort(fires.data[overlaps,"VB_ID"])
  vb_ids.concatenate <- paste(vb_ids,collapse=", ")
  return(vb_ids.concatenate)
}

vb_ids <- lapply(fire.perim.intersect,FUN=get_all_VB_IDs)
candidate.plots$fire.history <- unlist(vb_ids)

st_write(candidate.plots,"temp_test/candidate_plots.shp",delete_dsn=TRUE)

# note that we don't have to check whether the plots burned after the focal fire because the script to generate the planting unit slices already excludes planting unit slices that burned after the focal fire.
#   although control plots could fall outside this area (and thus have burned after the focal fire), we will make sure they didn't by making sure their fire history is the same as that of their paired treated plot




# Compare to FS ownership layer to make sure all plots are FS land
#    buffer FS layer in by 30 m to make sure not near private (i.e., that the boundary was set along ownerhip), allowing for some spatial error)
ownership.union <- st_union(ownership)
ownership.bufferin30 <- st_buffer(ownership.union,-30)
fires.focal.union <- st_union(fires.focal)

ownership.fires <- st_intersection(ownership.bufferin30,fires.focal.union)

d <- st_intersects(candidate.plots,ownership.fires)
e <- d %>% lapply(FUN=function(x) {ifelse(length(x)==0,"non-fs","fs")}) # convert to readable text indicating whether it is on fs land (buffered in) or not

candidate.plots$ownership <- e


# get all management that happened in planted and control plots (one concatenated string); one purpose is to later see if most control plots had management of some sort--need to decide if that shoudl be included or excluded
facts.all.intersect <- st_intersects(candidate.plots,facts.all.fires)

facts.data <- as.data.frame(facts.all.fires) # remove spatial data to speed up the following function
facts.data <- facts.data %>% select(-geometry)

get_all_facts_management <- function(x) { # function to get the VB_ID of all overlapping historical fires (since 1984) to make sure that treated and control plots have the same fire history
  overlaps <- x
  activity <- facts.data[overlaps,"ACTIV"]
  completed_date <- facts.data[overlaps,"DATE_C"]
  completed_date[which(is.na(completed_date))] <- "nodate"
  
  order.by.date <- order(completed_date)
  activity <- activity[order.by.date]
  completed_date <- completed_date[order.by.date]
  
  act.date <- paste(activity,completed_date,sep="-")
  act.date.concatenate <- paste(act.date,collapse="; ")
  return(act.date.concatenate)
}

activity.date <- lapply(facts.all.intersect,FUN=get_all_facts_management)
candidate.plots$management.history <- unlist(activity.date)





# include only candidate plots that have high severity surrounding them

# exclude plot pairs that did not match in terms of slope, aspect, elevation, and fire severity

# if multiple plot pairs from the same planting slice are within X distance, randomly remove them until they are > X distance

# summarize the planting pairs: planted-conttrol both salvaged or not, or different? replanted, shrub control, etc?

##!! if multiple treated plots claim the same control, choose the one that is the closest to its control


