setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)
library(rgdal)
library(raster)
library(dplyr)
library(sp)
library(units)

crs <- 3310 # CA albers


#### Convenience functions ####

deg2rad <- function(x) {
  x*pi/180
}

#### Load necessary layers ####

# load FACTS planting history slices of focal fires
planting.slices <- readOGR("data/site-selection/output/aggregated-management-history/shapefiles/management_history.gpkg",stringsAsFactors = FALSE)
planting.slices <- as(planting.slices,"sf")
planting.slices <- st_transform(planting.slices,crs=crs)
planting.slices <- planting.slices[planting.slices$planting.nyears > 0,]


# load names of focal fires
focal.fires.input <- read.csv("data/site-selection/analysis-parameters/focal_fires.csv",stringsAsFactors=FALSE)
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
ownership <- st_read("data/non-synced/existing-datasets/CPAD/CPAD_2017a_SuperUnits.shp")
ownership <- ownership[ownership$MNG_AGENCY == "United States Forest Service",]
ownership <- st_transform(ownership,crs=crs)




#### Place points ####

# Buffer out the planting units by 0.5 m so they merge back together
planting.zone <- st_buffer(planting.slices,dist=1)
planting.zone <- st_union(planting.zone)



# Buffer in by 20 m and out by 20 m and place points along the resulting perimeters to establish the candidate set of "treated" and "control" plots
treated.plot.perim <- st_buffer(planting.zone,dist=-30)
treated.plot.perim <- st_cast(treated.plot.perim,"MULTILINESTRING")
treated.perim.length <- st_length(treated.plot.perim) %>% sum() %>% as.numeric()
treated.plot.perim <- as(treated.plot.perim,"Spatial")
trt.candidate.plots <- spsample(treated.plot.perim,n=treated.perim.length/100,type="regular")
trt.candidate.plots <- as(trt.candidate.plots,"sf")


control.plot.perim <- st_buffer(planting.zone,dist=30)
control.plot.perim <- st_cast(control.plot.perim,"MULTILINESTRING")
control.perim.length <- st_length(control.plot.perim) %>% sum() %>% as.numeric()
control.plot.perim <- as(control.plot.perim,"Spatial")
ctrl.candidate.plots <- spsample(control.plot.perim,n=control.perim.length/50,type="regular")
ctrl.candidate.plots <- as(ctrl.candidate.plots,"sf")


trt.candidate.plots$type <- "treatment"
ctrl.candidate.plots$type <- "control"

candidate.plots <- rbind(trt.candidate.plots,ctrl.candidate.plots)
candidate.plots$id <- seq(1,nrow(candidate.plots))


#### Compile attributes of candidate plots ####


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





# Get slope, aspect, elevation, of all control and treated plots
candidate.plots <- as(candidate.plots,"Spatial") # change the plots to SpatialPointsDF because raster package doesn't work with them yet
candidate.plots$slope <- extract(slope.aspect[["slope"]],candidate.plots)
candidate.plots$aspect <- extract(slope.aspect[["aspect"]],candidate.plots)
candidate.plots$elev <- extract(dem,candidate.plots)

candidate.plots$northness <- cos(deg2rad(candidate.plots$aspect))


# Get the severity of the most recent overlapping focal fire(s) -- that is the fire that prompted the planting
#!!!!!!!!! Need to reconcile this with needing plots to be close to seed sources !!!!! As written currently, plots will be >= 55m from non-high severity
candidate.plots <- as(candidate.plots,"sf")
candidate.plots.buffer <- st_buffer(candidate.plots,40) # buffer out for 40m to make sure it's high-severity in the entire area surrounding
fire.sev.buffer <- st_buffer(fire.sev,15) # buffer fire severity out for 15 m in case there were inaccuracies in measuring fire severity
sev.intersect <- st_intersects(candidate.plots,fire.sev.buffer)

#! export fire sev buffer to make sure it worked

fire.sev.data <- as.data.frame(fire.sev) # remove spatial data to speed up the following function
fire.sev.data <- fire.sev.data %>% select(-geometry)

get_mostrecent_severity <- function(x) { # function to get the severities of the overlapping severity layers. if multiple overlapping, concatenate into a string listing all.
  
  overlaps <- x
  fire.sev.overlap <- fire.sev.data[overlaps,]
  overlap.years <- fire.sev.overlap$FIRE_YEAR
  max.yr.index <- which(overlap.years == max(overlap.years)) # indices of all overlap fires that were the most recent focal fire
  severities <- fire.sev.overlap[max.yr.index,"BURNSEV"]
  severity.mostrecent <- min(severities) # in case there was more than one severity within the buffered area around each plot, take the minimum so we can later filter to plots for which the entire surrounding area was > X severity. Note that this can be foiled if there was more than one fire that burned over a given plot in the most-recent fire year (e.g. one fire burned at high severity and the second burned at low severity and then the site was planted). But that should be a very small number of candidate plots.
  
  return(severity.mostrecent)
  
}

most.recent.focal.fire.sev <- lapply(sev.intersect,FUN=get_mostrecent_severity)
candidate.plots$focal.fire.sev <- unlist(most.recent.focal.fire.sev)



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


# note that we don't have to check whether the plots burned after the focal fire because the script to generate the planting unit slices already excludes planting unit slices that burned after the focal fire.
#   although control plots could fall outside this area (and thus have burned after the focal fire), we will make sure they didn't by making sure their fire history is the same as that of their paired treated plot




# Compare to FS ownership layer to make sure all plots are FS land
#    buffer FS layer in by 30 m to make sure not near private (i.e., that the boundary was set along ownerhip), allowing for some spatial error)
ownership.union <- st_union(ownership)
ownership.bufferin30 <- st_buffer(ownership.union,-100)
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


#### Filter out candidate plots to find those meeting criteria ####

candidate.plots.backup <- candidate.plots

# filter based on severity, ownership
candidate.plots <- candidate.plots %>%
  filter(focal.fire.sev == 4) %>%  # include only candidate plots that have high severity surrounding them #! in the future, may want to look out to a wider surrounding area
  filter(ownership == "fs") # include only candidate plots that are on (buffered in) FS land

# break back into treated and control
trt <- candidate.plots %>% filter(type=="treatment")
ctl <- candidate.plots %>% filter(type=="control")

trt$index <- 1:nrow(trt)
ctl$index <- 1:nrow(ctl)

# get distances between each treated plot and each control
a <- st_distance(trt,ctl) # takes 1-2 min. maybe do after narrowing

# for each row (treated plot), which columns (control plots) are within x distance
radius <- set_units(80,m)
close.pairs <- which(a < radius,arr.ind=T)
colnames(close.pairs) <- c("trt","ctl")
close.pairs <- as.data.frame(close.pairs)

trt.data <- as.data.frame(trt) %>% select(-geometry)
ctl.data <- as.data.frame(ctl) %>% select(-geometry)



## for each treated plot
for(i in 1:nrow(trt)) {
  
  cat("\r Running for row",i,"of",nrow(trt),"          ")
  
  trt.focal <- trt.data[i,]
  
  # close.ctl.plots <- close.pairs %>%
  #   filter(trt==i) %>%
  #   select(ctl)
  
  close.ctl.plots <- close.pairs[close.pairs$trt==i,"ctl"]
  
  ctl.close <- ctl.data[close.ctl.plots,]
  
  
  #check all close controls to make sure comparable:
  # same: focal fire (that triggered planting); fire history
  # within: 5 slope, 0.5 northness, 100 elevation
  
  ctl.close <- ctl.close[ctl.close$most.recent.focal.fire == trt.focal$most.recent.focal.fire &
                           ctl.close$fire.history == trt.focal$fire.history &
                           near(ctl.close$slope,trt.focal$slope,10) &
                           near(ctl.close$elev,trt.focal$elev,100) &
                           near(ctl.close$northness,trt.focal$northness,0.5),]
  

  
  # get the distance between the treated plot and each comparable control
  dist <- a[i,ctl.close$index]
  
  # get the distance to the closest control
  closest.distance <- min(dist)
  
  # which of them was the closest?
  closest <- which(dist == closest.distance)
  
  #get the id number of the closest control
  closest.id <- ctl.close[closest,"id"]
  
  if(length(closest.id) == 0) {
    closest.id <- NA
  }
  
  trt.data[i,"closest.ctl.id"] <- closest.id
  trt.data[i,"closest.ctl.dist"] <- closest.distance
  
}

## if multiple treated plots claim the same control, choose the one that is the closest to its control

# for each control plot ID that is paired with a treatment plot, load all paired treatment plots; flag those that are not the closest

trt.data$not.closest <- "no"

paired.ctl.ids <- unique(trt.data$closest.ctl.id)

for(ctl.id in paired.ctl.ids) {

  if(is.na(ctl.id)) next()  
  if(ctl.id == -1) next()

  
  trt.using.ctl <- trt.data[trt.data$closest.ctl.id == ctl.id & !is.na(trt.data$closest.ctl.id),]
  
  if(nrow(trt.using.ctl) < 2) next()
  
  distances <- trt.using.ctl$closest.ctl.dist
  non.min.dist.index <- which(distances != min(distances))
  non.min.dist.plot.ids <- trt.using.ctl[non.min.dist.index,"id"]
  
  # set those plots to "not closest"
  trt.data[trt.data$id %in% non.min.dist.plot.ids,"not.closest"] <- "yes"
  
  
}







trt$ctl.id <- trt.data$closest.ctl.id
trt$ctl.dist <- trt.data$closest.ctl.dist
trt$not.closest <- trt.data$not.closest

trt$ctl.dist <- ifelse(trt$ctl.dist == Inf,999999,trt$ctl.dist)
trt$ctl.id <- ifelse(is.na(trt$ctl.id),-1,trt$ctl.id)



ctl$ctl.id <- -1
ctl$ctl.dist <- 999999
ctl$not.closest <- "n/a"

candidate.plots.paired <- rbind(trt,ctl)


# Optional; not implemented: if multiple plot pairs from the same planting slice are within X distance, randomly remove them until they are > X distance



# keep only control plots and those treatment plots that are the closest
candidate.plots.paired <- candidate.plots.paired %>% filter(type == "control" | not.closest == "no")

# remove treatment plots that don't have a pair
candidate.plots.paired <- candidate.plots.paired  %>% filter(type == "control" | ctl.id != -1)

candidate.plots.paired$pair.id <- "-1"



## add "pair ids" to easily locate the two plots of a pair
# start by looking up the treatment plot id of the control plots
trt.plot.ids <- candidate.plots.paired[candidate.plots.paired$type=="treatment",]$id
for(trt.plot.id in trt.plot.ids) {
  
  # get control id of that treated plot
  ctl.id <- candidate.plots.paired[candidate.plots.paired$id == trt.plot.id,]$ctl.id
  
  
  
  # look up the corresponding control plot and give it the treated plot id
  candidate.plots.paired[candidate.plots.paired$id == ctl.id,"pair.id"] <- trt.plot.id
}


# now for all treated plots, add its own ID to the pair ID column (because the treated plot ID will be the ID for the pair)
candidate.plots.paired[candidate.plots.paired$type == "treatment","pair.id"] <- candidate.plots.paired[candidate.plots.paired$type == "treatment",]$id


# remove control plots that don't have a pair
candidate.plots.paired <- candidate.plots.paired %>% filter(pair.id != -1)

# remove columns that are meaningul only in filtering code
candidate.plots.paired <- candidate.plots.paired %>% select(-index,-ownership,-not.closest)

# add an empty column as a workaround to the fact that QGIS kml exoport needs to use a colum for feature display labels
candidate.plots.paired$label <- " "

st_write(candidate.plots.paired,"data/site-selection/output/candidate-plots/candidate_plots_paired.gpkg",delete_dsn=TRUE)


# summarize the planting pairs: planted-conttrol both salvaged or not, or different? replanted, shrub control, etc?




#### Export to other file formats ####
p <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired.gpkg",stringsAsFactors=FALSE)
p$name <- p$id

st_write(p,"data/site-selection/output/candidate-plots/candidate_plots_paired_5.kml")


p <- select(p,c(name))
p <- st_transform(p,crs=4326)

st_write(p,"data/site-selection/output/candidate-plots/candidate_plots_paired_5.gpx",driver="GPX") #! write gpx

##
##
##
##
##
#### Explore range of environment and management at paired plots ####
#### for narrowing to the most common type of factorial management, and for identifying fires with sufficient environmental variation
##
