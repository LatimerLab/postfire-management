setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)
library(rgdal)
library(raster)
library(sp)
library(units)
library(gridExtra)
library(stringr)
library(rgeos)
library(DT)

crs <- 3310 # CA albers

set.seed(1)

release <- c("Tree Release and Weed","Control of Understory Vegetation")
salvage <- c("Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (EA/RH/FH)","Patch Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)","Shelterwood Removal Cut (EA/NRH/FH)")


#### Convenience functions ####

deg2rad <- function(x) {
  x*pi/180
}

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}

tf_to_yn <- function(x) {

  a <- rep(NA,length(x))
  
  a[x==TRUE] <- "YES"
  a[x==FALSE] <- "no"
  
  return(a)
  
}

#### Load necessary layers ####

# load FACTS planting history slices of focal fires
# this layer already excludes management that burned after it was managed
# it also excludes management that was not completed (no assigned completed date)
# planting.slices <- readOGR("data/site-selection/output/aggregated-management-history/shapefiles/management_history.gpkg",stringsAsFactors = FALSE)
# planting.slices <- as(planting.slices,"sf")

planting.slices <- st_read("data/site-selection/output/aggregated-management-history/shapefiles/management_history.gpkg",stringsAsFactors=FALSE)
planting.slices <- st_transform(planting.slices,crs=crs)
planting.slices <- st_set_crs(planting.slices,value=crs)
# st_write(planting.slices,"data/site-selection/output/aggregated-management-history/shapefiles/management_history.shp",delete_dsn=TRUE)
# planting.slices <- st_read("data/site-selection/output/aggregated-management-history/shapefiles/management_history.shp",stringsAsFactors=FALSE)

planting.slices <- planting.slices[planting.slices$planting.nyears > 0,]


# load names of focal fires
focal.fires.input <- read.csv("data/site-selection/analysis-parameters/focal_fires.csv",stringsAsFactors=FALSE)
fires.focal.names <- unique(focal.fires.input$VB_ID)

# fires.focal.names <- c("2007ANTELOPE_CMPLX","2007MOONLIGHT")
# fires.focal.names <- c("2008GOVERNMENT")
# fires.focal.names <- c("1994COTTONWOOD")
fires.focal.names <- c("1987INDIAN","1990STORMY","1994COTTONWOOD","2002MCNALLY","2004POWER","2008GOVERNMENT","2008PIUTE")

# load fire perimeter database and thin to focal fires
fires <- readOGR("data/non-synced/existing-datasets/veg_severity_perimeters16_1.gdb",stringsAsFactors=FALSE)
fires <- as(fires,"sf")
fires <- st_transform(fires,crs=crs)
fires <- st_set_crs(fires,crs)
fires <- fires[fires$FIRE_YEAR > 1983,]
fires.focal <- fires[fires$VB_ID %in% fires.focal.names,]

planting.slices <- st_intersection(planting.slices,(fires.focal %>% dplyr::select(geometry)  ))


# load full FACTS (not just planting) and clip to focal fires
#    we need this to avoid putting plots in areas that were managed
facts.all <- st_read("data/non-synced/existing-datasets/pseudo-FACTS/CA clips/CA_Activity_merged.shp",stringsAsFactors=FALSE)
facts.all <- st_transform(facts.all,crs=crs)
facts.all.fires <- st_intersection(facts.all,fires.focal)
facts.all.fires$DATE_A <- as.character(facts.all.fires$DATE_A)
facts.all.fires$DATE_C <- as.character(facts.all.fires$DATE_C)
facts.all.fires$DATE_P <- as.character(facts.all.fires$DATE_P)


# remove facts units that were not completed (no completed date, unless on power fire, in which case if completed date is blank, set to accomplished date)
facts.all.fires[facts.all.fires$VB_ID == "2004POWER",]$DATE_C <- ifelse(is.na(facts.all.fires[facts.all.fires$VB_ID == "2004POWER",]$DATE_C),facts.all.fires[facts.all.fires$VB_ID == "2004POWER",]$DATE_A,facts.all.fires[facts.all.fires$VB_ID == "2004POWER",]$DATE_C)
facts.all.fires <- facts.all.fires[!is.na(facts.all.fires$DATE_C),]


# load fire severity layers and thin to focal fires
fire.sev <- st_read("data/non-synced/existing-datasets/VegBurnSeverity_shp/veg_burn_severity.shp",stringsAsFactors=FALSE)
fire.sev <- st_transform(fire.sev,crs=crs)

## Only need to run once
# fire.sev.high.1980 <- fire.sev[fire.sev$FIRE_YEAR > 1980 & fire.sev$BURNSEV == 4 & fire.sev$BEST_ASSES == "YES",]
# fire.sev.high.1980 <- st_buffer(fire.sev.high.1980,0)
# st_write(fire.sev.high.1980,"data/non-synced/existing-datasets/VegBurnSeverity_shp/veg_burn_severity_high_1980.shp",delete_dsn=TRUE)

fire.sev <- fire.sev[fire.sev$VB_ID %in% fires.focal.names,]
fire.sev <- fire.sev[fire.sev$BEST_ASSES == "YES",]

#disabled using all fire severities (this was only for distance to seed source):
#fire.sev.high.1980 <- st_read("data/non-synced/existing-datasets/VegBurnSeverity_shp/veg_burn_severity_high_1980.shp")

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


## set distances in and out
distin = 35
distout = 40
dist.apart = (distin + distout) * 1.7


# Buffer out the planting units by 0.5 m so they merge back together
planting.zone <- st_buffer(planting.slices,dist=1)
planting.zone <- st_union(planting.zone)


## Perimeter points

# Buffer in by 25 m and out by 25 m and place points along the resulting perimeters to establish the candidate set of "treated" and "control" plots

# treated
treated.plot.perim <- st_buffer(planting.zone,dist=-distin)
treated.plot.perim <- st_set_crs(treated.plot.perim,3310)
precast.crs <- st_crs(treated.plot.perim)
precast.precision <- st_precision(treated.plot.perim)
treated.plot.perim <- st_cast(treated.plot.perim,"MULTILINESTRING")
st_crs(treated.plot.perim) <- precast.crs
st_precision(treated.plot.perim) <- precast.precision
treated.perim.length <- st_length(treated.plot.perim) %>% sum() %>% as.numeric()
treated.plot.perim <- as(treated.plot.perim,"Spatial")
trt.candidate.plots <- spsample(treated.plot.perim,n=treated.perim.length/100,type="regular")
trt.candidate.plots <- as(trt.candidate.plots,"sf")

# treated dummy
treated.plot.perim <- st_buffer(planting.zone,dist=-distin)
treated.plot.perim <- st_set_crs(treated.plot.perim,3310)
precast.crs <- st_crs(treated.plot.perim)
precast.precision <- st_precision(treated.plot.perim)
treated.plot.perim <- st_cast(treated.plot.perim,"MULTILINESTRING")
st_crs(treated.plot.perim) <- precast.crs
st_precision(treated.plot.perim) <- precast.precision
treated.perim.length <- st_length(treated.plot.perim) %>% sum() %>% as.numeric()
treated.plot.perim <- as(treated.plot.perim,"Spatial")
trt.candidate.plots.dummy <- spsample(treated.plot.perim,n=treated.perim.length/20,type="regular")
trt.candidate.plots.dummy <- as(trt.candidate.plots.dummy,"sf")


# control
control.plot.perim <- st_buffer(planting.zone,dist=distout)
precast.crs <- st_crs(control.plot.perim)
precast.precision <- st_precision(control.plot.perim)
control.plot.perim <- st_cast(control.plot.perim,"MULTILINESTRING")
st_crs(control.plot.perim) <- precast.crs
st_precision(control.plot.perim) <- precast.precision
control.perim.length <- st_length(control.plot.perim) %>% sum() %>% as.numeric()
control.plot.perim <- as(control.plot.perim,"Spatial")
ctrl.candidate.plots <- spsample(control.plot.perim,n=control.perim.length/20,type="regular")
ctrl.candidate.plots <- as(ctrl.candidate.plots,"sf")

## Internal points
# internal.poly <- st_buffer(planting.zone,dist=-60)
internal.poly <- st_buffer(planting.slices,dist=-40)
# Must do fire-by-fire because it's too slow to do all at once
internal.candidate.plots.list <- list()
for(fire in fires.focal.names) {
  
  fire.focal.curr <- fires.focal[fires.focal$VB_ID==fire,]
  plt.zone.curr <- st_intersection(internal.poly,fire.focal.curr)
  plt.zone.curr <- st_union(plt.zone.curr)
  area <- st_area(plt.zone.curr)
  npts <- as.numeric(area)/(150*150) #200 m between points
  plt.zone.curr.sp <- as(plt.zone.curr,"Spatial")
  internal.candidate.plots.focal <- spsample(plt.zone.curr.sp,n=npts,type="regular")
  internal.candidate.plots.focal <- as(internal.candidate.plots.focal,"sf")
  internal.candidate.plots.list[[fire]] <- internal.candidate.plots.focal
  
  
}

int.candidate.plots<- do.call(rbind,internal.candidate.plots.list)


# st_write(internal.poly,"../internal.shp",delete_dsn=TRUE)
# st_write(planting.zone,"../planting.shp",delete_dsn=TRUE)
# st_write(int.candidate.plots,"../internal_plots.shp",delete_dsn=TRUE)

trt.candidate.plots$type <- "treatment"
trt.candidate.plots.dummy$type <- "treatment"
ctrl.candidate.plots$type <- "control"
int.candidate.plots$type <- "internal"


candidate.plots.pre.pre <- rbind(trt.candidate.plots.dummy,trt.candidate.plots)
candidate.plots.pre <- rbind(candidate.plots.pre.pre,ctrl.candidate.plots)
candidate.plots <- rbind(candidate.plots.pre,int.candidate.plots)
candidate.plots$id <- seq(1,nrow(candidate.plots))

dummy.ids <- 1:nrow(trt.candidate.plots.dummy) # because the dummy plots were first

# 
# 
# ## around each treated and control plot, place 10 plots 40 m out to evalute whether surroundings are comparable
# candidate.buffer <- st_buffer(candidate.plots,40) %>%st_cast("MULTILINESTRING")
# buffer.length <- st_length(candidate.buffer) %>% sum() %>% as.numeric()
# candidate.buffer <- as(candidate.buffer,"Spatial")
# candidate.surr <- spsample(candidate.buffer,n=buffer.length/25,type="regular") # one plot every 25 meters
# candidate.surr <- as(candidate.surr, "sf") %>% st_buffer(1) # buffer by a tiny bit so it definitely overlaps the line
# candidate.buffer <- as(candidate.buffer,"sf")
# # assign the main plot ids to the surrounding plots
# a <- st_intersects(candidate.surr,candidate.buffer,1)
# 
# 
# a <- st_intersection(candidate.surr,candidate.buffer)
# candidate.surr <- a %>%
#   #rename("main.plot.id" = "id") %>%
#   mutate(type = "surrounding")
# 
# candidate.plots <- rbind(candidate.plots,candidate.surr)


#### Compile attributes of candidate plots ####


# Optional, not implemented: Remove candidate control plots that are in an area with any FACTS management history (beyond planting)

# Find the most recent focal fire that each plot burned in so we can make sure treated and control are from the same fire
fire.intersect <- st_intersects(candidate.plots,fires.focal)
fires.focal.data <- as.data.frame(fires.focal) # remove spatial data to speed up the following function
fires.focal.data <- fires.focal.data %>% dplyr::select(-geometry)

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





# Get slope, aspect, elevation, of all control and treated and internal plots
candidate.plots <- as(candidate.plots,"Spatial") # change the plots to SpatialPointsDF because raster package doesn't work with them yet
candidate.plots$slope <- raster::extract(slope.aspect[["slope"]],candidate.plots)
candidate.plots$aspect <- raster::extract(slope.aspect[["aspect"]],candidate.plots)
candidate.plots$elev <- raster::extract(dem,candidate.plots)

candidate.plots$northness <- cos(deg2rad(candidate.plots$aspect))


# Get the name of the forest each plot falls on (or NA for no forest)
candidate.plots <- as(candidate.plots,"sf")
ownership_intersects <- st_intersects(candidate.plots,ownership)
ownership.intersect.first <- map_int(ownership_intersects,1,.default=NA) # get the first element of each list element (first public land unit it intersects--there should be only 1 but do it this way to be safe
candidate.plots$forest <- ownership[ownership.intersect.first,]$LABEL_NAME




# Get the severity of the most recent overlapping focal fire(s) -- that is the fire that prompted the planting
#!!!!!!!!! Need to reconcile this with needing plots to be close to seed sources !!!!! As written currently, plots will be >= 25 from non-high severity
# 
# #candidate.plots.buffer <- st_buffer(candidate.plots,15) # buffer out for 40m to make sure it's high-severity in the entire area surrounding
# fire.sev.buffer <- st_buffer(fire.sev,25) # buffer fire severity out for 15 m in case there were inaccuracies in measuring fire severity
# sev.intersect <- st_intersects(c2,fire.sev.buffer)
# rm(fire.sev.buffer)
# rm(candidate.plots.buffer)
# 
# #! export fire sev buffer to make sure it worked
# 
# fire.sev.data <- as.data.frame(fire.sev) # remove spatial data to speed up the following function
# fire.sev.data <- fire.sev.data %>% dplyr::select(-geometry)
# 
# get_mostrecent_severity <- function(x) { # function to get the severities of the overlapping severity layers. if multiple overlapping, concatenate into a string listing all.
#   
#   overlaps <- x
#   fire.sev.overlap <- fire.sev.data[overlaps,]
#   overlap.years <- fire.sev.overlap$FIRE_YEAR
#   max.yr.index <- which(overlap.years == max(overlap.years)) # indices of all overlap fires that were the most recent focal fire
#   severities <- fire.sev.overlap[max.yr.index,"BURNSEV"]
#   severity.mostrecent <- min(severities) # in case there was more than one severity within the buffered area around each plot, take the minimum so we can later filter to plots for which the entire surrounding area was > X severity. Note that this can be foiled if there was more than one fire that burned over a given plot in the most-recent fire year (e.g. one fire burned at high severity and the second burned at low severity and then the site was planted). But that should be a very small number of candidate plots, and this approach conservatively excludes those areas
#   
#   return(severity.mostrecent)
#   
# }
# 
# most.recent.focal.fire.sev <- lapply(sev.intersect,FUN=get_mostrecent_severity)
# candidate.plots$focal.fire.sev <- unlist(most.recent.focal.fire.sev)



# get fire history (in one alphabetized concatenated string) -- not just of focal fires -- to make sure that treated and control plots have same fire history
fire.perim.intersect <- st_intersects(candidate.plots,fires)

fires.data <- as.data.frame(fires) # remove spatial data to speed up the following function
fires.data <- fires.data %>% dplyr::select(-geometry)

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

candidate.plots.backup2 <- candidate.plots


# get all management that happened in planted and control plots (one concatenated string); one purpose is to later see if most control plots had management of some sort--need to decide if that shoudl be included or excluded
facts.all.intersect <- st_intersects(candidate.plots,facts.all.fires)

#facts.data <- as.data.frame(facts.all.fires) # remove spatial data to speed up the following function
facts.data <- facts.all.fires %>% st_drop_geometry()

chem.release <- c("Manual Chemical","Chemical")

get_all_facts_management <- function(x,type) { # function to get the VB_ID of all overlapping historical fires (since 1984) to make sure that treated and control plots have the same fire history
  overlaps <- x
  facts.data.overlaps <- facts.data[overlaps,] # select management that's overlapping
  
  # remove cert. of natural regen. because it is not active management
  facts.data.overlaps <- facts.data.overlaps[facts.data.overlaps$ACTIV != "Certification of Natural Regeneration without Site Prep",]
  
  

  completed_date <- facts.data.overlaps[,"DATE_C"]
  completed_date[which(is.na(completed_date))] <- "nodate"
  order.by.date <- order(completed_date)
  facts.data.overlaps <- facts.data.overlaps[order.by.date,]

  completed_date <- facts.data.overlaps[,"DATE_C"]    
  activity <- facts.data.overlaps[,"ACTIV"]
  fire.year <- facts.data.overlaps[,"FIRE_YEAR"]

  completed_year <- substr(completed_date,1,4)
  completed_year[which(completed_year == "nodate")] <- "1800" # assume activities with no date were from very early
  completed_date[which(completed_date == "nodate")] <- "1800-01-01" # assume activities with no date were from very early
  completed_year[which(completed_year == "noda")] <- "1800" # assume activities with no date were from very early
  completed_year <- as.numeric(completed_year)
  fire.year <- as.numeric(fire.year)
  
  fire.year.single <- max(fire.year)
  
  act.date <- paste(activity,completed_date,sep="-")
  act.date.concatenate <- paste(act.date,collapse="; ")
  
  prefire.activity.indeces <- which(completed_year < fire.year)
  postfire.activity.indeces <- which(completed_year > fire.year)
  
  prefire.act.date <- act.date[prefire.activity.indeces]
  prefire.act.date.concatenate <- paste(prefire.act.date,collapse="; ")
  
  postfire.act.date <- act.date[postfire.activity.indeces]
  postfire.act.date.concatenate <- paste(postfire.act.date,collapse="; ")
  
  facts.data.overlaps$fire.year <- fire.year
  facts.data.overlaps$completed.year <- completed_year
  facts.data.overlaps$completed.date <- completed_date
  
  activity.release <- facts.data.overlaps[facts.data.overlaps$ACTIV %in% release & facts.data.overlaps$completed.year > facts.data.overlaps$fire.year,] # select management that's release and that happened after the fire
  release.method.year <- activity.release$METHOD #previously had: paste(activity.release$TREATMENT_,activity.release$METHOD,activity.release$DATE_C,sep="-")
  release.concatenate <- paste(release.method.year,collapse=", ")
  
  #get the first year of planting post-fire
  plant.tree.post <- facts.data.overlaps[facts.data.overlaps$ACTIV == "Plant Trees" & facts.data.overlaps$completed.year > facts.data.overlaps$fire.year,]
  if(nrow(plant.tree.post)==0) {
    first.year.plant <- 2018
    first.date.plant <- 2018-01-01
  } else {
    first.year.plant <- min(as.numeric(plant.tree.post$completed.year))
    first.date.plant <- min(plant.tree.post$completed.date)
  }
  
  #compute years post-planting that release done
  release.method.cat <- ifelse(release.method.year %in% chem.release,"C","M") #classify as chemical (C) or mechanical (M), which is really everything that is not explicitly chemical
  release.year <- as.numeric(substr(activity.release$DATE_C,1,4))
  fire.year <- as.numeric(activity.release$fire.year)
  release.year.post <- release.year-first.year.plant
  release.year.post <- str_pad(release.year.post,width=2,pad="0")
  release.cat.post <- paste(release.year.post,release.method.cat,sep="")
  release.cat.post <- unique(release.cat.post)
  release.summ <- paste(release.cat.post,collapse=",")
  
  
  activity.salvage <- facts.data.overlaps[facts.data.overlaps$ACTIV %in% salvage & facts.data.overlaps$completed.year >= facts.data.overlaps$fire.year,] # select management that's salvage and that happened after the fire
  salvage.method.year <- activity.salvage$METHOD #previously had: paste(activity.release$TREATMENT_,activity.release$METHOD,activity.release$DATE_C,sep="-")
  
  #are there any salvage?
  salvage.bool <- nrow(activity.salvage) > 0
  
  #are any of the salvage methods helicopter?
  heli <- salvage.method.year == "Helicopter"
  heli.logical <- sum(heli) > 0
  salvage.heli <- heli.logical
  salvage.heli <- ifelse(salvage.heli,"YES","no")
  
  #if(nrow(facts.data.overlaps) > 0 && facts.data.overlaps$ACTIV %in% salvage) browser()
  
  # was any of the salvage done after the planting year?
  activity.salvage.postplant <- facts.data.overlaps[facts.data.overlaps$ACTIV %in% salvage & facts.data.overlaps$completed.date >= first.date.plant,] # select management that's salvage and that happened after the planting (planting happened in spring usually, so if salvage was in the same year it was after planting)
  salvage.postplant.bool <- nrow(activity.salvage.postplant) > 0
  
  
  return.var <- list(prefire.management.history=prefire.act.date.concatenate,management.history=act.date.concatenate,postfire.management.history=postfire.act.date.concatenate,
                     postfire.release.methods=release.concatenate,postfire.salvage=salvage.bool,postfire.salvage.heli=salvage.heli,postfire.salvage.postplant=salvage.postplant.bool,post.release.summ=release.summ)
  
  return(return.var)
  
}


# candidate.plots <- candidate.plots %>%
#   dplyr::select(1:14)

facts.management <- lapply(facts.all.intersect,FUN=get_all_facts_management)
q <- do.call(rbind.data.frame,facts.management)
candidate.plots2 <- bind_cols(candidate.plots,q)
candidate.plots <- candidate.plots2

rm(facts.all)
rm(candidate.plots2)
gc()
## store image
save.image("../image_1.Rimg")
#load("../image_1.Rimg")

### Get distance to non-high-sev

## first, make a band of unburned around each fire (in case the nearest non-high-sev area is outside the fire)
fires.focal.buffer <- st_buffer(fires.focal,dist=50)
#now remove the high-severity from all fires in the last 25 years
# and clip the severity layer to the focal fires

# if want to use all fires to get non-high sev (but this ignores that fires that happened later would not have affected seed sources for earlier fires), use this:
#fire.sev.high <- st_intersection(fire.sev.high.1980,fires.focal.buffer)

# otherwise, use this:
fire.sev.high <- fire.sev[fire.sev$BURNSEV == 4,]
fire.sev.high <- st_buffer(fire.sev.high,0)
fire.sev.high.union <- st_union(fire.sev.high)

# in case there was non-high-sev assessment overlapping high-sev, cut out the non-high-sev
fire.non.sev <- fire.sev[fire.sev$BURNSEV < 4,]
fire.non.sev <- st_buffer(fire.non.sev,0)
fire.non.sev.union <- st_union(fire.non.sev)

# need to compute the difference in rgeos because st_difference is not working for some reason
f.s.h.sp <- as(fire.sev.high.union,"Spatial")
f.f.b.sp <- as(fires.focal.buffer,"Spatial")
f.n.h.sp <- gDifference(f.f.b.sp,f.s.h.sp)
fires.non.high <- as(f.n.h.sp,"sf")

#fires.non.high <- st_difference(fires.focal.buffer,fire.sev.high)
#fires.non.high <- st_union(fires.non.high)


# get distance to non-high-sev for each point
plots.dist.non.high <- st_distance(candidate.plots,fires.non.high)
plots.dist.low.mod <- st_distance(candidate.plots,fire.non.sev.union)
candidate.plots$dist.non.high.sev <- plots.dist.non.high
candidate.plots$dist.low.mod <- plots.dist.low.mod
candidate.plots$dist.non.high <- as.numeric(pmin(plots.dist.non.high,plots.dist.low.mod))

#because before, this was not an acceptable data type
candidate.plots$ownership <- as.character(candidate.plots$ownership)
candidate.plots$dist.non.high <- as.numeric(candidate.plots$dist.non.high)
candidate.plots$dist.non.high.sev <- as.numeric(candidate.plots$dist.non.high.sev)
candidate.plots$dist.low.mod <- as.numeric(candidate.plots$dist.low.mod)

# remove plots not in high sev
candidate.plots <- candidate.plots %>%
  filter(dist.non.high > 10)


### Save the unfiltered set of candidate plots to shapefile
st_write(candidate.plots[,],"data/site-selection/output/candidate-plots/candidate_plots_unpaired_unfiltered.gpkg",delete_dsn=TRUE)

## resume here if necessary (but it loses ownership)
#candidate.plots <- st_read("data/site-selection/output/candidate-plots/candidate_plots_unpaired_unfiltered.gpkg")


#### Filter out candidate plots to find those meeting criteria ####

candidate.plots.backup <- candidate.plots

# filter based on severity, ownership, salvage methods
candidate.plots <- candidate.plots %>%
  filter(ownership == "fs") %>% # include only candidate plots that are on (buffered in) FS land
  filter(dist.non.high > 20) # all high severity in and surrounding th eplot
  
# break back into treated and control
trt <- candidate.plots %>% filter(type=="treatment")
ctl <- candidate.plots %>% filter(type=="control")
int <- candidate.plots %>% filter(type=="internal")



trt$index <- 1:nrow(trt)
ctl$index <- 1:nrow(ctl)
int$index <- 1:nrow(int)

# get distances between each treated plot and each control
a <- st_distance(trt,ctl) # takes 1-2 min. maybe do after narrowing

# for each row (treated plot), which columns (control plots) are within x distance
radius <- set_units(dist.apart,m)
close.pairs <- which(a < radius,arr.ind=T)
colnames(close.pairs) <- c("trt","ctl")
close.pairs <- as.data.frame(close.pairs)

trt.data <- as.data.frame(trt) %>% dplyr::select(-geometry)
ctl.data <- as.data.frame(ctl) %>% dplyr::select(-geometry)


#### Identify treated and nearby comparable untreated plots ####
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
                           near(ctl.close$slope,trt.focal$slope,15) &
                           near(ctl.close$elev,trt.focal$elev,100) &
                           (near(ctl.close$northness,trt.focal$northness,0.5) | (min(ctl.close$slope,trt.focal$slope) < 8)) & # either (a) there is little difference in aspect between the paired plots, or (b) at least one of the plots is quite flat (so aspect is not very relevant)
                           ctl.close$prefire.management.history == trt.focal$prefire.management.history & # pre-fire management in control plot same as in planted plot
                           ctl.close$forest == trt.focal$forest &
                           near(ctl.close$dist.non.high,trt.focal$dist.non.high,50) &
                           ((trt.focal$postfire.salvage.heli==ctl.close$postfire.salvage.heli) | (ctl.close$postfire.salvage==FALSE)) # either they are both heli salvage, both not heli salvage, or the control is not salvaged at all
                         ,] # removed a line to exclude control plots with no post-fire management, because want to allow for salvage
  

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
  trt.data[i,"n.close.paired.plots"] <- nrow(ctl.close)
  
}


## if multiple treated plots claim the same control, choose the one that is the closest to its control

# for each control plot ID that is paired with a treatment plot, load all paired treatment plots; flag those that are not the closest

trt.data$not.closest <- "no"

trt.data.no.dummy <- trt.data %>%
  filter(!(id %in% dummy.ids))

paired.ctl.ids <- unique(trt.data$closest.ctl.id)

for(ctl.id in paired.ctl.ids) {

  if(is.na(ctl.id)) next()  
  if(ctl.id == -1) next()

  
  trt.using.ctl <- trt.data[trt.data$closest.ctl.id == ctl.id & !is.na(trt.data$closest.ctl.id),]
  
  ctl.data[ctl.data$id == ctl.id,"n.close.paired.plots"] <- nrow(trt.using.ctl)
  trt.data.no.dummy[trt.data.no.dummy$closest.ctl.id == ctl.id & !is.na(trt.data.no.dummy$closest.ctl.id),"n.similar.adj.trt.plots"] <- nrow(trt.using.ctl)
  
  trt.using.ctl.no.dummy <- trt.data.no.dummy[trt.data.no.dummy$closest.ctl.id == ctl.id & !is.na(trt.data.no.dummy$closest.ctl.id),]
  
  if(nrow(trt.using.ctl.no.dummy) < 2) next()
  
  distances <- trt.using.ctl.no.dummy$closest.ctl.dist
  non.min.dist.index <- which(distances != min(distances))
  non.min.dist.plot.ids <- trt.using.ctl.no.dummy[non.min.dist.index,"id"]
  


  
  # set those plots to "not closest"
  trt.data.no.dummy[trt.data.no.dummy$id %in% non.min.dist.plot.ids,"not.closest"] <- "yes"
  
  
}


trt.backup <- trt

##remove the dummy plots from trt
trt <- trt %>%
  filter(!(id %in% dummy.ids))


trt$ctl.id <- trt.data.no.dummy$closest.ctl.id
trt$ctl.dist <- trt.data.no.dummy$closest.ctl.dist
trt$not.closest <- trt.data.no.dummy$not.closest
trt$ctl.dist <- ifelse(trt$ctl.dist == Inf,999999,trt$ctl.dist)
trt$ctl.id <- ifelse(is.na(trt$ctl.id),-1,trt$ctl.id)
trt$n.similar.adj.trt.plots <- trt.data.no.dummy$n.similar.adj.trt.plots
trt$n.close.paired.plots <- trt.data.no.dummy$n.close.paired.plots


ctl$ctl.id <- -1
ctl$ctl.dist <- 999999
ctl$not.closest <- "n/a"
ctl$n.similar.adj.trt.plots <- NA
ctl$n.close.paired.plots <- NA

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

# merge back in the internal plots (which don't get paired)
#which columns in paired are not in int?
newcols <- setdiff(names(candidate.plots.paired),names(int))
int[,newcols] <- NA # add them
int <- int[,names(candidate.plots.paired)] # order cols the same
candidate.plots.paired <- candidate.plots.paired[,names(int)]
candidate.plots.paired <- rbind(candidate.plots.paired,int)

# remove columns that are meaningul only in filtering code
candidate.plots.paired <- candidate.plots.paired %>% dplyr::select(-index,-ownership,-not.closest)


# add an empty column as a workaround to the fact that QGIS kml exoport needs to use a colum for feature display labels
candidate.plots.paired$label <- " "

#candidate.plots.paired$dist.nonhigh <- ifelse(candidate.plots.paired$dist.non.high < 100,"< 100 m","> 100 m")
candidate.plots.paired[candidate.plots.paired$dist.non.high < 90,"dist.nonhigh"] <- "< 80 m"
candidate.plots.paired[candidate.plots.paired$dist.non.high > 120,"dist.nonhigh"] <- "> 120 m"

candidate.plots.paired = candidate.plots.paired %>%
  mutate(postfire.salvage = ifelse(postfire.salvage,"YES","no"),
         postfire.salvage.postplant = ifelse(postfire.salvage.postplant,"YES","no"))

st_write(candidate.plots.paired,"data/site-selection/output/candidate-plots/candidate_plots_paired.gpkg",delete_dsn=TRUE)


#### Export to other file formats ####
# p <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired.gpkg",stringsAsFactors=FALSE)
# p$name <- p$id
# 
# st_write(p,"data/site-selection/output/candidate-plots/candidate_plots_paired_5.kml",delete_dsn=TRUE)
# 
# 
# p <- dplyr::select(p,c(name))
# p <- st_transform(p,crs=4326)

#st_write(p,"data/site-selection/output/candidate-plots/candidate_plots_paired_6.gpx",driver="GPX",delete_dsn=TRUE) #! write gpx

##
##
##
##
##
#### Explore range of environment and management at paired plots; further summarize them for plot selection ####
#### for narrowing to the most common type of factorial management, and for identifying fires with sufficient environmental variation
##

## summarize the candidate plot pairs: planted-conttrol both salvaged or not, or different? replanted, shrub control, etc?

# Read in candidate plots
p <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired.gpkg",stringsAsFactors=FALSE)

# Set Moonlight and antelope to same name
moontelope <- c("2007ANTELOPE_CMPLX","2007MOONLIGHT")
p <- p %>%
  mutate(fire.name = ifelse(most.recent.focal.fire %in% moontelope,"2007MOONTELOPE",most.recent.focal.fire))

# read in summarized FACTS slices
crs <- 3310 # CA albers
facts.slices <- st_read("data/site-selection/output/aggregated-management-history/shapefiles/management_history_summarized.gpkg",stringsAsFactors = FALSE)
facts.slices <- st_transform(facts.slices,crs=crs)

#give these columns unique names so we know where they came from when we extract values at candidate points
names(facts.slices)[names(facts.slices) != "geom"] <- paste0("f.s.",names(facts.slices)[names(facts.slices) != "geom"])

# load and extract solar radiation data
rad <- raster("data/non-synced/existing-datasets/solar radiation/march_rad.tif")
p$rad <- raster::extract(rad,p,method="bilinear")


## for each treated plot, get the management done and determine whether the control plot was salvaged
p.trt <- p[p$type=="treatment",]
p.trt <- st_intersection(p.trt,facts.slices) # get management done
# set helicopter salvage as not salvaged
p.trt[p.trt$postfire.salvage.heli == "YES","f.s.salvage"] <- "no"


p.int <- p[p$type=="internal",]
p.int <- st_intersection(p.int,facts.slices)
# set helicopter salvage as not salvaged
p.int[p.int$postfire.salvage.heli == "YES","f.s.salvage"] <- "no"



p.ctl <- p[p$type=="control",]
p.ctl.salvage <- st_intersection(p.ctl,facts.slices)

# set helicopter salvage as not salvaged
p.ctl.salvage[p.ctl.salvage$postfire.salvage.heli == "YES","f.s.salvage"] <- "no"

p.ctl.salvage <- p.ctl.salvage %>%
  st_drop_geometry() %>%
  dplyr::select(id,f.s.salvage) %>%
  rename(id.ctl=id,f.s.salvage.ctl=f.s.salvage)

p.ctl <- p.ctl %>%
  st_drop_geometry() %>%
  dplyr::select(id,slope,aspect,elev,northness,rad)
names(p.ctl) <- paste0(names(p.ctl),".ctl")


p.dat <- left_join(p.trt,p.ctl.salvage,by=c("ctl.id" = "id.ctl"))
p.dat <- left_join(p.dat,p.ctl,by=c("ctl.id" = "id.ctl"))

p.dat$f.s.salvage.ctl[is.na(p.dat$f.s.salvage.ctl)] <- "no"


### Salvage cat: trt only, both, none

p.dat$salv.cat <- NA

p.dat$salv.cat[p.dat$f.s.salvage == "yes" & p.dat$f.s.salvage.ctl == "yes"] <- "both"
p.dat$salv.cat[p.dat$f.s.salvage == "yes" & p.dat$f.s.salvage.ctl == "no"] <- "planted"
p.dat$salv.cat[p.dat$f.s.salvage == "no" & p.dat$f.s.salvage.ctl == "no"] <- "neither"
p.dat$salv.cat[p.dat$f.s.salvage == "no" & p.dat$f.s.salvage.ctl == "yes"] <- "control"


salvage.text.w.Post <- paste0(p.dat$salv.cat,"Post")
salvage.text <- p.dat$salv.cat

p.dat <- p.dat %>%
  mutate(salv.cat = ifelse(p.dat$postfire.salvage.postplant == "YES",salvage.text.w.Post,salvage.text))

## all salvage.cat that is neitherPost is because it was heli salvaged post planting
p.dat[p.dat$postfire.salvage.heli == "YES" & p.dat$salv.cat == "neitherPost","salv.cat"] <- "neither"



#add back the internal plots
newcols <- setdiff(names(p.dat),names(p.int)) # which columns were added to the paired plots?
p.int[,newcols] <- NA # add them
p.int <- p.int[,names(p.dat)] # order cols the same
p.dat <- p.dat[,names(p.int)]
p.dat <- rbind(p.dat,p.int)



### remove plots that had reporting discrepancies
###!!! resume here. maybe none on moontelope
p.dat <- p.dat %>%
  mutate(any.discrepancies = f.s.planting.reporting.discrepancy | f.s.salvage.reporting.discrepancy | f.s.release.reporting.discrepancy | f.s.thin.reporting.discrepancy | f.s.prep.reporting.discrepancy)

p.dat <- p.dat %>%
  filter(any.discrepancies == FALSE)



## Summarize: early planting (TF; 0-2), site prepped,  late planting (TF; 3-4), times planted incl. replanted (#), released (y/n), thinned (y/n)

# Compute year first planted
p.dat <- p.dat %>%
  separate(f.s.planting.years.post,"first.pltd.yr",sep=", ",remove=FALSE,extra="drop")

p.dat <- p.dat %>%
  mutate(
    pltd.yr12 = (as.numeric(first.pltd.yr) < 3),
    pltd.yr34 = (as.numeric(first.pltd.yr) >2),
    site.prepped = f.s.prep.n.unique.years > 0,
    released = f.s.release.n.unique.years > 0,
    # released.1x = release.nyears == 1,
    # released.2x = release.nyears == 2,
    # released.morex = release.nyears > 2,
    replanted = ((as.numeric(f.s.replant.n.unique.years) + as.numeric(f.s.planting.n.unique.years)) > 1),
    # replanted.1x = replant.nyears == 1,
    # replanted.morex = replant.nyears == 2,
    # thinned.0x = thin.nyears == 0,
    # thinned.1x = thin.nyears == 1,
    thinned = f.s.thin.n.unique.years > 0)

p.dat$plant.timing <- NA
p.dat$plant.timing[p.dat$pltd.yr12 == TRUE & p.dat$pltd.yr34 == FALSE] <- "early"
p.dat$plant.timing[p.dat$pltd.yr12 == FALSE & p.dat$pltd.yr34 == TRUE] <- "late"

## the important columns are: salv.cat, plant.timing, site.prepped, released, replanted, thinned

# change logical to y/n
p.dat <- p.dat %>%
  mutate_at(vars(site.prepped,released,replanted,thinned),tf_to_yn)

# make fire the name of the fire and the name of the forest
p.dat$fire.dist <- paste(p.dat$fire.name,p.dat$forest,sep=" - ")

# for McNally on the forest and the monument, make it all on the firest
p.dat[p.dat$fire.dist == "2002MCNALLY - Giant Sequoia NM","fire.dist"] <- "2002MCNALLY - Sequoia NF"



##classify release treatment more broadly: at least one release in yrs 1-2, 3-5, 6-12
release.classify <- function(release.code) {
  
  release.sep <- unlist(str_split(release.code,","))
  release.yrs <- str_sub(release.sep,1,-2) %>% as.numeric()
  release.mthd <- str_sub(release.sep,-1,-1)
  
  
  
  yrs.e <- 0:2 # early
  yrs.m <- 3:5 # mid
  yrs.l <- 6:10 # late
  
  yrs.e.txt <- ifelse(any(release.yrs %in% yrs.e),"e","")
  yrs.m.txt <- ifelse(any(release.yrs %in% yrs.m),"m","")
  yrs.l.txt <- ifelse(any(release.yrs %in% yrs.l),"l","")
  
  release.txt <- paste0(yrs.e.txt,yrs.m.txt,yrs.l.txt)
  
  #if chem used on any treatment
  if(any(release.mthd =="C")) {
    release.txt <- paste0(release.txt,"C")
  }
  
  
  if(release.txt == "") {
    release.txt <- "no"
  }
  
  return(release.txt)
  
  
}

p.dat$release.txt <- sapply(p.dat$post.release.summ,FUN=release.classify)


## For internal plots, if they were salvaged, replicate them, once classified as "both" and once classified as "planted". This is so that they appear as comparable plots along all relevant salvage categories of perimeter plots
##!!HERE
p.dat.int.salv.copy <- p.dat %>%
  filter(type == "internal" & f.s.salvage == "yes")
p.dat.int.salv.copy$salv.cat <- "both"

p.dat[p.dat$type=="internal" & p.dat$f.s.salvage == "yes","salv.cat"] <- "planted"
p.dat[p.dat$type=="internal" & p.dat$f.s.salvage == "no","salv.cat"] <- "neither"
p.dat <- rbind(p.dat,p.dat.int.salv.copy)

## remove internal plots that are close to seed source
p.dat <- p.dat %>%
  filter(p.dat$type != "internal" | p.dat$dist.non.high > 120)


### need to tag all internal plots as postplanting salvaged if applicable
salvage.text.w.Post <- paste0(p.dat$salv.cat,"Post")
salvage.text <- p.dat$salv.cat

p.dat <- p.dat %>%
  mutate(salv.cat = ifelse(p.dat$postfire.salvage.postplant == "YES" & p.dat$type=="internal",salvage.text.w.Post,salvage.text))

## all salvage.cat that is neitherPost is because it was heli salvaged post planting
p.dat[p.dat$postfire.salvage.heli == "YES" & p.dat$salv.cat == "neitherPost","salv.cat"] <- "neither"



# revalue the important columns to something intelligible
p.dat$fire2 <- p.dat$fire.name
p.dat$fire.dist2 <- p.dat$fire.dist
p.dat$salv.cat2 <- paste0("salv: ",p.dat$salv.cat)
p.dat$plant.timing2 <- paste0("plt: ",p.dat$plant.timing)
p.dat$site.prepped2 <- paste0("prp: ",p.dat$site.prepped)
p.dat$released2 <- paste0("rel: ",p.dat$release.txt)
p.dat$replanted2 <- paste0("replt: ",p.dat$replanted)
p.dat$thinned2 <- paste0("thn: ",p.dat$thinned)
p.dat$heli2 <- paste0("heli: ",p.dat$postfire.salvage.heli)
p.dat <- p.dat %>%
  mutate(mgmt.factorial = paste(fire.dist2,salv.cat2,site.prepped2,released2,thinned2,replanted2,sep=", ")) # make a column with factorial management
  # original: mutate(mgmt.factorial = paste(fire2,salv.cat2,plant.timing2,site.prepped2,released2,replanted2,thinned2,sep=", ")) # make a column with factorial management
  # also removed heli2 because now all helicopter slavage is considered unsalvaged.


#classify as perimeter or internal
p.dat <- p.dat %>%
  mutate(class = ifelse(type=="internal","internal","perimeter"))

# # remove internal plots close to seed source (the whole purpose of them is to provide additional plots far from seed source)
# p.dat <- p.dat %>%
#   filter(dist.non.high > 120 | class == "perimeter")

# for computing if there's enough plots to justify study, only look at perimeter plots close to seed source
# p.dat.close <- p.dat[(p.dat$dist.non.high < 100) & (p.dat$class != "internal"),]
p.dat.close <- p.dat ##!! temp

# 
# 
# ### First-pass summary of breakdown of abundance by fire of: salvage, released, thinned
# p.dat.plt <- p.dat %>%
#   dplyr::filter(type=="treatment") %>%
#   mutate(salv.tf = (f.s.salvage == "yes"),
#          rel.tf = (f.s.release.nyears > 0),
#          replt.tf = (f.s.replant.nyears > 0),
#          thin.tf = (f.s.thin.nyears > 0))
# 
# p.dat.plt.summ <- p.dat.plt %>%
#   st_drop_geometry %>%
#   group_by(fire.dist2) %>%
#   summarize(salv.perc = sum(salv.tf)/n(),
#             replt.perc = sum(replt.tf)/n(),
#             rel.perc = sum(rel.tf)/n(),
#             thin.perc = sum(thin.tf)/n())
# 
# prop_to_perc <- function(x) {
#   return(round(x*100))
# }
# 
# p.dat.plt.summ <- p.dat.plt.summ %>%
#   mutate_at(.vars=vars(salv.perc,rel.perc,replt.perc,thin.perc),
#             .funs = prop_to_perc)
# 
# 
# write.csv(p.dat.plt.summ,row.names=FALSE,"data/site-selection/output/candidate-plots/plot_summ_for_workshop.csv")
# 
# 
# ## resuming main code
# 

# 
# 
# ### temp testing of mcnally: why are so many getting dropped
# p.inspect <- p.dat.close %>%
#   filter(fire.dist == "2002MCNALLY - Sequoia NF")
# 


## OK, now for each fire, sum the number of perimeter plots in each factorial combination of each of the important treatment columns
# only consider perimeter plots when tallying if there are enough
p.dat.agg <- p.dat.close %>%
  st_drop_geometry() %>%
  #filter(class=="perimeter") %>%
  # formerly before reduced number of factorial vars: group_by(fire2,salv.cat2,plant.timing2,site.prepped2,released2,replanted2,thinned2) %>%
  group_by(fire.dist2,salv.cat2,site.prepped2,released2,thinned2,replanted2) %>%
  #! could we just do group_by the mgmt.factorial col?
  summarize(nplots = n()) %>%
  arrange(fire.dist2,-nplots) %>%
  # formerly before reduced number of factorial vars: mutate(mgmt.factorial = paste(fire2,salv.cat2,plant.timing2,site.prepped2,released2,replanted2,thinned2,sep=", ")) %>% # make a column with factorial management
  mutate(mgmt.factorial = paste(fire.dist2,salv.cat2,site.prepped2,released2,thinned2,replanted2,sep=", ")) %>% # make a column with factorial management
  ungroup()
  #! removed heli2 from mgmt factorical because considering heli salvage to be unsalvaged

  
# keep only the ones with enough candidate plots
p.dat.agg.many <- p.dat.agg[p.dat.agg$nplots >= 5,] %>% 
  as.data.frame()

## save to csv
write.csv(p.dat.agg.many,"data/site-selection/output/candidate-plots/candidate_plots_management_stratification_v3allrelease.csv",row.names=FALSE)

# save as an HTML widget
path <- file.path(getwd(),"data/site-selection/output/candidate-plots/","candidate_plots_management_stratification_v4allrelease.html")
datatable(p.dat.agg.many,options=list(pageLength=100)) %>%
  saveWidget(file=path)

# filter the full plot database to only those fires and factorial management categories that have enough member plots
p.dat.many <- p.dat[p.dat$mgmt.factorial %in% p.dat.agg.many$mgmt.factorial,]
#p.dat.many <- p.dat

# make some variables better for plotting
p.dat.many$mgmt.factorial.nofire <- str_split(p.dat.many$mgmt.factorial,", ",n=2) %>%
  map_chr(2)

p.dat.many <- p.dat.many %>%
  mutate(yr.pltd = as.numeric(first.pltd.yr)) %>%
  mutate(yr.pltd = ifelse(yr.pltd > 4,4,yr.pltd)) %>%
  mutate(yr.pltd = as.character(yr.pltd)) %>%
  mutate(yr.pltd = ifelse(yr.pltd == "4","4+",yr.pltd))



# need to restore the control plots back to the filtered candidate plots
p.ctl <- p[p$type=="control",]
p.ctl.match <- p.ctl[p.ctl$id %in% p.dat.many$ctl.id,]
newcols <- setdiff(names(p.dat.many),names(p.ctl.match)) # which columns were added to the paired plots?
p.ctl.match[,newcols] <- NA # add them
p.dat.many <- p.dat.many[,names(p.ctl.match)] # order cols the same
p.ctl.match <- p.ctl.match[,names(p.dat.many)]
p.dat.many.w.ctl <- rbind(p.dat.many,p.ctl.match)

# write the filtered candidate plot dataset
st_write(p.dat.many.w.ctl,"data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_all_lesswide_v1.gpkg",delete_dsn=TRUE)
  
## Plot environmental range of each factorial management type
mgmt.cats <- unique(p.dat.many$mgmt.factorial)
fires <- unique(p.dat.many$fire.dist2)

plts <- list()

yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")

p.plot <- p.dat.many

##make the management category text have a newline (after the 2nd comma)
comma.locs <- str_locate(p.plot$mgmt.factorial.nofire,fixed(" thn:"))
p.plot <- p.plot %>%
  dplyr::mutate(splitpos = str_locate(mgmt.factorial.nofire,fixed(" rel:"))[,"start"]) %>%
  dplyr::mutate(first.part = str_sub(mgmt.factorial.nofire,1,splitpos),
         second.part = str_sub(mgmt.factorial.nofire,splitpos,-1)) %>%
  dplyr::mutate(mgmt.w.newline = paste0(first.part,"\n",second.part))

#remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh)
p.plot <- p.plot %>%
  filter(!is.na(dist.nonhigh))

# Remove plots that are from incomplete planting slices (had other management overlapping a portion of the planting unit)
p.plot <- p.plot %>%
  filter(f.s.planting.slice.split == "no")

# Remove plots that are from roadside salvage+planting stringers
p.plot <- p.plot %>%
  filter(f.s.stringer == "no")
# 
# 
# d.perim.check <- d.perim %>%
#   dplyr::select(elev,rad,yr.pltd,dist.nonhigh,fire.dist2,mgmt.w.newline)

# ## temporary: because McNally has no rad, make up values (which also allow for differentiation of dist to seed tree)
# p.plot[p.plot$fire.name == "2002MCNALLY" & p.plot$dist.nonhigh == "< 80 m","rad"] <- 6000
# p.plot[p.plot$fire.name == "2002MCNALLY" & p.plot$dist.nonhigh == "> 120 m","rad"] <- 6500



plts <- NULL
for(i in 1:length(fires)) {
  
  fire <- fires[i]
    
    type <- mgmt.cats[i]
    d <- p.plot[p.plot$fire.dist2 == fire,]
    
    if(nrow(d) < 10) next()
    
    d.perim <- d[d$class=="perimeter",]
    d.int.close <- d[d$class=="internal" & d$dist.nonhigh == "< 80 m",]
    d.int.far <- d[d$class=="internal" & d$dist.nonhigh == "> 120 m",]
    
    #! would like to add a symbology (shape? for whether it was replanted)
    plts[[i]] <- ggplot(d.perim,aes(x=elev,y=rad,color=yr.pltd,shape=dist.nonhigh)) +
      geom_point(data=d.int.close,shape=18,size=1) +
      geom_point(data=d.int.far,shape=3,size=0.7) +
      geom_point(size=1.5) +
      ggtitle(d[1,]$fire.dist2) +
      theme_bw(8) +
      theme(plot.title = element_text(size=8)) +
      facet_wrap(~mgmt.w.newline) +
      scale_shape_manual(values=c(16,1)) +
      scale_colour_manual(values=yr.colors) +
      theme(strip.text.x = element_text(size = 8)) +
      labs(color="Yr planted",shape="Seed dist")
  

}



pdf("data/site-selection/output/candidate-plots/stratification_v23_all_lesswide.pdf")
for(i in seq_along(plts)) {
  print(plts[[i]])
}
dev.off()


### Now explore on each fire what the major treatment (release) types were

## for each row, replicate it with each entry in f.s.release.methods as a separate row
d.simp <- p.dat.many %>%
  dplyr::select(fire.dist2,f.s.release.methods) %>%
  st_drop_geometry

d.simp.long <- d.simp %>%
  mutate(f.s.release.methods = strsplit(as.character(f.s.release.methods),", ")) %>%
  unnest(f.s.release.methods)

release.method.summary <- d.simp.long %>%
  group_by(fire.dist2,f.s.release.methods) %>%
  summarize(frequency = n()) %>%
  ungroup() %>%
  arrange(fire.dist2,-frequency)

names(release.method.summary) <- c("fire.forest","release.method","frequency")

write.csv(release.method.summary,"data/site-selection/output/candidate-plots/candidate_plots_release_methods_summary.csv",row.names=FALSE)

# save as an HTML widget
path <- file.path(getwd(),"data/site-selection/output/candidate-plots/","candidate_plots_release_methods_summary.html")
datatable(release.method.summary,options=list(pageLength=100)) %>%
  saveWidget(file=path)

