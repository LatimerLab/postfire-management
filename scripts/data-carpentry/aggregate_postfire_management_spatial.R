setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)
library(rgeos)
library(sp)
library(rgdal)
library(raster)


planting <- c("Plant Trees")
salvage <- c("Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (EA/RH/FH)","Patch Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)","Shelterwood Removal Cut (EA/NRH/FH)")
prep <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging","Site Preparation for Planting - Mechanical","Site Preparation for Planting - Manual","Site Preparation for Planting - Burning","Site Preparation for Planting - Other","Site Preparation for Natural Regeneration - Manual","Site Preparation for Natural Regeneration - Burning","Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels")
release <- c("Tree Release and Weed","Control of Understory Vegetation")
thin <- c("Precommercial Thin","Commercial Thin","Thinning for Hazardous Fuels Reduction","Single-tree Selection Cut (UA/RH/FH)")
replant <- c("Fill-in or Replant Trees")
prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune")
manage.except.plant <- c(salvage,prep,release,thin,replant,prune)
manage <- c(planting,manage.except.plant)

## Load FACTS data
facts <- st_read(dsn = "data/non-synced/existing-datasets/CA_Activity_merged.shp", stringsAsFactors = FALSE)
st_precision(facts) <- 100000 #this seems to translate to about one meter. make number larger for more precision
facts$year <- as.numeric(substr(facts$DATE_COMPL,1,4)) #! or do we want accomplished?
facts$year <- ifelse(is.na(facts$year),3000,facts$year)
facts <- st_transform(facts,crs=3310)
facts$id <- 1:nrow(facts)
facts <- st_buffer(facts,0)

## Load fire data
fires <- st_read(dsn = "data/non-synced/existing-datasets/veg_severity_perimeters16_1.gdb",layer="veg_severity_perimeters",stringsAsFactors = FALSE)
st_precision(fires) <- 1
fires <- st_transform(fires,crs=3310)
st_precision(fires) <- 1
fires <- fires[fires$FIRE_YEAR > 1984,] # only fires since 1984

#optional output list of fires and years
#write.csv(as.data.frame(fires)[,c("FIRE_YEAR","FIRE_NAME","VB_ID","NUM_ASSESS","NIFMID_LNK","AGENCY","ICS_CODE","EDIT_DATE")],"data/output-exploratory/fire-names/veg_severity_perimeters.csv",row.names=FALSE)

fires.focal.names <- c("2007ANTELOPE_CMPLX","2007MOONLIGHT","2001STREAM","2006BOULDER_CMPLX","1989LAYMAN","2008COLD","2012CHIPS","2000STORRIE","2008RICH",
                 "2012READING","1987LOST","2009SUGARLOAF","2002CONE","2004STRAYLOR","2008PIT",
                 "2008GOVERNMENT","2001STAR",
                 "2007RALSTON","1992CLEVELAND","2004FREDS","2004POWER",
                 "1994BIG_CREEK")

fires.focal <- fires[fires$VB_ID %in% fires.focal.names,]




## for each fire, get management history, separately for each area that did not have all the same management applied to it
planting.management <- NULL


for(i in 1:nrow(fires.focal))  {

  fire.focal <- fires.focal[i,]
  year.focal <- as.numeric(fire.focal$FIRE_YEAR)
  fire.name <- fire.focal$FIRE_NAME
  
  cat("\nProcessing management history of fire:",fire.name,"(",i,"of",nrow(fires.focal),")\n")
  
  # remove areas of the fire that burned later
  fires.later <- fires[fires$FIRE_YEAR >= year.focal & fires$VB_ID!=fire.focal$VB_ID,]
  fires.later <- st_combine(fires.later)
  fires.later <- st_buffer(fires.later,0)
  fire.focal <- st_buffer(fire.focal,0)
  fire.focal <- st_difference(fire.focal,fires.later) # take only the part that was not burned later
  
  ## get all facts units overlapping it
  facts.fire <- st_intersection(facts,fire.focal)
  facts.fire <- st_buffer(facts.fire,0)

  facts.fire <- as(facts.fire,"Spatial")
  fire.focal <- as(fire.focal,"Spatial")
  
  setScale(100)
  
  facts.fire <- gBuffer(facts.fire,width=0,byid=TRUE)
  
  ## pull out all FACTS management of interest (defined above)
  facts.fire.management <- facts.fire[facts.fire$ACTIVITY %in% manage,]
  facts.fire.management <- facts.fire.management[facts.fire.management$year >= year.focal,]
  
  ## pull out planting units only
  facts.fire.planting <- facts.fire[facts.fire$ACTIVITY %in% planting,]
  facts.fire.planting <- facts.fire.planting[facts.fire.planting$year >= year.focal,] # must have been planted after fire #! should we also exclude areas that were planted before the fire?
  
  ## pull out all other relevant management (except planting)
  facts.fire.othermanagement <- facts.fire[facts.fire$ACTIVITY %in% manage.except.plant,]
  facts.fire.othermanagement <- facts.fire.othermanagement[facts.fire.othermanagement$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all salvage
  facts.fire.salvage <- facts.fire[facts.fire$ACTIVITY %in% salvage,]
  facts.fire.salvage <- facts.fire.salvage[facts.fire.salvage$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all salvage + planting
  facts.fire.salvage.planting <- facts.fire[facts.fire$ACTIVITY %in% c(planting,salvage),]
  facts.fire.salvage.planting <- facts.fire.salvage.planting[facts.fire.salvage.planting$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all prep
  facts.fire.prep <- facts.fire[facts.fire$ACTIVITY %in% prep,]
  facts.fire.prep <- facts.fire.prep[facts.fire.prep$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all release
  facts.fire.release <- facts.fire[facts.fire$ACTIVITY %in% release,]
  facts.fire.release <- facts.fire.release[facts.fire.release$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all thin
  facts.fire.thin <- facts.fire[facts.fire$ACTIVITY %in% thin,]
  facts.fire.thin <- facts.fire.thin[facts.fire.thin$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all replant
  facts.fire.replant <- facts.fire[facts.fire$ACTIVITY %in% replant,]
  facts.fire.replant <- facts.fire.replant[facts.fire.replant$year >= year.focal,] # management must have occurred the same year as the fire or later
  
  ## pull out all prune
  facts.fire.prune <- facts.fire[facts.fire$ACTIVITY %in% prune,]
  facts.fire.prune <- facts.fire.prune[facts.fire.prune$year >= year.focal,] # management must have occurred the same year as the fire or later
  

  ## split the planting units along the boundaries of the all management plygons (including planting, in case there were multiple overlapping plantings)
  facts.fire.management.lines <- as(facts.fire.management,"SpatialLines")
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines,width=0.1,byid=TRUE)
  facts.fire.planting.union <- gBuffer(facts.fire.salvage.planting,width=0)

  setScale(10)
  
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines.buffer,width=0)
  facts.fire.planting.union <- gBuffer(facts.fire.planting.union,width=0)
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines.buffer,width=0)
  facts.fire.planting.union <- gBuffer(facts.fire.planting.union,width=0)
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines.buffer,width=0)
  facts.fire.planting.union <- gBuffer(facts.fire.planting.union,width=0)
  
  facts.fire.planting.split <- gDifference(facts.fire.planting.union,facts.fire.management.lines.buffer)
  facts.fire.planting.split <- disaggregate(facts.fire.planting.split)
  
  data <- data.frame(slice.id=seq(from=1,to=length(facts.fire.planting.split)))
  facts.fire.planting.split <- SpatialPolygonsDataFrame(facts.fire.planting.split,data=data)
  
  #get rid of small slivers < 10 sq m
  facts.fire.planting.split$area.sqm <- gArea(facts.fire.planting.split,byid=TRUE)
  facts.fire.planting.split <- facts.fire.planting.split[facts.fire.planting.split$area.sqm > 10,]

  pl.spl <- facts.fire.planting.split
  
  
  #remove topology errors
  facts.fire.planting <- gBuffer(facts.fire.planting,width=0,byid=TRUE)
  facts.fire.salvage <- gBuffer(facts.fire.salvage,width=0,byid=TRUE)
  facts.fire.prep <- gBuffer(facts.fire.prep,width=0,byid=TRUE)
  facts.fire.release <- gBuffer(facts.fire.release,width=0,byid=TRUE)
  facts.fire.thin <- gBuffer(facts.fire.thin,width=0,byid=TRUE)
  facts.fire.replant <- gBuffer(facts.fire.replant,width=0,byid=TRUE)
  facts.fire.prune <- gBuffer(facts.fire.prune,width=0,byid=TRUE)
  

  
  for(j in 1:nrow(pl.spl)) {
    
    cat("\r--- Planting unit slice",j,"of",nrow(pl.spl))
    
    planting.slice <- pl.spl[j,]
    
    planting.slice <- gBuffer(planting.slice,width=0)


    # get all overlapping planting units and their associated info
    if(is.null(facts.fire.planting)) planting.over <- NULL else planting.over <- intersect(facts.fire.planting,planting.slice)
    planting.years <- planting.over$year
    planting.years.post <- planting.years - year.focal
    years.order <- order(planting.years.post)
    planting.years.post <- planting.years.post[years.order]
    planting.suids <- planting.over$SUID[years.order]
    planting.methods <- planting.over$METHOD[years.order]
    planting.nyears <- length(planting.years)
    planting.n.unique.years <- length(unique(planting.years))
    
    pl.spl[j,"planting.years.post"] <- paste(planting.years.post,collapse=", ")
    pl.spl[j,"planting.suids"] <- paste(planting.suids,collapse=", ")
    pl.spl[j,"planting.methods"] <- paste(planting.methods,collapse=", ")
    pl.spl[j,"planting.nyears"] <- paste(planting.nyears,collapse=", ")
    pl.spl[j,"planting.n.unique.years"] <- paste(planting.n.unique.years,collapse=", ")
    
    
    # get all overlapping salvage units and their associated info
    

    
    if(is.null(facts.fire.salvage)) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.salvage,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"salvage.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"salvage.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"salvage.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"salvage.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"salvage.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
  
    # get all overlapping prep units and their associated info
    

    
    if(is.null(facts.fire.prep)) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.prep,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"prep.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"prep.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"prep.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"prep.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"prep.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    
    # get all overlapping release units and their associated info
    
    if(is.null(facts.fire.release)) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.release,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"release.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"release.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"release.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"release.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"release.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    
    # get all overlapping thin units and their associated info
    
    if(is.null(facts.fire.thin)) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.thin,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"thin.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"thin.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"thin.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"thin.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"thin.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    
    # get all overlapping replant units and their associated info
    

    if(is.null(facts.fire.replant)) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.replant,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"replant.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"replant.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"replant.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"replant.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"replant.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    
    # get all overlapping prune units and their associated info
    
    if(is.null(facts.fire.prune)) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.prune,planting.slice)
    mgmt.years <- mgmt.over$year
    mgmt.years.post <- mgmt.years - year.focal
    years.order <- order(mgmt.years.post)
    mgmt.years.post <- mgmt.years.post[years.order]
    mgmt.suids <- mgmt.over$SUID[years.order]
    mgmt.methods <- mgmt.over$METHOD[years.order]
    mgmt.nyears <- length(mgmt.years)
    mgmt.n.unique.years <- length(unique(mgmt.years))
    pl.spl[j,"prune.years.post"] <- paste(mgmt.years.post,collapse=", ")
    pl.spl[j,"prune.suids"] <- paste(mgmt.suids,collapse=", ")
    pl.spl[j,"prune.methods"] <- paste(mgmt.methods,collapse=", ")
    pl.spl[j,"prune.nyears"] <- paste(mgmt.nyears,collapse=", ")
    pl.spl[j,"prune.n.unique.years"] <- paste(mgmt.n.unique.years,collapse=", ")
    

  }

  pl.spl$fire.name <- fire.name
  pl.spl$fire.year <- year.focal
  
  
  if(is.null(planting.management)) {
    planting.management <- pl.spl
  } else {
    planting.management <- rbind(planting.management,pl.spl)
  }
  
}


#add an overall ID and write to file

out <- as(planting.management,"sf")
out$id <- paste0(out$fire.year,out$fire.name,out$slice.id)

st_write(out,dsn="data/output-exploratory/aggregated-management-history/shapefiles/management_history.gpkg",driver="GPKG",delete_dsn=TRUE)
st_write(out,dsn="data/output-exploratory/aggregated-management-history/shapefiles/management_history2.shp",driver="ESRI shapefile",delete_dsn=TRUE)

out.nogeom <- as.data.frame(out)[-ncol(out)]
write.csv(out.nogeom,"data/output-exploratory/aggregated-management-history/aggregated_management_history.csv",row.names=FALSE)
