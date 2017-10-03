setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)


planting <- c("Plant Trees")
salvage <- c("Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (EA/RH/FH)","Patch Clearcut (EA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Sanitation Cut","Group Selection Cut (UA/RH/FH)","Overstory Removal Cut (from advanced regeneration) (EA/RH/FH)","Seed-tree Seed Cut (with and without leave trees) (EA/RH/NFH)","Shelterwood Removal Cut (EA/NRH/FH)")
prep <- c("Piling of Fuels, Hand or Machine","Burning of Piled Material","Yarding - Removal of Fuels by Carrying or Dragging","Site Preparation for Planting - Mechanical","Site Preparation for Planting - Manual","Site Preparation for Planting - Burning","Site Preparation for Planting - Other","Site Preparation for Natural Regeneration - Manual","Site Preparation for Natural Regeneration - Burning","Rearrangement of Fuels","Chipping of Fuels","Compacting/Crushing of Fuels")
release <- c("Tree Release and Weed","Control of Understory Vegetation")
thin <- c("Precommercial Thin","Commercial Thin","Thinning for Hazardous Fuels Reduction","Single-tree Selection Cut (UA/RH/FH)")
replant <- c("Fill-in or Replant Trees")
prune <- c("Pruning to Raise Canopy Height and Discourage Crown Fire","Prune")
manage.except.plant <- c(salvage,prep,release,thin,replant,prune)
manage <- c(planting,manage.except.plant)

#! there is also prune






## Load FACTS data
facts <- st_read(dsn = "data/non-synced/existing-datasets/CA_Activity_merged.shp", stringsAsFactors = FALSE)
st_precision(facts) <- 100000 #this seems to translate to about one meter. make number larger for more precision
facts$year <- as.numeric(substr(facts$DATE_COMPL,1,4)) #! or do we want accomplished?
facts$year <- ifelse(is.na(facts$year),3000,facts$year)



#project to CA Albers
facts <- st_transform(facts,crs=3310)
#st_precision(facts) <- 1
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

# fires.focal.names <- c("2007ANTELOPE_CMPLX","2001STREAM","2006BOULDER_CMPLX","1989LAYMAN","2008COLD","2012CHIPS","2000STORRIE","2008RICH",
#                        "2012READING","1987LOST","2009SUGARLOAF","2002CONE","2004STRAYLOR","2008PIT",
#                        "2008GOVERNMENT","2001STAR",
#                        "2007RALSTON","1992CLEVELAND","2004FREDS","2004POWER",
#                        "1994BIG_CREEK")

fires.focal <- fires[fires$VB_ID %in% fires.focal.names,]




## for each fire, get management history, separately for each area that did not have all the same management applied to it
planting.management <- NULL


for(i in 13:nrow(fires.focal))  {

  fire.focal <- fires.focal[i,]
  year.focal <- as.numeric(fire.focal$FIRE_YEAR)
  fire.name <- fire.focal$FIRE_NAME
  
  cat("\nProcessing management history of fire:",fire.name,"(",i,"of",nrow(fires.focal),")\n")
  
  # remove areas of the fire that burned later
  fires.later <- fires[fires$FIRE_YEAR >= year.focal & fires$VB_ID!=fire.focal$VB_ID,]
  fires.later <- st_combine(fires.later)
  fires.later <- st_buffer(fires.later,0)
  fire.focal <- st_difference(fire.focal,fires.later) # take only the part that was not burned later
  
  ## get all facts units overlapping it
  facts.fire <- st_intersection(facts,fire.focal)
  facts.fire <- st_buffer(facts.fire,0)
  
  # 
  # ##testing
  # #write the focal fire
  # st_write(fire.focal,"temp_test/focal_fire.shp")
  # 
  # 
  # ##testing
  # #write the facts clipped to focal fire
  # st_write(facts.fire,"temp_test/focal_facts.shp")
  # 
  
  
  library(rgeos)
  library(sp)
  library(rgdal)
  library(raster)
  
  # facts.fire <- readOGR("temp_test/focal_facts.shp")
  # fire.focal <- readOGR("temp_test/focal_fire.shp")
  # 
  
  facts.fire <- as(facts.fire,"Spatial")
  fire.focal <- as(fire.focal,"Spatial")
  
  
  ## pull out all FACTS management of interest (defined above)
  facts.fire.management <- facts.fire[facts.fire$ACTIVITY %in% manage,]
  facts.fire.management <- facts.fire.management[facts.fire.management$year > year.focal,]
  
  ## pull out planting units only
  facts.fire.planting <- facts.fire[facts.fire$ACTIVITY %in% planting,]
  facts.fire.planting <- facts.fire.planting[facts.fire.planting$year > year.focal,] # must have been planted after fire #! should we also exclude areas that were planted before the fire?
  
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
  
  # ## define points where there was a problem, to remove from management shapefile #! later move to top of code for entire facts shapefile or else this will result in not slicing a polygon where it should be sliced
  # point <- st_multipoint(matrix(c(-32795,85515,
  #                                -32836,85757,
  #                               -56997,248401,
  #                               -55342,248374,
  #                               -55286,248363,
  #                               -55173,248421,
  #                               -55104,248478),
  #                                ncol=2,byrow=TRUE))
  # point <- st_sfc(point,crs=3310)
  # poly <- st_buffer(point,10)
  # 

  setScale(1000)
  
  ## split the planting units along the boundaries of the all management plygons (including planting, in case there were multiple overlapping plantings)
  #facts.fire.management <-gBuffer(facts.fire.management,width=0,byid=TRUE)
  #facts.fire.management <- st_difference(facts.fire.management,poly) # remove trouble area
  facts.fire.management.lines <- as(facts.fire.management,"SpatialLines")
  #facts.fire.management.lines2 <- st_cast(facts.fire.management.lines,"LINESTRING")
  #facts.fire.management.lines <- st_simplify(facts.fire.management.lines,1)
  #facts.fire.management.lines <- st_buffer(facts.fire.management.lines,dist=0) 
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines,width=0.1,byid=TRUE)
  #facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines.buffer,width=0,byid=FALSE)
  #facts.fire.management.lines.buffer <- unionSpatialPolygons(facts.fire.management.lines.buffer,IDs=rep("a",nrow(facts.fire.management.lines.buffer))) # union them all together
  facts.fire.planting.union <- gBuffer(facts.fire.salvage.planting,width=0)
  #!facts.fire.planting.union <- gUnaryUnion(facts.fire.planting.union) # dissolving any adjacent (or overlapping) planting and salvage boundaries, because they are about to be split back out (including by salvage and planting)
  #facts.fire.planting.union <- st_difference(facts.fire.planting.union,poly)
  #facts.fire.planting.union <- st_union(facts.fire.planting.union) # dissolving any adjacent (or overlapping) planting and salvage boundaries, because they are about to be split back out (including by salvage and planting)
  #facts.fire.management.lines.buffer <- st_union(facts.fire.management.lines.buffer)
  
  facts.fire.management.lines.buffer <- gBuffer(facts.fire.management.lines.buffer,width=0)
  facts.fire.planting.union <- gBuffer(facts.fire.planting.union,width=0)
  
  facts.fire.planting.split <- gDifference(facts.fire.planting.union,facts.fire.management.lines.buffer)
  facts.fire.planting.split <- disaggregate(facts.fire.planting.split)
  
  data <- data.frame(slice.id=seq(from=1,to=length(facts.fire.planting.split)))
  facts.fire.planting.split <- SpatialPolygonsDataFrame(facts.fire.planting.split,data=data)
  
  #get rid of small slivers < 10 sq m
  facts.fire.planting.split$area.sqm <- gArea(facts.fire.planting.split,byid=TRUE)
  facts.fire.planting.split <- facts.fire.planting.split[facts.fire.planting.split$area.sqm > 10,]

  
    
  #facts.fire.planting.split <- st_sf(st_cast(facts.fire.planting.split,"POLYGON"))
  pl.spl <- facts.fire.planting.split

  
  
  # 
  # #testing
  # #write the split planting layer
  # st_write(facts.fire.planting.split,"temp_test/facts_split_lookfor_largeone.shp",delete_dsn=TRUE)
  # 
  # 
  # 
  #writeOGR(facts.fire.planting.split,"temp_test",layer="facts_split_lookfor_largeone4.shp",driver="ESRI Shapefile",overwrite_layer=TRUE)
  # 
  # 
  
  for(j in 1:nrow(pl.spl)) {
    
    cat("\r--- Planting unit slice",j,"of",nrow(pl.spl))
    
    planting.slice <- pl.spl[j,]
    #planting.slice <- gBuffer(planting.slice,width=0.1)
    
    # get all overlapping planting units and their associated info
    if(nrow(facts.fire.planting) == 0) planting.over <- NULL else planting.over <- intersect(facts.fire.planting,planting.slice)
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
    
    if(nrow(facts.fire.salvage) == 0) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.salvage,planting.slice)
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
    if(nrow(facts.fire.prep) == 0) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.prep,planting.slice)
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
    if(nrow(facts.fire.release) == 0) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.release,planting.slice)
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
    if(nrow(facts.fire.thin) == 0) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.thin,planting.slice)
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
    if(nrow(facts.fire.replant) == 0) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.replant,planting.slice)
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
    if(nrow(facts.fire.prune) == 0) mgmt.over <- NULL else mgmt.over <- intersect(facts.fire.prune,planting.slice)
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




## get sizes of the slices




fire.planting.split <- st_intersection(facts.fire.planting.split,facts.fire.planting)


##testing
#write the split planting layer
st_write(planting.management,"temp_test/planting_slices_w_attr.shp")

#write the intersection of the split planting layer with the planting layer
st_write(fire.planting.split,"temp_test/planting_split_intersection.shp")

#write the planting layer
st_write(facts.fire.planting,"temp_test/planting.shp")

#write the new one-deep split planting layer
st_write(facts.fire.planting.split,"temp_test/planting_split_single.shp",delete_dsn=TRUE)





# for each split planting unit component, get what overlaps with it

for(j in 1:nrow(facts.fire.planting.split)) {
  
  planting.split.row <- planting.split[]
  
  planting.intersect <- st_intersect(facts.fire.planting,)
  
  
  
  
}
  
















test <- aggregate(facts.fire.management,by = facts.fire.planting.split,FUN=paste)






p1 <- rbind(c(0,0), c(1,0), c(3,2), c(2,4), c(1,4), c(0,0))
pol1 <-st_polygon(list(p1))
p2 <- rbind(c(3,0), c(4,0), c(4,1), c(3,1), c(3,0))
pol2 <-st_polygon(list(p2))
p3 <- rbind(c(4,0), c(4,1), c(5,1), c(5,0),c(4,0))
pol3 <-st_polygon(list(p3))
p4 <- rbind(c(3,3), c(4,2), c(4,3), c(3,3))
pol4 <-st_polygon(list(p4))

d = data.frame(some_attribute = 1:4)
d$geometry = st_sfc(pol1,pol2,pol3,pol4)
df = st_as_sf(d)

df_union <- st_union(df)

df2 <- st_cast(df,"POLYGON")





















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
rd <- rd[rd$FORESTNAME == "Tahoe National Forest",]
rd <- st_transform(rd,crs=3310)
rd <- st_buffer(rd,0)

facts.salvage <- st_intersection(facts.salvage,rd)
facts.planting <- st_intersection(facts.planting,rd)


## For each planted polygon, get all touching salvage polygons from up to 6 years prior to the planting
all.overlap.ids <- NULL #just add first row as an example

for(i in 1:nrow(facts.planting)) {
  
  if(i == 318) next()
  
  facts.planting.polygon <- facts.planting[i,]
  planting.year <- as.numeric(facts.planting.polygon$year)
  salvage.year.range <- (planting.year-10):planting.year #this includes salvage that happened in the same year as planting
  facts.salvage.matchyears <- facts.salvage[facts.salvage$year %in% salvage.year.range,]
  
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

all.overlap.ids <- unique(all.overlap.ids)
overlapping.salvage.polygons <- facts.salvage[facts.salvage$id %in% all.overlap.ids,]


## Save the planting and salvage polygons
st_write(facts.planting,dsn="data/output-exploratory/salvage-overlap-planting/planting.shp",delete_dsn=TRUE)
st_write(overlapping.salvage.polygons,dsn="data/output-exploratory/salvage-overlap-planting/salvage_that_overlaps.shp",delete_dsn=TRUE)
#st_write(facts.salvage,dsn="data/output-exploratory/salvage-overlap-planting/salvage_all.shp") # for testing

## Load them out again

facts.planting <- st_read(dsn="data/output-exploratory/salvage-overlap-planting/planting.shp")

