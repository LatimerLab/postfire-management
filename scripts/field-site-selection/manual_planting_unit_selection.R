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

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}



#### Load the data ####
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered.gpkg",stringsAsFactors=FALSE)
d.trt <- d.full[d.full$type %in% c("internal","treatment"),] # remove the paired "control" plots because they do not have the environemntal data associated with them

# FACTS slices
planting.slices <- st_read("data/site-selection/output/aggregated-management-history/shapefiles/management_history.gpkg",stringsAsFactors=FALSE)



#### Perform initial filtering ####
## The first three filters were also done in the plot selection script, prior to plotting the panel plots, but after exporting the filtered dataset.
## It might make more sense to perform this filtering before exporting the filtered dataset in the plot selection script.
##!! Also, in the plot selection script, to determine whether factorial management categories have sufficient plots, plots are counted before these filters are applied. Only the incomplete planting slices and stringers filters are an issue in this way though, because only perimeter plots are used for determining if there are enough plots in a management category.

#remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh) (this is only removing internal plots, as perimeter plots were already removed earlier in the script)
d.trt <- d.trt %>%
  filter(!is.na(dist.nonhigh))
# 
# # Remove plots that are from incomplete planting slices (had other management overlapping a portion of the planting unit)
# d.trt <- d.trt %>%
#   filter(f.s.planting.slice.split == "no")

# Remove plots that are from roadside salvage+planting stringers 
d.trt <- d.trt %>%
  filter(f.s.stringer == "no")


#### Identify Moontelope options ####

d <- d.trt %>%
  filter(fire.name == "2007MOONTELOPE" &
           mgmt.factorial.nofire == "salv: neither, prp: no, rel: no, thn: no, replt: no" &
           first.pltd.yr == 1)



d.perim <- d[d$class=="perimeter",]
d.int.close <- d[d$class=="internal" & d$dist.nonhigh == "< 80 m",]
d.int.far <- d[d$class=="internal" & d$dist.nonhigh == "> 120 m",]

#! would like to add a symbology (shape? for whether it was replanted)
ggplot(d.perim,aes(x=elev,y=rad,color=yr.pltd,shape=dist.nonhigh)) +
  geom_point(data=d.int.close,shape=18,size=4) +
  geom_point(data=d.int.far,shape=6,size=3) +
  geom_point(size=6) +
  ggtitle(d[1,]$fire.dist2) +
  theme_bw(15) +
  theme(plot.title = element_text(size=12)) +
  scale_shape_manual(values=c(16,1)) +
  scale_colour_manual(values=yr.colors) +
  theme(strip.text.x = element_text(size = 12)) +
  labs(color="Yr planted",shape="Seed dist")









#### Temporary: Identify Power focal planting units ####

d.pwr <- d.trt %>%
  filter(most.recent.focal.fire == "2004POWER" &
           mgmt.factorial.nofire == "salv: neither, prp: no, rel: e, thn: no" &
           yr.pltd == 3)

st_write(d.pwr,"../power_focal_plots.gpkg")

pwr.focal.suids <- unique(d.pwr$f.s.first.planting.suid)

# see if there were important follow-up SUIDs
pwr.focal.slices <- planting.slices %>%
  filter(first.planting.suid %in% pwr.focal.suids &
           planting.slice.split == "no")



#### Identify AmRiveR focal planting SUIDs

d.gvt <- d.trt %>%
  filter(most.recent.focal.fire == "2008GOVERNMENT" &
           mgmt.factorial.nofire == "salv: neither, prp: no, rel: e, thn: no" &
           yr.pltd %in% c(1,2) &
           dist.nonhigh == "< 80 m")
gvt.focal.suids <- unique(d.gvt$f.s.first.planting.suid)

# see if there were important follow-up SUIDs
gvt.focal.slices <- planting.slices %>%
  filter(first.planting.suid %in% gvt.focal.suids &
           planting.slice.split == "no")



#### Identify Piute focal planting SUIDs

d.piute <- d.trt %>%
  filter(most.recent.focal.fire == "2008PIUTE" &
           mgmt.factorial.nofire == "salv: neither, prp: no, rel: no, thn: no" &
           yr.pltd %in% c("2","3","4+"))
piute.focal.suids <- unique(d.piute$f.s.first.planting.suid)

d.piute.print <- d.piute %>%
  dplyr::select(id,yr.pltd,elev,rad,dist.non.high,dist.nonhigh) %>%
  st_drop_geometry()

## write these plots as scouting plots
st_write(d.piute,"data/site-selection/output/site-scouting/piute_example_plots.gpkg")


