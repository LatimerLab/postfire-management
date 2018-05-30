setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)

source("scripts/site-selection/plot_stratification_functions_geo.R")

#### 0. Load overall data ####
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered.gpkg",stringsAsFactors=FALSE)





#### 1. For Moonlight / Antelope perimeter plots close to seed source ####

d.trt <- d.full[d.full$type %in% c("treatment"),] ## remove the paired "control" plots because they do not have the environemntal data associated with them

#remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh) (this is only removing internal plots, as perimeter plots were already removed earlier in the script)
# and which had roadsode salvage stringers
d.trt <- d.trt %>%
  filter(!is.na(dist.nonhigh)) %>%
  filter(f.s.stringer == "no")


### Determine the env. range for which to get data ###

## Specify fire and management of interest ##

foc.fire.name <- "2007MOONTELOPE - Plumas NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "no"
foc.replanted <- "no"
foc.thinned <- "no"
foc.yrs.pltd <- c("1","3")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)

# define study range using only plots near seed source
d.foc.overall.forenv <- d.foc.overall %>%
  filter(dist.non.high < 80)

focal.env.range <- get.overall.env.range(plt.yrs=foc.yrs.pltd,plots=d.foc.overall.forenv)
# alternative focal.env.range <- data.frame(elev.low=custom.elev.low,elev.high=custom.elev.high,rad.low=custom.rad.low,rad.high=custom.rad.high)



#### 1.a. Moontelope planted yr 1 ####

## identify default plots to avoid
default.plots <- NULL  ##!!! need to expand this
foc.default.geocells <- NULL ##!!! need to expand this to determine which geocells the default plots fall into


## determine the env. quads
rad.cats <- 2
elev.cats <- 3
subquads.goal <- 2  ## this is for how many subquads to try to fill

env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
sub.quads.df <- define.subquads(env.quads)


## subset to the relevant plots
foc.yr.pltd  <- 1

d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == foc.yr.pltd &
           type == "treatment" &
           dist.non.high < 80)


## define the geocells
geocells_all <- determine_geocells(d.foc.yr,cellsize=5000)

# determine which geocell each plot falls into
d.foc.yr <- st_intersection(d.foc.yr,geocells_all)
#plot_geocells_env(d.foc.yr)

# select plots

source("scripts/site-selection/plot_stratification_geo.R")
# now we have selected.plots for this mgmt.type
all.selected.plots <- selected.plots




#### 1.b. Moontelope planted yr 3 ####

## identify default plots to avoid
default.plots <- NULL  ##!!! need to expand this
foc.default.geocells <- NULL ##!!! need to expand this to determine which geocells the default plots fall into


## determine the env. quads
rad.cats <- 2
elev.cats <- 3
subquads.goal <- 2  ## this is for how many subquads to try to fill

env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
sub.quads.df <- define.subquads(env.quads)


## subset to the relevant plots
foc.yr.pltd  <- 3

d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == foc.yr.pltd &
           type == "treatment" &
           dist.non.high < 80)


## define the geocells
geocells_all <- determine_geocells(d.foc.yr,cellsize=5000)

# determine which geocell each plot falls into
d.foc.yr <- st_intersection(d.foc.yr,geocells_all)
plot_geocells_env(d.foc.yr)

# select plots

source("scripts/site-selection/plot_stratification_geo.R")
# now we have selected.plots for this mgmt.type



all.selected.plots <- rbind(all.selected.plots,selected.plots)





