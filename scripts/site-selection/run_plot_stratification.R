setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)
library(readODS)

source("scripts/site-selection/plot_stratification_functions_geo.R")

#### 0. Load overall data ####

## candidate plots
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered.gpkg",stringsAsFactors=FALSE)

## plots to avoid
suids_exclude <- read_ods("data/site-selection/analysis-parameters/planting_unit_preferences_a_priori.ods",col_names=TRUE) %>%
  dplyr::filter(exclude_include == "e") %>%
  select(suid)



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



#### 1.a. Moontelope planted yr 1 close to seed source ####

## identify default plots to avoid
default.plots <- NULL  ##!!! need to expand this
foc.default.geocells <- NULL ##!!! need to expand this to determine which geocells the default plots fall into

## specify plots for which to target matching geocells
geocell.scoping.plots <- NULL

## determine the env. quads
rad.cats <- 2
elev.cats <- 3
subquads.goal <- 2  ## this is for how many subquads to try to fill

env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
sub.quads.df <- define.subquads(env.quads)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 1 &
           type == "treatment" &
           dist.non.high < 80)


# select plots

source("scripts/site-selection/plot_stratification_geo.R")

# now we have selected.plots for this mgmt.type
all.selected.plots <- selected.plots




#### 1.b. Moontelope planted yr 3 close to seed source ####

## identify default plots to avoid
default.plots <- NULL  ##!!! need to expand this
foc.default.geocells <- NULL ##!!! need to expand this to determine which geocells the default plots fall into

## specify plots for which to target matching geocells
geocell.scoping.plots <- NULL


## determine the env. quads
rad.cats <- 2
elev.cats <- 3
subquads.goal <- 2  ## this is for how many subquads to try to fill

env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
sub.quads.df <- define.subquads(env.quads)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 3 &
           type == "treatment" &
           dist.non.high < 80)


# select plots

source("scripts/site-selection/plot_stratification_geo.R")
# now we have selected.plots for this mgmt.type


all.selected.plots <- rbind(all.selected.plots,selected.plots)





#### 1.c. Moontelope planted yr 1 far from seed source ####

## identify default plots to avoid
default.plots <- NULL  ##!!! need to expand this
foc.default.geocells <- NULL ##!!! need to expand this to determine which geocells the default plots fall into


## specify plots for which to target matching geocells
geocell.scoping.plots <- all.selected.plots




## determine the env. quads
rad.cats <- 2
elev.cats <- 2
subquads.goal <- 2  ## this is for how many subquads to try to fill

env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
sub.quads.df <- define.subquads(env.quads)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 3 &
           type == "treatment" &
           dist.non.high > 120)


# select plots

source("scripts/site-selection/plot_stratification_geo.R")

# now we have selected.plots for this mgmt.type



all.selected.plots <- rbind(all.selected.plots,selected.plots)

st_write(all.selected.plots,"../test_selected_strat_plots.gpkg",delete_dsn=TRUE)


### plot all the plots for this management type on this fire: but first tier only ###
d <- all.selected.plots %>%
  filter(rank == 1)

##make the management category text have a newline (after the 2nd comma)
comma.locs <- str_locate(d$mgmt.factorial.nofire,fixed(" thn:"))
d <- d %>%
  dplyr::mutate(splitpos = str_locate(mgmt.factorial.nofire,fixed(" rel:"))[,"start"]) %>%
  dplyr::mutate(first.part = str_sub(mgmt.factorial.nofire,1,splitpos),
                second.part = str_sub(mgmt.factorial.nofire,splitpos,-1)) %>%
  dplyr::mutate(mgmt.w.newline = paste0(first.part,"\n",second.part))


d.perim <- d[d$class=="perimeter",]
d.int.close <- d[d$class=="internal" & d$dist.nonhigh == "< 80 m",]
d.int.far <- d[d$class=="internal" & d$dist.nonhigh == "> 120 m",]

#! would like to add a symbology (shape? for whether it was replanted)
g <- ggplot(d.perim,aes(x=elev,y=rad,color=yr.pltd,shape=dist.nonhigh)) +
  #geom_point(data=d.int.close,shape=18,size=1) +
  #geom_point(data=d.int.far,shape=3,size=0.7) +
  geom_point(size=2.5) +
  ggtitle(d[1,]$fire.dist2) +
  theme_bw(12) +
  theme(plot.title = element_text(size=8)) +
  facet_wrap(~mgmt.w.newline) +
  scale_shape_manual(values=c(16,1)) +
  scale_colour_manual(values=yr.colors) +
  theme(strip.text.x = element_text(size = 8)) +
  labs(color="Yr planted",shape="Seed dist")

print(g)



setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)
library(readODS)

source("scripts/site-selection/plot_stratification_functions_geo.R")





#### 2. For Power Fire ####

## no management except early release ##
## All plots close to seed source, randomly half far from seed source, and all internal ##

d.trt <- d.full[d.full$type %in% c("treatment","internal"),] ## remove the paired "control" plots because they do not have the environemntal data associated with them

#remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh) (this is only removing internal plots, as perimeter plots were already removed earlier in the script)
# and which had roadsode salvage stringers
d.trt <- d.trt %>%
  filter(!is.na(dist.nonhigh)) %>%
  filter(f.s.stringer == "no")


### Determine the env. range for which to get data ###

## Specify fire and management of interest ##

foc.fire.name <- "2004POWER - Eldorado NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "e"
foc.replanted <- "no"
foc.thinned <- "no"
foc.yrs.pltd <- c("3")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)

# define study range using only plots near seed source
d.foc.close <- d.foc.overall %>%
  filter(dist.non.high < 80 | type == "internal")

d.foc.far <- d.foc.overall %>%
  filter(dist.non.high > 120 & type == "treatment")

d.foc.all <- rbind(d.foc.close,d.foc.far)

all.selected.plots.power <- d.foc.all


## make half of the far-from-seed-source plots second-priority because they are so close together
second.tier.plots <- c(8974,8972,8970,8979,8968,8957)

all.selected.plots.power <- all.selected.plots.power %>%
  mutate(rank = case_when(id %in% second.tier.plots ~ 2,
                              !(id %in% second.tier.plots) ~1))




st_write(all.selected.plots.power,"../selected_plots_power.gpkg",delete_dsn=TRUE)
