setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)
library(readODS)

source("scripts/site-selection/plot_stratification_functions_geo.R")

#### 0. Load overall data ####

## candidate plots
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_v2.gpkg",stringsAsFactors=FALSE)

## plots to avoid
suids_exclude <- read_ods("data/site-selection/analysis-parameters/planting_unit_preferences_a_priori.ods",col_names=TRUE) %>%
  dplyr::filter(exclude_include == "e") %>%
  select(suid)



#### 1. For Moonlight / Antelope perimeter plots close to seed source ####

all.selected.plots <- NULL

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

## specify plots for which to target matching geocells
geocell.scoping.plots <- NULL

## specify plots for which to avoid placing new plots that are spatially or environmentally close
existing.plots.avoid <- NULL

## determine the env. quads
rad.cats <- 2
elev.cats <- 3
subquads.goal <- 3  ## this is for how many subquads to try to fill

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

selected.plots$plt.cat = 1 # 1 means: perimeter, no management, restricted env. range
selected.plots$plt.seeddist = "N"


all.selected.plots <- selected.plots




#### 1.b. Moontelope planted yr 3 close to seed source ####

## specify plots for which to target matching geocells
geocell.scoping.plots <- NULL

## specify plots for which to avoid placing new plots that are spatially or environmentally close
existing.plots.avoid <- NULL

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

selected.plots$plt.cat = 1 # 1 means: perimeter, no management, restricted env. range
selected.plots$plt.seeddist = "N"


all.selected.plots <- rbind(all.selected.plots,selected.plots)





#### 1.c. Moontelope planted yr 3 far from seed source ####


## specify plots for which to target matching geocells
geocell.scoping.plots <- all.selected.plots

## specify plots for which to avoid placing new plots that are spatially or environmentally close
existing.plots.avoid <- NULL



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

selected.plots$plt.cat = 1 # 1 means: perimeter, no management, restricted env. range
selected.plots$plt.seeddist = "F"

all.selected.plots <- rbind(all.selected.plots,selected.plots)









#### 1.d. Planted yr 3 full env. range close to seed source ####


### Determine new env. range based only on yr 3 close to seed source
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
d.foc.overall.forenv <- d.foc.overall %>%
  filter(dist.non.high < 80)

focal.env.range <- get.overall.env.range(plt.yrs=foc.yrs.pltd,plots=d.foc.overall.forenv)



## specify plots for which to target matching geocells
geocell.scoping.plots <- all.selected.plots # everything that's already been selected for this fire


## determine the env. quads
rad.cats <- 4
elev.cats <- 4
subquads.goal <- 1  ## this is for how many subquads to try to fill

env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
sub.quads.df <- define.subquads(env.quads)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 3 &
           type == "treatment" &
           dist.non.high < 80)


### Specify already-selected plots for which to not select new plots that are environmentally or spatially close

# Take already-selected plots and filter to the management categories that we're interested in here
# Note that if this is for already-surveyed plots, this would be different: filter the dataset from which those plots were selected, making sure to use only those plots that were actually surveyed
existing.plots.avoid.pre <- all.selected.plots %>%
  filter(fire.dist2 %in% foc.fire.name &
           salv.cat %in% foc.salv.cat &
           site.prepped %in% foc.site.prepped &
           release.txt %in% foc.released &
           replanted %in% foc.replanted &
           thinned %in% foc.thinned &
           yr.pltd %in% foc.yrs.pltd &
           dist.non.high < 80 &
           rank == 1)

existing.plots.avoid.pre <- existing.plots.avoid.pre[rep(seq_len(nrow(existing.plots.avoid.pre)), each=3),]

## selected only rank 1 (which is tier1), but also replicating it 3 times so in plot selection it can count as replacing tiers 1,2,3
existing.plots.avoid <- existing.plots.avoid.pre



# select plots

source("scripts/site-selection/plot_stratification_geo.R")

# now we have selected.plots for this mgmt.type

selected.plots$plt.cat = 2 # 2 means: perimeter, no management, broad env. range
selected.plots$plt.seeddist = "N"

all.selected.plots <- rbind(all.selected.plots,selected.plots)






#### 1.e. Planted yr 3 full env. range far from seed source ####

### Determine the env. range for which to get data ###

## Specify fire and management of interest ##

foc.fire.name <- "2007MOONTELOPE - Plumas NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "no"
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
d.foc.overall.forenv <- d.foc.overall %>%
  filter(dist.non.high < 80)

focal.env.range <- get.overall.env.range(plt.yrs=foc.yrs.pltd,plots=d.foc.overall.forenv)



## specify plots for which to target matching geocells
geocell.scoping.plots <- all.selected.plots # everything that's already been selected for this fire


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


### Specify already-selected plots for which to not select new plots that are environmentally or spatially close

# Take already-selected plots and filter to the management categories that we're interested in here
# Note that if this is for already-surveyed plots, this would be different: filter the dataset from which those plots were selected, making sure to use only those plots that were actually surveyed
existing.plots.avoid.pre <- all.selected.plots %>%
  filter(fire.dist2 %in% foc.fire.name &
           salv.cat %in% foc.salv.cat &
           site.prepped %in% foc.site.prepped &
           release.txt %in% foc.released &
           replanted %in% foc.replanted &
           thinned %in% foc.thinned &
           yr.pltd %in% foc.yrs.pltd &
           dist.non.high > 120 &
           rank == 1)

existing.plots.avoid.pre <- existing.plots.avoid.pre[rep(seq_len(nrow(existing.plots.avoid.pre)), each=3),]

## selected only rank 1 (which is tier1), but also replicating it 3 times so in plot selection it can count as replacing tiers 1,2,3
existing.plots.avoid <- existing.plots.avoid.pre



# select plots

source("scripts/site-selection/plot_stratification_geo.R")

# now we have selected.plots for this mgmt.type

selected.plots$plt.cat = 2 # 2 means: perimeter, no management, broad env. range
selected.plots$plt.seeddist = "F"

all.selected.plots <- rbind(all.selected.plots,selected.plots)








#### 1.f. Moontelope planted yr 3 internal far from seed source full range ####

d.trt <- d.full[d.full$type %in% c("internal"),] ## remove the paired "control" plots because they do not have the environemntal data associated with them

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
d.foc.overall.forenv <- d.foc.overall %>%
  filter(dist.non.high > 120)

focal.env.range <- get.overall.env.range(plt.yrs=foc.yrs.pltd,plots=d.foc.overall.forenv)
# alternative focal.env.range <- data.frame(elev.low=custom.elev.low,elev.high=custom.elev.high,rad.low=custom.rad.low,rad.high=custom.rad.high)





## specify plots for which to target matching geocells
geocell.scoping.plots <- NULL

## specify plots for which to avoid placing new plots that are spatially or environmentally close
existing.plots.avoid <- NULL

## determine the env. quads
rad.cats <- 3
elev.cats <- 3
subquads.goal <- 3  ## this is for how many subquads to try to fill

env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
sub.quads.df <- define.subquads(env.quads)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 3 &
           type == "internal" &
           dist.non.high > 120)


# select plots

source("scripts/site-selection/plot_stratification_geo.R")
# now we have selected.plots for this mgmt.type

selected.plots$plt.cat = 3 # 3 means: internal, no management, broad env. range
selected.plots$plt.seeddist = "F"

all.selected.plots <- rbind(all.selected.plots,selected.plots)




#### 1.Z. Complete compilation of Moontelope plots ####


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

### prepare the plot names
all.selected.plots.w.names = all.selected.plots %>%
  mutate(plot.iter = 1,
         plot.fire = "A",
         plot.cat = plt.cat,
         plot.yr = first.pltd.yr,
         plot.seed.dist = plt.seeddist,
         plot.elev1 = str_sub(subquad.overall.label,start=2,end=2),
         plot.rad1 = str_sub(subquad.overall.label,start=4,end=4),
         plot.elev2 = str_sub(subquad.overall.label,start=7,end=7),
         plot.rad2 = str_sub(subquad.overall.label,start=9,end=9),
         plot.tier = recode(rank,`1`="A",`2`="B",`3`="C"))
### bring in the control plots
d.ctl <- d.full %>%
  filter(type == "control")

## pull in the names of the treated plots they are paired with
d.ctl <- inner_join(d.ctl,
                    all.selected.plots.w.names %>% select(ctl.id,subquad.overall.label:plot.tier) %>% st_drop_geometry(),
                    by=c("id"="ctl.id"))

## find out what vars they have in common to keep all of them
vars.in.common <- intersect(names(all.selected.plots.w.names),names(d.ctl))

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  select(vars.in.common)
d.ctl <- d.ctl %>%
  select(vars.in.common)

all.selected.plots.w.names.w.ctl <- rbind(all.selected.plots.w.names,d.ctl)


all.selected.plots.w.names.w.ctl <- all.selected.plots.w.names.w.ctl %>%
  mutate(plot.type = recode(type,"treatment"="T","internal"="I","control"="C")) %>%
  mutate(plot.name = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1,plot.rad1,plot.elev2,plot.rad2,plot.tier,plot.type))

# reorder columns so plot name comes first
all.selected.plots.w.names.w.ctl <- all.selected.plots.w.names.w.ctl %>%
  select(plot.name,plot.iter:plot.type,everything())


st_write(all.selected.plots.w.names.w.ctl,"data/site-selection/output/selected-plots/moontelope_v1.gpkg",delete_dsn=TRUE)











setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)
library(readODS)

source("scripts/site-selection/plot_stratification_functions_geo.R")





#### 2. For Power Fire ####

all.selected.plots <- NULL

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
