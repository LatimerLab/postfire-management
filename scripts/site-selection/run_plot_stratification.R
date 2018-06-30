setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)
library(readODS)

source("scripts/site-selection/plot_stratification_functions_geo.R")

#### 0. Load overall data ####

## candidate plots
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_cottonwood_v1.gpkg",stringsAsFactors=FALSE)
# candidate_plots_paired_filtered_moontelope_v3
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_v5_amriv_widespacing_medfirebuff.gpkg",stringsAsFactors=FALSE)
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_all_45m-45m_v1.gpkg",stringsAsFactors=FALSE)


## plots to avoid
suids_exclude <- read_ods("data/site-selection/analysis-parameters/planting_unit_preferences_a_priori.ods",col_names=TRUE) %>%
  dplyr::filter(exclude_include == "e") %>%
  select(suid)

## previous plots (selected plots, some may have been surveyed)
prev.plots <- st_read("data/site-selection/output/selected-plots/moontelope_v2.gpkg",stringsAsFactors=FALSE)
prev.plots.done.ids <- c("A1041T", "A1140T", "A1147T", "A1096T" ,"A1157T" )
prev.plots.done <- prev.plots[prev.plots$plot.id.gps %in% prev.plots.done.ids,]

yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")


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
focal.env.range$rad.high <- 8000


#### 1.a. Moontelope planted yr 1 close to seed source ####

## specify plots for which to target matching geocells
geocell.scoping.plots <- NULL

## specify plots for which to avoid placing new plots that are spatially or environmentally close
existing.plots.avoid <- NULL

min.homogeneity <- 1

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
existing.plots.avoid <- prev.plots.done %>%
  filter(plot.yr == 3)

existing.plots.avoid <- existing.plots.avoid[rep(seq_len(nrow(existing.plots.avoid)), each=3),] # rep 3 times so it can count for all 3 tiers

## determine the env. quads
rad.cats <- 3
elev.cats <- 4
subquads.goal <- 1  ## this is for how many subquads to try to fill

env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
sub.quads.df <- define.subquads(env.quads)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 3 &
           type == "treatment" &
           dist.non.high < 80)


min.homogeneity <- 2




# select plots

source("scripts/site-selection/plot_stratification_geo.R")
# now we have selected.plots for this mgmt.type

selected.plots$plt.cat = 1 # 1 means: perimeter, no management, restricted env. range
selected.plots$plt.seeddist = "N"


all.selected.plots <- rbind(all.selected.plots,selected.plots)





# #### 1.c. Moontelope planted yr 3 far from seed source ####
# 
# 
# ## specify plots for which to target matching geocells
# geocell.scoping.plots <- all.selected.plots
# 
# ## specify plots for which to avoid placing new plots that are spatially or environmentally close
# existing.plots.avoid <- NULL
# 
# 
# 
# ## determine the env. quads
# rad.cats <- 2
# elev.cats <- 2
# subquads.goal <- 2  ## this is for how many subquads to try to fill
# 
# env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
# sub.quads.df <- define.subquads(env.quads)
# 
# 
# ## subset to the relevant plots
# d.foc.yr <- d.foc.overall %>%
#   filter(first.pltd.yr == 3 &
#            type == "treatment" &
#            dist.non.high > 120)
# 
# 
# # select plots
# 
# source("scripts/site-selection/plot_stratification_geo.R")
# 
# # now we have selected.plots for this mgmt.type
# 
# selected.plots$plt.cat = 1 # 1 means: perimeter, no management, restricted env. range
# selected.plots$plt.seeddist = "F"
# 
# all.selected.plots <- rbind(all.selected.plots,selected.plots)
# 
# 
# 
# 
# 




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
rad.cats <- 3
elev.cats <- 2
subquads.goal <- 4  ## this is for how many subquads to try to fill

min.homogeneity <- 1

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

existing.plots.avoid.pre.pre <- prev.plots.done %>%
  filter(plot.yr == 3)
existing.plots.avoid.pre.pre <- existing.plots.avoid.pre.pre[rep(seq_len(nrow(existing.plots.avoid.pre.pre)), each=3),] %>%
  st_transform(3310)


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
existing.plots.avoid <- st_rbind_all(existing.plots.avoid.pre,existing.plots.avoid.pre.pre)
#existing.plots.avoid <- existing.plots.avoid.pre


# select plots

source("scripts/site-selection/plot_stratification_geo.R")

# now we have selected.plots for this mgmt.type

selected.plots$plt.cat = 2 # 2 means: perimeter, no management, broad env. range
selected.plots$plt.seeddist = "N"

all.selected.plots <- rbind(all.selected.plots,selected.plots)





# 
# #### 1.e. Planted yr 3 full env. range far from seed source ####
# 
# ### Determine the env. range for which to get data ###
# 
# ## Specify fire and management of interest ##
# 
# foc.fire.name <- "2007MOONTELOPE - Plumas NF"
# foc.salv.cat <- "neither"
# foc.site.prepped <- "no"
# foc.released <- "no"
# foc.replanted <- "no"
# foc.thinned <- "no"
# foc.yrs.pltd <- c("3")
# 
# d.foc.overall <- d.trt %>%
#   dplyr::filter(fire.dist2 %in% foc.fire.name &
#                   salv.cat %in% foc.salv.cat &
#                   site.prepped %in% foc.site.prepped &
#                   release.txt %in% foc.released &
#                   replanted %in% foc.replanted &
#                   thinned %in% foc.thinned &
#                   yr.pltd %in% foc.yrs.pltd)
# 
# # define study range using only plots near seed source
# d.foc.overall.forenv <- d.foc.overall %>%
#   filter(dist.non.high < 80)
# 
# focal.env.range <- get.overall.env.range(plt.yrs=foc.yrs.pltd,plots=d.foc.overall.forenv)
# 
# 
# 
# ## specify plots for which to target matching geocells
# geocell.scoping.plots <- all.selected.plots # everything that's already been selected for this fire
# 
# 
# ## determine the env. quads
# rad.cats <- 2
# elev.cats <- 2
# subquads.goal <- 2  ## this is for how many subquads to try to fill
# 
# env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
# sub.quads.df <- define.subquads(env.quads)
# 
# 
# ## subset to the relevant plots
# d.foc.yr <- d.foc.overall %>%
#   filter(first.pltd.yr == 3 &
#            type == "treatment" &
#            dist.non.high > 120)
# 
# 
# ### Specify already-selected plots for which to not select new plots that are environmentally or spatially close
# 
# # Take already-selected plots and filter to the management categories that we're interested in here
# # Note that if this is for already-surveyed plots, this would be different: filter the dataset from which those plots were selected, making sure to use only those plots that were actually surveyed
# existing.plots.avoid.pre <- all.selected.plots %>%
#   filter(fire.dist2 %in% foc.fire.name &
#            salv.cat %in% foc.salv.cat &
#            site.prepped %in% foc.site.prepped &
#            release.txt %in% foc.released &
#            replanted %in% foc.replanted &
#            thinned %in% foc.thinned &
#            yr.pltd %in% foc.yrs.pltd &
#            dist.non.high > 120 &
#            rank == 1)
# 
# existing.plots.avoid.pre <- existing.plots.avoid.pre[rep(seq_len(nrow(existing.plots.avoid.pre)), each=3),]
# 
# ## selected only rank 1 (which is tier1), but also replicating it 3 times so in plot selection it can count as replacing tiers 1,2,3
# existing.plots.avoid <- existing.plots.avoid.pre
# 
# 
# 
# # select plots
# 
# source("scripts/site-selection/plot_stratification_geo.R")
# 
# # now we have selected.plots for this mgmt.type
# 
# selected.plots$plt.cat = 2 # 2 means: perimeter, no management, broad env. range
# selected.plots$plt.seeddist = "F"
# 
# all.selected.plots <- rbind(all.selected.plots,selected.plots)
# 
# 
# 
# 



# 
# #### 1.f. Moontelope planted yr 3 internal far from seed source full range ####
# 
# d.trt <- d.full[d.full$type %in% c("internal"),] ## remove the paired "control" plots because they do not have the environemntal data associated with them
# 
# #remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh) (this is only removing internal plots, as perimeter plots were already removed earlier in the script)
# # and which had roadsode salvage stringers
# d.trt <- d.trt %>%
#   filter(!is.na(dist.nonhigh)) %>%
#   filter(f.s.stringer == "no")
# 
# 
# ### Determine the env. range for which to get data ###
# 
# ## Specify fire and management of interest ##
# 
# foc.fire.name <- "2007MOONTELOPE - Plumas NF"
# foc.salv.cat <- "neither"
# foc.site.prepped <- "no"
# foc.released <- "no"
# foc.replanted <- "no"
# foc.thinned <- "no"
# foc.yrs.pltd <- c("3")
# 
# d.foc.overall <- d.trt %>%
#   dplyr::filter(fire.dist2 %in% foc.fire.name &
#                   salv.cat %in% foc.salv.cat &
#                   site.prepped %in% foc.site.prepped &
#                   release.txt %in% foc.released &
#                   replanted %in% foc.replanted &
#                   thinned %in% foc.thinned &
#                   yr.pltd %in% foc.yrs.pltd)
# 
# # define study range using only plots near seed source
# d.foc.overall.forenv <- d.foc.overall %>%
#   filter(dist.non.high > 120)
# 
# focal.env.range <- get.overall.env.range(plt.yrs=foc.yrs.pltd,plots=d.foc.overall.forenv)
# # alternative focal.env.range <- data.frame(elev.low=custom.elev.low,elev.high=custom.elev.high,rad.low=custom.rad.low,rad.high=custom.rad.high)
# 
# 
# 
# 
# 
# ## specify plots for which to target matching geocells
# geocell.scoping.plots <- NULL
# 
# ## specify plots for which to avoid placing new plots that are spatially or environmentally close
# existing.plots.avoid <- NULL
# 
# ## determine the env. quads
# rad.cats <- 3
# elev.cats <- 3
# subquads.goal <- 3  ## this is for how many subquads to try to fill
# 
# env.quads <- define.quadrants(focal.env.range,rad.cats,elev.cats)
# sub.quads.df <- define.subquads(env.quads)
# 
# 
# ## subset to the relevant plots
# d.foc.yr <- d.foc.overall %>%
#   filter(first.pltd.yr == 3 &
#            type == "internal" &
#            dist.non.high > 120)
# 
# 
# # select plots
# 
# source("scripts/site-selection/plot_stratification_geo.R")
# # now we have selected.plots for this mgmt.type
# 
# selected.plots$plt.cat = 3 # 3 means: internal, no management, broad env. range
# selected.plots$plt.seeddist = "F"
# 
# all.selected.plots <- rbind(all.selected.plots,selected.plots)
# 



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

### prepare the plot codes
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
         plot.tier = rank)

### make new geocells just for IDing plots in a reasonable geospatial order:
geogrid <- determine_geocells(all.selected.plots.w.names,cellsize=5000) %>%
  rename(geogrid.cell.id = geocell.id)
plot(geogrid)
# determine which geocell each plot falls into
all.selected.plots.w.names <- st_intersection(all.selected.plots.w.names,geogrid)

### also get the x and y lat long for arranging within the geogrids
all.selected.plots.w.names$lat <- a <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <- a <-st_coordinates(all.selected.plots.w.names)[,1]

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  mutate_at(vars(lat,long),round,digits=6)

## Arrange the plots geographically to assign ID numbers
all.selected.plots.w.names <- all.selected.plots.w.names %>%
  arrange(geogrid.cell.id,long)

starting.map.id <- 2000
map.ids <- starting.map.id:(starting.map.id+nrow(all.selected.plots.w.names)-1)
all.selected.plots.w.names$plot.id.map <- map.ids
all.selected.plots.w.names$plot.id.gps.pre <- paste0(all.selected.plots.w.names$plot.fire,map.ids)


### bring in the control plots
d.ctl <- d.full %>%
  filter(type == "control")

d.ctl <- st_transform(d.ctl,4326)
d.ctl$lat <-st_coordinates(d.ctl)[,2]
d.ctl$long <-st_coordinates(d.ctl)[,1]
all.selected.plots.w.names <- st_transform(all.selected.plots.w.names,4326)
all.selected.plots.w.names$lat <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <-st_coordinates(all.selected.plots.w.names)[,1]


## pull in the names of the treated plots they are paired with
d.ctl <- inner_join(d.ctl,
                    all.selected.plots.w.names %>% st_drop_geometry() %>% select(ctl.id,subquad.overall.label:plot.tier),
                    by=c("id"="ctl.id"))

##need to order d.ctl in the same order as: all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]
order.of.ctl.id <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$ctl.id
reorder <- match(order.of.ctl.id,d.ctl$id)
d.ctl <- d.ctl[reorder,]


plot.id.gps.pre <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.gps.pre
d.ctl$plot.id.gps.pre <- plot.id.gps.pre
plot.id.map <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.map
d.ctl$plot.id.map <- plot.id.map

# d.ctl <- st_drop_geometry(d.ctl)
# all.selected.plots.w.names <- st_drop_geometry(all.selected.plots.w.names)

## find out what vars they have in common to keep all of them
vars.in.common <- intersect(names(all.selected.plots.w.names),names(d.ctl))

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  select(vars.in.common)
d.ctl <- d.ctl %>%
  select(vars.in.common)

sel <- rbind(all.selected.plots.w.names,d.ctl)

sel.p <- sel %>%
  mutate(plot.type = recode(type,"treatment"="T","internal"="I","control"="C")) %>%
  mutate(plot.code = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1,plot.rad1,plot.elev2,plot.rad2,plot.tier,plot.type))


### also get the x and y lat long for arranging within the geogrids
sel.p <- sel.p %>%
  mutate_at(vars(lat,long),round,digits=6)


## make a plotting category
sel.p <- sel.p %>%
  mutate(map.cat = paste0(type,ifelse(type!="control",plot.tier,""))) %>%
  mutate(plot.id.gps = paste0(plot.id.gps.pre,plot.type))

# reorder columns so plot code etc. comes first
sel.p <- sel.p %>%
  select(plot.id.gps,plot.id.map,plot.code,plot.iter:plot.type,map.cat,everything())



st_write(sel.p,"data/site-selection/output/selected-plots/moontelope_v3_priHomogeneity.gpkg",delete_dsn=TRUE)



### Export plot data table and GPS waypoints
p <- st_read("data/site-selection/output/selected-plots/moontelope_v3_priHomogeneity.gpkg",stringsAsFactors=FALSE)


### for each tier1 plot, determine acceptable tier2 and tier3 plots
p2 <- p %>% st_drop_geometry() %>%
  select(plot.type,plot.id.gps,plot.code,lat,long,plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1:plot.rad2,plot.tier) %>%
  mutate(plot.code2 = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1,plot.rad1,plot.elev2,plot.rad2,plot.type)) %>%
  mutate(t2_opt = NA, t3_opt = NA)

for(i in 1:nrow(p2)) {
  if((p2[i,]$plot.type != "T") | p2[i,]$plot.tier != 1) next()

  
  focal.code2 <- p2[i,]$plot.code2
  
  # find matching tier 2 plots
  p.t2.match <- p2 %>%
    filter(plot.code2 == focal.code2,
           plot.tier == 2)
  
  plot.ids.tier2 <- paste(p.t2.match$plot.id.gps,collapse=", ")
  
  # find matching tier 3 plots
  p.t3.match <- p2 %>%
    filter(plot.code2 == focal.code2,
           plot.tier == 3)
  
  plot.ids.tier3 <- paste(p.t3.match$plot.id.gps,collapse=", ")
  
  p2[i,"t2_opt"] <- plot.ids.tier2
  p2[i,"t3_opt"] <- plot.ids.tier3
  
  
}




## export a table
p.table <- p2 %>%
  select(type=plot.type,tier=plot.tier,id=plot.id.gps,lat,long,t2_opt,t3_opt,elev1=plot.elev1,rad1=plot.rad1,elev2=plot.elev2,rad2=plot.rad2,year=plot.yr,cat=plot.cat,seed=plot.seed.dist) %>%
  arrange(desc(type),tier,id)
  
#six decimals on lat and long
p.table <- p.table %>%
  mutate_at(vars(lat,long),format,nsmall=6)
  
write.csv(p.table,"data/site-selection/output/selected-plots/wpt_table_moontelope_v2.csv")
  
  
### Export GPS waypoints
wpts <- p %>%
  select(name=plot.id.gps)
  
st_write(wpts,"data/site-selection/output/selected-plots/wpts_moontelope_v2.gpx",driver="GPX")











#### 2alt. Power Fire ####

all.selected.plots <- NULL

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

#### 2alt.a: Power plots paired unmanaged close to seed source ####


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 3 &
           type == "treatment" &
           dist.non.high < 80)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 1 # 1 means: perimeter unmanaged
selected.plots$plt.seeddist = "N"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- selected.plots




#### 2alt.b: Power plots paired unmanaged far from seed source ####


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 3 &
           type == "treatment" &
           dist.non.high > 120)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 1 # 1 means: perimeter unmanaged
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 2

## remove excessive plots far from seed source (we already have anouth and some are really close to each other)
selected.plots <- selected.plots %>%
  filter(!(id %in% c(8723,8721,8719,8704)))


all.selected.plots <- rbind(all.selected.plots,selected.plots)

#### 2alt.c: Power plots internal unmanaged far from seed source ####


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr == 3 &
           type == "internal" &
           dist.non.high > 120)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 2 # 1 means: internal unmanaged
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 3

all.selected.plots <- rbind(all.selected.plots,selected.plots)





#### 2alt.Z. Complete compilation of Power plots ####


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

### prepare the plot codes
all.selected.plots.w.names = all.selected.plots %>%
  mutate(plot.iter = 1,
         plot.fire = "B",
         plot.cat = plt.cat,
         plot.yr = first.pltd.yr,
         plot.seed.dist = plt.seeddist,
         #plot.elev1 = str_sub(subquad.overall.label,start=2,end=2),
         #plot.rad1 = str_sub(subquad.overall.label,start=4,end=4),
         #plot.elev2 = str_sub(subquad.overall.label,start=7,end=7),
         #plot.rad2 = str_sub(subquad.overall.label,start=9,end=9),
         plot.tier = rank)

### make new geocells just for IDing plots in a reasonable geospatial order:
geogrid <- determine_geocells(all.selected.plots.w.names,cellsize=4000) %>%
  rename(geogrid.cell.id = geocell.id)
plot(geogrid)
# determine which geocell each plot falls into
all.selected.plots.w.names <- st_intersection(all.selected.plots.w.names,geogrid)

### also get the x and y lat long for arranging within the geogrids
all.selected.plots.w.names$lat <- a <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <- a <-st_coordinates(all.selected.plots.w.names)[,1]

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  mutate_at(vars(lat,long),round,digits=6)

## Arrange the plots geographically to assign ID numbers
all.selected.plots.w.names <- all.selected.plots.w.names %>%
  arrange(geogrid.cell.id,long)

starting.map.id <- 1000
map.ids <- starting.map.id:(starting.map.id+nrow(all.selected.plots.w.names)-1)
all.selected.plots.w.names$plot.id.map <- map.ids
all.selected.plots.w.names$plot.id.gps.pre <- paste0(all.selected.plots.w.names$plot.fire,map.ids)


### bring in the control plots
d.ctl <- d.full %>%
  filter(type == "control")

d.ctl <- st_transform(d.ctl,4326)
d.ctl$lat <-st_coordinates(d.ctl)[,2]
d.ctl$long <-st_coordinates(d.ctl)[,1]
all.selected.plots.w.names <- st_transform(all.selected.plots.w.names,4326)
all.selected.plots.w.names$lat <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <-st_coordinates(all.selected.plots.w.names)[,1]


## pull in the names of the treated plots they are paired with
d.ctl <- inner_join(d.ctl,
                    all.selected.plots.w.names %>% st_drop_geometry() %>% select(ctl.id,plt.cat:plot.tier),
                    by=c("id"="ctl.id"))

##need to order d.ctl in the same order as: all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]
order.of.ctl.id <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$ctl.id
reorder <- match(order.of.ctl.id,d.ctl$id)
d.ctl <- d.ctl[reorder,]


plot.id.gps.pre <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.gps.pre
d.ctl$plot.id.gps.pre <- plot.id.gps.pre
plot.id.map <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.map
d.ctl$plot.id.map <- plot.id.map

# d.ctl <- st_drop_geometry(d.ctl)
# all.selected.plots.w.names <- st_drop_geometry(all.selected.plots.w.names)

## find out what vars they have in common to keep all of them
vars.in.common <- intersect(names(all.selected.plots.w.names),names(d.ctl))

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  select(vars.in.common)
d.ctl <- d.ctl %>%
  select(vars.in.common)

sel <- rbind(all.selected.plots.w.names,d.ctl)

sel.p <- sel %>%
  mutate(plot.type = recode(type,"treatment"="T","internal"="I","control"="C")) %>%
  mutate(plot.code = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.tier,plot.type))


### also get the x and y lat long for arranging within the geogrids
sel.p <- sel.p %>%
  mutate_at(vars(lat,long),round,digits=6)


## make a plotting category
sel.p <- sel.p %>%
  mutate(map.cat = paste0(type,ifelse(type!="control",plot.tier,""))) %>%
  mutate(plot.id.gps = paste0(plot.id.gps.pre,plot.type))

# reorder columns so plot code etc. comes first
sel.p <- sel.p %>%
  select(plot.id.gps,plot.id.map,plot.code,plot.iter:plot.type,map.cat,everything())



st_write(sel.p,"data/site-selection/output/selected-plots/power_v1.gpkg",delete_dsn=TRUE)



### Export plot data table and GPS waypoints
p <- st_read("data/site-selection/output/selected-plots/power_v1.gpkg",stringsAsFactors=FALSE)


# ### for each tier1 plot, determine acceptable tier2 and tier3 plots
# p2 <- p %>% st_drop_geometry() %>%
#   select(plot.type,plot.id.gps,plot.code,lat,long,plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1:plot.rad2,plot.tier) %>%
#   mutate(plot.code2 = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1,plot.rad1,plot.elev2,plot.rad2,plot.type)) %>%
#   mutate(t2_opt = NA, t3_opt = NA)
# 
# for(i in 1:nrow(p2)) {
#   if((p2[i,]$plot.type != "T") | p2[i,]$plot.tier != 1) next()
#   
#   
#   focal.code2 <- p2[i,]$plot.code2
#   
#   # find matching tier 2 plots
#   p.t2.match <- p2 %>%
#     filter(plot.code2 == focal.code2,
#            plot.tier == 2)
#   
#   plot.ids.tier2 <- paste(p.t2.match$plot.id.gps,collapse=", ")
#   
#   # find matching tier 3 plots
#   p.t3.match <- p2 %>%
#     filter(plot.code2 == focal.code2,
#            plot.tier == 3)
#   
#   plot.ids.tier3 <- paste(p.t3.match$plot.id.gps,collapse=", ")
#   
#   p2[i,"t2_opt"] <- plot.ids.tier2
#   p2[i,"t3_opt"] <- plot.ids.tier3
#   
#   
# }


p2 <- p

## export a table
p.table <- p2 %>%
  select(type=plot.type,tier=plot.tier,id=plot.id.gps,lat,long,year=plot.yr,cat=plot.cat,seed=plot.seed.dist) %>%
  arrange(desc(type),tier,id)

#six decimals on lat and long
p.table <- p.table %>%
  mutate_at(vars(lat,long),format,nsmall=6)

write.csv(p.table,"data/site-selection/output/selected-plots/wpt_table_power.csv")


### Export GPS waypoints
wpts <- p %>%
  select(name=plot.id.gps)

st_write(wpts,"data/site-selection/output/selected-plots/wpts_power.gpx",driver="GPX",delete_dsn=TRUE)






























#### 3. AmRiv Fire ####

all.selected.plots <- NULL

d.trt <- d.full[d.full$type %in% c("treatment","internal"),] ## remove the paired "control" plots because they do not have the environemntal data associated with them

#remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh) (this is only removing internal plots, as perimeter plots were already removed earlier in the script)
# and which had roadsode salvage stringers
d.trt <- d.trt %>%
  filter(!is.na(dist.nonhigh)) %>%
  filter(f.s.stringer == "no")


### Determine the env. range for which to get data ###

## Specify fire and management of interest ##

foc.fire.name <- "2008GOVERNMENT - Tahoe NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "e"
foc.replanted <- c("no","YES")
foc.thinned <- c("no")
foc.yrs.pltd <- c("1","2")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)

#### 3alt.a: AmRiv plots perimeter early release close to seed source, plus yr 1 far from seed source ####


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr %in% c(1,2) &
           type == "treatment" &
           dist.non.high < 80)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 1 # 1 means: perimeter 
selected.plots$plt.seeddist = "N"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- selected.plots




#### 2alt.b: AmRiv plots paired unmanaged far from seed source yr 1 ####


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr %in% c(1,2) &
           type == "treatment" &
           (dist.non.high > 120 & yr.pltd == 1))

selected.plots <- d.foc.yr


selected.plots$plt.cat = 1 # 1 means: perimeter 
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- rbind(all.selected.plots,selected.plots)




#### 3alt.c: AmRiv plots internal unmanaged far from seed source ####


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr  %in% c(1,2) &
           type == "internal" &
           dist.non.high > 120)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 2 # 2 means: internal 
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 2

all.selected.plots <- rbind(all.selected.plots,selected.plots)





#### 3alt.Z. Complete compilation of AmRiv plots ####


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

### prepare the plot codes
all.selected.plots.w.names = all.selected.plots %>%
  mutate(plot.iter = 1,
         plot.fire = "C",
         plot.cat = plt.cat,
         plot.yr = first.pltd.yr,
         plot.seed.dist = plt.seeddist,
         #plot.elev1 = str_sub(subquad.overall.label,start=2,end=2),
         #plot.rad1 = str_sub(subquad.overall.label,start=4,end=4),
         #plot.elev2 = str_sub(subquad.overall.label,start=7,end=7),
         #plot.rad2 = str_sub(subquad.overall.label,start=9,end=9),
         plot.tier = rank)

### make new geocells just for IDing plots in a reasonable geospatial order:
geogrid <- determine_geocells(all.selected.plots.w.names,cellsize=4000) %>%
  rename(geogrid.cell.id = geocell.id)
plot(geogrid)
# determine which geocell each plot falls into
all.selected.plots.w.names <- st_intersection(all.selected.plots.w.names,geogrid)

### also get the x and y lat long for arranging within the geogrids
all.selected.plots.w.names$lat <- a <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <- a <-st_coordinates(all.selected.plots.w.names)[,1]

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  mutate_at(vars(lat,long),round,digits=6)

## Arrange the plots geographically to assign ID numbers
all.selected.plots.w.names <- all.selected.plots.w.names %>%
  arrange(geogrid.cell.id,long)

starting.map.id <- 1000
map.ids <- starting.map.id:(starting.map.id+nrow(all.selected.plots.w.names)-1)
all.selected.plots.w.names$plot.id.map <- map.ids
all.selected.plots.w.names$plot.id.gps.pre <- paste0(all.selected.plots.w.names$plot.fire,map.ids)


### bring in the control plots
d.ctl <- d.full %>%
  filter(type == "control")

d.ctl <- st_transform(d.ctl,4326)
d.ctl$lat <-st_coordinates(d.ctl)[,2]
d.ctl$long <-st_coordinates(d.ctl)[,1]
all.selected.plots.w.names <- st_transform(all.selected.plots.w.names,4326)
all.selected.plots.w.names$lat <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <-st_coordinates(all.selected.plots.w.names)[,1]


## pull in the names of the treated plots they are paired with
d.ctl <- inner_join(d.ctl,
                    all.selected.plots.w.names %>% st_drop_geometry() %>% select(ctl.id,plt.cat:plot.tier),
                    by=c("id"="ctl.id"))

##need to order d.ctl in the same order as: all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]
order.of.ctl.id <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$ctl.id
reorder <- match(order.of.ctl.id,d.ctl$id)
d.ctl <- d.ctl[reorder,]


plot.id.gps.pre <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.gps.pre
d.ctl$plot.id.gps.pre <- plot.id.gps.pre
plot.id.map <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.map
d.ctl$plot.id.map <- plot.id.map

# d.ctl <- st_drop_geometry(d.ctl)
# all.selected.plots.w.names <- st_drop_geometry(all.selected.plots.w.names)

## find out what vars they have in common to keep all of them
vars.in.common <- intersect(names(all.selected.plots.w.names),names(d.ctl))

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  select(vars.in.common)
d.ctl <- d.ctl %>%
  select(vars.in.common)

sel <- rbind(all.selected.plots.w.names,d.ctl)

sel.p <- sel %>%
  mutate(plot.type = recode(type,"treatment"="T","internal"="I","control"="C")) %>%
  mutate(plot.code = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.tier,plot.type))


### also get the x and y lat long for arranging within the geogrids
sel.p <- sel.p %>%
  mutate_at(vars(lat,long),round,digits=6)


## make a plotting category
sel.p <- sel.p %>%
  mutate(map.cat = paste0(type,ifelse(type!="control",plot.tier,""))) %>%
  mutate(plot.id.gps = paste0(plot.id.gps.pre,plot.type))

# reorder columns so plot code etc. comes first
sel.p <- sel.p %>%
  select(plot.id.gps,plot.id.map,plot.code,plot.iter:plot.type,map.cat,everything())



st_write(sel.p,"data/site-selection/output/selected-plots/AmRiv_v1.gpkg",delete_dsn=TRUE)



### Export plot data table and GPS waypoints
p <- st_read("data/site-selection/output/selected-plots/AmRiv_v1.gpkg",stringsAsFactors=FALSE)


# ### for each tier1 plot, determine acceptable tier2 and tier3 plots
# p2 <- p %>% st_drop_geometry() %>%
#   select(plot.type,plot.id.gps,plot.code,lat,long,plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1:plot.rad2,plot.tier) %>%
#   mutate(plot.code2 = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1,plot.rad1,plot.elev2,plot.rad2,plot.type)) %>%
#   mutate(t2_opt = NA, t3_opt = NA)
# 
# for(i in 1:nrow(p2)) {
#   if((p2[i,]$plot.type != "T") | p2[i,]$plot.tier != 1) next()
#   
#   
#   focal.code2 <- p2[i,]$plot.code2
#   
#   # find matching tier 2 plots
#   p.t2.match <- p2 %>%
#     filter(plot.code2 == focal.code2,
#            plot.tier == 2)
#   
#   plot.ids.tier2 <- paste(p.t2.match$plot.id.gps,collapse=", ")
#   
#   # find matching tier 3 plots
#   p.t3.match <- p2 %>%
#     filter(plot.code2 == focal.code2,
#            plot.tier == 3)
#   
#   plot.ids.tier3 <- paste(p.t3.match$plot.id.gps,collapse=", ")
#   
#   p2[i,"t2_opt"] <- plot.ids.tier2
#   p2[i,"t3_opt"] <- plot.ids.tier3
#   
#   
# }


p2 <- p

## export a table
p.table <- p2 %>%
  select(type=plot.type,tier=plot.tier,id=plot.id.gps,lat,long,year=plot.yr,cat=plot.cat,seed=plot.seed.dist) %>%
  arrange(desc(type),tier,id)

#six decimals on lat and long
p.table <- p.table %>%
  mutate_at(vars(lat,long),format,nsmall=6)

write.csv(p.table,"data/site-selection/output/selected-plots/wpt_table_amriv.csv")


### Export GPS waypoints
wpts <- p %>%
  select(name=plot.id.gps)

st_write(wpts,"data/site-selection/output/selected-plots/wpts_amriv.gpx",driver="GPX",delete_dsn=TRUE)
































#### 4. Cottonwood Fire ####

all.selected.plots <- NULL

d.trt <- d.full[d.full$type %in% c("treatment","internal"),] ## remove the paired "control" plots because they do not have the environemntal data associated with them

#remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh) (this is only removing internal plots, as perimeter plots were already removed earlier in the script)
# and which had roadsode salvage stringers
d.trt <- d.trt %>%
  filter(f.s.stringer == "no")


#### 4.a: Cottonwood plots paired unmanaged close to seed source ####


## Specify fire and management of interest ##

foc.fire.name <- "1994COTTONWOOD - Tahoe NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "e"
foc.replanted <- "YES"
foc.thinned <- "no"
foc.yrs.pltd <- c("3","4+")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(dist.non.high < 90)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 1 # 1 means: early release
selected.plots$plt.seeddist = "N"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- selected.plots



## Specify fire and management of interest ##

foc.fire.name <- "1994COTTONWOOD - Tahoe NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "e"
foc.replanted <- "YES"
foc.thinned <- "no"
foc.yrs.pltd <- c("3","4+")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(dist.non.high > 120)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 1 # 1 means: early release
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- rbind(all.selected.plots,selected.plots)







## Specify fire and management of interest ##

foc.fire.name <- "1994COTTONWOOD - Tahoe NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "el"
foc.replanted <- "YES"
foc.thinned <- "no"
foc.yrs.pltd <- c("2")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(dist.non.high > 120)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 2 # 1 means: early + late release
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- rbind(all.selected.plots,selected.plots)





## Specify fire and management of interest ##

foc.fire.name <- "1994COTTONWOOD - Tahoe NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "no"
foc.replanted <- c("no","YES")
foc.thinned <- "no"
foc.yrs.pltd <- c("2","3")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(dist.non.high < 90)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 3 # 3 means: no release
selected.plots$plt.seeddist = "N"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- rbind(all.selected.plots,selected.plots)






## Specify fire and management of interest ##

foc.fire.name <- "1994COTTONWOOD - Tahoe NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "no"
foc.replanted <- c("no","YES")
foc.thinned <- "no"
foc.yrs.pltd <- c("2","3")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(dist.non.high > 120)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 3 # 3 means: no release
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- rbind(all.selected.plots,selected.plots)







## Specify fire and management of interest ##

foc.fire.name <- "1994COTTONWOOD - Tahoe NF"
foc.salv.cat <- "planted"
foc.site.prepped <- "no"
foc.released <- "e"
foc.replanted <- c("no","YES")
foc.thinned <- "no"
foc.yrs.pltd <- c("2","3")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(dist.non.high > 120)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 4 # 3 means: salvaged
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- rbind(all.selected.plots,selected.plots)





## Specify fire and management of interest ##

foc.fire.name <- "1994COTTONWOOD - Tahoe NF"
foc.salv.cat <- "planted"
foc.site.prepped <- "no"
foc.released <- "e"
foc.replanted <- c("no","YES")
foc.thinned <- "no"
foc.yrs.pltd <- c("2","3")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(dist.non.high < 90)

selected.plots <- d.foc.yr


selected.plots$plt.cat = 4 # 3 means: salvaged
selected.plots$plt.seeddist = "N"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- rbind(all.selected.plots,selected.plots)






# 
# 
# 
# 
# 
# 
# 
# 
# #### 4.b: Cottonwood plots paired unmanaged far from seed source ####
# 
# 
# ## subset to the relevant plots
# d.foc.yr <- d.foc.overall %>%
#   filter(first.pltd.yr %in% c("2","3") &
#            type == "treatment" &
#            dist.non.high > 120)
# 
# selected.plots <- d.foc.yr
# 
# 
# selected.plots$plt.cat = 1 # 1 means: perimeter unmanaged
# selected.plots$plt.seeddist = "F"
# 
# ##manually set the rank
# selected.plots$rank <- 1
# 
# 
# all.selected.plots <- rbind(all.selected.plots,selected.plots)
# 
# 
# 
# 
# 
# #### 4.c: Cottonwood plots internal unmanaged far from seed source ####
# 
# 
# ## subset to the relevant plots
# d.foc.yr <- d.foc.overall %>%
#   filter(first.pltd.yr %in% c("2","3") &
#            type == "internal" &
#            dist.non.high > 120)
# 
# selected.plots <- d.foc.yr
# 
# 
# selected.plots$plt.cat = 2 # 1 means: internal unmanaged
# selected.plots$plt.seeddist = "F"
# 
# ##manually set the rank
# selected.plots$rank <- 2
# 
# 
# all.selected.plots <- rbind(all.selected.plots,selected.plots)
# 
# 


#### 4.Z. Complete compilation of Cottonwood plots ####


### plot all the plots for this management type on this fire: but first tier only ###
d <- all.selected.plots

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
  geom_point(data=d.int.far,shape=3,size=0.7) +
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

### prepare the plot codes
all.selected.plots.w.names = all.selected.plots %>%
  mutate(plot.iter = 1,
         plot.fire = "D",
         plot.cat = plt.cat,
         plot.yr = first.pltd.yr,
         plot.seed.dist = plt.seeddist,
         #plot.elev1 = str_sub(subquad.overall.label,start=2,end=2),
         #plot.rad1 = str_sub(subquad.overall.label,start=4,end=4),
         #plot.elev2 = str_sub(subquad.overall.label,start=7,end=7),
         #plot.rad2 = str_sub(subquad.overall.label,start=9,end=9),
         plot.tier = rank)

### make new geocells just for IDing plots in a reasonable geospatial order:
geogrid <- determine_geocells(all.selected.plots.w.names,cellsize=4000) %>%
  rename(geogrid.cell.id = geocell.id)
plot(geogrid)
# determine which geocell each plot falls into
all.selected.plots.w.names <- st_intersection(all.selected.plots.w.names,geogrid)

### also get the x and y lat long for arranging within the geogrids
all.selected.plots.w.names$lat <- a <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <- a <-st_coordinates(all.selected.plots.w.names)[,1]

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  mutate_at(vars(lat,long),round,digits=6)

## Arrange the plots geographically to assign ID numbers
all.selected.plots.w.names <- all.selected.plots.w.names %>%
  arrange(geogrid.cell.id,long)

starting.map.id <- 1000
map.ids <- starting.map.id:(starting.map.id+nrow(all.selected.plots.w.names)-1)
all.selected.plots.w.names$plot.id.map <- map.ids
all.selected.plots.w.names$plot.id.gps.pre <- paste0(all.selected.plots.w.names$plot.fire,map.ids)


### bring in the control plots
d.ctl <- d.full %>%
  filter(type == "control")

d.ctl <- st_transform(d.ctl,4326)
d.ctl$lat <-st_coordinates(d.ctl)[,2]
d.ctl$long <-st_coordinates(d.ctl)[,1]
all.selected.plots.w.names <- st_transform(all.selected.plots.w.names,4326)
all.selected.plots.w.names$lat <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <-st_coordinates(all.selected.plots.w.names)[,1]


## pull in the names of the treated plots they are paired with
d.ctl <- inner_join(d.ctl,
                    all.selected.plots.w.names %>% st_drop_geometry() %>% select(ctl.id,plt.cat:plot.tier),
                    by=c("id"="ctl.id"))

##need to order d.ctl in the same order as: all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]
order.of.ctl.id <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$ctl.id
reorder <- match(order.of.ctl.id,d.ctl$id)
d.ctl <- d.ctl[reorder,]


plot.id.gps.pre <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.gps.pre
d.ctl$plot.id.gps.pre <- plot.id.gps.pre
plot.id.map <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.map
d.ctl$plot.id.map <- plot.id.map

# d.ctl <- st_drop_geometry(d.ctl)
# all.selected.plots.w.names <- st_drop_geometry(all.selected.plots.w.names)

## find out what vars they have in common to keep all of them
vars.in.common <- intersect(names(all.selected.plots.w.names),names(d.ctl))

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  select(vars.in.common)
d.ctl <- d.ctl %>%
  select(vars.in.common)

sel <- rbind(all.selected.plots.w.names,d.ctl)

sel.p <- sel %>%
  mutate(plot.type = recode(type,"treatment"="T","internal"="I","control"="C")) %>%
  mutate(plot.code = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.tier,plot.type))


### also get the x and y lat long for arranging within the geogrids
sel.p <- sel.p %>%
  mutate_at(vars(lat,long),round,digits=6)


## make a plotting category
sel.p <- sel.p %>%
  mutate(map.cat = paste0(type,ifelse(type!="control",plot.tier,""))) %>%
  mutate(plot.id.gps = paste0(plot.id.gps.pre,plot.type))

# reorder columns so plot code etc. comes first
sel.p <- sel.p %>%
  select(plot.id.gps,plot.id.map,plot.code,plot.iter:plot.type,map.cat,everything())



st_write(sel.p,"data/site-selection/output/selected-plots/cottonwood_v1.gpkg",delete_dsn=TRUE)



### Export plot data table and GPS waypoints
p <- st_read("data/site-selection/output/selected-plots/cottonwood_v1.gpkg",stringsAsFactors=FALSE)


# ### for each tier1 plot, determine acceptable tier2 and tier3 plots
# p2 <- p %>% st_drop_geometry() %>%
#   select(plot.type,plot.id.gps,plot.code,lat,long,plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1:plot.rad2,plot.tier) %>%
#   mutate(plot.code2 = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1,plot.rad1,plot.elev2,plot.rad2,plot.type)) %>%
#   mutate(t2_opt = NA, t3_opt = NA)
# 
# for(i in 1:nrow(p2)) {
#   if((p2[i,]$plot.type != "T") | p2[i,]$plot.tier != 1) next()
#   
#   
#   focal.code2 <- p2[i,]$plot.code2
#   
#   # find matching tier 2 plots
#   p.t2.match <- p2 %>%
#     filter(plot.code2 == focal.code2,
#            plot.tier == 2)
#   
#   plot.ids.tier2 <- paste(p.t2.match$plot.id.gps,collapse=", ")
#   
#   # find matching tier 3 plots
#   p.t3.match <- p2 %>%
#     filter(plot.code2 == focal.code2,
#            plot.tier == 3)
#   
#   plot.ids.tier3 <- paste(p.t3.match$plot.id.gps,collapse=", ")
#   
#   p2[i,"t2_opt"] <- plot.ids.tier2
#   p2[i,"t3_opt"] <- plot.ids.tier3
#   
#   
# }


p2 <- p

## export a table
p.table <- p2 %>%
  select(type=plot.type,tier=plot.tier,id=plot.id.gps,lat,long,year=plot.yr,cat=plot.cat,seed=plot.seed.dist) %>%
  arrange(desc(type),tier,id)

#six decimals on lat and long
p.table <- p.table %>%
  mutate_at(vars(lat,long),format,nsmall=6)

write.csv(p.table,"data/site-selection/output/selected-plots/wpt_table_cottonwood.csv")


### Export GPS waypoints
wpts <- p %>%
  select(name=plot.id.gps)

st_write(wpts,"data/site-selection/output/selected-plots/wpts_cottonwood.gpx",driver="GPX",delete_dsn=TRUE)























#### 5. AmRiv v2 ####


all.selected.plots <- NULL

d.trt <- d.full[d.full$type %in% c("treatment","internal"),] ## remove the paired "control" plots because they do not have the environemntal data associated with them

#remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh) (this is only removing internal plots, as perimeter plots were already removed earlier in the script)
# and which had roadsode salvage stringers
d.trt <- d.trt %>%
  filter(f.s.stringer == "no")


### determine what plots already done
prev.plots <- st_read("data/site-selection/output/selected-plots/AmRiv_v1.gpkg")
done.data <- read.csv("data/site-selection/analysis-parameters/plots_surveyed.csv",header=TRUE,stringsAsFactors=FALSE)
done.ids <- toupper(done.data$plot)
done.plots <- prev.plots %>%
  filter(plot.id.gps %in% done.ids) %>%
  st_transform(3310)


### Determine the env. range for which to get data ###

## Specify fire and management of interest ##

foc.fire.name <- "2008GOVERNMENT - Tahoe NF"
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "e"
foc.replanted <- c("no","YES")
foc.thinned <- c("no")
foc.yrs.pltd <- c("1","2")

d.foc.overall <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)


### exclude any candidate plots within 120 m of already-done plots
a <- st_distance(d.foc.overall,done.plots)
d.foc.overall$dist.to.prev <- apply(a,1,min)

d.foc.overall <- d.foc.overall %>%
  filter((dist.to.prev > 120) |
           ((yr.pltd == "2" & (elev > 1900))))










#### 3alt.a: AmRiv plots perimeter early release close to seed source


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr %in% c(1,2) &
           type == "treatment" &
           dist.non.high < 90 &
           !(first.pltd.yr == 1 & rad < 7500))

min.homogeneity = 1

d.foc.yr <- d.foc.yr %>%
  mutate(homogeneity = pmin(n.similar.adj.trt.plots,n.close.paired.plots),
         homogeneous = homogeneity > min.homogeneity)

g <- ggplot(d.foc.yr %>% filter(type=="treatment"),aes(x=elev,y=rad,color=homogeneous)) +
  geom_point(size=2) +
  theme_bw(15) +
  scale_shape_manual(values=c(16,1,2,17)) +
  labs(color="Homogeneity") +
  labs(title=d.foc.yr$fire.dist2[1]) +
  geom_point(data=done.plots,aes(shape=yr.pltd),color="black")
print(g)


### remove plots that are near to already-done plots





selected.plots <- d.foc.yr


selected.plots$plt.cat = 1 # 1 means: perimeter 
selected.plots$plt.seeddist = "N"

##manually set the rank
selected.plots$rank <- 1


all.selected.plots <- selected.plots




#### 2alt.b: AmRiv plots paired unmanaged far from seed source yr 1 ####


## subset to the relevant plots
d.foc.yr <- d.foc.overall %>%
  filter(first.pltd.yr %in% c(1,2) &
           type == "treatment" &
           (dist.non.high > 120 & first.pltd.yr == 1) &
           (rad > 7600) &
           (elev < 1970))

d.foc.yr <- d.foc.yr %>%
  mutate(homogeneity = pmin(n.similar.adj.trt.plots,n.close.paired.plots),
         homogeneous = homogeneity > min.homogeneity)


selected.plots <- d.foc.yr


selected.plots$plt.cat = 1 # 1 means: perimeter 
selected.plots$plt.seeddist = "F"

##manually set the rank
selected.plots$rank <- 1

all.selected.plots <- rbind(all.selected.plots,selected.plots)


# 
# 
# #### 3alt.c: AmRiv plots internal unmanaged far from seed source ####
# 
# 
# ## subset to the relevant plots
# d.foc.yr <- d.foc.overall %>%
#   filter(first.pltd.yr  %in% c(1,2) &
#            type == "internal" &
#            dist.non.high > 120)
# 
# selected.plots <- d.foc.yr
# 
# 
# selected.plots$plt.cat = 2 # 2 means: internal 
# selected.plots$plt.seeddist = "F"
# 
# ##manually set the rank
# selected.plots$rank <- 2
# 
# all.selected.plots <- rbind(all.selected.plots,selected.plots)
# 
# 



#### 3alt.Z. Complete compilation of AmRiv plots ####


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

### prepare the plot codes
all.selected.plots.w.names = all.selected.plots %>%
  mutate(plot.iter = 1,
         plot.fire = "C",
         plot.cat = plt.cat,
         plot.yr = first.pltd.yr,
         plot.seed.dist = plt.seeddist,
         #plot.elev1 = str_sub(subquad.overall.label,start=2,end=2),
         #plot.rad1 = str_sub(subquad.overall.label,start=4,end=4),
         #plot.elev2 = str_sub(subquad.overall.label,start=7,end=7),
         #plot.rad2 = str_sub(subquad.overall.label,start=9,end=9),
         plot.tier = rank)

### make new geocells just for IDing plots in a reasonable geospatial order:
geogrid <- determine_geocells(all.selected.plots.w.names,cellsize=4000) %>%
  rename(geogrid.cell.id = geocell.id)
plot(geogrid)
# determine which geocell each plot falls into
all.selected.plots.w.names <- st_intersection(all.selected.plots.w.names,geogrid)

### also get the x and y lat long for arranging within the geogrids
all.selected.plots.w.names$lat <- a <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <- a <-st_coordinates(all.selected.plots.w.names)[,1]

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  mutate_at(vars(lat,long),round,digits=6)

## Arrange the plots geographically to assign ID numbers
all.selected.plots.w.names <- all.selected.plots.w.names %>%
  arrange(geogrid.cell.id,long)

starting.map.id <- 2000
map.ids <- starting.map.id:(starting.map.id+nrow(all.selected.plots.w.names)-1)
all.selected.plots.w.names$plot.id.map <- map.ids
all.selected.plots.w.names$plot.id.gps.pre <- paste0(all.selected.plots.w.names$plot.fire,map.ids)


all.selected.plots.w.names <- all.selected.plots.w.names %>%
  filter(plot.id.map != 2000)


### bring in the control plots
d.ctl <- d.full %>%
  filter(type == "control")

d.ctl <- st_transform(d.ctl,4326)
d.ctl$lat <-st_coordinates(d.ctl)[,2]
d.ctl$long <-st_coordinates(d.ctl)[,1]
all.selected.plots.w.names <- st_transform(all.selected.plots.w.names,4326)
all.selected.plots.w.names$lat <-st_coordinates(all.selected.plots.w.names)[,2]
all.selected.plots.w.names$long <-st_coordinates(all.selected.plots.w.names)[,1]


## pull in the names of the treated plots they are paired with
d.ctl <- inner_join(d.ctl,
                    all.selected.plots.w.names %>% st_drop_geometry() %>% dplyr::select(ctl.id,plt.cat:plot.tier),
                    by=c("id"="ctl.id"))

##need to order d.ctl in the same order as: all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]
order.of.ctl.id <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$ctl.id
reorder <- match(order.of.ctl.id,d.ctl$id)
d.ctl <- d.ctl[reorder,]


plot.id.gps.pre <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.gps.pre
d.ctl$plot.id.gps.pre <- plot.id.gps.pre
plot.id.map <- all.selected.plots.w.names[all.selected.plots.w.names$type == "treatment",]$plot.id.map
d.ctl$plot.id.map <- plot.id.map

# d.ctl <- st_drop_geometry(d.ctl)
# all.selected.plots.w.names <- st_drop_geometry(all.selected.plots.w.names)

## find out what vars they have in common to keep all of them
vars.in.common <- intersect(names(all.selected.plots.w.names),names(d.ctl))

all.selected.plots.w.names <- all.selected.plots.w.names %>%
  dplyr::select(vars.in.common)
d.ctl <- d.ctl %>%
  dplyr::select(vars.in.common)

sel <- rbind(all.selected.plots.w.names,d.ctl)

sel.p <- sel %>%
  mutate(plot.type = recode(type,"treatment"="T","internal"="I","control"="C")) %>%
  mutate(plot.code = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.tier,plot.type))


### also get the x and y lat long for arranging within the geogrids
sel.p <- sel.p %>%
  mutate_at(vars(lat,long),round,digits=6)


## make a plotting category
sel.p <- sel.p %>%
  mutate(map.cat = paste0(type,ifelse(type!="control",plot.tier,""))) %>%
  mutate(plot.id.gps = paste0(plot.id.gps.pre,plot.type))

# reorder columns so plot code etc. comes first
sel.p <- sel.p %>%
  dplyr::select(plot.id.gps,plot.id.map,plot.code,plot.iter:plot.type,map.cat,everything())



st_write(sel.p,"data/site-selection/output/selected-plots/AmRiv_v2.gpkg",delete_dsn=TRUE)



### Export plot data table and GPS waypoints
p <- st_read("data/site-selection/output/selected-plots/AmRiv_v2.gpkg",stringsAsFactors=FALSE)


# ### for each tier1 plot, determine acceptable tier2 and tier3 plots
# p2 <- p %>% st_drop_geometry() %>%
#   select(plot.type,plot.id.gps,plot.code,lat,long,plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1:plot.rad2,plot.tier) %>%
#   mutate(plot.code2 = paste0(plot.iter,plot.fire,plot.cat,plot.yr,plot.seed.dist,plot.elev1,plot.rad1,plot.elev2,plot.rad2,plot.type)) %>%
#   mutate(t2_opt = NA, t3_opt = NA)
# 
# for(i in 1:nrow(p2)) {
#   if((p2[i,]$plot.type != "T") | p2[i,]$plot.tier != 1) next()
#   
#   
#   focal.code2 <- p2[i,]$plot.code2
#   
#   # find matching tier 2 plots
#   p.t2.match <- p2 %>%
#     filter(plot.code2 == focal.code2,
#            plot.tier == 2)
#   
#   plot.ids.tier2 <- paste(p.t2.match$plot.id.gps,collapse=", ")
#   
#   # find matching tier 3 plots
#   p.t3.match <- p2 %>%
#     filter(plot.code2 == focal.code2,
#            plot.tier == 3)
#   
#   plot.ids.tier3 <- paste(p.t3.match$plot.id.gps,collapse=", ")
#   
#   p2[i,"t2_opt"] <- plot.ids.tier2
#   p2[i,"t3_opt"] <- plot.ids.tier3
#   
#   
# }


p2 <- p

## export a table
p.table <- p2 %>%
  dplyr::select(type=plot.type,tier=plot.tier,id=plot.id.gps,lat,long,year=plot.yr,cat=plot.cat,seed=plot.seed.dist) %>%
  arrange(desc(type),tier,id)

#six decimals on lat and long
p.table <- p.table %>%
  mutate_at(vars(lat,long),format,nsmall=6)

write.csv(p.table,"data/site-selection/output/selected-plots/wpt_table_amriv_v2.csv")


### Export GPS waypoints
wpts <- p %>%
  dplyr::select(name=plot.id.gps)

st_write(wpts,"data/site-selection/output/selected-plots/wpts_amriv_v2.gpx",driver="GPX",delete_dsn=TRUE)








