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

source("scripts/site-selection/plot_stratification_spatial_functions.R")

crs <- 3310 # CA albers

set.seed(1)



#### Load the data ####
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered.gpkg",stringsAsFactors=FALSE)
d.trt <- d.full[d.full$type %in% c("internal","treatment"),] # remove the paired "control" plots because they do not have the environemntal data associated with them


#### Perform initial filtering ####
## The first three filters were also done in the plot selection script, prior to plotting the panel plots, but after exporting the filtered dataset.
## It might make more sense to perform this filtering before exporting the filtered dataset in the plot selection script.
##!! Also, in the plot selection script, to determine whether factorial management categories have sufficient plots, plots are counted before these filters are applied. Only the incomplete planting slices and stringers filters are an issue in this way though, because only perimeter plots are used for determining if there are enough plots in a management category.

#remove plots that are in between 80 and 120 m from seed source (for them, no value stored for dist.nonhigh) (this is only removing internal plots, as perimeter plots were already removed earlier in the script)
d.trt <- d.trt %>%
  filter(!is.na(dist.nonhigh))

# Remove plots that are from incomplete planting slices (had other management overlapping a portion of the planting unit)
d.trt <- d.trt %>%
  filter(f.s.planting.slice.split == "no")

# Remove plots that are from roadside salvage+planting stringers 
d.trt <- d.trt %>%
  filter(f.s.stringer == "no")



#### Perform fire-specific stratification ####
## NOTE that some internal plots that were salvaged are in the database twice if needed to match with the salvage categories "both" and "planted"

#### 1. Narrow the environmental space based on the intersection of env. space represented by each planting year (near to seed source only)  ####

### Moontelope ###

## First category: neither salvage, no prep, no release, no thin
foc.fire.name <- "2007MOONTELOPE - Plumas NF"

#environmental categories for stratification
rad.cats <- 2
elev.cats <- 2

foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "no"
foc.replanted <- "no"
foc.thinned <- "no"
foc.yr.pltd <- c("1","2","3")

d.foc <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  released %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yr.pltd)

plt.yrs <- unique(d.foc$yr.pltd)

env.ranges <- purrr::map(plt.yrs,plt.yr.env.range,plots=d.foc)
env.ranges <- bind_rows(env.ranges)

common.range <- env.ranges %>%
  summarize(elev.low = max(elev.low),
            elev.high = min(elev.high),
            rad.low = max(rad.low),
            rad.high = min(rad.high))

## expand each end of the ranges by 10% of the range

focal.range <- common.range %>%
  mutate(elev.low = elev.low-0.05*(elev.high-elev.low),
         elev.high = elev.high+0.05*(elev.high-elev.low),
         rad.low = rad.low-0.05*(rad.high-rad.low),
         rad.high = rad.high + 0.05*(rad.high-rad.low))

plotting.range <- focal.range

## plot all the plots and the focal range

yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")

ggplot(d.foc,aes(x=elev,y=rad,color=yr.pltd,shape=dist.nonhigh)) +
  geom_point(size=2) +
  theme_bw(15) +
  scale_shape_manual(values=c(16,1)) +
  scale_colour_manual(values=yr.colors) +
  labs(color="Yr planted",shape="Seed dist") +
  geom_rect(xmin=plotting.range$elev.low,
            xmax=plotting.range$elev.high,
            ymin=plotting.range$rad.low,
            ymax=plotting.range$rad.high,
            fill=NA,color="black") +
  labs(title=d.foc$fire.dist2[1])


###

## Define the environmental quadrants
# In a data frame with each row contianing a quadrant definition

rad.low <- focal.range$rad.low
rad.high <- focal.range$rad.high
elev.low <- focal.range$elev.low
elev.high <- focal.range$elev.high

env.quads <- define.quadrants(rad.low,rad.high,elev.low,elev.high,rad.cats,elev.cats)


### For each planting year, select plots stratified across the focal environmental range, using as few FACTS units (first planting SUIDs) as possible ###

# only look at plots that were planted in yr 1 and were treated
plt.yr <- "1"

#only look at plots in the focal planting year, and that were treated
d.foc.yr <- d.foc %>%
  filter(yr.pltd == plt.yr &
         type == "treatment" &
         dist.non.high < 80)
  

## What first planting SUIDS are we working with?

suids <- unique(d.foc.yr$f.s.first.planting.suid)

## Plot what we are working with

ggplot(d.foc.yr,aes(x=elev,y=rad,color=f.s.first.planting.suid,shape=dist.nonhigh)) +
  geom_point(size=2) +
  theme_bw(15) +
  scale_shape_manual(values=c(16,1)) +
  labs(color="SUID",shape="Seed dist") +
  geom_rect(xmin=plotting.range$elev.low,
            xmax=plotting.range$elev.high,
            ymin=plotting.range$rad.low,
            ymax=plotting.range$rad.high,
            fill=NA,color="red") +
  labs(title=d.foc$fire.dist2[1])

## X 1. Divide into named quadrants based on the variables to stratify by
## 2. For each planting unit, determint the number of sub-quadrants it covers within each quadrant
## 3. Compare planting units based on how thoroughly they cover each sub-quadrant

### Create subquadrants for each quadrant and compile into a data frame ###
sub.quads.df <- data.frame()

# so for each quadrant
for(j in 1:nrow(env.quads)) {
  
  env.quad <- env.quads[j,]

  #define sub-quadrants #! we can adjust the number of sub-quads here based on the desired number of survey plots
  sub.quads <- define.quadrants(env.quad$rad.low,env.quad$rad.high,env.quad$elev.low,env.quad$elev.high,rad.cats.arg=2,elev.cats.arg=2)

    for(k in 1:nrow(sub.quads)) {
    
    sub.quad <- sub.quads[k,]
    
    sub.quad.df <- data.frame(quad.label=env.quad$label,
                              subquad.label=paste0("s",sub.quad$label),
                              elev.low=sub.quad$elev.low,
                              elev.high=sub.quad$elev.high,
                              rad.low=sub.quad$rad.low,
                              rad.high=sub.quad$rad.high)
    
    sub.quads.df <- rbind(sub.quads.df,sub.quad.df)
  }
}

sub.quads.df$overall.label <- paste0(sub.quads.df$quad.label,sub.quads.df$subquad.label)

### determine which subquad each plot belongs to

# go through each subquad, and assign its overall label to each plot that falls within it
d.foc.yr.classified <- data.frame()
for(i in 1:nrow(sub.quads.df)) {
  
  subquad <- sub.quads.df[i,]

  plots.in.subquad <- d.foc.yr %>%
    filter(elev > subquad$elev.low,
           elev < subquad$elev.high,
           rad > subquad$rad.low,
           rad < subquad$rad.high)
  
  if(nrow(plots.in.subquad) == 0) {
    next()
  }
  
  plots.in.subquad$quad.label <- subquad$quad.label
  plots.in.subquad$subquad.label <- subquad$subquad.label
  plots.in.subquad$overall.label <- subquad$overall.label
  
  if(nrow(d.foc.yr.classified) == 0) {
    d.foc.yr.classified <- plots.in.subquad
  } else {
    d.foc.yr.classified <- rbind(d.foc.yr.classified,plots.in.subquad)    
  }
}

## To visualize the classifications, plot the classified plots by quadrant
ggplot(d.foc.yr.classified,aes(x=elev,y=rad,color=quad.label,shape=subquad.label)) +
  geom_point(size=2) +
  theme_bw(15) +
  scale_shape_manual(values=c(16,1,2,17)) +
  labs(color="Quad",shape="Subquad") +
  geom_rect(xmin=plotting.range$elev.low,
            xmax=plotting.range$elev.high,
            ymin=plotting.range$rad.low,
            ymax=plotting.range$rad.high,
            fill=NA,color="red") +
  labs(title=d.foc$fire.dist2[1])


## Summarize by suid and quadrant: what subquadrants are represented by each SUID
concatenate.unique <- function(x) {
  
}

d.foc.yr.classified <- st_drop_geometry(d.foc.yr.classified)

suid.summ <- d.foc.yr.classified %>%
  group_by(f.s.first.planting.suid, quad.label) %>%
  summarize(subquads = list(unique(subquad.label)),
            n.subquads = length(unique(subquad.label))) %>%
  ungroup()


### Now the goal is to find the smallest number of SUIDS that can fill at least 2 subquads of every quad

# for keeping track of which SUIDs we still have as candidates to add
# initialize it with all SUIDS that contain points of the focal management category
suids.remaining <- unique(suid.summ$f.s.first.planting.suid)

# for keeping track of which SUIDS we've already added
suids.selected <- NULL

# for keeping track of which subquads have been filled, use the subquads DF
sub.quads.df$nplots <- 0
sub.quads.df$nplots.tier1 <- 0
sub.quads.df$nplots.tier2 <- 0
sub.quads.df$nplots.tier3 <- 0

# for keeping track of which tier of the search we are in
current.tier <- 1
current.iteration <- 1
iteration.when.1st.tier.reached <- 0
iteration.when.2nd.tier.reached <- 0
iteration.when.3rd.tier.reached <- 0


## start the loop here
while((current.tier < 4) & (length(suids.remaining) != 0)) {  
 
   tier.just.completed <- FALSE
  
  #1. Compute the current stratification scores
  strat.scores <- score.stratif(sub.quads.df,simp=TRUE)
  
  #2. Test all remaining suids to see which best improves the stratification scores
  suids.remaining ##! need to remove the SUIDS that were added in the last iteration
  
  
  new.strat.scores <- map(suids.remaining,.f=strat.scores.w.new.suid,sub.quads.df=sub.quads.df,d.foc.yr.classified=d.foc.yr.classified)
  new.strat.scores.df <- bind_rows(new.strat.scores)
  new.strat.scores.df$suid <- suids.remaining
  
  #find all the suids that achieve the greatest numver of quadrants that have at least 2+ subquadrants with at least one plot  ###!!! might want to reverse this to start looking for subquads with at least 3 plots? but then that deprioritizes SUIDs that fill lots of quadrants with one plot
  max.quads.filled <- max(new.strat.scores.df$n.quads.w.two.plus.subquads)
  best.scores <- new.strat.scores.df %>%
    filter(n.quads.w.two.plus.subquads == max.quads.filled)
  
  #among those, find the one that achieve the greatest number of quadrants that have at least 2+ subquadrants with at least 2 plots
  max.quads.filled.double <- max(best.scores$n.quads.w.two.plus.subquads.double)
  best.scores <- best.scores %>%
    filter(n.quads.w.two.plus.subquads.double == max.quads.filled.double)
  
  #among those, find the one that achieve the greatest number of quadrants that have at least 2+ subquadrants with at least 3 plots
  max.quads.filled.triple <- max(best.scores$n.quads.w.two.plus.subquads.triple)
  best.scores <- best.scores %>%
    filter(n.quads.w.two.plus.subquads.triple == max.quads.filled.triple)
  
  #among those, find the one that fills the greatest number of subquads
  max.sub.quads.filled <- max(best.scores$n.sub.quads.filled)
  best.scores <- best.scores %>%
    filter(n.sub.quads.filled == max.sub.quads.filled)
  
  #! here, we should take the SUID with the greatest number of plots total: #amont those, find the one that has the greatest number of plots total
  
  # now, in case there are still multiple tied, just take the first one (this should be rare)
  best.scores <- best.scores[1,]
  
  ## compute the new strat scores with that suid
  new.strat.scores <- strat.scores.w.new.suid(best.scores$suid,sub.quads.df,d.foc.yr.classified)
  
  
  #make sure they went up (if already hit the first tier goal, check for increase in second tier goal instead, then third tier)
  scores.increased <- FALSE
  if(current.tier == 1) {
    if(new.strat.scores$n.quads.w.two.plus.subquads > strat.scores$n.quads.w.two.plus.subquads) {
      scores.increased <- TRUE
    } else {
      scores.increased <- FALSE
    }
  } else if (current.tier == 2) {
    if(new.strat.scores$n.quads.w.two.plus.subquads.double > strat.scores$n.quads.w.two.plus.subquads.double) {
      scores.increased <- TRUE
    } else {
      scores.increased <- FALSE
    }
  } else if (current.tier == 3) {
    if(new.strat.scores$n.quads.w.two.plus.subquads.triple > strat.scores$n.quads.w.two.plus.subquads.triple) {
      scores.increased <- TRUE
    } else {
      scores.increased <- FALSE
    }
  }
  
  ## if we were looking to increase first tier scoring, did we meet the goal?
  if(current.tier == 1 & new.strat.scores$n.quads.w.two.plus.subquads == 4) {
    
    # add the new selected SUID to the list of selected SUIDs
    new.suid <- best.scores$suid
    suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
    suids.selected <- rbind(suids.selected,suids.selected.new)
    sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=1)
    suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
    
    # go on to try to achieve tier 2 goal
    tier.just.completed <- TRUE
    iteration.when.1st.tier.reached <- current.iteration
    
  } else if (current.tier == 1 & scores.increased == TRUE) { # score increased but did not meet goal
    
    # add the new selected SUID to the list of selected SUIDS, but keep searching to meet goal
    new.suid <- best.scores$suid
    suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
    suids.selected <- rbind(suids.selected,suids.selected.new)
    sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=1)
    suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
    
    
  } else if(current.tier == 1 & scores.increased == FALSE) { # if we were looking to increase first tier scoring (n quads with 2+ subquads filled once), but we didn't manage to increase it, print message, then go on to search for second tier goal
  
    cat("\nFirst tier strat goal not reached for:",foc.fire.name,", Plant year:",plt.yr)
    cat("\n   Filled",new.strat.scores$n.quads.w.two.plus.subquads,"quadrants suitably.")
    tier.just.completed <- TRUE
    iteration.when.1st.tier.reached <- current.iteration
  
  } else if(current.tier == 2 & new.strat.scores$n.quads.w.two.plus.subquads.double == 4) { # if we were looking to increase second tier scoring, did we meet goal?
    
    # add the new selected SUID to the list of selected SUIDs
    new.suid <- best.scores$suid
    suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
    suids.selected <- rbind(suids.selected,suids.selected.new)
    sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=2)
    suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
    
    
    # go on to try to achieve tier 3 goal
    tier.just.completed <- TRUE
    iteration.when.2st.tier.reached <- current.iteration
    
  } else if (current.tier == 2 & scores.increased == TRUE) { # score increased but did not meet goal
    
    # add the new selected SUID to the list of selected SUIDS, but keep searching to meet goal
    new.suid <- best.scores$suid
    suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
    suids.selected <- rbind(suids.selected,suids.selected.new)
    sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=2)
    suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
    
    
  } else if(current.tier == 2 & scores.increased == FALSE) { # if we were looking to increase second tier scoring (n quads w/ 2+ subquads filled twice), but we didn't manage to increase it, print message, then go on to search for third tier goal
    
    cat("\nSecond tier strat goal not reached for:",foc.fire.name,", Plant year:",plt.yr)
    cat("\n   Filled",new.strat.scores$n.quads.w.two.plus.subquads.double,"quadrants suitably.")
    tier.just.completed <- TRUE
    iteration.when.2nd.tier.reached <- current.iteration
  
  } else if(current.tier == 3 & new.strat.scores$n.quads.w.two.plus.subquads.triple == 4) { # if we were looking to increase third tier scoring, did we meet goal?
    
    # add the new selected SUID to the list of selected SUIDs
    new.suid <- best.scores$suid
    suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
    suids.selected <- rbind(suids.selected,suids.selected.new)
    sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=3)
    suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
    
    
    # completed tier 3 goal
    tier.just.completed <- TRUE
    iteration.when.3rd.tier.reached <- current.iteration
    
  } else if (current.tier == 3 & scores.increased == TRUE) { # score increased but did not meet goal
    
    # add the new selected SUID to the list of selected SUIDS, but keep searching to meet goal
    new.suid <- best.scores$suid
    suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
    suids.selected <- rbind(suids.selected,suids.selected.new)
    sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=3)
    suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
    
    
  } else if(current.tier == 3 & scores.increased == FALSE) { # if we were looking to increase third tier scoring (n quads w/ 2+ subquads filled triply), but we didn't manage to increase it, print message, then consider tier complete
    
    cat("\nThird tier strat goal not reached for:",foc.fire.name,", Plant year:",plt.yr)
    cat("\n   Filled",new.strat.scores$n.quads.w.two.plus.subquads.triple,"quadrants suitably.")
    tier.just.completed <- TRUE
    iteration.when.3rd.tier.reached <- current.iteration
  }
    
  current.iteration <- current.iteration + 1
  
  # if we complieted a tier (even if it was incomplete but there was no way to improve it), print a message
  if(tier.just.completed) {
    cat("\nFire:",foc.fire.name,", Plant year:",plt.yr,"-- Tier",current.tier," completed")
    current.tier <- current.tier + 1 ## if this goes up to 4, the loop will exit upon starting the next iteration
  }
  
  # if we used up all the suid options, print a message that we did not achieve all tier goals
  if(length(suids.remaining) == 0 & (current.tier != 4)) {
    cat("\nFire:",foc.fire.name,", Plant year:",plt.yr,"-- All SUID options exhausted without completing three tiers")
  }
}


suids.remaining
sub.quads.df
suids.selected

####$$$ In sub.quads.df, nplots.tier1 is the number of plots within that subquad that were selected in the first tier of stratifiction
# that is, finding SUIDS that filled at lesat 2 subquads of every quad at least once). In order to minimize the number of SUIDs we survey:
# In each quad, our candidates are subquads that have a least 3 nplots. If none have at least 3 nplots, 2 nplots is OK.
# Then, for each of those, choose the top two subquads in terms of nplots.tier1. If none of those has at least 3 plots,#
#select those that would maximize the total sum of nplots tier1 + tier2. then if not that, tier1 + tier2 + tier3


# and if so update the strat records
# update the current strat scores
# then see if we hit our goal (for first tier, then for second tier)

# after meeting all tier goals, pick which sub-quads to be focal for plots: first all those that have 2-3 first tier plots, then add those where first-plus second-tier plots bring up to 2-3 plots, until we have enough subquadrants filled.










#! note that may want to have an addl scoring column that reflects the number of quads that have 2+ subquads with at least two (or three) plots (so we can have second and third tier)

## will need a way to make sure that we have second-tier plots in every quadrant (i.e., two-to-three plots), and if not, add more SUIDs, but make sure those SUIDS stay marked as required only for second-tier

## when finding second-tier plots for each sub-quadrant, first draw upon the first-tier SUIDs

## when possible, second-tier plots should be from the same quadrant as the first-tier plots. also, second-tier plots should be from first-tier SUIDS when avaialble











## plot naming

# FF A M ER ER T P

# FF = fire
# A = planting year
# M = other management differentiator
# ER = elev and rad quad
# ER = elev and rad subquad
# T = type (Trt, control, interior)
# P = priority (which tier)


########~~~~~ For each subquad, figure out in which in which tier of suids it was filled once, twice, three times
  
  ### And for each quad, prioritize the subquads that were triply fileld in tier 1, then doubly filled in tier 1, then triply filled in tier 2, then doubly in 2, then triply in tier 3, then doubly in 3








