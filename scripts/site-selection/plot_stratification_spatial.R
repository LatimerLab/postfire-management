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

#function to find outliers
# an outlier is a point with a value so extreme (high or low) that it has a < 1% chance of being observed, given the size of the dataset and assuming a normal distribution
outliers <- function(x) {
  mean <- mean(x)
  sd <- sd(x)
  x.dev <- abs(x-mean)
  prob <- pnorm(x.dev,0,sd,lower.tail=FALSE)

  threshold <- 1/(length(x)*3)
  
  outlier <- (prob < threshold)
  return(outlier)
}

#possible future alternative outliers alg: make sure there are at least 5% of the total number of plots (or, at least 3 plots) within 10% of the range of variable values of the focal plot

# function to find the range of elev and rad encompassed by all focal plots
plt.yr.env.range <- function(plt.yr,plots) {
  
  fire <- plots$fire2[1]
  
  # filter to the current planting year
  plots.yr <- plots %>%
    filter(yr.pltd == plt.yr &
             dist.non.high < 80) # and when computing range, only look at plots close to seed source (this means only looking at perimeter plots, because previously we filtered out the internal plots close to seed source)
  
  # find the non-outliers of elevation and radiation
  elevs <- plots.yr$elev
  elevs.keep <- elevs[!outliers(elevs)]
  elevs.outliers <- elevs[outliers(elevs)]
  elev.low <- min(elevs.keep)
  elev.high <- max(elevs.keep)
  
  if(length(elevs.outliers) > 0) {
    cat("\nOn",fire,"planting year",plt.yr,"removed elev outliers:",elevs.outliers)
  }
  
  rads <- plots.yr$rad
  rads.keep <- rads[!outliers(rads)]
  rads.outliers <- rads[outliers(rads)]
  rad.low <- min(rads.keep)
  rad.high <- max(rads.keep)
  
  if(length(rads.outliers) > 0) {
    cat("\nOn",fire,"planting year",plt.yr,"removed rad outliers:",rads.outliers)
  }
  
  env.range.yr <- data.frame(plt.yr,elev.low,elev.high,rad.low,rad.high)
  row.names(env.range.yr) <- NULL
    
  return(env.range.yr)
  
}


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

define.quadrants <- function(rad.low.arg,rad.high.arg,elev.low.arg,elev.high.arg,rad.cats.arg,elev.cats.arg) {

  # create radiation and elev breaks
  rad.breaks <- seq(from=rad.low.arg,to=rad.high.arg,length.out=rad.cats.arg+1)
  elev.breaks <- seq(from=elev.low.arg,to=elev.high.arg,length.out=elev.cats.arg+1)
  
  # put into data frame form
  rad.matrix <- cbind(rad.low = rad.breaks[-(rad.cats.arg+1)],
                      rad.high = rad.breaks[-1]) %>%
    as.data.frame()
  rad.matrix$rad.label <- paste0("R",1:nrow(rad.matrix))
  
  elev.matrix <- cbind(elev.low = elev.breaks[-(elev.cats.arg+1)],
                      elev.high = elev.breaks[-1]) %>%
    as.data.frame()
  elev.matrix$elev.label <- paste0("E",1:nrow(elev.matrix))
  
  # merge the two data frames
  # first replicate the rad data frame the number of times equal to the number of rows in the elev data frame
  rad.matrix.rep <- rad.matrix[rep(row.names(rad.matrix),each=nrow(elev.matrix)),]
  elev.matrix.rep <- elev.matrix[rep(row.names(elev.matrix),times=nrow(rad.matrix)),]
  
  env.cats <- bind_cols(rad.matrix.rep,elev.matrix.rep)
  env.cats$label <- paste0(env.cats$elev.label,env.cats$rad.label)
  
  return(env.cats)
  
}

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
for(j in 1:nrow(env.cats)) {
  
  env.cat <- env.cats[j,]

  #define sub-quadrants #! we can adjust the number of sub-quads here based on the desired number of survey plots
  sub.quads <- define.quadrants(env.cat$rad.low,env.cat$rad.high,env.cat$elev.low,env.cat$elev.high,rad.cats.arg=2,elev.cats.arg=2)

    for(k in 1:nrow(sub.quads)) {
    
    sub.quad <- sub.quads[k,]
    
    sub.quad.df <- data.frame(quad.label=env.cat$label,
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
  labs(color="SUID",shape="Seed dist") +
  geom_rect(xmin=plotting.range$elev.low,
            xmax=plotting.range$elev.high,
            ymin=plotting.range$rad.low,
            ymax=plotting.range$rad.high,
            fill=NA,color="red") +
  labs(title=d.foc$fire.dist2[1])


## Summarize by suid and quadrant: what subquadrants were represented by each SUID
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
suids.remaining <- suid.summ

# for keeping track of which subquads have been filled, use the subquads DF
sub.quads.df$nplots <- NA

## start the loop here

# count the number of subquads filled
sub.quads.filled <- sum(sub.quads.df$nplots>0,na.rm=TRUE)

# count the number of quadrants with 2+ subquads filled
quads <- sub.quads.df %>%
  group_by(quad.label) %>%
  summarize(n.subquads.full = sum(nplots >= 1,na.rm=TRUE)) %>%
  mutate(two.plus.subquads.full = n.subquads.full >= 2) %>%
  ungroup()

n.quads.w.two.plus.subquads <- sum(quads$two.plus.subquads.full,na.rm=TRUE)



## will need a way to make sure that we have second-tier plots in every quadrant, and if not, add more SUIDs, but make sure those SUIDS stay marked as required only for second-tier

## when finding second-tier plots for each sub-quadrant, first draw upon the first-tier SUIDs

## when possible, second-tier plots should be from the same quadrant as the first-tier plots







## plot naming

# FF A M ER ER T P

# FF = fire
# A = planting year
# M = other management differentiator
# ER = elev and rad quad
# ER = elev and rad subquad
# T = type (Trt, control, interior)
# P = priority (which tier)











