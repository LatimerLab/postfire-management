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

source("scripts/site-selection/plot_stratification_functions.R")

crs <- 3310 # CA albers

set.seed(1)



#### Load the data ####
d.full <- st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered.gpkg",stringsAsFactors=FALSE)
d.trt <- d.full[d.full$type %in% c("treatment"),] # #! previously we allowed internal plots here.  ## remove the paired "control" plots because they do not have the environemntal data associated with them


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


## make a DF of all the fire, management, planting year combinations that we want to get stratified plots for.
mgmt.cats.df <- NULL

### Moontelope ###

## First category: neither salvage, no prep, no release, no thin
foc.fire.name <- "2007MOONTELOPE - Plumas NF"
# Environmental categories for stratification
rad.cats <- 2
elev.cats <- 2
subquads.goal <- 2
# Management variables
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "no"
foc.replanted <- "no"
foc.thinned <- "no"
mgmt.cat.string <- paste0("Salv: ",foc.salv.cat," - Prep: ",foc.site.prepped," - Rel: ",foc.released," - Thn: ",foc.thinned," - Replt: ",foc.replanted)
custom.rad.low <- NA
custom.rad.high <- NA
custom.elev.low <- NA
custom.elev.high <- NA
mgmt.cats.df.row <- data.frame(foc.fire.name,rad.cats,elev.cats,foc.salv.cat,foc.site.prepped,foc.released,foc.thinned,foc.replanted,subquads.goal,custom.rad.low,custom.rad.high,custom.elev.low,custom.elev.high,mgmt.cat.string)
mgmt.cats.df.row$foc.yrs.pltd <- list(c("1","2","3"))
mgmt.cats.df.row$default.suids <- NA #list(c("0511022080104000000","0511022080102000001","0511022080100000000"))
mgmt.cats.df <- rbind(mgmt.cats.df,mgmt.cats.df.row)

## Second category: planted salvage, no prep, no release, no thin
foc.fire.name <- "2007MOONTELOPE - Plumas NF"
# Environmental categories for stratification
rad.cats <- 2
elev.cats <- 2
subquads.goal <- 4
# Management variables
foc.salv.cat <- "planted"
foc.site.prepped <- "no"
foc.released <- "no"
#foc.replanted <- "no"
foc.thinned <- "no"
mgmt.cat.string <- paste0("Salv: ",foc.salv.cat," - Prep: ",foc.site.prepped," - Rel: ",foc.released," - Thn: ",foc.thinned)
custom.rad.low <- NA
custom.rad.high <- NA
custom.elev.low <- NA
custom.elev.high <- NA
mgmt.cats.df.row <- data.frame(foc.fire.name,rad.cats,elev.cats,foc.salv.cat,foc.site.prepped,foc.released,foc.thinned,subquads.goal,custom.rad.low,custom.rad.high,custom.elev.low,custom.elev.high,mgmt.cat.string)
mgmt.cats.df.row$foc.yrs.pltd <- list(c("2","3"))
mgmt.cats.df <- rbind(mgmt.cats.df,mgmt.cats.df.row)

### 2008 Piute ###

## First category: neither salvage, no prep, no release, no thin
foc.fire.name <- "2008PIUTE - Sequoia NF"
# Environmental categories for stratification
rad.cats <- 2
elev.cats <- 1
subquads.goal <- 2
# Management variables
foc.salv.cat <- "neither"
foc.site.prepped <- "no"
foc.released <- "no"
#foc.replanted <- "no"
foc.thinned <- "no"
mgmt.cat.string <- paste0("Salv: ",foc.salv.cat," - Prep: ",foc.site.prepped," - Rel: ",foc.released," - Thn: ",foc.thinned)
# custom bounds for bounding box
custom.rad.low <- 4200
custom.rad.high <- 8300
custom.elev.low <- 2200
custom.elev.high <- 2400
mgmt.cats.df.row <- data.frame(foc.fire.name,rad.cats,elev.cats,foc.salv.cat,foc.site.prepped,foc.released,foc.thinned,subquads.goal,custom.rad.low,custom.rad.high,custom.elev.low,custom.elev.high,mgmt.cat.string)
mgmt.cats.df.row$foc.yrs.pltd <- list(c("2","3","4+"))
mgmt.cats.df <- rbind(mgmt.cats.df,mgmt.cats.df.row)




suid.prioritization <- NULL # this will store a growing data frame

### For each management category
for(k in 1:nrow(mgmt.cats.df)) {
  
  mgmt.cats.df.row <- mgmt.cats.df[k,]
  
  foc.fire.name <- as.character(mgmt.cats.df.row$foc.fire.name)
  rad.cats <- mgmt.cats.df.row$rad.cats
  elev.cats <- mgmt.cats.df.row$elev.cats
  max.cats <- rad.cats*elev.cats
  foc.salv.cat <- mgmt.cats.df.row$foc.salv.cat
  foc.site.prepped <- mgmt.cats.df.row$foc.site.prepped
  foc.released <- mgmt.cats.df.row$foc.released
  mgmt.cats.string <- mgmt.cats.df.row$mgmt.cats.string
  subquads.goal <- mgmt.cats.df.row$subquads.goal
  #foc.replanted <- mgmt.cats.df.row$foc.replanted
  foc.thinned <- mgmt.cats.df.row$foc.thinned
  foc.yrs.pltd <- mgmt.cats.df.row$foc.yrs.pltd[[1]]
  foc.default.suids <- mgmt.cats.df.row$default.suids[[1]]

  
  
  
  #### 1. Narrow the environmental space based on the intersection of env. space represented by each planting year (near to seed source only)  ####
  
  # turn mgmt cat into a string for saving it

  d.foc <- d.trt %>%
    dplyr::filter(fire.dist2 %in% foc.fire.name &
                    salv.cat %in% foc.salv.cat &
                    site.prepped %in% foc.site.prepped &
                    release.txt %in% foc.released &
                    #replanted %in% foc.replanted &
                    thinned %in% foc.thinned &
                    yr.pltd %in% foc.yrs.pltd)
  
  plt.yrs <- as.character(foc.yrs.pltd)
  
  
  if(is.na(custom.rad.low)) { # no custom rad specified, so calc automatically
    
    env.ranges <- purrr::map(plt.yrs,plt.yr.env.range,plots=d.foc)
    env.ranges <- suppressWarnings(bind_rows(env.ranges))
    env.ranges$plt.yr <- as.character(env.ranges$plt.yr)
    
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
    
  } else { #use custom specified values
    focal.range <- data.frame(elev.low=custom.elev.low,elev.high=custom.elev.high,rad.low=custom.rad.low,rad.high=custom.rad.high)
  }
      
  
  plotting.range <- focal.range
  
  ## plot all the plots and the focal range
  
  yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")
  
  g <- ggplot(d.foc,aes(x=elev,y=rad,color=yr.pltd,shape=dist.nonhigh)) +
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
  print(g)
  
  ###
  
  ## Define the environmental quadrants
  # In a data frame with each row contianing a quadrant definition
  
  rad.low <- focal.range$rad.low
  rad.high <- focal.range$rad.high
  elev.low <- focal.range$elev.low
  elev.high <- focal.range$elev.high
  
  env.quads <- define.quadrants(rad.low,rad.high,elev.low,elev.high,rad.cats,elev.cats)
  
  
  
  ### For each planting year
  for(plt.yr in plt.yrs) {
    
    
    #only look at plots in the focal planting year, and that were treated
    d.foc.yr <- d.foc %>%
      filter(yr.pltd == plt.yr &
             type == "treatment" &
             dist.non.high < 80)
      
    
    ## What first planting SUIDS are we working with?
    
    suids <- unique(c(d.foc.yr$f.s.first.planting.suid,foc.default.suids))
    
    ## Plot what we are working with
    
    g <- ggplot(d.foc.yr,aes(x=elev,y=rad,color=f.s.first.planting.suid,shape=dist.nonhigh)) +
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
    #print(g)
    
    default.suids.remaining <- unlist(foc.default.suids)
    
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
    g <- ggplot(d.foc.yr.classified,aes(x=elev,y=rad,color=quad.label,shape=subquad.label)) +
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
    #print(g)
    
    
    ## Summarize by suid and quadrant: what subquadrants are represented by each SUID
    
    
    # concatenate.unique <- function(x) {
    #   
    # }
    
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
    
    suids.remaining <- setdiff(suids.remaining,default.suids.remaining)
    
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
    
    # for keeping track of the highest tier goal met
    highest.tier.goal.met <- 0
    
    ## start the loop here
    while((current.tier < 4) & (length(suids.remaining)+length(default.suids.remaining) != 0)) {  
     
      tier.just.completed <- FALSE
      added.defauld.suid <- FALSE
      
      #1. Compute the current stratification scores
      strat.scores <- score.stratif(sub.quads.df,simp=TRUE)
      
      
      # if there are default SUIDs that need to be added, just select them as the ones to be added
      if(length(default.suids.remaining) > 0) {
        suid.to.add <- default.suids.remaining[1]
        default.suids.remaining <- setdiff(default.suids.remaining,suid.to.add)
        
        new.strat.scores <- strat.scores.w.new.suid(suid.to.add,sub.quads.df,d.foc.yr.classified,subquads.goal=subquads.goal)
        
        added.default.suid <- TRUE
        
      } else {
      
      
        #2. Test all remaining suids to see which best improves the stratification scores
        suids.remaining ##! need to remove the SUIDS that were added in the last iteration
        
        
        new.strat.scores <- map(suids.remaining,.f=strat.scores.w.new.suid,sub.quads.df=sub.quads.df,d.foc.yr.classified=d.foc.yr.classified,subquads.goal=subquads.goal)
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
        new.strat.scores <- strat.scores.w.new.suid(best.scores$suid,sub.quads.df,d.foc.yr.classified,subquads.goal=subquads.goal)
        
        suid.to.add <- best.scores$suid
      }
      
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
      
      ## make it think the scores went up when we added the default SUID; otherwise, if the default SUID was not helpful, it would exit the loop
      if(added.default.suid) {
        scores.increased <- TRUE
      }
      
      ## if we were looking to increase first tier scoring, did we meet the goal?
      if(current.tier == 1 & new.strat.scores$n.quads.w.two.plus.subquads == max.cats) {
        
        # record that we met the goal
        highest.tier.goal.met <- 1
        
        # add the new selected SUID to the list of selected SUIDs
        new.suid <- suid.to.add
        suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
        suids.selected <- rbind(suids.selected,suids.selected.new)
        sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=1)
        suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
        
        # go on to try to achieve tier 2 goal
        tier.just.completed <- TRUE
        iteration.when.1st.tier.reached <- current.iteration
        
      } else if (current.tier == 1 & scores.increased == TRUE) { # score increased but did not meet goal
        
        # add the new selected SUID to the list of selected SUIDS, but keep searching to meet goal
        new.suid <- suid.to.add
        suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
        suids.selected <- rbind(suids.selected,suids.selected.new)
        sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=1)
        suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
        
        
      } else if(current.tier == 1 & scores.increased == FALSE) { # if we were looking to increase first tier scoring (n quads with 2+ subquads filled once), but we didn't manage to increase it, print message, then go on to search for second tier goal
      
        cat("\nFirst tier strat goal not reached for:",foc.fire.name,", Plant year:",plt.yr)
        cat("\n   Filled",new.strat.scores$n.quads.w.two.plus.subquads,"quadrants suitably.")
        tier.just.completed <- TRUE
        iteration.when.1st.tier.reached <- current.iteration
      
      } else if(current.tier == 2 & new.strat.scores$n.quads.w.two.plus.subquads.double == max.cats) { # if we were looking to increase second tier scoring, did we meet goal?
        
        # record that we met the goal
        highest.tier.goal.met <- 2
        
        # add the new selected SUID to the list of selected SUIDs
        new.suid <- suid.to.add
        suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
        suids.selected <- rbind(suids.selected,suids.selected.new)
        sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=2)
        suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
        
        
        # go on to try to achieve tier 3 goal
        tier.just.completed <- TRUE
        iteration.when.2st.tier.reached <- current.iteration
        
      } else if (current.tier == 2 & scores.increased == TRUE) { # score increased but did not meet goal
        
        # add the new selected SUID to the list of selected SUIDS, but keep searching to meet goal
        new.suid <- suid.to.add
        suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
        suids.selected <- rbind(suids.selected,suids.selected.new)
        sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=2)
        suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
        
        
      } else if(current.tier == 2 & scores.increased == FALSE) { # if we were looking to increase second tier scoring (n quads w/ 2+ subquads filled twice), but we didn't manage to increase it, print message, then go on to search for third tier goal
        
        cat("\nSecond tier strat goal not reached for:",foc.fire.name,", Plant year:",plt.yr)
        cat("\n   Filled",new.strat.scores$n.quads.w.two.plus.subquads.double,"quadrants suitably.")
        tier.just.completed <- TRUE
        iteration.when.2nd.tier.reached <- current.iteration
      
      } else if(current.tier == 3 & new.strat.scores$n.quads.w.two.plus.subquads.triple == max.cats) { # if we were looking to increase third tier scoring, did we meet goal?
        
        # record that we met the goal
        highest.tier.goal.met <- 3
        
        # add the new selected SUID to the list of selected SUIDs
        new.suid <- suid.to.add
        suids.selected.new <- data.frame(suid=new.suid,tier=current.tier)
        suids.selected <- rbind(suids.selected,suids.selected.new)
        sub.quads.df <- add.suid(new.suid,d.foc.yr.classified,sub.quads.df,tier=3)
        suids.remaining <- dplyr::setdiff(suids.remaining,new.suid)
        
        
        # completed tier 3 goal
        tier.just.completed <- TRUE
        iteration.when.3rd.tier.reached <- current.iteration
        
      } else if (current.tier == 3 & scores.increased == TRUE) { # score increased but did not meet goal
        
        # add the new selected SUID to the list of selected SUIDS, but keep searching to meet goal
        new.suid <- suid.to.add
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
    
    
    ## Create a data frame, where columns are: Fire, mgmt cat, plant year, suids.selected DF, and sub.quads DF, also columns with SUIDS for Tier 1, SUIDS for Tier 2, and SUIDS for Tier 3
    
    suid.prioritization.single <- data.frame(foc.fire.name,mgmt.cat=mgmt.cat.string,plant.yr=plt.yr,highest.tier.goal.met,salv.cat=foc.salv.cat,site.prepped=foc.site.prepped,released=foc.released,thinned=foc.thinned,replanted=foc.replanted)
    
    
    
    suid.prioritization.single$suids.selected <- list(suids.selected)
    suid.prioritization.single$sub.quads <- list(sub.quads.df)
    
    suids.tier1 <- as.character(suids.selected[suids.selected$tier==1,"suid"])
    suids.tier2 <- as.character(suids.selected[suids.selected$tier==2,"suid"])
    suids.tier3 <- as.character(suids.selected[suids.selected$tier==3,"suid"])
    
    suid.prioritization.single$suids.tier1 <- list(suids.tier1)
    suid.prioritization.single$suids.tier2 <- list(suids.tier2)
    suid.prioritization.single$suids.tier3 <- list(suids.tier3)
    
    suid.prioritization <- rbind(suid.prioritization,suid.prioritization.single)
  }
  
  
  ## make a plot of all plots in the first three tiers of suids, colored by planting year and shped by tier
  suid.prioritization.focal <- suid.prioritization %>%
    filter(foc.fire.name == foc.fire.name &
             mgmt.cat == mgmt.cat.string)
  
  suids.tier1 <- data.frame(tier=1,f.s.first.planting.suid=unlist(suid.prioritization$suids.tier1))
  suids.tier2 <- data.frame(tier=2,f.s.first.planting.suid=unlist(suid.prioritization$suids.tier2))
  suids.tier3 <- data.frame(tier=3,f.s.first.planting.suid=unlist(suid.prioritization$suids.tier3))
  
  suids.priorities.pre <- suppressWarnings(bind_rows(suids.tier1,suids.tier2))
  suids.priorities <- suppressWarnings(bind_rows(suids.priorities.pre,suids.tier3))
  
    
  ##!! now merge this with plots data so we have plot rad and elevation which can be colored by planting year and shaped by tier
  d.foc.suids.pri <- left_join(d.foc,suids.priorities,by="f.s.first.planting.suid") %>%
    filter(!is.na(tier)) %>%
    mutate(tier = as.character(tier),
           yr.pltd = as.character(yr.pltd)) %>%
    filter(dist.nonhigh == "< 80 m")
  
  
  yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")
  
  
  g <- ggplot(d.foc.suids.pri,aes(x=elev,y=rad,color=yr.pltd,shape=tier)) +
    geom_point(size=2.5) +
    theme_bw(15) +
    scale_shape_manual(values=c(16,1,8)) +
    scale_colour_manual(values=yr.colors) +
    labs(color="Yr planted",shape="Strat tier") +
    labs(title=d.foc.suids.pri$fire.dist2[1])+
    geom_rect(xmin=plotting.range$elev.low,
              xmax=plotting.range$elev.high,
              ymin=plotting.range$rad.low,
              ymax=plotting.range$rad.high,
              fill=NA,color="black")
  print(g)
  
  
  
}


#### Based on SUID prioritization, select actual plots in tiers, so we have the plots and also the top-priority SUIDs for study. ####

selected.subquads <- NULL

### For each row
for(i in 1:nrow(suid.prioritization)) {
  
  
  suid.pri.focal <- suid.prioritization[i,]
  
  ## Look up what the subquad goal was
  mgmt.cat.focal <- mgmt.cats.df %>%
    filter(foc.fire.name == suid.pri.focal$foc.fire.name &
             mgmt.cat.string == suid.pri.focal$mgmt.cat)
  subquads.goal <- mgmt.cat.focal$subquads.goal
  
  ### For each quad
  # First, what are the quads?
  sub.quads.focal <- suid.pri.focal$sub.quads[[1]]
  quads <- unique(sub.quads.focal$quad.label)
  
  for(j in 1:length(quads)) {
  
    quad <- quads[j]
    
    ## Find candidate subquads: those that hav at least 3 nplots.
    candidate.subquads <- sub.quads.focal %>%
      filter(quad.label == quad &
               nplots >= 3) %>%
      arrange(desc(nplots.tier1),desc(nplots.tier2),desc(nplots.tier3)) # sort by how many plots they had in the first round of searching
    
    ## Are there at least two subquads that hav at least 3 nplots?
    n.candidate.subquads <- nrow(candidate.subquads)
    
    ## If not, allow there to be at least 2 nplots
    if(n.candidate.subquads < subquads.goal) {
      
      addl.subquads.needed <- subquads.goal - n.candidate.subquads
    
      ## Find more candidate subquads: those that have 2 nplots (not >2 because we already included those with 3+, and not those with < 2 because that is too few for redundancy)
      addl.candidate.subquads <- sub.quads.focal %>%
        filter(quad.label == quad &
                 nplots == 2) %>%
        arrange(desc(nplots.tier1),desc(nplots.tier2),desc(nplots.tier3)) # sort by how many plots they had in the first round of searching
      
      ## Are there enough more?
      if(nrow(addl.candidate.subquads) < addl.subquads.needed) {
        
        
        
        # if not, allow there to be at least 1 nplots
        ## Find more candidate subquads: those that have 1 nplots (not >1 because we already included those with 2+, ## warning, selecting those with < 2 will mean not enough for redundancy)
        addl.addl.candidate.subquads <- sub.quads.focal %>%
          filter(quad.label == quad &
                   nplots == 1) %>%
          arrange(desc(nplots.tier1),desc(nplots.tier2),desc(nplots.tier3)) # sort by how many plots they had in the first round of searching
        
        ## are there enough more? if not...
        if(nrow(addl.candidate.subquads) + nrow(addl.addl.candidate.subquads) < addl.subquads.needed) {
        
          
          #How many were there?
          n.candidate.subquads <- nrow(candidate.subquads) + nrow(addl.candidate.subquads)
          cat("\nFor",as.character(suid.pri.focal$foc.fire.name),", quad ",as.character(quad),": only",n.candidate.subquads,"subquads with >= 2 plots; goal was",subquads.goal)
        
        }
        
      }
      
      candidate.subquads <- rbind(candidate.subquads,addl.candidate.subquads)
      candidate.subquads <- rbind(candidate.subquads,addl.addl.candidate.subquads)
      n.candidate.subquads <- nrow(candidate.subquads)
      
    }
    
    subquads.goal <- min(subquads.goal,n.candidate.subquads)
    
    ## For sorting for priority selection of subquads, make new variables
    candidate.subquads <- candidate.subquads %>%
      mutate(nplots.tiers12 = nplots.tier1 + nplots.tier2) %>%
      mutate(nplots.tiers123 = nplots.tiers12 + nplots.tier3)
    
    ## Sort them: 
    ## 1. If we can meet subquad goal with those that have >= 3 plots in Tier 1, great. if not:
    ## 2. Try to meet subquad goal with plots that have >= 3 plots in Tiers12. If not:
    ## 3. Try to meet subquad goal with plots that hve >= 2 plots in Tier 1. If not:
    ## 4. Try to meet subquad goal with plots that have >= 2 plots in Tiers12. If not:
    ## 5. Try to meet subquad goal with plots that have >= 2 plots in Tiers 123. If not:
    ## 6. Take as many subquads as possible with plots that have >= 2 plots in Tier 123.
    
    goals <- candidate.subquads %>%
      mutate(goal1 = nplots.tier1 >= 3,
             goal2 = nplots.tiers12 >= 3,
             goal3 = nplots.tier1 >= 2,
             goal4 = nplots.tiers12 >= 2,
             goal5 = nplots.tiers123 >= 2,
             goal6 = nplots.tiers123 >= 1)
    
    goal1met <- sum(goals$goal1) >= subquads.goal
    goal2met <- sum(goals$goal2) >= subquads.goal
    goal3met <- sum(goals$goal3) >= subquads.goal
    goal4met <- sum(goals$goal4) >= subquads.goal
    goal5met <- sum(goals$goal5) >= subquads.goal
    goal6met <- sum(goals$goal6) >= subquads.goal
      
    if(goal1met) {
      candidate.subquads <- goals %>%
        filter(goal1)
    } else if(goal2met) {
      candidate.subquads <- goals %>%
        filter(goal1 | goal2)
    } else if(goal3met) {
      candidate.subquads <- goals %>%
        filter(goal1 | goal2 | goal3)
    } else if(goal4met) {
      candidate.subquads <- goals %>%
        filter(goal1 | goal2 | goal3 | goal4)
    } else if(goal5met) {
      candidate.subquads <- goals %>%
        filter(goal1 | goal2 | goal3 | goal4 | goal5)
    } else if(goal6met) {
      candidate.subquads <- goals %>%
        filter(goal1 | goal2 | goal3 | goal4 | goal5 | goal6)
    } else {
      candidate.subquads <- goals %>%
        filter(nplots >= 1)
    }

    
    candidate.subquads <- candidate.subquads %>%
      arrange(desc(goal1),desc(goal2),desc(goal3),desc(goal4),desc(goal5),desc(goal6))
    
    candidate.subquads <- candidate.subquads[0:subquads.goal,]
    candidate.subquads$foc.fire.name <- suid.pri.focal$foc.fire.name
    candidate.subquads$mgmt.cat <- suid.pri.focal$mgmt.cat
    candidate.subquads$plant.yr <- suid.pri.focal$plant.yr
    candidate.subquads$salv.cat <- suid.pri.focal$salv.cat
    candidate.subquads$released <- suid.pri.focal$released
    candidate.subquads$thinned <- suid.pri.focal$thinned
    candidate.subquads$site.prepped <- suid.pri.focal$site.prepped
    candidate.subquads$replanted <- suid.pri.focal$replanted
    
    
    ##^ this is the subquads we want to use for this quadrant
    
    selected.subquads <- rbind(selected.subquads,candidate.subquads)
  }
}

## Now, for each selected subquad, order the member plots by SUIDs tier, then by SIUD nplots in that subquad, then by nplots total, then random.
# If plot ranked 2 and 3 are from different SUID tiers, drop the third one, else keep it.
all.selected.plots <- NULL
for(i in 1:nrow(selected.subquads)) {
  
  
  subquad.focal <- selected.subquads[i,]
  
  ## get the SUIDs for that subquad, along with their tiers.
  suids <- suid.prioritization %>%
    filter(foc.fire.name == as.character(subquad.focal$foc.fire.name) &
             plant.yr == as.character(subquad.focal$plant.yr) &
             salv.cat == as.character(subquad.focal$salv.cat) &
             site.prepped == as.character(subquad.focal$site.prepped) &
             released == as.character(subquad.focal$released) &
             thinned == as.character(subquad.focal$thinned) &
             replanted == as.character(subquad.focal$replanted)) 
  
  suids.w.tiers <- suids$suids.selected[[1]]
  
  ## now get the PLOTs for that subquad
  plots.mgmt <- d.trt %>%
    filter(fire.dist == as.character(subquad.focal$foc.fire.name) &
             yr.pltd == as.character(subquad.focal$plant.yr) &
             salv.cat == as.character(subquad.focal$salv.cat) &
             site.prepped == as.character(subquad.focal$site.prepped) &
             released == as.character(subquad.focal$released) &
             thinned == as.character(subquad.focal$thinned) &
             replanted == as.character(subquad.focal$replanted))
             
  plots.subquad <- plots.mgmt %>%
    filter(  elev < subquad.focal$elev.high &
             elev > subquad.focal$elev.low &
             rad < subquad.focal$rad.high &
             rad > subquad.focal$rad.low)
  
  ## attach the SUID prioritization
  plots.subquad <- inner_join(plots.subquad,suids.w.tiers,by=c("f.s.first.planting.suid" = "suid"))
  
  ## get the number of plots of each SUID in this subquadrant
  suid.subquad.counts <- plots.subquad %>%
    st_drop_geometry() %>%
    group_by(f.s.first.planting.suid) %>%
    summarize(suid.subquad.count=n())
  
  suid.overall.counts <- plots.mgmt %>%
    st_drop_geometry() %>%
    group_by(f.s.first.planting.suid) %>%
    summarize(suid.overall.count =n())
  
  plots.subquad <- left_join(plots.subquad,suid.subquad.counts,by="f.s.first.planting.suid")
  plots.subquad <- left_join(plots.subquad,suid.overall.counts,by="f.s.first.planting.suid")
  plots.subquad$random <- sample(0:nrow(plots.subquad),nrow(plots.subquad),replace=FALSE)
  
  plots.subquad.ranked <- plots.subquad %>%
    arrange(tier,desc(suid.subquad.count),desc(suid.overall.count),random)
  
  ## if there are more than 2 plots,## keep 3 max   see if plots 2 and 3 are from the same tier; if so, keep #3
  if(nrow(plots.subquad.ranked) > 2) {
    plots.subquad.ranked <- plots.subquad.ranked[1:3,]
  }
  
  plots.subquad.ranked$subquad.overall.label <- subquad.focal$overall.label
  plots.subquad.ranked$rank <- 1:nrow(plots.subquad.ranked)
  
  if(is.null(all.selected.plots)) {
    all.selected.plots <- plots.subquad.ranked
  } else {
  all.selected.plots <- rbind(all.selected.plots,plots.subquad.ranked)
  }
}


## save it
st_write(all.selected.plots,"data/site-selection/output/selected-plots/selected_plots_v1.gpkg")


#### For each fire and management cat, plot the resulting stratification ####
mgmt.factorial.types <- unique(all.selected.plots$mgmt.factorial)

for(i in 1:length(mgmt.factorial.types)) {
  
  factorial.type <- mgmt.factorial.types[i]
  
  plots.type <- all.selected.plots %>%
    filter(mgmt.factorial == factorial.type)
  
  yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")
  
  
  g <- ggplot(plots.type,aes(x=elev,y=rad,color=yr.pltd,shape=as.character(rank))) +
    geom_point(size=2.5) +
    theme_bw(15) +
    scale_shape_manual(values=c(16,8,1)) +
    scale_colour_manual(values=yr.colors) +
    labs(color="Yr planted",shape="Rank") +
    labs(title=factorial.type)
  
  print(g)
  
}


## Tabulate the selected SUIDs by plot priority (rank)

target.suids <- all.selected.plots %>%
  st_drop_geometry() %>%
  group_by(mgmt.factorial,first.pltd.yr,most.recent.focal.fire,f.s.first.planting.suid,rank) %>%
  summarize(nplots = n()) %>%
  arrange(most.recent.focal.fire,mgmt.factorial,rank,desc(nplots))


write.csv(target.suids,"plumas_target_suids.csv")




####$$$ In sub.quads.df, nplots.tier1 is the number of plots within that subquad that were selected in the first tier of stratifiction
# that is, finding SUIDS that filled at lesat 2 subquads of every quad at least once). In order to minimize the number of SUIDs we survey:
# In each quad, our candidates are subquads that have a least 3 nplots. If none have at least 3 nplots, 2 nplots is OK.
# Then, for each of those, choose the top two subquads in terms of nplots.tier1. If none of those has at least 3 plots,#
#select those that would maximize the total sum of nplots tier1 + tier2. then if not that, tier1 + tier2 + tier3


## alternate plan (could be effectively the same): after meeting all tier goals, pick which sub-quads to be focal for plots: first all those that have 2-3 first tier plots, then add those where first-plus second-tier plots bring up to 2-3 plots, until we have enough subquadrants filled.
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








