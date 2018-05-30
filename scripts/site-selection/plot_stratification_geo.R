
  
geocell.prioritization <- NULL # this will store a growing data frame


default.geocells.remaining <- foc.default.geocells

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
  geom_rect(xmin=focal.env.range$elev.low,
            xmax=focal.env.range$elev.high,
            ymin=focal.env.range$rad.low,
            ymax=focal.env.range$rad.high,
            fill=NA,color="red") +
  labs(title=d.foc.yr.classified$fire.dist2[1])
print(g)


## Summarize by suid and quadrant: what subquadrants are represented by each SUID


d.foc.yr.classified <- st_drop_geometry(d.foc.yr.classified)

geocell.summ <- d.foc.yr.classified %>%
  group_by(geocell.id, quad.label) %>%
  summarize(subquads = list(unique(subquad.label)),
            n.subquads = length(unique(subquad.label))) %>%
  ungroup()








invisible(readline(prompt="Press [enter] to continue"))










### Now the goal is to find the smallest number of SUIDS that can fill at least 2 subquads of every quad


max.cats <- rad.cats*elev.cats

# for keeping track of which SUIDs we still have as candidates to add
# initialize it with all SUIDS that contain points of the focal management category
geocells.remaining <- unique(geocell.summ$geocell.id)

geocells.remaining <- setdiff(geocells.remaining,default.geocells.remaining)

# for keeping track of which SUIDS we've already added
geocells.selected <- NULL

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
while((current.tier < 4) & (length(geocells.remaining)+length(default.geocells.remaining) != 0)) {  
 
  tier.just.completed <- FALSE
  added.default.geocell <- FALSE
  
  #1. Compute the current stratification scores
  strat.scores <- score.stratif(sub.quads.df,simp=TRUE)
  
  
  # if there are default SUIDs that need to be added, just select them as the ones to be added
  if(length(default.geocells.remaining) > 0) {
    geocell.to.add <- default.geocellss.remaining[1]
    default.geocellss.remaining <- setdiff(default.geocellss.remaining,geocell.to.add)
    
    new.strat.scores <- strat.scores.w.new.geocell(geocell.to.add,sub.quads.df,d.foc.yr.classified,subquads.goal=subquads.goal)
    
    added.default.suid <- TRUE
    
  } else {
  
  
    #2. Test all remaining suids to see which best improves the stratification scores
    geocells.remaining ##! need to remove the SUIDS that were added in the last iteration
    
    
    new.strat.scores <- map(geocells.remaining,.f=strat.scores.w.new.geocell,sub.quads.df=sub.quads.df,d.foc.yr.classified=d.foc.yr.classified,subquads.goal=subquads.goal)
    new.strat.scores.df <- bind_rows(new.strat.scores)
    new.strat.scores.df$geocell <- geocells.remaining
    
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
    
    #! here, we should take the geocell with the greatest number of plots total: #amont those, find the one that has the greatest number of plots total
    
    # now, in case there are still multiple tied, just take the first one (this should be rare)
    best.scores <- best.scores[1,]
    
    ## compute the new strat scores with that suid
    new.strat.scores <- strat.scores.w.new.geocell(best.scores$geocell,sub.quads.df,d.foc.yr.classified,subquads.goal=subquads.goal)
    
    geocell.to.add <- best.scores$geocell
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
  if(added.default.geocell) {
    scores.increased <- TRUE
  }
  
  ## if we were looking to increase first tier scoring, did we meet the goal?
  if(current.tier == 1 & new.strat.scores$n.quads.w.two.plus.subquads == max.cats) {
    
    # record that we met the goal
    highest.tier.goal.met <- 1
    
    # add the new selected SUID to the list of selected SUIDs
    new.geocell <- geocell.to.add
    geocells.selected.new <- data.frame(geocell=new.geocell,tier=current.tier)
    geocells.selected <- rbind(geocells.selected,geocells.selected.new)
    sub.quads.df <- add.geocell(new.geocell,d.foc.yr.classified,sub.quads.df,tier=1)
    geocells.remaining <- dplyr::setdiff(geocells.remaining,new.geocell)
    
    # go on to try to achieve tier 2 goal
    tier.just.completed <- TRUE
    iteration.when.1st.tier.reached <- current.iteration
    
  } else if (current.tier == 1 & scores.increased == TRUE) { # score increased but did not meet goal
    
    # add the new selected SUID to the list of selected SUIDS, but keep searching to meet goal
    new.geocell <- geocell.to.add
    geocells.selected.new <- data.frame(geocell=new.geocell,tier=current.tier)
    geocells.selected <- rbind(geocells.selected,geocells.selected.new)
    sub.quads.df <- add.geocell(new.geocell,d.foc.yr.classified,sub.quads.df,tier=1)
    geocells.remaining <- dplyr::setdiff(geocells.remaining,new.geocell)
    
    
  } else if(current.tier == 1 & scores.increased == FALSE) { # if we were looking to increase first tier scoring (n quads with 2+ subquads filled once), but we didn't manage to increase it, print message, then go on to search for second tier goal
  
    cat("\nFirst tier strat goal not reached for:")
    cat("\n   Filled",new.strat.scores$n.quads.w.two.plus.subquads,"quadrants suitably.")
    tier.just.completed <- TRUE
    iteration.when.1st.tier.reached <- current.iteration
  
  } else if(current.tier == 2 & new.strat.scores$n.quads.w.two.plus.subquads.double == max.cats) { # if we were looking to increase second tier scoring, did we meet goal?
    
    # record that we met the goal
    highest.tier.goal.met <- 2
    
    # add the new selected SUID to the list of selected SUIDs
    new.geocell <- geocell.to.add
    geocells.selected.new <- data.frame(geocell=new.geocell,tier=current.tier)
    geocells.selected <- rbind(geocells.selected,geocells.selected.new)
    sub.quads.df <- add.geocell(new.geocell,d.foc.yr.classified,sub.quads.df,tier=2)
    geocells.remaining <- dplyr::setdiff(geocells.remaining,new.geocell)
    
    
    # go on to try to achieve tier 3 goal
    tier.just.completed <- TRUE
    iteration.when.2st.tier.reached <- current.iteration
    
  } else if (current.tier == 2 & scores.increased == TRUE) { # score increased but did not meet goal
    
    # add the new selected geocell to the list of selected geocellS, but keep searching to meet goal
    new.geocell <- geocell.to.add
    geocells.selected.new <- data.frame(geocell=new.geocell,tier=current.tier)
    geocells.selected <- rbind(geocells.selected,geocells.selected.new)
    sub.quads.df <- add.geocell(new.geocell,d.foc.yr.classified,sub.quads.df,tier=2)
    geocells.remaining <- dplyr::setdiff(geocells.remaining,new.geocell)
    
    
  } else if(current.tier == 2 & scores.increased == FALSE) { # if we were looking to increase second tier scoring (n quads w/ 2+ subquads filled twice), but we didn't manage to increase it, print message, then go on to search for third tier goal
    
    cat("\nSecond tier strat goal not reached for:")
    cat("\n   Filled",new.strat.scores$n.quads.w.two.plus.subquads.double,"quadrants suitably.")
    tier.just.completed <- TRUE
    iteration.when.2nd.tier.reached <- current.iteration
  
  } else if(current.tier == 3 & new.strat.scores$n.quads.w.two.plus.subquads.triple == max.cats) { # if we were looking to increase third tier scoring, did we meet goal?
    
    # record that we met the goal
    highest.tier.goal.met <- 3
    
    # add the new selected geocell to the list of selected geocells
    new.geocell <- geocell.to.add
    geocells.selected.new <- data.frame(geocell=new.geocell,tier=current.tier)
    geocells.selected <- rbind(geocells.selected,geocells.selected.new)
    sub.quads.df <- add.geocell(new.geocell,d.foc.yr.classified,sub.quads.df,tier=3)
    geocells.remaining <- dplyr::setdiff(geocells.remaining,new.geocell)
    
    
    # completed tier 3 goal
    tier.just.completed <- TRUE
    iteration.when.3rd.tier.reached <- current.iteration
    
  } else if (current.tier == 3 & scores.increased == TRUE) { # score increased but did not meet goal
    
    # add the new selected geocell to the list of selected geocellS, but keep searching to meet goal
    new.geocell <- geocell.to.add
    geocells.selected.new <- data.frame(geocell=new.geocell,tier=current.tier)
    geocells.selected <- rbind(geocells.selected,geocells.selected.new)
    sub.quads.df <- add.geocell(new.geocell,d.foc.yr.classified,sub.quads.df,tier=3)
    geocells.remaining <- dplyr::setdiff(geocells.remaining,new.geocell)
    
    
  } else if(current.tier == 3 & scores.increased == FALSE) { # if we were looking to increase third tier scoring (n quads w/ 2+ subquads filled triply), but we didn't manage to increase it, print message, then consider tier complete
    
    cat("\nThird tier strat goal not reached")
    cat("\n   Filled",new.strat.scores$n.quads.w.two.plus.subquads.triple,"quadrants suitably.")
    tier.just.completed <- TRUE
    iteration.when.3rd.tier.reached <- current.iteration
  }
    
  current.iteration <- current.iteration + 1
  
  # if we complieted a tier (even if it was incomplete but there was no way to improve it), print a message
  if(tier.just.completed) {
    cat("-- Tier",current.tier," completed")
    current.tier <- current.tier + 1 ## if this goes up to 4, the loop will exit upon starting the next iteration
  }
  
  # if we used up all the geocell options, print a message that we did not achieve all tier goals
  if(length(geocells.remaining) == 0 & (current.tier != 4)) {
    cat("-- All geocell options exhausted without completing three tiers")
  }
}


## Create a data frame, where columns are: Fire, mgmt cat, plant year, suids.selected DF, and sub.quads DF, also columns with SUIDS for Tier 1, SUIDS for Tier 2, and SUIDS for Tier 3

geocell.prioritization <- data.frame(highest.tier.goal.met)
geocell.prioritization$geocells.selected <- list(geocells.selected)
geocell.prioritization$sub.quads <- list(sub.quads.df)

geocells.tier1 <- as.character(geocells.selected[geocells.selected$tier==1,"geocell"])
geocells.tier2 <- as.character(geocells.selected[geocells.selected$tier==2,"geocell"])
geocells.tier3 <- as.character(geocells.selected[geocells.selected$tier==3,"geocell"])

geocell.prioritization$geocells.tier1 <- list(geocells.tier1)
geocell.prioritization$geocells.tier2 <- list(geocells.tier2)
geocell.prioritization$geocells.tier3 <- list(geocells.tier3)




## make a plot of all plots in the first three tiers of suids, colored by planting year and shped by tier

geocells.tier1 <- data.frame(tier=1,geocell.id=unlist(geocell.prioritization$geocells.tier1))

if(length(unlist(geocell.prioritization$geocells.tier2)) > 0) {
  geocells.tier2 <- data.frame(tier=2,geocell.id=unlist(geocell.prioritization$geocells.tier2))
} else {
  geocells.tier2 <- NULL
}

if(length(unlist(geocell.prioritization$geocells.tier3)) > 0) {
  geocells.tier3 <- data.frame(tier=3,geocell.id=unlist(geocell.prioritization$geocells.tier3))
} else {
  geocells.tier3 <- NULL
}

geocells.priorities.pre <- suppressWarnings(bind_rows(geocells.tier1,geocells.tier2))
geocells.priorities <- suppressWarnings(bind_rows(geocells.priorities.pre,geocells.tier3))

geocells.priorities <- geocells.priorities %>%
  mutate(geocell.id = as.numeric(as.character(geocell.id)))

  
##!! now merge this with plots data so we have plot rad and elevation which can be colored by planting year and shaped by tier
d.foc.geocells.pri <- left_join(d.foc.yr,geocells.priorities,by="geocell.id") %>%
  filter(!is.na(tier)) %>%
  mutate(tier = as.character(tier),
         yr.pltd = as.character(yr.pltd)) %>%
  filter(dist.nonhigh == "< 80 m")


# yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")
# 
# 
# g <- ggplot(d.foc.geocells.pri,aes(x=elev,y=rad,color=yr.pltd,shape=tier)) +
#   geom_point(size=2.5) +
#   theme_bw(15) +
#   scale_shape_manual(values=c(16,1,8)) +
#   scale_colour_manual(values=yr.colors) +
#   labs(color="Yr planted",shape="Strat tier") +
#   labs(title=d.foc.geocells.pri$fire.dist2[1])+
#   geom_rect(xmin=focal.env.range$elev.low,
#             xmax=focal.env.range$elev.high,
#             ymin=focal.env.range$rad.low,
#             ymax=focal.env.range$rad.high,
#             fill=NA,color="black")
# print(g)
#   
# 
# 
# 
# 
# 
# 
# 
# invisible(readline(prompt="Press [enter] to continue"))
# 
# 






#### Based on geocell prioritization, select actual plots in tiers, so we have the plots and also the top-priority SUIDs for study. ####

selected.subquads <- NULL


geocell.pri.focal <- geocell.prioritization

## Look up what the subquad goal was
subquads.goal


### For each quad
# First, what are the quads?
sub.quads.focal <- geocell.pri.focal$sub.quads[[1]]
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
        cat("\nFor quad ",as.character(quad),": only",n.candidate.subquads,"subquads with >= 2 plots; goal was",subquads.goal)
      
      }
      
    }
    
    candidate.subquads <- rbind(candidate.subquads,addl.candidate.subquads)
    candidate.subquads <- rbind(candidate.subquads,addl.addl.candidate.subquads)
    n.candidate.subquads <- nrow(candidate.subquads)
    
  }
  
  subquads.goal.new <- min(subquads.goal,n.candidate.subquads)
  
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
  
  goal1met <- sum(goals$goal1) >= subquads.goal.new
  goal2met <- sum(goals$goal2) >= subquads.goal.new
  goal3met <- sum(goals$goal3) >= subquads.goal.new
  goal4met <- sum(goals$goal4) >= subquads.goal.new
  goal5met <- sum(goals$goal5) >= subquads.goal.new
  goal6met <- sum(goals$goal6) >= subquads.goal.new
    
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
  
  candidate.subquads <- candidate.subquads[0:subquads.goal.new,]
  # candidate.subquads$foc.fire.name <- geocell.pri.focal$foc.fire.name
  # candidate.subquads$mgmt.cat <- geocell.pri.focal$mgmt.cat
  # candidate.subquads$plant.yr <- geocell.pri.focal$plant.yr
  # candidate.subquads$salv.cat <- geocell.pri.focal$salv.cat
  # candidate.subquads$released <- geocell.pri.focal$released
  # candidate.subquads$thinned <- geocell.pri.focal$thinned
  # candidate.subquads$site.prepped <- geocell.pri.focal$site.prepped
  # candidate.subquads$replanted <- geocell.pri.focal$replanted
  
  
  ##^ this is the subquads we want to use for this quadrant
  
  selected.subquads <- rbind(selected.subquads,candidate.subquads)
}




## Now, for each selected subquad, order the member plots by geocells tier, then by SIUD nplots in that subquad, then by nplots total, then random.
# If plot ranked 2 and 3 are from different geocell tiers, drop the third one, else keep it.
selected.plots <- NULL
for(i in 1:nrow(selected.subquads)) {   ## check this for i = 4 because it's not filling that subquad with plots
  
  
  subquad.focal <- selected.subquads[i,]
  
  ## get the geocells for that subquad, along with their tiers.
  geocells <- geocell.prioritization  #%>%
    # filter(foc.fire.name == as.character(subquad.focal$foc.fire.name) &
    #          plant.yr == as.character(subquad.focal$plant.yr) &
    #          salv.cat == as.character(subquad.focal$salv.cat) &
    #          site.prepped == as.character(subquad.focal$site.prepped) &
    #          released == as.character(subquad.focal$released) &
    #          thinned == as.character(subquad.focal$thinned) &
    #          replanted == as.character(subquad.focal$replanted))
  
  geocells.w.tiers <- geocells$geocells.selected[[1]]
  
  ## now get the PLOTs for that subquad
  plots.mgmt <- d.foc.yr #%>%
    # filter(fire.dist == as.character(subquad.focal$foc.fire.name) &
    #          yr.pltd == as.character(subquad.focal$plant.yr) &
    #          salv.cat == as.character(subquad.focal$salv.cat) &
    #          site.prepped == as.character(subquad.focal$site.prepped) &
    #          released == as.character(subquad.focal$released) &
    #          thinned == as.character(subquad.focal$thinned) &
    #          replanted == as.character(subquad.focal$replanted))
        
  ##!! here we can just use the same df of plots from the earlier code, when doing for a single management type and age at a time (right?)
  # yes, doing that. commented out the following line
  # plots.mgmt <- st_intersection(plots.mgmt,geocells_all)
  
       
  plots.subquad <- plots.mgmt %>%
    filter(  elev < subquad.focal$elev.high &
             elev > subquad.focal$elev.low &
             rad < subquad.focal$rad.high &
             rad > subquad.focal$rad.low)
  

  
  
  ## attach the geocell prioritization
  plots.subquad <- inner_join(plots.subquad,geocells.w.tiers,by=c("geocell.id" = "geocell"))
  
  ## get the number of plots of each geocell in this subquadrant
  geocell.subquad.counts <- plots.subquad %>%
    st_drop_geometry() %>%
    group_by(geocell.id) %>%
    summarize(geocell.subquad.count=n())
  
  geocell.overall.counts <- plots.mgmt %>%
    st_drop_geometry() %>%
    group_by(geocell.id) %>%
    summarize(geocell.overall.count =n())
  
  plots.subquad <- left_join(plots.subquad,geocell.subquad.counts,by="geocell.id")
  plots.subquad <- left_join(plots.subquad,geocell.overall.counts,by="geocell.id")
  plots.subquad$random <- sample(0:nrow(plots.subquad),nrow(plots.subquad),replace=FALSE)
  
  plots.subquad.ranked <- plots.subquad %>%
    arrange(tier,desc(geocell.subquad.count),desc(geocell.overall.count),random)
  
  ## if there are more than 2 plots,## keep 3 max   see if plots 2 and 3 are from the same tier; if so, keep #3
  if(nrow(plots.subquad.ranked) > 2) {
    plots.subquad.ranked <- plots.subquad.ranked[1:3,]
  }
  
  plots.subquad.ranked$subquad.overall.label <- subquad.focal$overall.label
  plots.subquad.ranked$rank <- 1:nrow(plots.subquad.ranked)
  
  if(is.null(selected.plots)) {
    selected.plots <- plots.subquad.ranked
  } else {
  selected.plots <- rbind(selected.plots,plots.subquad.ranked)
  }
}




#### plot the resulting stratification ####

plots.type <- selected.plots

yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")


g <- ggplot(plots.type,aes(x=elev,y=rad,color=yr.pltd,shape=as.character(rank))) +
  geom_point(size=2.5) +
  theme_bw(15) +
  scale_shape_manual(values=c(16,8,1)) +
  scale_colour_manual(values=yr.colors) +
  labs(color="Yr planted",shape="Rank")
print(g)







# 
# 
# ###!!! here need to return the plots to the other script, which writes them
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# ## save it
# st_write(all.selected.plots,"data/site-selection/output/selected-plots/selected_plots_moontelope_v1.gpkg",delete_dsn=TRUE)
# 
# 




# 
# 
# 
# ## Tabulate the selected suids by plot priority (rank)
# 
# target.suids <- all.selected.plots %>%
#   st_drop_geometry() %>%
#   group_by(mgmt.factorial,first.pltd.yr,most.recent.focal.fire,f.s.first.planting.suid,rank) %>%
#   summarize(nplots = n()) %>%
#   arrange(most.recent.focal.fire,mgmt.factorial,rank,desc(nplots))
# 
# 
# write.csv(target.suids,"plumas_target_suids.csv")
# 



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








