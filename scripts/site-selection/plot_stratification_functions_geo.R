#### Convenience functions ####

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}


#### Determining env. categories (quadrants) and classifying plots ####

#function to find outliers
# an outlier is a point with a value so extreme (high or low) that it has a < 1% chance of being observed, given the size of the dataset and assuming a normal distribution
#! possible future alternative outliers alg: make sure there are at least 5% of the total number of plots (or, at least 3 plots) within 10% of the range of variable values of the focal plot
outliers <- function(x) {
  mean <- mean(x)
  sd <- sd(x)
  x.dev <- abs(x-mean)
  prob <- pnorm(x.dev,0,sd,lower.tail=FALSE)
  
  threshold <- 1/(length(x)*3)
  
  outlier <- (prob < threshold)
  return(outlier)
}


# function to find the range of elev and rad encompassed by all focal plots
plt.yr.env.range <- function(plt.yr,plots) {
  
  fire <- plots$fire2[1]
  
  # filter to the current planting year
  plots.yr <- plots %>%
    filter(yr.pltd == plt.yr) # and when computing range, only look at plots close to seed source (this means only looking at perimeter plots, because previously we filtered out the internal plots close to seed source)
  
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
  
  env.range.yr <- data.frame(plt.yr=as.character(plt.yr),elev.low,elev.high,rad.low,rad.high)
  row.names(env.range.yr) <- NULL
  
  return(env.range.yr)
  
}


## Define the environmental quadrants
# In a data frame with each row contianing a quadrant definition
define.quadrants <- function(focal.range,rad.cats.arg,elev.cats.arg) {
  
  rad.low.arg <- focal.range$rad.low
  rad.high.arg <- focal.range$rad.high
  elev.low.arg <- focal.range$elev.low
  elev.high.arg <- focal.range$elev.high
  
  
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



#### Geocell selection/stratification/scoring ####

##function to compute the quality of stratification given a tally of how many plots are in each sub-quad
score.stratif <- function(sub.quads.df,simp=FALSE,subquads.goal=2) {
  
  # count the number of subquads filled
  n.sub.quads.filled <- sum(sub.quads.df$nplots>0,na.rm=TRUE)
  
  # count the number of quadrants with 2+ subquads filled
  quads <- sub.quads.df %>%
    group_by(quad.label) %>%
    summarize(n.subquads.full = sum(nplots >= 1,na.rm=TRUE)) %>%
    mutate(two.plus.subquads.full = n.subquads.full >= subquads.goal) %>%
    ungroup()
  n.quads.w.two.plus.subquads <- sum(quads$two.plus.subquads.full,na.rm=TRUE)
  
  # count the number of quadrants with 2+ subquads double-filled
  quads <- sub.quads.df %>%
    group_by(quad.label) %>%
    summarize(n.subquads.full = sum(nplots >= 2,na.rm=TRUE)) %>%
    mutate(two.plus.subquads.full = n.subquads.full >= subquads.goal) %>%
    ungroup()
  n.quads.w.two.plus.subquads.double <- sum(quads$two.plus.subquads.full,na.rm=TRUE)
  
  # count the number of quadrants with 2+ subquads triple-filled
  quads <- sub.quads.df %>%
    group_by(quad.label) %>%
    summarize(n.subquads.full = sum(nplots >= 3,na.rm=TRUE)) %>%
    mutate(two.plus.subquads.full = n.subquads.full >= subquads.goal) %>%
    ungroup()
  n.quads.w.two.plus.subquads.triple <- sum(quads$two.plus.subquads.full,na.rm=TRUE)
  
  ret <- list(n.sub.quads.filled=n.sub.quads.filled,quads=quads,n.quads.w.two.plus.subquads=n.quads.w.two.plus.subquads,n.quads.w.two.plus.subquads.double=n.quads.w.two.plus.subquads.double,n.quads.w.two.plus.subquads.triple=n.quads.w.two.plus.subquads.triple)
  
  if(simp==TRUE) {
    #return only the "n.sub.quads.filled" and "n.quads.w.two.plus.subquads"
    ret <- ret[c("n.sub.quads.filled","n.quads.w.two.plus.subquads","n.quads.w.two.plus.subquads.double","n.quads.w.two.plus.subquads.triple")]
    ret <- as.data.frame(ret)
  }
  
  return(ret)
  
}


### function to take a geocell and compute what the sub.quads.df (stratification records) would be if it were added
add.geocell <- function(geocell,d.foc.yr.classified,sub.quads.df,tier=0) {
  
  d.foc.yr.classified.geocell <- d.foc.yr.classified %>%
    filter(geocell.id == geocell) %>% # look only at the geocell in question
    group_by(overall.label) %>% 
    summarize(nplots.added = n()) %>% # compute the number of plots within each subquadrant
    ungroup()
  
  ##add the new counts to the stratification records: first as a new column
  new.sub.quads.df <- left_join(sub.quads.df,d.foc.yr.classified.geocell,by="overall.label")
  new.sub.quads.df[is.na(new.sub.quads.df$nplots.added),"nplots.added"] <- 0
  
  # then replace the original "nplots" column
  new.sub.quads.df <- new.sub.quads.df %>%
    mutate(nplots = nplots + nplots.added)
  
  # then if tier is 1, add to nplots.tier1
  if(tier==1) {
    new.sub.quads.df <- new.sub.quads.df %>%
      mutate(nplots.tier1 = nplots.tier1 + nplots.added)
  }
  
  # then if tier is 2, add to nplots.tier2
  if(tier==2) {
    new.sub.quads.df <- new.sub.quads.df %>%
      mutate(nplots.tier2 = nplots.tier2 + nplots.added)
  }
  
  # then if tier is 3, add to nplots.tier3
  if(tier==3) {
    new.sub.quads.df <- new.sub.quads.df %>%
      mutate(nplots.tier3 = nplots.tier3 + nplots.added)
  }
  
  # finally, remove the temporary "nplots.added" column
  new.sub.quads.df <- new.sub.quads.df %>%
    dplyr::select(-nplots.added)
  
  return(new.sub.quads.df)
  
}

## function to take a Geocell and calculate the new stratification scores (simplified) if it were added
strat.scores.w.new.geocell <- function(geocell,sub.quads.df,d.foc.yr.classified,subquads.goal=2) {
  
  #get the new sub quads DF if the geocell were added
  new.sub.quads.df <- add.geocell(geocell,d.foc.yr.classified,sub.quads.df)
  
  #score it
  strat.score <- score.stratif(new.sub.quads.df,simp=TRUE,subquads.goal)
  
  return(strat.score)
}


get.overall.env.range <- function(plt.yrs,plots) {

  env.ranges <- purrr::map(plt.yrs,plt.yr.env.range,plots=plots)
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
  
  
  ### plot it
  
  yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")
  
  g <- ggplot(plots,aes(x=elev,y=rad,color=yr.pltd,shape=dist.nonhigh)) +
    geom_point(size=2) +
    theme_bw(15) +
    scale_shape_manual(values=c(16,1)) +
    scale_colour_manual(values=yr.colors) +
    labs(color="Yr planted",shape="Seed dist") +
    geom_rect(xmin=focal.range$elev.low,
              xmax=focal.range$elev.high,
              ymin=focal.range$rad.low,
              ymax=focal.range$rad.high,
              fill=NA,color="black") +
    labs(title=plots$fire.dist2[1])
  print(g)
  
  
  return(focal.range)
  
}




define.subquads <- function(env.quads) {
  
  ## X 1. Divide into named quadrants based on the variables to stratify by
  ## 2. For each planting unit, determint the number of sub-quadrants it covers within each quadrant
  ## 3. Compare planting units based on how thoroughly they cover each sub-quadrant
  
  ### Create subquadrants for each quadrant and compile into a data frame ###
  sub.quads.df <- data.frame()
  
  # so for each quadrant
  for(j in 1:nrow(env.quads)) {
    
    env.quad <- env.quads[j,]
    
    #define sub-quadrants #! we can adjust the number of sub-quads here based on the desired number of survey plots
    sub.quads <- define.quadrants(env.quad,rad.cats.arg=2,elev.cats.arg=2)
    
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
  
  return(sub.quads.df)
}




###################################################
#### For actual stratification ####
###################################################


determine_geocells <- function(d.foc.yr,cellsize) {
  ### create a geo grid of the spanning the area covered by the candidate plots
  geocells_all <- st_make_grid(d.foc.yr,cellsize=cellsize) %>% st_sf()

  # plot(d.foc.yr,max.plot=1)  
  # plot(geocells_all,add=TRUE)

  
  geocells_all$geocell.id <- 1:nrow(geocells_all)
  return(geocells_all)
}


plot_geocells_env <- function(d.foc.yr) {
  
  ## What geocells are we working with?
  
  geocells <- unique(c(d.foc.yr$geocell.id,foc.default.geocells))
  
  ## Plot what we are working with
  
  g <- ggplot(d.foc.yr,aes(x=elev,y=rad,color=geocell.id,shape=dist.nonhigh)) +
    geom_point(size=2) +
    theme_bw(15) +
    scale_shape_manual(values=c(16,1)) +
    labs(color="Geocell",shape="Seed dist") +
    geom_rect(xmin=focal.env.range$elev.low,
              xmax=focal.env.range$elev.high,
              ymin=focal.env.range$rad.low,
              ymax=focal.env.range$rad.high,
              fill=NA,color="red") +
    labs(title=d.foc.yr$fire.dist2[1])
  print(g)
}





