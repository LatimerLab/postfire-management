





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
elev.cats <- 3
subquads.goal <- 2  ## this is for how many plots per subquad
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
foc.yrs.pltd <- c("1","3")
default.plots <- NULL  ##!!! need to expand this

mgmt.cats.df <- mgmt.cats.df.row



foc.default.geocells <- NULL ##!!! need to expand this to determine which geocells the default plots fall into














#### 1. Narrow the environmental space based on the intersection of env. space represented by each planting year (near to seed source only)  ####

# turn mgmt cat into a string for saving it

## this is the dataset for computing the env. bounds of sampling

d.foc.envbounds <- d.trt %>%
  dplyr::filter(fire.dist2 %in% foc.fire.name &
                  salv.cat %in% foc.salv.cat &
                  site.prepped %in% foc.site.prepped &
                  release.txt %in% foc.released &
                  replanted %in% foc.replanted &
                  thinned %in% foc.thinned &
                  yr.pltd %in% foc.yrs.pltd)

plt.yrs <- as.character(foc.yrs.pltd)











if(is.na(custom.rad.low)) { # no custom rad specified, so calc automatically
  
  env.ranges <- purrr::map(plt.yrs,plt.yr.env.range,plots=d.foc.envbounds)
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

g <- ggplot(d.foc.envbounds,aes(x=elev,y=rad,color=yr.pltd,shape=dist.nonhigh)) +
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



## determine the env. quads

rad.low <- focal.range$rad.low
rad.high <- focal.range$rad.high
elev.low <- focal.range$elev.low
elev.high <- focal.range$elev.high

env.quads <- define.quadrants(rad.low,rad.high,elev.low,elev.high,rad.cats,elev.cats)





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


















## now run for just one year

foc.yr.pltd  <- 1

d.foc <- d.foc.envbounds %>%
  filter(first.pltd.yr == foc.yr.pltd &
           type == "treatment" &
           dist.non.high < 80)

d.foc.yr <- d.foc












