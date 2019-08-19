setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)
library(readxl)
library(stringr)
library(lubridate)

#### Convenience functions ####
deg2rad <- function(deg) {(deg * pi) / (180)}
rad2deg <- function(rad) {(rad*180)/pi}

adjust_point_for_slope = function(point_radius,point_bearing,slope,aspect) {
  max_slope_reduction = cos(deg2rad(slope)) # this is how much a radius would be reduced by if it was exactly down the slope
  major_radius = point_radius
  minor_radius = point_radius * max_slope_reduction
  x_offset = major_radius * cos(deg2rad(aspect))*cos(deg2rad(point_bearing)) + minor_radius * sin(deg2rad(aspect)) * sin(deg2rad(point_bearing))
  y_offset = minor_radius * cos(deg2rad(aspect))*sin(deg2rad(point_bearing)) - major_radius * sin(deg2rad(aspect)) * cos(deg2rad(point_bearing))
  offsets = c(x_offset,y_offset)
  new_radius = sqrt(x_offset^2 + y_offset^2)
  return(offsets)
}


adjust_distance_for_slope = function(point_radius,point_bearing,slope,aspect) {
  ## make an ellipse
  offsets = matrix(nrow=360,ncol=2)
  for(i in 1:360) {
    offset =  adjust_point_for_slope(point_radius,i,slope,aspect)
    x = offset[1]
    y = offset[2]
    offsets[i,] = offset
  }
  
  lookup = data.frame(offsets)
  names(lookup) = c("x","y")
  
  # for each point, get the angle and radius
  lookup$angle = atan(lookup$y/lookup$x) %>% rad2deg
  lookup$radius = radius = sqrt(lookup$x^2 + lookup$y^2)
  lookup$angle = (lookup$angle - 90) *-1
  point_bearing = point_bearing %% 180
  closest_row = which.min(abs(lookup$angle - point_bearing))
  radius = lookup[closest_row,"radius"]

  return(radius)
}




#### ** Compile and clean data ** ####

#### Load the raw field data ####

## function to open a tab from all spreadsheets and compile
compile_tab = function(tab_name) {
  na_vals = c("","NA","MA") # MA because there was a typo
  tab_a = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_CM.xlsx",sheet = tab_name,na=na_vals)
  tab_b = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_AG.xlsx",sheet = tab_name,na=na_vals)
  tab_c = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD.xlsx",sheet = tab_name,na=na_vals)
  tab_d = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_NB.xlsx",sheet = tab_name,na=na_vals)
  tab_e = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD2.xlsx",sheet = tab_name,na=na_vals)
  
  compiled = bind_rows(tab_a,tab_b,tab_c,tab_d,tab_e)
  
  return(compiled)
}

plots = compile_tab("Plot")
shrubs = compile_tab("Shrub")
seed_trees = compile_tab("SeedTree")
cwd = compile_tab("CWD")
subsample_threshold = compile_tab("SubsampleThreshold")
seedlings_plot = compile_tab("Seedlings_Plot")
seedlings_transect = compile_tab("Seedlings_Transect")
seedlings_dead = compile_tab("Seedlings_Dead")
prefire_trees = compile_tab("PrefireTrees")


#### Infer seedling distance metric (i.e., laser, haglof, tape) based on measurements used ####

## If they used the laser  (1 decimal places), interpret as horizontal distance; otherwise it was haglof (2 decimal places) so it is slope distance
decimals_length = str_split_fixed(seedlings_plot$Distance,pattern="\\.",n=2)[,2] %>% # split on the decimal, take second result
  nchar() # see how many digits
seedlings_plot$DistanceMetric = ifelse(decimals_length %in% c(0,2,3),"slope","horizontal")

## If they used the transect tape based on comments (or estimated in general based on comments), interpret as slope distance (overrides assessment based on decimals)
measure_w_tape = str_detect(seedlings_plot$Comments,fixed("dist",ignore_case=TRUE)) |
  str_detect(seedlings_plot$Comments,fixed("transect est",ignore_case=TRUE)) |
  str_detect(seedlings_plot$Comments,fixed("trasect est",ignore_case=TRUE)) |
  str_detect(seedlings_plot$Comments,fixed("used tape to measure",ignore_case=TRUE))

seedlings_plot$distance_estimate = ifelse(measure_w_tape,"yes","no")
seedlings_plot[which(seedlings_plot$distance_estimate == "yes"),"DistanceMetric"] = "slope"


#### Correct ambiguous data entries ####

plots[which(plots$FireSev == "4 OR 5"),"FireSev"] = 4.5




#### Interpret number columns as numeric ####

## Easiest way seems to be to write to CSV then read back in
write.csv(plots,"data/field-processed/compiled-uncleaned/plots.csv",row.names=FALSE)
write.csv(shrubs,"data/field-processed/compiled-uncleaned/shrubs.csv",row.names=FALSE)
write.csv(seed_trees,"data/field-processed/compiled-uncleaned/seed_trees.csv",row.names=FALSE)
write.csv(cwd,"data/field-processed/compiled-uncleaned/cwd.csv",row.names=FALSE)
write.csv(subsample_threshold,"data/field-processed/compiled-uncleaned/subsample_threshold.csv",row.names=FALSE)
write.csv(seedlings_plot,"data/field-processed/compiled-uncleaned/seedlings_plot.csv",row.names=FALSE)
write.csv(seedlings_transect,"data/field-processed/compiled-uncleaned/seedlings_transect.csv",row.names=FALSE)
write.csv(seedlings_dead,"data/field-processed/compiled-uncleaned/seedlings_dead.csv",row.names=FALSE)
write.csv(prefire_trees,"data/field-processed/compiled-uncleaned/prefire_trees.csv",row.names=FALSE)

plots = read.csv("data/field-processed/compiled-uncleaned/plots.csv",stringsAsFactors = FALSE)
shrubs = read.csv("data/field-processed/compiled-uncleaned/shrubs.csv",stringsAsFactors = FALSE)
seed_trees = read.csv("data/field-processed/compiled-uncleaned/seed_trees.csv",stringsAsFactors = FALSE)
cwd = read.csv("data/field-processed/compiled-uncleaned/cwd.csv",stringsAsFactors = FALSE)
subsample_threshold = read.csv("data/field-processed/compiled-uncleaned/subsample_threshold.csv",stringsAsFactors = FALSE)
seedlings_plot = read.csv("data/field-processed/compiled-uncleaned/seedlings_plot.csv",stringsAsFactors = FALSE)
seedlings_transect = read.csv("data/field-processed/compiled-uncleaned/seedlings_transect.csv",stringsAsFactors = FALSE)
seedlings_dead = read.csv("data/field-processed/compiled-uncleaned/seedlings_dead.csv",stringsAsFactors = FALSE)
prefire_trees = read.csv("data/field-processed/compiled-uncleaned/prefire_trees.csv",stringsAsFactors = FALSE)





#### Clean field data ####

## Function to replace a given value with another
fix_val = function(df,column,from,to) {
  df[which(df[,column] == from),column] = to
  return(df)
}


### Plots
plots = fix_val(plots,"Lat",30.50467,38.50467)
plots = fix_val(plots,"Long",120.34401,120.54401)
plots = fix_val(plots,"LiveOverstory","O",0)
plots = fix_val(plots,"LiveUnderstory","O.5",0.5)
plots = fix_val(plots,"PlotID","E00521","E0052I")
plots = fix_val(plots,"PlotID","E00551","E0055I")
plots = fix_val(plots,"PlotID","E0032T","E0032I")
plots = fix_val(plots,"PlotID","E0048C","E0048I")



# remove null rows
plots = plots %>%
  filter(!is.na(PlotID))

# fix a column name
plots = plots %>%
  rename(FuelELitter1 = "FuelELitter1..45",
    FuelELitter2 = "FuelELitter1..46")

plots[plots$PlotID == "A1147T",c("SlopeDeg","Aspect")] = c(0,0) ##!! TEMPORARY until we extract these

  
##!!TO-DO: correct the "4 or 5" fire sev


### Seedlings
seedlings_plot = fix_val(seedlings_plot,"Bearing",1001,101)
seedlings_plot = fix_val(seedlings_plot,"DBH","0/5",0.5)
seedlings_plot = fix_val(seedlings_plot,"CompCover","O",0)
seedlings_plot = fix_val(seedlings_plot,"CompHeight","9S",95) ##!! confirm C2006C
seedlings_plot = fix_val(seedlings_plot,"CompHeight","00",0) ##!! confirm E0054I

seedlings_plot = seedlings_plot %>%
  mutate(Subsample = recode(Subsample,"150"="N","NO"="N","YES"="Y"),
         Subsample = replace_na(Subsample,"N")) %>% # Make subsample text consistent
  filter(Species != "NONE") # remove the seedlings from plots that have no seedlings


### Shrubs
shrubs = fix_val(shrubs,"PctDead","S",5) ##!! confirm A4177C

## get rid of records that are not useful
shrubs = shrubs %>%
  filter(!Species %in% c("NONE","ERECT","PROSTRATE","HEIGHTCLASS1","HEIGHTCLASS2"))


### CWD
cwd = cwd %>%
  filter(Transect != "NONE")

cwd = fix_val(cwd,"Sound_Rotten",4,"S")



#### Write cleaned field data, read again to re-interpret numeric columns ####

## write
write.csv(plots,"data/field-processed/compiled-cleaned/plots.csv",row.names=FALSE)
write.csv(shrubs,"data/field-processed/compiled-cleaned/shrubs.csv",row.names=FALSE)
write.csv(seed_trees,"data/field-processed/compiled-cleaned/seed_trees.csv",row.names=FALSE)
write.csv(cwd,"data/field-processed/compiled-cleaned/cwd.csv",row.names=FALSE)
write.csv(subsample_threshold,"data/field-processed/compiled-cleaned/subsample_threshold.csv",row.names=FALSE)
write.csv(seedlings_plot,"data/field-processed/compiled-cleaned/seedlings_plot.csv",row.names=FALSE)
write.csv(seedlings_transect,"data/field-processed/compiled-cleaned/seedlings_transect.csv",row.names=FALSE)
write.csv(seedlings_dead,"data/field-processed/compiled-cleaned/seedlings_dead.csv",row.names=FALSE)
write.csv(prefire_trees,"data/field-processed/compiled-cleaned/prefire_trees.csv",row.names=FALSE)

## read
plots = read.csv("data/field-processed/compiled-cleaned/plots.csv",stringsAsFactors = FALSE)
shrubs = read.csv("data/field-processed/compiled-cleaned/shrubs.csv",stringsAsFactors = FALSE)
seed_trees = read.csv("data/field-processed/compiled-cleaned/seed_trees.csv",stringsAsFactors = FALSE)
cwd = read.csv("data/field-processed/compiled-cleaned/cwd.csv",stringsAsFactors = FALSE)
subsample_threshold = read.csv("data/field-processed/compiled-cleaned/subsample_threshold.csv",stringsAsFactors = FALSE)
seedlings_plot = read.csv("data/field-processed/compiled-cleaned/seedlings_plot.csv",stringsAsFactors = FALSE)
seedlings_transect = read.csv("data/field-processed/compiled-cleaned/seedlings_transect.csv",stringsAsFactors = FALSE)
seedlings_dead = read.csv("data/field-processed/compiled-cleaned/seedlings_dead.csv",stringsAsFactors = FALSE)
prefire_trees = read.csv("data/field-processed/compiled-cleaned/prefire_trees.csv",stringsAsFactors = FALSE)



#### ** Process data ** ####

#### Calculate fuel loads ####

# Convert survey date to date format
plots = plots %>%
  mutate(EnteredDate = mdy(EnteredDate),
         SurveyDate = mdy(SurveyDate))

# Determine plot attributes needed for calculating fuels
plots = plots %>%
  mutate(FuelsProtocol = ifelse(SurveyDate >= ymd("2018-06-17"),"new","old")) %>% # which fuel protocol were we using
  mutate(FuelsTransects = ifelse(!is.na(FuelE1),2,1)) # a value of 1 for FuelsTransects means N+S; a value of 2 means N+S+E+W

# Compute fine fuels averages across all transects
plots = plots %>%
  mutate(Fuel1hCount = rowMeans(cbind(FuelN1,FuelS1,FuelE1,FuelW1),na.rm=TRUE),
         Fuel10hCount = rowMeans(cbind(FuelN10,FuelS10,FuelE10,FuelW10),na.rm=TRUE),
         Fuel100hCount = rowMeans(cbind(FuelN100,FuelS100,FuelE100,FuelW100),na.rm=TRUE),
         LitterDepth = rowMeans(cbind(FuelNLitter1,FuelNLitter2,FuelSLitter1,FuelSLitter2,FuelELitter1,FuelELitter2,FuelWLitter1,FuelWLitter2),na.rm=TRUE),
         DuffDepth = rowMeans(cbind(FuelNDuff1,FuelNDuff2,FuelSDuff1,FuelSDuff2,FuelEDuff1,FuelEDuff2,FuelWDuff1,FuelWDuff2),na.rm=TRUE)) %>%
  # it it was with early protocol, multiply 100 hr fuels by adjustment factor to expand it to full transect
  mutate(Fuel100hCount = ifelse(FuelsProtocol == "old",Fuel100hCount*2.303,Fuel100hCount))

# Compute CWD per plot
cwd_plot = cwd %>%
  filter(!is.na(Diameter)) %>%
  mutate(Diam_sq = as.numeric(Diameter)^2) %>%
  group_by(PlotID,Sound_Rotten) %>%
  summarize(sum_sq_diam = sum(Diam_sq)) %>%
  ungroup() %>%
  tidyr::complete(PlotID,Sound_Rotten,fill=list(sum_sq_diam=0)) %>%
  spread(key=Sound_Rotten,value=sum_sq_diam) %>%
  rename(CWD_rotten = R,CWD_sound = S)

# Bring in info on how many fuels transects done
cwd_plot = left_join(cwd_plot,plots %>% select(PlotID,FuelsTransects),by="PlotID")

# Apply adjustment factors to CWD
cwd_plot = cwd_plot %>%
  mutate(CWD_rotten = CWD_rotten/FuelsTransects, # a value of 1 for FuelsTransects means N+S; a value of 2 means N+S+E+W
         CWD_sound = CWD_sound/FuelsTransects)

# Bring in CWD into plot data
plots = left_join(plots,cwd_plot %>% select(PlotID,CWD_rotten,CWD_sound),by="PlotID") %>%
  mutate_at(vars(CWD_rotten,CWD_sound), funs(ifelse(is.na(.),0,.)) )

## Brown's calcs

## Sq diam        specific grav   angle corr fact   length of line (ft)
# 1 hr: 0.0151     0.48            1.13              3.3*3.28
# 10 hr: 0.289    0.48            1.13              3.3*3.28
# 100 hr: 2.76   0.40            1.13              7.6*3.28
# cwd sound       0.40            1.00              7.6*2*3.28
# cwd rotten      0.30            1.00              7.6*2*3.28

# slope correction = 1

# convert sq cm to sq in: * 0.155

## calculate fuel mass
plots = plots %>%
  mutate(mass_1h_slope = (11.64 * `Fuel1hCount` * 0.0151 * 0.48 * 1.13 * 1)/ (3.3*3.28),
         mass_10h_slope = (11.64 * `Fuel10hCount` * 0.289 * 0.48 * 1.13 * 1) / (3.3*3.28),
         mass_100h_slope = (11.64 * `Fuel100hCount` * 2.76 * 0.40 * 1.13 * 1) / (7.6*3.28),
         mass_cwd_sound_slope = (11.64 * CWD_sound*0.155 * 0.40 * 1 * 1) / (7.6*2*3.28),
         mass_cwd_rotten_slope = (11.64 * CWD_rotten*0.155 * 0.30 * 1 * 1) / (7.6*2*3.28)) %>%
  mutate(mass_total_slope = mass_1h_slope + mass_10h_slope + mass_100h_slope + mass_cwd_sound_slope + mass_cwd_rotten_slope,
         mass_fine_slope = mass_1h_slope + mass_10h_slope + mass_100h_slope,
         mass_cwd_slope = mass_cwd_sound_slope + mass_cwd_rotten_slope) %>%
  mutate_at(vars(starts_with("mass_")),funs(.*2.2417)) # convert to Mg/ha

### Compute for horizontal area too

plots = plots %>%
  mutate(mass_total_horizontal = mass_total_slope / cos(deg2rad(SlopeDeg)),
         mass_fine_horizontal = mass_fine_slope / cos(deg2rad(SlopeDeg)),
         mass_cwd_horizontal = mass_cwd_slope/ cos(deg2rad(SlopeDeg)))




#### Calculate pre-fire oak distances (TO-DO) ####
##(see plot B1024C comment for example)



#### Apply expansion factors when subsampled ####
##!! TO-DO: check whether table of subsample thresholds matches subsample column of seedlings_plot datasheet

# If plot was a drone subsample plot, do not consider the tree a subsample (subsample should indicate transect subsample only)
plot_last_letter = str_sub(seedlings_plot$PlotID,-1,-1)
seedlings_plot = seedlings_plot %>%
  mutate(DronePlotTree = (plot_last_letter=="D" | plot_last_letter=="d")) %>%
  mutate(Subsample = ifelse(DronePlotTree,"N",Subsample)) # if it's a drone plot, it's not a subsample tree, otherwise use what it was before

# Store expansion factor
seedlings_plot = seedlings_plot %>%
  mutate(ExpansionFactor = ifelse(Subsample == "Y",8.87,1)) %>%
  mutate(ExpansionFactor = ifelse(Subsample == "Y" & PlotID == "C2001C",8.87/2,ExpansionFactor)) # halve the expansion factor for C2001C (where 4 transects were sampled)


#### Correct seedling measurements for slope ####
## Distance and density
## (i.e., increase distance, if shot with laser in HD mode, based on slope and aspect, to get slope distance)

# Determine plot area along slope and horizontally
plots = plots %>%
  mutate(SlopeAreaSeedlings = 3.14*11.3^2, # for seedlings, the plot radius was measured on the ground
         HorizontalAreaSeedlings = SlopeAreaSeedlings * cos(deg2rad(SlopeDeg)),
         HorizontalAreaTrees = 3.14*11.3^2, # for trees and snags, the plot radius was measured with a laser (horizontal)
         SlopeAreaTrees = HorizontalAreaTrees / cos(deg2rad(SlopeDeg))) %>%
  mutate_at(vars(contains("Area")),funs(.*0.0001)) # convert area to hectares

# for each seedling, pull in the plot's slope and aspect
seedlings_plot = seedlings_plot %>%
  mutate(PlotID_6digit = str_sub(PlotID,1,6))
seedlings_plot = left_join(seedlings_plot,plots %>% select(PlotID,SlopeDeg,Aspect,contains("Area")),by=c("PlotID_6digit" = "PlotID"))

# compute by what factor the slope distance is greater than horizontal, based on slope and aspect of plot and bearing of tree
seedlings_plot$SlopeAdjustedDistance = NA
for(i in 1:nrow(seedlings_plot)) {
  seedling_plot = seedlings_plot[i,]
  seedlings_plot[i,"SlopeAdjustedDistance"] = adjust_distance_for_slope(seedling_plot$Distance,seedling_plot$Bearing+180,seedling_plot$SlopeDeg,seedling_plot$Aspect)
}

seedlings_plot = seedlings_plot %>%
  mutate(DistanceExpansion = Distance/SlopeAdjustedDistance)

# compute slope distance and horizontal distance
seedlings_plot = seedlings_plot %>%
  mutate(SlopeDistance = ifelse(DistanceMetric == "horizontal",Distance*DistanceExpansion,Distance),
         HorizontalDistance = ifelse(DistanceMetric == "slope",Distance/DistanceExpansion,Distance)) %>%
  # Exclude seedlings that fell outside the plot radius on the ground (but were counted as in because the laser measures HD)
  filter(SlopeDistance <= 11.3)

# compute how much each seedling contrubutes to tree density
seedlings_plot = seedlings_plot %>%
  mutate(DensitySlope = ExpansionFactor/SlopeAreaSeedlings,
         DensityHorizontal = ExpansionFactor/HorizontalAreaSeedlings)




#### Status flags for plots ####

# Does plot have a drone subsample?
drone_plots = unique(seedlings_plot[seedlings_plot$DronePlotTree == TRUE,"PlotID"])
plots_w_drone_plots = str_sub(drone_plots,1,-2)
plots$HasDronePlot = plots$PlotID %in% plots_w_drone_plots

# Is plot internal, treatment, or control?
plots$Type = str_sub(plots$PlotID,6,6)
plots = plots %>%
  mutate(Type = recode(Type,C = "control", T = "treatment", I = "internal"))

# Do plots have pairs?
control_ids = plots[plots$Type == "control",]$PlotID %>% str_sub(1,5)
trt_ids = plots[plots$Type == "treatment",]$PlotID %>% str_sub(1,5)
plots = plots %>%
  mutate(PlotID_notype = str_sub(PlotID,1,5)) %>%
  mutate(HasPair = ifelse((Type == "control" & PlotID_notype %in% trt_ids) | (Type == "treatment" & PlotID_notype %in% control_ids),TRUE,FALSE)) %>%
  filter(HasPair | Type == "internal") # remove any paired plots (just one) without a pair

# What is the code of the fire
plots = plots %>%
  mutate(fire_code = str_sub(PlotID,start=1,end=1))

# Did a plot have subsampled trees in it?
plots_subsampled = unique(seedlings_plot[seedlings_plot$Subsample == "Y",]$PlotID)
plots = plots %>%
  mutate(Subsampled = PlotID %in% plots_subsampled)
##!! TO-DO: above is based on trees; make sure it conforms to plot-level records of subsampling too

# Names of fires
plots = plots %>%
  mutate(Fire = recode(fire_code,
                       A = "MoonAnt",
                       B = "AmRiv",
                       C = "Power",
                       D = "Piute",
                       E = "Ctnwd"))




#### Write processed plot data, ready to analyze ####

## Remove unnecessary columns processed plot data

plots = plots %>%
  select(-starts_with("Fuel")) %>%
  select(-Notes,Notes)

seedlings_plot = seedlings_plot %>%
  select(-HorizontalAreaSeedlings,-HorizontalAreaTrees,-SlopeAreaSeedlings,-SlopeAreaTrees)


## Write to CSV then read back in
write.csv(plots,"data/field-processed/compiled-processed/plots.csv",row.names=FALSE)
write.csv(shrubs,"data/field-processed/compiled-processed/shrubs.csv",row.names=FALSE)
write.csv(seed_trees,"data/field-processed/compiled-processed/seed_trees.csv",row.names=FALSE)
write.csv(cwd,"data/field-processed/compiled-processed/cwd.csv",row.names=FALSE)
write.csv(subsample_threshold,"data/field-processed/compiled-processed/subsample_threshold.csv",row.names=FALSE)
write.csv(seedlings_plot,"data/field-processed/compiled-processed/seedlings_plot.csv",row.names=FALSE)
write.csv(seedlings_transect,"data/field-processed/compiled-processed/seedlings_transect.csv",row.names=FALSE)
write.csv(seedlings_dead,"data/field-processed/compiled-processed/seedlings_dead.csv",row.names=FALSE)
write.csv(prefire_trees,"data/field-processed/compiled-processed/prefire_trees.csv",row.names=FALSE)

plots = read.csv("data/field-processed/compiled-processed/plots.csv",stringsAsFactors = FALSE)
shrubs = read.csv("data/field-processed/compiled-processed/shrubs.csv",stringsAsFactors = FALSE)
seed_trees = read.csv("data/field-processed/compiled-processed/seed_trees.csv",stringsAsFactors = FALSE)
cwd = read.csv("data/field-processed/compiled-processed/cwd.csv",stringsAsFactors = FALSE)
subsample_threshold = read.csv("data/field-processed/compiled-processed/subsample_threshold.csv",stringsAsFactors = FALSE)
seedlings_plot = read.csv("data/field-processed/compiled-processed/seedlings_plot.csv",stringsAsFactors = FALSE)
seedlings_transect = read.csv("data/field-processed/compiled-processed/seedlings_transect.csv",stringsAsFactors = FALSE)
seedlings_dead = read.csv("data/field-processed/compiled-processed/seedlings_dead.csv",stringsAsFactors = FALSE)
prefire_trees = read.csv("data/field-processed/compiled-processed/prefire_trees.csv",stringsAsFactors = FALSE)





#### ** Create spatial data ** ####

#### Plot perimeter ####

## Convert surveyed plots to spatial
plots = plots %>%
  mutate(Long = as.numeric(Long)*-1,
         Lat = as.numeric(Lat))

plots_sp <- st_as_sf(plots, coords = c("Long","Lat"), crs = 4326) %>%
  st_transform(3310)

## get Albers coordinates
plots_sp = cbind(plots_sp,st_coordinates(plots_sp))

## create an oval that accounts for slope; also that has a smaller radius if it was a drone subsample plot

plot_perims = list()




for(i in 1:nrow(plots_sp)) {
  
  plot = plots_sp[i,]
  slope = plot$SlopeDeg
  aspect = plot$Aspect
  radius = ifelse(is.na(plot$DronePlotRadius),11.3,plot$DronePlotRadius)
  x_coord = plot$X
  y_coord = plot$Y
  


  # for each of 360 degrees, get a point a radius-away from plot but adjusted for slope
  
  perim_coords = matrix(nrow=101,ncol=2)
  
  angles = c(seq(from=0,to=360,length.out=100),0)
  for(j in 1:101) {
    
    bearing = angles[j]
    
    offsets = adjust_point_for_slope(radius,bearing,slope,aspect)
    
    x_perim = x_coord + offsets[1]
    y_perim = y_coord + offsets[2]
    
    perim_coords[j,] = c(x_perim,y_perim)
    
  }
  
  plot_perim = st_polygon(list(perim_coords)) %>% st_sfc %>% st_sf
  plot_perims[[i]] = plot_perim
  
}

## take coordinates and make sf object
plots_perims = reduce(plot_perims,rbind)
plots_perims$PlotID = plots_sp$PlotID

plots_perims = left_join(plots_perims,plots_sp %>% st_set_geometry(NULL),by="PlotID")

# drop plot perimeters if they were subsampled but were not a drone plot
plots_perims = plots_perims %>%
  filter(!Subsampled | HasDronePlot)


## Write plot geospatial data
st_write(plots_sp,"data/field-processed/spatial/plots_points.gpkg",delete_dsn = TRUE)
st_write(plots_perims,"data/field-processed/spatial/plots_polygons.gpkg",delete_dsn = TRUE)


#### Make stem map ####

## for plots that have drone seedlings, drop all non-drone seedlings
plots_w_drone_plots = unique(plots[plots$HasDronePlot,]$PlotID)
seedlings_plot = seedlings_plot %>%
  filter(!PlotID %in% plots_w_drone_plots) %>%
  filter(!PlotID %in% plots_subsampled) %>% # also remove all seedlings from plots that were subsampled, even if some were not subsampled (since this is just used for stem mapping / drone comparisons)
  mutate(PlotID = str_sub(PlotID,1,6))

## determine each tree's offset from plot center in meters
seedlings_map = seedlings_plot %>%
  filter(Subsample == "N")

seedlings_map = seedlings_map %>%
  mutate(Brg_from_ctr = (as.numeric(Bearing) + 180)%%360 ) %>%
  mutate(Dst_from_ctr = as.numeric(HorizontalDistance)) %>%
  mutate(OffsetX = sin(deg2rad(Brg_from_ctr))*Dst_from_ctr) %>%
  mutate(OffsetY = cos(deg2rad(Brg_from_ctr))*Dst_from_ctr)

## for each seedling
# pull in its plot's coordinates
seedlings_map = left_join(seedlings_map,(plots_sp %>% select(PlotID,X,Y) %>% st_set_geometry(NULL) ),by="PlotID") %>% 
  rename(PlotX = "X",PlotY = "Y") %>%
  mutate(X = PlotX + OffsetX, # compute the seedling's coords based on offset
         Y = PlotY + OffsetY)

## make it spatial
seedlings_sp = seedlings_map %>%
  filter(!is.na(X)) %>% # no missing vals in coordinates; these are only for records of "none" for seedlings; obsolete now
  st_as_sf(coords = c("X", "Y"), crs = 3310)

## write seedling data
st_write(seedlings_sp,"data/field-processed/spatial/seedlings.gpkg",delete_dsn = TRUE)

seedlings_sp_over = seedlings_sp %>%
  filter(OMU == "O")
st_write(seedlings_sp_over,"data/field-processed/spatial/seedlings_over.gpkg",delete_dsn = TRUE)
##!!!! NOTE that this will exclude some drone seedlings because a few drone plots had no O/M/U recorded. Perhaps figure out if they were all O and if so, mark them as such so they get exported with this shapefile
