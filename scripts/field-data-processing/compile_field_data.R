setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)
library(readxl)

#### Convenience functions ####



#### Load the raw field data ####

## function to open a tab from all spreadsheets and compile
compile_tab = function(tab_name) {
  na_vals = c("","NA","MA") # MA because there was a typo
  tab_a = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_CM.xlsx",sheet = tab_name,na=na_vals)
  tab_b = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_AG.xlsx",sheet = tab_name,na=na_vals)
  tab_c = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD.xlsx",sheet = tab_name,na=na_vals)
  tab_d = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_NB.xlsx",sheet = tab_name,na=na_vals)
  tab_e = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD2.xlsx",sheet = tab_name)
  
  compiled = bind_rows(tab_a,tab_b,tab_c,tab_d,tab_e)
  
  # make NAs (actual missing values) into 
  # make "NA"s (character strings) into blanks (no)
  
  
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


#### Interpret number columns as numeric ####

## Need to write to CSV then read back in
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
plots = fix_val(plots,"LiveOverstory","O",0)
plots = fix_val(plots,"LiveUnderstory","O.5",0.5)

##!!TO-DO: Pull in nearest QUKE and QUCH if in tree data



### Seedlings

seedlings_plot = fix_val(seedlings_plot,"Bearing",1001,101)
seedlings_plot = fix_val(seedlings_plot,"DBH","0/5",0.5)
seedlings_plot = fix_val(seedlings_plot,"CompCover","O",0)
seedlings_plot = fix_val(seedlings_plot,"CompHeight","9S",95) ##!! confirm C2006C
seedlings_plot = fix_val(seedlings_plot,"CompHeight","00",0) ##!! confirm E0054I





#### Apply expansion factors etc. ####



#### Write compiled plot data ####

write.csv(plots,"data/field-processed/plots.csv",row.names=FALSE)




#### Convert plots to spatial ####

## Convert surveyed plots to spatial
plots = plots %>%
  mutate(Long = as.numeric(Long)*-1,
         Lat = as.numeric(Lat)) %>%
  mutate(fire_code = str_sub(PlotID,start=1,end=1))

plots_sp <- st_as_sf(plots, coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(3310)

## get Albers coordinates
plots_sp = cbind(plots_sp,st_coordinates(plots_sp))

## convert to polygon representing the area covered by the plot
plots_sp_poly = plots_sp %>%
  st_buffer(11.3)


## Write plot geospatial data
st_write(plots_sp,"data/field-processed/plots_points.gpkg",delete_dsn = TRUE)
st_write(plots_sp_poly,"data/field-processed/plots_polygons.gpkg",delete_dsn = TRUE)


#### Convert trees to spatial ####

## determine each tree's offset from plot center in meters

seedlings = seedlings %>%
  mutate(Brg_from_ctr = (as.numeric(Bearing) + 180)%%360 ) %>%
  mutate(Brg_from_ctr_rad = (Brg_from_ctr * pi)/180) %>%
  mutate(Dst_from_ctr = as.numeric(Distance)) %>%
  mutate(OffsetX = sin(Brg_from_ctr_rad)*Dst_from_ctr) %>%
  mutate(OffsetY = cos(Brg_from_ctr_rad)*Dst_from_ctr)

## for each seedling
seedlings = left_join(seedlings,(plots_sp %>% select(PlotID,X,Y) %>% st_drop_geometry() )) %>% # pull in its plot's coordinates
  rename(PlotX = "X",PlotY = "Y") %>%
  mutate(X = PlotX + OffsetX, # compute the seedling's coords based on offset
         Y = PlotY + OffsetY)

## make it spatial
seedlings_sp = seedlings %>%
  filter(!is.na(X)) %>% # no missing vals in coordinates; these are only for records of "none" for seedlings
  st_as_sf(coords = c("X", "Y"), crs = 3310)

## write seedling data
st_write(seedlings_sp,"data/field-processed/seedlings.gpkg",delete_dsn = TRUE)

seedlings_sp_over = seedlings_sp %>%
  filter(OMU == "O")
st_write(seedlings_sp_over,"data/field-processed/seedlings_over.gpkg",delete_dsn = TRUE)



