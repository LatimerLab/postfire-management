setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(sf)

#### Convenience functions ####

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}




#### Load the raw field data ####
library(readxl)
plots_a = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_CM.xlsx",sheet = "Plot")
plots_b = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_AG.xlsx",sheet = "Plot")
plots_c = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD.xlsx",sheet = "Plot")
plots_d = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_NB.xlsx",sheet = "Plot")
plots_e = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD2.xlsx",sheet = "Plot")


seedlings_a = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_CM.xlsx",sheet = "Seedlings_Plot")
seedlings_b = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_AG.xlsx",sheet = "Seedlings_Plot")
seedlings_c = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD.xlsx",sheet = "Seedlings_Plot")
seedlings_d = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_NB.xlsx",sheet = "Seedlings_Plot")
seedlings_e = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD2.xlsx",sheet = "Seedlings_Plot")

plots = bind_rows(plots_a,plots_b,plots_c,plots_d,plots_e)
seedlings = bind_rows(seedlings_a,seedlings_b,seedlings_c,seedlings_d,plots_e)

#### Clean field data ####

## correct a bad latitude
plots = plots %>%
  mutate(Lat = ifelse(Lat == 30.50467,38.50467,Lat))

## correct a bad bearing
seedlings[which(seedlings$Bearing == 1001),"Bearing"] = 101



#### Apply expansion factors etc. ####

# make numeric columns numeric
seedlings = seedlings %>%
  mutate_at(vars(DBH,TotHeight,BudScars,Sprouts,CompCover,CompHeight),funs(as.numeric))
plots = plots %>%
  mutate_at(vars(Aspect,SlopeDeg,LgRocks,Litter,WoodyDebris,BasalVeg,Forbs,ForbHt,Grasses,GrassHt,Shrubs,ShrubHt,LiveOverstory,LiveUnderstory,SeedWallConifer),funs(as.numeric))



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



