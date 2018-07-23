setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(raster)
library(sf)
library(tidyverse)
library(plotKML)

focal_fire = c("2007MOONLIGHT","2007ANTELOPE_CMPLX","2007MOONTELOPE")
focal_mgmt = c("salv: neither, prp: no, rel: e, thn: no, replt: no","salv: neither, prp: no, rel: e, thn: no, replt: YES") # amriv
focal_mgmt = c("salv: neither, prp: no, rel: no, thn: no, replt: no") # moontelope

out_folder = c("Moontelope-v1")

firesev  <- st_read("data/non-synced/existing-datasets/VegBurnSeverity_shp/veg_burn_severity.shp",stringsAsFactors=FALSE)

firesev = firesev %>%
  filter(VB_ID %in% focal_fire,
         BURNSEV %in% c(4),
         BEST_ASSES == "YES") %>%
  st_buffer(0) %>%
  st_union() %>%
  st_transform(3310)


planting = st_read("data/site-selection/output/aggregated-management-history/shapefiles/management_history.gpkg") %>%
  mutate(yr.fire.name = paste0(fire.year,fire.name)) %>%
  st_transform(3310)

## find the planting units on the focal fire
planting_overlaps = st_intersects(planting,firesev,sparse=FALSE)
polygons_overlapped = rowSums(planting_overlaps) # sum across rows to see which planting polygons overlapped candidate plots
planting_fire = planting[which(polygons_overlapped > 0),]

candidate_plots = st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_all_45m-45m_v1.gpkg") # amriv
candidate_plots = st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_moontelope_45m-45m_v1.gpkg") # moontelope

candidate_plots = candidate_plots %>%
  filter(fire.name %in% focal_fire,
         mgmt.factorial.nofire  %in% focal_mgmt)

## select the relevant planting polygons as the ones the selected plots overlap with
planting_overlaps = st_intersects(planting_fire,candidate_plots,sparse=FALSE)
polygons_overlapped = rowSums(planting_overlaps) # sum across rows to see which planting polygons overlapped candidate plots
planting_focal = planting_fire[which(polygons_overlapped > 0),] %>%
  st_buffer(1) %>%
  st_union() %>%
  st_buffer(-1)

planting_focal_data = st_sf(planting_focal,dummy="dummydata")

planting_nonfocal = planting_fire[which(polygons_overlapped == 0),] %>%
  st_buffer(1) %>%
  st_union() %>%
  st_buffer(-1)
planting_focal_zone = st_buffer(planting_focal,600) %>%
  st_buffer(1) %>%
  st_union() %>%
  st_buffer(-1)

## subtract the buffered planting units out from the high-severity
focal_zone = st_intersection(firesev,planting_focal_zone)

## subtract the non-focal planting units from the focal zone
focal_zone = st_difference(focal_zone,planting_nonfocal)

## buffer it in by 10 m to clean it up and so we don't get super close to irrelevant area
focal_zone = focal_zone %>%
  st_buffer(-10)


#### Make planting unit perimeters that are colored by whether they are comparable on both sides

candidate_plots_trt = candidate_plots %>%
  filter(type == "treatment")

## buffer the candidate plots out
cand_plots_trt_buff = candidate_plots_trt %>%
  st_buffer(80) %>%
  st_union()

## turn the planting unit perimeters into lines

planting_focal_perim = planting_focal %>%
  st_cast("MULTILINESTRING") %>%
  st_union()

# get the lines that overlap the buffered candidate plots

planting_perim_comparable = st_intersection(planting_focal_perim,cand_plots_trt_buff) %>% st_union()
planting_perim_notcomparable = st_difference(planting_focal_perim,cand_plots_trt_buff) %>% st_union()




### plot the focal area with a red outline; planting units with light green shading; two perim types colored

## write the comparable planting unit perimeter
t = planting_perim_comparable %>% st_buffer(5) %>% as("Spatial")
kml(t,
    file.name = paste0("data/uav/site-selection/",out_folder,"/perim_comparable.kml"),
    colour="purple",
    fill="purple"
    )


## write the focal study zone
st_write(focal_zone,paste0("data/uav/site-selection/",out_folder,"/focal_zone.kml"),delete_dsn = TRUE)

## write the focal planting units
kml(planting_focal_data %>% as("Spatial"),
    file.name = paste0("data/uav/site-selection/",out_folder,"/planting_focal.kml"),
    colour="green",
    alpha=.2
)







#### load the surveyed plots (for coordinates) and trees (for drone training data availability) data
library(readxl)
plots_a = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_CM.xlsx",sheet = "Plot")
plots_b = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_AG.xlsx",sheet = "Plot")
plots_c = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD.xlsx",sheet = "Plot")
plots_d = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_NB.xlsx",sheet = "Plot")

seedlings_a = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_CM.xlsx",sheet = "Seedlings_Plot")
seedlings_b = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_AG.xlsx",sheet = "Seedlings_Plot")
seedlings_c = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_KD.xlsx",sheet = "Seedlings_Plot")
seedlings_d = read_excel("data/field-raw/Latimer_JFSP_2018_Data_Spreadsheet_NB.xlsx",sheet = "Seedlings_Plot")

plots = bind_rows(plots_a,plots_b,plots_c,plots_d)
seedlings = bind_rows(seedlings_a,seedlings_b,seedlings_c,seedlings_d)

## determine how many over trees
seedlings_agg = seedlings %>%
  filter(OMU == "O") %>%
  group_by(PlotID) %>%
  summarize(n_over = n())

plots = left_join(plots,seedlings_agg)

plots[is.na(plots$n_over),]$n_over = 0

plots = plots %>%
  mutate(n_over_color = cut(n_over,breaks = c(-0.5,0.5,2.5,6.5,10000), labels=c(4,3,2,1))) %>%
  mutate(n_over_color = as.numeric(as.character(n_over_color)))





## Convert surveyed plots to spatial
plots = plots %>%
  mutate(Long = as.numeric(Long)*-1,
         Lat = as.numeric(Lat)) %>%
  mutate(fire_code = str_sub(PlotID,start=1,end=1)) %>%
  filter(fire_code == "A")

plots_sp_point <- st_as_sf(plots, coords = c("Long", "Lat"), crs = 4326) %>%
  st_transform(3310)

plots_sp_poly = plots_sp_point %>%
  st_buffer(15)


kml(plots_sp_point %>% as("Spatial"),
    file.name = paste0("data/uav/site-selection/",out_folder,"/surveyed_plots_point.kml"),
    colour=n_over_color,
    labels=n_over
)

kml(plots_sp_poly %>% as("Spatial"),
    file.name = paste0("data/uav/site-selection/",out_folder,"/surveyed_plots_poly.kml"),
    colour=n_over_color
)


## zip all the files in the focal folder
zip(zipfile = paste0("data/uav/site-selection/",out_folder,".kmz"), files = paste0("data/uav/site-selection/",out_folder))
