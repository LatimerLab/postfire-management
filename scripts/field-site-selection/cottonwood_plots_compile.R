library(sf)
library(tidyverse)
library(stringi)


st_rbind_all <- function(a,b) {
  
  cols.to.add.to.b <- setdiff(names(a),names(b))
  cols.to.remove.from.b <- setdiff(names(b),names(a))
  # add them
  b[,cols.to.add.to.b] <- NA
  # remove them
  b <- b %>%
    select(-one_of(cols.to.remove.from.b))
  # make sure we're only using the same columns from d.foc.yr
  a <- a[,names(b)]
  # put the columns in the same order
  b <- b[,names(a)]
  # combine them
  combined <- rbind(a,b)
  
  return(combined)
}

st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}





## open cottonwood candidate plots
cwd_ctl = st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_cottonwood_customfilter_control.gpkg")
cwd_int = st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_cottonwood_customfilter_internal.gpkg")
cwd_perim = st_read("data/site-selection/output/candidate-plots/candidate_plots_paired_filtered_cottonwood_customfilter_perimeter.gpkg")
cwd_plots_pre = rbind(cwd_ctl,cwd_int)
cwd_plots = rbind(cwd_plots_pre,cwd_perim)

focal_areas = st_read("data/site-selection/input/cottonwood_plot_focal_area.shp") %>% st_transform(st_crs(cwd_plots))

plots_foc = st_intersection(cwd_plots,focal_areas)

# only keep the important cols
plots_foc = plots_foc %>%
  select(type,id,slope,aspect,rad,elev,ctl.id) %>%
  rename(geometry =  "geom")


# load the manual plots
man_trt = st_read("data/site-selection/input/cottonwood_manual_plots_trt.shp") %>% mutate(type = "treatment")
man_ctl = st_read("data/site-selection/input/cottonwood_manual_plots_ctl.shp") %>% mutate(type = "control")

man_plts = rbind(man_trt,man_ctl) %>%
  mutate(id = row_number()+9000,
         type = type,
         slope = 10,
         aspect = 10,
         rad = 5000,
         elev=2000,
         ctl.id=NA) %>%
  select(type,id,everything()) %>%
  st_transform(st_crs(plots_foc))


## create plot grid in ctw control areas
ctl_areas = focal_areas %>% filter(id > 10)
ctl_int = st_make_grid(ctl_areas,cellsize=150,what="centers")
ctl_int = st_intersection(ctl_int,ctl_areas) %>% st_sf() %>%
  mutate(type = "internal",
         id=row_number()+8000,
         slope = 10,aspect=10,rad=5000,elev=2000,ctl.id=NA)


## merge those back in with the other kept plots
plots_all_pre = rbind(plots_foc,ctl_int)
plots_all = rbind(plots_all_pre,man_plts)




## assign IDs based on ascending X coordinate

plots_all$x = st_coordinates(plots_all)[,1]

plots_all = plots_all %>%
  arrange(x) %>%
  mutate(name = row_number()) %>%
  mutate(prefix = "E") %>%
  mutate(suffix = stri_sub(type,from=1,to=1) %>% toupper()) %>%
  mutate(name = str_pad(name, width=4, side="left", pad="0"),
         name2 = paste0(prefix,name,suffix)) %>%
  mutate(type = as.factor(type),
         type = factor(type,levels=c("treatment","control","internal","internal_ctl"))) %>%
  arrange(type)

st_write(plots_all,"data/site-selection/output/selected-plots/selected_plots_cottonwood_v1.gpkg")

plots_all = st_read("data/site-selection/output/selected-plots/selected_plots_cottonwood_v1_fixed.gpkg")


head(plots_all)

coords = st_coordinates(plots_all %>% st_transform(4326))

plots_table = plots_all %>%
  select(type,ID = name2,
         rad,
         elev) %>%
  mutate(lat = coords[,2],
         long = coords[,1]) %>%
  st_drop_geometry() %>%
  mutate(rad = round(rad),
         elev = round(elev))


write.csv(plots_table,"data/site-selection/output/selected-plots/selected_plots_cottonwood_v1_fixed.csv")

## write separate treatment and control

plots_all_trt = plots_all %>%
  filter(type != "control")
plots_all_ctl = plots_all %>%
  filter(type == "control")

st_write(plots_all_trt,"data/site-selection/output/selected-plots/selected_plots_cottonwood_v1_fixed_trt.gpkg")
st_write(plots_all_ctl,"data/site-selection/output/selected-plots/selected_plots_cottonwood_v1_fixed_ctl.gpkg")


## write gpx

plots_gps = plots_all %>%
  select(name = name2) %>%
  st_transform(4326)

st_write(plots_gps,"data/site-selection/output/selected-plots/selected_plots_cottonwood_v1_fixed.gpx",driver="GPX",delete_dsn = TRUE)





