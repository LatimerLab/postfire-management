#setwd("C:/Users/DYoung/Documents/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)

## load data
plots = read.csv("data/field-processed/compiled-processed/plots_w_gis_data.csv",stringsAsFactors = FALSE)
seed_trees = read.csv("data/field-processed/compiled-processed/seed_trees.csv",stringsAsFactors = FALSE)
seedlings_plot = read.csv("data/field-processed/compiled-processed/seedlings_plot.csv",stringsAsFactors = FALSE)
prefire_trees = read.csv("data/field-processed/compiled-processed/prefire_trees.csv",stringsAsFactors = FALSE)


#### Data prep that applies to more than one section below ####

# get the nearest seed tree by plot; creates var MinSeedDist of the closest seed tree
nearest_seed_tree = seed_trees %>%
  group_by(PlotID) %>%
  summarize(MinSeedDist = min(Distance))

# append it to the main plot data
plots = left_join(plots,nearest_seed_tree,by="PlotID") #adds MinSeedDist to plots







#### Analysis of paired plots ####

plots = plots %>%
  filter(Type %in% c("control"))

# capture species more generally
seedlings_plot <- seedlings_plot %>%
  mutate(Species = recode(Species,PIPO = "PIPJ",PIJE = "PIPJ"))

# summarize trees by plot, make wide
seedl_summ <- seedlings_plot %>%
  filter(Species %in% c("PIPJ","PILA","ABCO","CADE","ABMA","PSME")) %>%
  group_by(PlotID, Species) %>%
  summarize(count = n(),
            plot_size = mean(ExpansionFactor)) %>%
  mutate(plot_size = 401/plot_size) %>%
  pivot_wider(id_cols = PlotID,
              names_from = Species,
              values_from = c("count","plot_size"))
  
seedl_summ = left_join(plots %>% select(PlotID), seedl_summ) %>%
  mutate(across(starts_with("plot_size_"),~ifelse(is.na(.),401,.)),
           across(starts_with("count_"),~ifelse(is.na(.),0,.))) %>%
  rename(plot_id = PlotID)


### summarize seed tree

seed_trees_wide = seed_trees %>%
  mutate(Species = ifelse(Species %in% c("PIPO","PIJE","PIPJ","PIPJE"), "PIPJ",Species)) %>%
  filter(Species %in% c("PIPJ","PSME","ABCO","ABMA","CADE","PILA")) %>%
  group_by(PlotID,Species) %>%
  summarize(Distance = min(Distance)) %>%
  pivot_wider(id_cols = PlotID,values_from = Distance, names_from = Species,names_prefix = "distance_seed_source_") %>%
  rename(plot_id = PlotID)


### Summarize prefire trees? skipping for now becuause there were so few

  

### Prep plot data

## coords
p_sf = st_as_sf(plots,coords=c("X","Y"),crs=3310) %>% st_transform(4326)
coords = st_coordinates(p_sf) %>% as.data.frame
p_sf$longitude = coords$X
p_sf$latitude = coords$Y
st_geometry(p_sf) = NULL
p = p_sf

p = p %>%
  mutate(contributor = "DJNY",
         sample_year = 2018,
         time_since_fire = 2018-fire_year,
         fire_severity_category = "high") %>%
  select(plot_id = PlotID,
         contributor,
         longitude,
         latitude,
         fire_year,
         sample_year,
         time_since_fire,
         distance_seed_source = MinSeedDist,
         fire_severity_category,
         canopy_cover = LiveOverstory,
         litter_cover = Litter,
         grass_cover = Grasses,
         forb_cover = Forbs,
         shrub_cover = Shrubs)


p = left_join(p,seedl_summ)
p = left_join(p,seed_trees_wide)

write_csv(p,"data/field-processed/compiled-processed/data_kdavis.csv")
