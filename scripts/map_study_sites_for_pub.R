library(tidyverse)
library(sf)
library(ggspatial)
library(raster)
library(Cairo)

# load nice TIFF basemap
basemap = stack("data/mapping_data/colored_elev_map/California_TIFF_Basemap.tiff") %>% aggregate(fact=3)



# load fire layer and thin to focal

st_layers("/home/derek/gis/Fire/fire19_1.gdb")
fires = st_read("/home/derek/gis/Fire/fire19_1.gdb",layer = "firep19_1") %>%
  mutate(year_name = paste(YEAR_,FIRE_NAME,sep=" "))

focal_fires = c("2007 Moonlight", "2007 Wheeler", "2004 Power", "2008 Piute", "2008 Government", "2008 Westville", "1994 Cottonwood") %>% toupper()

fires_foc = fires %>%
  filter(year_name %in% focal_fires) %>%
  filter(UNIT_ID != "SKU")


## load FACTS
facts = st_read("/home/derek/repos/postfire-management/data/non-synced/existing-datasets/FACTS/CA_Activity_merged.shp")
facts = facts %>%
  filter(DATE_A > 2008)

## load plots
plots = st_read("/home/derek/repos/postfire-management/data/field-processed/spatial/plots_points.gpkg")

focal_plots = c("C1020C","C1020T","C1032C","C1032T","C2010C")
plots_focal = plots %>%
  filter(PlotID %in% focal_plots)


# get a bbox around the plots
focal_area = st_bbox(plots_focal) %>% st_as_sfc() %>% st_set_crs(st_crs(plots_focal)) %>% st_buffer(300)
focal_area_rect = st_bbox(focal_area) %>% st_as_sfc() %>% st_set_crs(st_crs(focal_area))

activ_exclude = c("Compacting/Crushing of Fuels", "Wildfire - Human Ignition")

## get focal FACTS
facts_focal = st_intersection(facts,focal_area_rect %>% st_buffer(150) %>% st_transform(st_crs(facts))) %>%
  mutate(year = str_sub(DATE_A,1,4) %>% as.numeric) %>%
  filter(year > 2008) %>%
  filter(!(ACTIV %in% activ_exclude)) %>%
  filter(!grepl("cut|Cut",ACTIV)) %>%
  st_union()

facts_focal2 = facts_focal %>% st_transform(3310) %>%
  st_buffer(3)

st_write(facts_focal2, "data/mapping_data/facts_focal_map_fig.gpkg")



a1 = ggplot() +
  layer_spatial(basemap) +
  theme_void() +
  # geom_sf(data=nfs, fill=NA, color="white", size=0.2) +
  # geom_sf(data=nps, fill=NA, color="white", size=0.2) +
  # lims_bbox(focal_area) +
  #geom_sf(data=focal_area %>% st_transform(crs(ppt_norm))) +
  geom_sf(data=fires_foc, color=NA, fill="red") +
  # geom_sf_text(data=nfs,aes(label=initial), color = "white", nudge_x = 2000, nudge_y = -5000, fontface = "bold") +
  # geom_sf_text(data=nps,aes(label=initial), color = "white", nudge_x = -2000, fontface = "bold") +
  # coord_sf(crs=3310, expand=FALSE) +
  # scale_fill_viridis(limits=c(0,2000), direction=-1, name="Precipitation\n(mm)") +
  # theme(legend.position=c(.95,.97),
  #       legend.background = element_rect(fill=alpha("white",0.7)),
  #       legend.justification = c(1,1),
  #       legend.key.size = unit(0.48,"cm"),
  #       legend.spacing.y = unit(0.3,"cm"),
  #       #axis.text = element_blank(),
  #       #axis.ticks = element_blank(),
  #       panel.border= element_rect(color = "black", fill=NA, size=1),
  #       axis.title = element_blank()) +
  # labs(title="b) Normal precipitation") +
  annotation_scale(pad_y=unit(1.5,"cm"),
                   pad_x=unit(1.5,"cm"))

Cairo("data/mapping_data/ca_fires_map_for_pub.png", width=500*5,height=500*5,res=70*5)
a1
dev.off()
