# Check shrub composition by fire 
# get lists of dominant shrubs for Table 1 in paper characterizoing the fires 

shrubs <- read.csv("./data/field-processed/compiled-processed/shrubs.csv")

shrubs$firecode <- substr(shrubs$PlotID, start = 1, stop = 1)

# Moonlight/Antelope
shrubs_A <- filter(shrubs, firecode == "A")
shrubs_A %>% group_by(Species) %>%
  summarise(mean_cover = sum(Cover)/length(unique(shrubs_A$PlotID))) # CEAVEL

# Power
shrubs_B <- filter(shrubs, firecode == "B")
shrubs_B %>% group_by(Species) %>%
  summarise(mean_cover = sum(Cover)/length(unique(shrubs_B$PlotID))) # CEACOR

# AmRiv
shrubs_C <- filter(shrubs, firecode == "C")
shrubs_C %>% group_by(Species) %>%
  summarise(mean_cover = sum(Cover)/length(unique(shrubs_C$PlotID))) # CEAINT

# Piute
shrubs_D <- filter(shrubs, firecode == "D")
shrubs_D %>% group_by(Species) %>%
  summarise(mean_cover = sum(Cover)/length(unique(shrubs_D$PlotID))) # CEACOR

# Cottonwood
shrubs_E <- filter(shrubs, firecode == "E")
shrubs_E %>% group_by(Species) %>%
  summarise(mean_cover = sum(Cover)/length(unique(shrubs_E$PlotID))) # ARCPA


# Check seedling density at "hottest driest" sites 
hist(plot_dhm$normal_annual_precip)
hist(plot_dhm$tmin)
plot_dhm_hotdry <- filter(plot_dhm, normal_annual_precip <= 1000, tmin>2)

hist(plot_dhm_hotdry$dens.conif, breaks = 10)
mean(plot_dhm_hotdry$dens.conif[plot_dhm_hotdry$fsplanted == "unplanted"])
mean(plot_dhm_hotdry$dens.conif[plot_dhm_hotdry$fsplanted == "planted"])
mean(plot_dhm$dens.conif)
