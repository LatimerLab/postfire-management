setwd("C:/Users/DYoung/Documents/Research Projects/Post-fire management/postfire-management")

library(dplyr)
library(stringr)

plots = read.csv("existing_regen/regen_plots.csv",stringsAsFactors = FALSE)
plots = plots %>%
  mutate(fire_code = str_sub(Regen_Plot,1,3))

unique(plots$fire_code)


### Load tree and sapling regen and find plots with apparent planted trees

seedling = read.csv("existing_regen/tree_regen.csv",stringsAsFactors = FALSE)
sapling = read.csv("existing_regen/sapling_regen.csv",stringsAsFactors = FALSE)

seedl_plant = seedling %>%
  select(Regen_Plot,seed_veg_plant) %>%
  filter(seed_veg_plant == "P")

sapl_plant = sapling %>%
  select(Regen_Plot,seed_veg_plant) %>%
  filter(seed_veg_plant == "P")

planted_plots = unique(c(seedl_plant$Regen_Plot,sapl_plant$Regen_Plot))

### Load FACTS managed plots

facts_managed = read.csv("existing_regen/plots_exclude_FACTS.csv",stringsAsFactors = FALSE) %>%
  select(Regen_Plot,rel_yrs:managed)
facts_managed = facts_managed$Regen_Plot

### Load crew-observed managed plots

crew_managed = read.csv("existing_regen/plots_exclude.csv",stringsAsFactors = FALSE) %>%
  filter(Exclude != "")
crew_managed = crew_managed$Regen_Plot

### Combine all managed plots

managed_plots = unique(c(planted_plots,facts_managed,crew_managed))


### Filter regen plots to unmanaged, high severity
## only select necessary columns

plots = plots %>%
  filter(!(Regen_Plot %in% managed_plots),
         FIRE_SEV == 5) %>%
  mutate(survey_year = str_sub(Date,-12,-9)) %>%
  mutate(survey_years_post = as.numeric(survey_year) - as.numeric(Year.of.Fire)) %>%
  select(Regen_Plot,Fire,Year.of.Fire,survey_years_post,Easting, Northing, SHRUB, dominant_shrub_ht_cm)







