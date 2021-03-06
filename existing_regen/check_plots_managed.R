setwd("C:/Users/DYoung/Documents/Research Projects/Post-fire management/postfire-management")

library(sf)
library(tidyverse)
library(lubridate)


## For each plot, get all FACTS that overlap ##
# If fire is POWER, use Accmp date, else use Compl date
# Thin to FACTS within year of fire and year of survey

## Keep only FACTS that are planting, salvage (except helicopter)
## Concatenate into year and action

salvage = c("Salvage Cut (intermediate treatment, not regeneration)","Stand Clearcut (w/ leave trees) (EA/RH/FH)")
planting = c("Plant Trees")
release = c("Tree Release and Weed")

facts = st_read("data/non-synced/existing-datasets/FACTS/CA_Activity_merged.shp",stringsAsFactors=FALSE)

facts_focal = facts %>%
  filter(ACTIV %in% c(salvage,planting,release))

facts_focal = facts_focal %>%
  mutate(yr_accomp = str_sub(DATE_A,1,4),
         yr_compl = str_sub(DATE_C,1,4)) %>%
  filter(!(is.null(yr_accomp)&is.null(yr_compl)))

plots = st_read("existing_regen/regen_plots.gpkg",stringsAsFactors=FALSE)
plots$rel_yrs = NA
plots$plant_yrs = NA
plots$salv_yrs = NA

facts_focal = st_transform(facts_focal,st_crs(plots))

intersection = st_intersects(plots,facts_focal)


### Compute survey years post
plots = plots %>%
  mutate(survey_year = str_sub(Date,-12,-9) %>% as.numeric) %>%
  mutate(survey.years.post = survey_year - Year.of.Fire)



for(i in 1:nrow(plots)) {
  
  plot = plots[i,]
  yr_fire = plot$Year.of.Fire
  yr_survey = yr_fire + plot$survey.years.post
  fire = plot$Fire
  
  facts_intersect_rows = intersection[[i]]
  facts_intersect = facts_focal[facts_intersect_rows,]
  
  facts_intersect = facts_intersect %>%
    mutate(yr_mgd = ifelse(fire == "Power" & is.na(yr_compl),yr_accomp,yr_compl)) %>%
    filter(!is.na(yr_mgd)) %>%
    filter((yr_mgd >= yr_fire) & (yr_mgd <= yr_survey))
  
  facts_int_rel = facts_intersect %>%
    filter(ACTIV %in% release)
  
  facts_int_plant = facts_intersect %>%
    filter(ACTIV %in% planting)
  
  facts_int_salv = facts_intersect %>%
    filter(ACTIV %in% salvage)
  
  rel_yrs = paste(facts_int_rel$yr_mgd,collapse = ", ")
  plant_yrs = paste(facts_int_plant$yr_mgd,collapse = ", ")
  salv_yrs = paste(facts_int_salv$yr_mgd,collapse = ", ")
  
  plots[i,"rel_yrs"] = rel_yrs
  plots[i,"plant_yrs"] = plant_yrs
  plots[i,"salv_yrs"] = salv_yrs
  
}


plots = plots %>%
  mutate(released = (rel_yrs != "") * 1,
         salvaged = (salv_yrs != "") * 1,
         planted = (plant_yrs != "") * 1) %>%
  mutate(managed = (released | salvaged | planted) * 1)

#st_write(plots,"../shapefiles/plots_w_mgt.gpkg")


# export list of apparent facts-managed plots
plots_mgd = plots %>%
  filter(managed == 1)

st_geometry(plots_mgd) = NULL

write.csv(plots_mgd,"existing_regen/plots_exclude_FACTS.csv")
