setwd("~/Research Projects/Post-fire management/postfire-management")

libs <- c("raster", "rgdal", "sf", "spatialEco", "tidyverse", "maptools", "rgeos","readxl")
lapply(libs, require, character.only = TRUE)

speciesorig <- read_excel("data/existing-datasets/r5-species-planting-1980-2018.xlsx")
colnames(speciesorig) <- tolower(colnames(speciesorig))

speciesorig <- speciesorig %>%
  dplyr::mutate(for_num = substr(speciesorig$suid, 3,4)) 

relevantforests <- c("03", "11", "13", "15", "16", "17")  #Eldorado, Plumas, Sequoia, Sierra, Stanislaus, Tahoe
speciesorig <- speciesorig %>%
  dplyr::filter(for_num %in% relevantforests) %>%
  rename("planned_seedlings.uom" = "planned_seedlings/uom") %>%
  rename("actual_seedlings.uom" = "actual_seedlings/uom")

species = speciesorig


# # ----- change trees uom to tpa uom
# 
# species <- species %>%
#   dplyr::mutate(tpa_planned = NA) %>%
#   dplyr::mutate(tpa_actual = NA)
# 
# standardize_uom <- function(i) {
#   unit <- species[i,]
#   
#   if(is.na(unit$seedling_uom)) return(0)
# 
#   if (unit$seedling_uom == "Trees per Acre") {
# 
#     unit$tpa_planned = unit$planned_seedlings.uom
#     unit$tpa_actual = unit$actual_seedlings.uom
# 
#   } else if (unit$seedling_uom == "Trees per Hectare") {
# 
#     unit$tpa_planned = unit$planned_seedlings.uom / 2.471
#     unit$tpa_actual = unit$actual_seedlings.uom / 2.471
# 
#   }
#   else if (unit$seedling_uom == "Trees") {
# 
#     unit$tpa_planned = unit$planned_seedlings.uom / unit$acres_planted
#     unit$tpa_actual = unit$actual_seedlings.uom/ unit$acres_planted
# 
#   } else if (unit$seedling_uom == "Thousand Seedlings") {
# 
#     unit$tpa_planned = unit$planned_seedlings.uom * 1000 / unit$acres_planted
#     unit$tpa_actual = unit$actual_seedlings.uom * 1000 / unit$acres_planted
# 
#   }
#   return(unit)
# 
# }
# 
# rows = 1:nrow(species)
# names(rows) = 1:nrow(species)
# 
# species <- sapply(rows, FUN=standardize_uom)  %>% bind_rows()


## we don't care about units because we just care about presence-absence
species_planted = species %>%
  mutate(planted = ((!is.na(actual_seedlings.uom)) & (actual_seedlings.uom != 0)))

# only keep the species that were planted
species_planted = species_planted %>%
  filter(planted == TRUE)


# repeat for PLANNED species

## we don't care about units because we just care about presence-absence
species_planned = species %>%
  mutate(planted = ((!is.na(planned_seedlings.uom)) & (planned_seedlings.uom != 0)))

# only keep the species that were planted
species_planned = species_planned %>%
  filter(planted == TRUE)




# ----- match species data to other planting data from FACTS

studyplots = read.csv("data/field-processed/compiled-processed/plots_w_gis_data.csv")
  

## make a list of all SUIDs and the plots they occur in 

plots_df = data.frame()

for(i in 1:nrow(studyplots)) {
  
  plot = studyplots[i,]
  suids = str_split(plot$facts.planting.suids.noslivers,pattern=fixed(", "))[[1]]
  
  plot_df = data.frame(PlotID=plot$PlotID,fire_year = plot$fire_year, suid = suids)
  plots_df = bind_rows(plots_df,plot_df)

}


## summarize our SUIDs to get density
sp_summ = species_planted %>%
  filter(suid %in% plots_df$suid) %>%
  filter(seedling_uom == "Trees per Acre") %>%
  group_by(suid) %>%
  summarize(tpa = sum(actual_seedlings.uom))
  

## from the species data, only keep records from our focal suids
species_planted = species_planted %>%
  filter(suid %in% plots_df$suid) %>%
  dplyr::select(suid,fy_accomp,sci_species_code,planted)

## from the species data, only keep records from our focal suids
species_planned = species_planned %>%
  filter(suid %in% plots_df$suid) %>%
  dplyr::select(suid,fy_accomp,sci_species_code,planted)






# for each row of the SUIDs (plots_df) data frame, look up that SUID from the species data and append it (as long as the year is later than the fire year)
  
species_per_plot_planted = data.frame()

for (i in 1:nrow(plots_df)) {
  
  plot = plots_df[i,]
  
  species_foc = species_planted %>%
    filter(suid == plot$suid,
           fy_accomp > plot$fire_year)
  
  planted_species_plot = unique(species_foc$sci_species_code)
  
  a = data.frame(test = planted_species_plot) %>% t %>% as.data.frame
  names(a) = planted_species_plot
  
  a = ifelse(!is.na(a),"planted","no") %>% as.data.frame()
  names(a) = planted_species_plot
  
  a$PlotID = plot$PlotID
  
  species_per_plot_planted = bind_rows(species_per_plot_planted,a)
  
  
}
  
species_per_plot_planted = species_per_plot_planted %>%
  dplyr::select(PlotID,everything())
  
  

## repeat for planned

species_per_plot_planned = data.frame()

for (i in 1:nrow(plots_df)) {
  
  plot = plots_df[i,]
  
  species_foc = species_planned %>%
    filter(suid == plot$suid,
           fy_accomp > plot$fire_year)
  
  planned_species_plot = unique(species_foc$sci_species_code)
  
  a = data.frame(test = planned_species_plot) %>% t %>% as.data.frame
  names(a) = planned_species_plot
  
  a = ifelse(!is.na(a),"planned","no") %>% as.data.frame()
  names(a) = planned_species_plot
  
  a$PlotID = plot$PlotID
  
  species_per_plot_planned = bind_rows(species_per_plot_planned,a)
  
  
}

species_per_plot_planned = species_per_plot_planned %>%
  dplyr::select(PlotID,everything())



# function for merging planned and planted tables. if a cell has both planned and planted, give planted priority.
plant_plan = function(x) {
  if("planted" %in% x) {
      return("yes")
  } else if ("planned" %in% x) {
      return("planned")
  #} else if (is.na(x[1]) & is.na(x[2])) {
  #    return(NA)
  } else {
    return("no")
  }
}

species_per_plot = bind_rows(species_per_plot_planted,species_per_plot_planned)

species_per_plot = species_per_plot %>%
  group_by(PlotID) %>%
  summarize_all(plant_plan)

#what plots have no records?  
spsums = rowSums(species_per_plot[,-1]=="no")
all_nos = spsums == (ncol(species_per_plot) - 1)

species_per_plot[all_nos,-1] = NA

names(species_per_plot)[-1] = paste0("planted_",names(species_per_plot)[-1])

write.csv(species_per_plot,"data/intermediate/species_per_plot.csv",row.names=FALSE)
