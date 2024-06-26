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
sp_summ = species %>%
  filter(suid %in% plots_df$suid) %>%
  filter(seedling_uom %in% c("Trees","Trees per Acre")) %>%
  ## if reported as "Trees", compute TPA
  mutate(planted_density = ifelse(seedling_uom == "Trees", actual_seedlings.uom / acres_planted, actual_seedlings.uom)) %>%
  mutate(planned_density = ifelse(seedling_uom == "Trees", planned_seedlings.uom / acres_planted, planned_seedlings.uom)) %>%
  ## if actual is not avaialable, use planned
  mutate(seedlings = ifelse(is.na(planted_density),planned_density,planted_density)) %>%
  
  ### Can run just the above to get suid-specific summaries
  group_by(suid) %>%
  summarize(tpa = sum(seedlings))



### Get the fire associated with each SUID, summarize by fire

# For each plot, get the average density of all the suids with planting data
plots_planted = studyplots %>%
  filter(Type == "treatment")

for(i in 1:nrow(plots_planted)) {
  
  plot = plots_planted[i,]
  
  suids = str_split(plot$facts.planting.suids.noslivers,pattern=fixed(", "))[[1]]
  
  density = sp_summ %>%
    filter(suid %in% suids) %>%
    pull(tpa) %>%
    mean
  
  plots_planted[i,"mean_planted_density"] = density
  
}

plots_planted = plots_planted %>%
  dplyr::select(PlotID,Fire,facts.planting.suids.noslivers,mean_planted_density) %>%
  mutate(density_reported = !is.na(mean_planted_density))


#### for the plots that don't have density, extract it from the nearest plot that does.

## load  plot geospatial

plots_sf = st_read("data/field-processed/spatial/plots_points.gpkg")
plots_sf_planted = plots_sf %>%
  filter(Type == "treatment")

plots_sf_w_density = left_join(plots_sf_planted,plots_planted,by="PlotID") %>%
  filter(!is.na(mean_planted_density))

for(i in 1:nrow(plots_planted)) {
  
  plot = plots_planted[i,]
  
  if(is.na(plot$mean_planted_density)) {
    
    ## find the nearest plot that does have density
    focal_plot_sf = plots_sf %>%
      filter(PlotID == plot$PlotID)
    
    dists = st_distance(focal_plot_sf,plots_sf_w_density)
    # get closest
    closest_index = which(dists == min(dists))[1]
    closest_plot = plots_sf_w_density[closest_index,]
    closest_plot_density = closest_plot$mean_planted_density
    
    plots_planted[i,"mean_planted_density"] = closest_plot_density
    
  }

}


## summarize the mean and variation by fire
planted_summ = plots_planted %>%
  group_by(Fire) %>%
  summarize(mean = mean(mean_planted_density, na.rm = TRUE),
            min = min(mean_planted_density, na.rm = TRUE),
            lwr = quantile(mean_planted_density, 0.25, na.rm = TRUE),
            upr = quantile(mean_planted_density, 0.75, na.rm = TRUE),
            max = max(mean_planted_density, na.rm = TRUE),
            n_plots_w_reported_density = sum(density_reported),
            n_plots_total = n()) %>%
  select(Fire,min,lwr,mean,upr,max, everything()) %>%
  mutate_at(vars(min,lwr,mean,upr,max),~.*2.471) %>%
  mutate_if(is.numeric,round)

write.csv(planted_summ,"tables/planting_density.csv", row.names=FALSE)




## Save plots with planted density
plots_planted = plots_planted %>%
  select(PlotID,mean_planted_density)

write.csv(plots_planted,"data/intermediate/density_per_plot.csv", row.names=FALSE)





  

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
