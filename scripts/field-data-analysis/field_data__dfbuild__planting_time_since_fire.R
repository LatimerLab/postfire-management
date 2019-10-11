library(lme4)
library(gridExtra)
library(qpcR)
library(tidyverse)


#####  load data -----------------------------------------------------------------------------------

plots = read.csv("data/field-processed/compiled-processed/plots_w_gis_data.csv",stringsAsFactors = FALSE)
subsample_threshold = read.csv("data/field-processed/compiled-processed/subsample_threshold.csv",stringsAsFactors = FALSE)
seedlings_plot = read.csv("data/field-processed/compiled-processed/seedlings_plot.csv",stringsAsFactors = FALSE)
seedlings_transect = read.csv("data/field-processed/compiled-processed/seedlings_transect.csv",stringsAsFactors = FALSE)

seedlings_plot <- seedlings_plot %>%
  mutate(Species = recode(Species, "ACMA" = "ABMA", "AMBA" = "ABMA"))

plots <- plots %>%
  mutate(fsplanted = ifelse(!is.na(facts.planting.first.year), "planted", "unplanted")) %>% # create a variable for whether were planted or no
  mutate(PairID = str_sub(PlotID, 1,5)) %>%
  mutate(planted_PIPO = ifelse(is.na(planted_PIPO) & Type == "treatment", "yes", planted_PIPO)) %>%
  mutate(planted_PIJE = ifelse(is.na(planted_PIJE) & Type == "treatment" & Fire == "Ctnwd", "yes", planted_PIJE)) %>% 
  mutate(plantingList = paste(ifelse(planted_ABCO == "no", NA,"ABCO"), 
                              ifelse(planted_CADE27 == "no", NA,"CADE"),
                              ifelse(planted_PIJE == "no", NA, "PIJE_PIPJ"),
                              ifelse(planted_PIPO == "no", NA, "PIPO_PIPJ"),
                              ifelse(planted_PILA == "no", NA, "PILA"),
                              ifelse(planted_PSME == "no", NA, "PSME"),
                              ifelse(planted_ABMA == "no", NA, "ABMA"),
                              ifelse(planted_SEGI2 == "no", NA, "SEGI"),
                              sep = "_"))
plantedList <- plots %>% 
  filter(fsplanted == "planted") %>% select(PairID, plantingList)
plots <- plots %>%
  select(-plantingList) %>%
  left_join(plantedList, by = "PairID")

  
    




##### make summaries of each variable --------------------------------------------------------------

# group species
seedlings_plot <- seedlings_plot %>%
  filter(DronePlotTree == FALSE) %>%
  left_join(plots %>% select(PlotID, plantingList), by = "PlotID") %>%
  mutate(yelPine = Species %in% c("PIPO", "PIJE", "PIPJ"), # combine yellowpines.
         pine = Species %in% c("PIPO", "PIJE", "PILA", "PIPJ"), # creates T/F column for Pine
         shadeTol = Species %in% c("ABCO","ABMA","CADE", "FIR"), # creates T/F column for Firs and incense cedar
         oak = Species %in% c("QUKE", "QUCH"),
         conif = Species %in% c("PIPO", "PIJE", "PIPJ", "PILA", "ABCO", "ABMA", "CADE", "FIR", "PSME", "JUOC"))

seedlings_plot$plantedFromList <- TRUE
for (i in 1:nrow(seedlings_plot)) {
  seedlings_plot[i,]$plantedFromList <- ifelse(seedlings_plot[i,]$Species %in% as.vector(str_split_fixed(seedlings_plot[i,]$plantingList, "_", n = 10)), TRUE, FALSE)
  } #I spent way too long trying to do this in dplyr. Screw you dplyr. for loops and base R for life!

# summarize trees by plot
seedl_dhm <- seedlings_plot %>%
  group_by(PlotID) %>%
  summarize(dens.all = sum(DensitySlope),
            dens.yelPine = sum(DensitySlope*yelPine),
            dens.pine = sum(DensitySlope*pine),
            dens.shadeTol = sum(DensitySlope*shadeTol),
            #dens.oak = sum(DensitySlope*oak),
            dens.conif = sum(DensitySlope*conif),
            dens.planted =sum(DensitySlope*plantedFromList),
            
            ave.ht.all = mean(TotHeight * DensitySlope),
            ave.ht.yelPine = mean(TotHeight * DensitySlope*yelPine),
            ave.ht.pine = mean(TotHeight * DensitySlope*pine),
            ave.ht.shadeTol = mean(TotHeight * DensitySlope*shadeTol),
            #ave.ht.oak = mean(TotHeight * DensitySlope*oak),
            ave.ht.conif = mean(TotHeight * DensitySlope*conif),
            ave.ht.planted = mean(TotHeight * DensitySlope*plantedFromList),
            
            tot.ht.all = sum(TotHeight * DensitySlope),
            tot.ht.yelPine = sum(TotHeight * DensitySlope*yelPine),
            tot.ht.pine = sum(TotHeight * DensitySlope*pine),
            tot.ht.shadeTol = sum(TotHeight * DensitySlope*shadeTol),
            #tot.ht.oak = sum(TotHeight * DensitySlope*oak),
            tot.ht.conif = sum(TotHeight * DensitySlope*conif),
            tot.ht.planted = sum(TotHeight * DensitySlope*plantedFromList),
            
            ave.mass.all = mean(TotHeight^2 * DensitySlope),
            ave.mass.yelPine = mean(TotHeight^2 * DensitySlope*yelPine),
            ave.mass.pine = mean(TotHeight^2 * DensitySlope*pine),
            ave.mass.shadeTol = mean(TotHeight^2 * DensitySlope*shadeTol),
            #ave.mass.oak = mean(TotHeight^2 * DensitySlope*oak),
            ave.mass.conif = mean(TotHeight^2 * DensitySlope*conif),
            ave.mass.planted = mean(TotHeight^2 * DensitySlope*plantedFromList),
            
            tot.mass.all = sum(TotHeight^2 * DensitySlope),
            tot.mass.yelPine = sum(TotHeight^2 * DensitySlope*yelPine),
            tot.mass.pine = sum(TotHeight^2 * DensitySlope*pine),
            tot.mass.shadeTol = sum(TotHeight^2 * DensitySlope*shadeTol),
            #tot.mass.oak = sum(TotHeight^2 * DensitySlope*oak),
            tot.mass.conif = sum(TotHeight^2 * DensitySlope*conif),
            tot.mass.planted = sum(TotHeight^2 * DensitySlope*plantedFromList))
 
# combine with plot data
plot_dhm <- plots %>%
  mutate(fsplanted = ifelse(!is.na(facts.planting.first.year), "planted", "unplanted")) %>% # create a variable for whether were planted or not
  mutate(PairID = ifelse(Type == "internal" & fire_code == "E", "Eint", PlotID_notype)) %>% #creates new PlotID where all the intenal plots at one site are within one pair
  mutate(PairID = ifelse(Type == "internal" & fire_code == "A", "Aint", PlotID_notype)) %>%         
  left_join(seedl_dhm, by="PlotID") 

## make it long form so we can plot them as panels

plot_dhm_long <- plot_dhm %>%
  gather(key="metric",value="value", colnames(seedl_dhm) %>% .[.!= "PlotID"]) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>% #assign 0 to na values
  mutate(met.type = str_replace(metric, "\\.[^.]+$" , ""))

#save(plot_dhm_long, plot_dhm, file= "output/plotSeedlingData.RData")






#####################################################################################
#####################################################################################
#  ___/-\___  #                  __                       .__                       #
# |---------| #                _/  |_____________    _____|  |__                    #
#  |   |   |  #                \   __\_  __ \__  \  /  ___/  |  \                   #
#  | | | | |  #                 |  |  |  | \// __ \_\___ \|   Y  \                  #
#  | | | | |  #                 |__|  |__|  (____  /____  >___|  /                  #
#  | | | | |  #                                  \/     \/     \/                   #
#  |_______|  #######################################################################
#####################################################################################

plot_dhm %>%  filter(Type == "internal", fire_code != "E") #search for plots that are coded internal that arent in Cw site

cor(plots %>% dplyr::select(elev, rad_march, slope_dem),  use = "complete.obs", method = "pearson")
