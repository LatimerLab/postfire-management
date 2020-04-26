library(lme4)
library(gridExtra)
library(qpcR)
library(tidyverse)


#####  load data -----------------------------------------------------------------------------------

plots = read.csv("data/field-processed/compiled-processed/plots_w_gis_data_newTWI.csv",stringsAsFactors = FALSE)
subsample_threshold = read.csv("data/field-processed/compiled-processed/subsample_threshold.csv",stringsAsFactors = FALSE)
seedlings_plot = read.csv("data/field-processed/compiled-processed/seedlings_plot.csv",stringsAsFactors = FALSE)
seedlings_transect = read.csv("data/field-processed/compiled-processed/seedlings_transect.csv",stringsAsFactors = FALSE)

seedlings_plot <- seedlings_plot %>%
  mutate(Species = recode(Species, "ACMA" = "ABMA", "AMBA" = "ABMA"))
seelings_plot <- seedlings_plot %>%
  mutate(CompCover = ifelse(PlotID == "B1025C" & Distance == 10, 40, CompCover)) %>%
  mutate(CompCover = ifelse(PlotID == "E0069T", 100, CompCover)) %>%
  mutate(CompCover = ifelse(PlotID == "A2030T" & Distance == 1.66, 90, CompCover)) %>%
  mutate(CompHeight = ifelse(PlotID == "A2030T" & Distance == 1.66, 130, CompHeight)) %>%
  mutate(CompCover = ifelse(PlotID == "B1023T" & Distance == 9.8, 100, CompCover)) %>%
  mutate(CompHeight = ifelse(PlotID == "B1023T" & Distance == 9.8, 300, CompHeight)) %>%
  mutate(CompCover = ifelse(PlotID == "B1024C" & Distance == 5.59, 100, CompCover)) %>%
  mutate(CompCover = ifelse(PlotID == "B1024C" & Distance == 11.20, 100, CompCover)) %>%
  mutate(CompCover = ifelse(PlotID == "B1024C" & Distance == 5.95, 100, CompCover)) %>%
  mutate(CompCover = ifelse(PlotID == "B1024C" & Distance == 4.46, 100, CompCover))
  
#D1000C should be dropped
#B1015C should be 40
#E0069T change all to 100
#A2030T should be 
#B1023T switch comp cover and height
#B1024C change all to 100

plots <- plots %>%
  mutate(fsplanted = ifelse(!is.na(facts.planting.first.year), "planted", "unplanted")) %>% # create a variable for whether were planted or no
  mutate(PairID = str_sub(PlotID, 1,5)) %>% # make a pair ID
  mutate(planted_PIPO = ifelse(is.na(planted_PIPO) & Type == "treatment", "yes", planted_PIPO)) %>% # Remove the NAs from the planted list
  mutate(planted_PIJE = ifelse(is.na(planted_PIJE) & Type == "treatment" & Fire == "Ctnwd", "yes", planted_PIJE)) %>% # Remove the NAs from the planted list
  mutate(plantingList = paste(ifelse(planted_ABCO == "no", NA,"ABCO"), #create a string with the planted species
                              ifelse(planted_CADE27 == "no", NA,"CADE"),
                              ifelse(planted_PIJE == "no", NA, "PIJE_PIPJ"),
                              ifelse(planted_PIPO == "no", NA, "PIPO_PIPJ"),
                              ifelse(planted_PILA == "no", NA, "PILA"),
                              ifelse(planted_PSME == "no", NA, "PSME"),
                              ifelse(planted_ABMA == "no", NA, "ABMA"),
                              ifelse(planted_SEGI2 == "no", NA, "SEGI"),
                              sep = "_"))


plantedList <- plots %>% # assign planting list to nonplanted plots
  filter(fsplanted == "planted") %>% 
  select(PairID, plantingList)
plots <- plots %>%
  select(-plantingList) %>%
  left_join(plantedList, by = "PairID") %>%
  select(-facts.planting.first.year) %>%
  left_join(plots %>% select(PairID, facts.planting.first.year) %>% filter(!is.na(facts.planting.first.year)), by ="PairID") %>%
  select(-facts.released) %>%
  left_join(plots %>% select(PairID, facts.released) %>% filter(!is.na(facts.released)), by ="PairID")

  
    
##### make summaries of each variable --------------------------------------------------------------

# group species
seedlings_plot <- seedlings_plot %>%
  filter(DronePlotTree == FALSE) %>%
  left_join(plots %>% select(PlotID, plantingList), by = "PlotID") %>% # adds planting list to each seedling
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
  #mutate(fsplanted = ifelse(!is.na(facts.planting.first.year), "planted", "unplanted")) %>% # create a variable for whether were planted or not
  mutate(PairID = ifelse(Type == "internal" & fire_code == "E", "Eint", PlotID_notype)) %>% #creates new PlotID where all the intenal plots at one site are within one pair
  mutate(PairID = ifelse(Type == "internal" & fire_code == "A", "Aint", PlotID_notype)) %>%         
  left_join(seedl_dhm, by="PlotID") 


plot_dhm[c("dens.all", "dens.yelPine", "dens.pine", "dens.shadeTol", "dens.conif", "dens.planted", "ave.ht.all",
    "ave.ht.yelPine", "ave.ht.pine",  "ave.ht.shadeTol", "ave.ht.conif", "ave.ht.planted", "tot.ht.all",
    "tot.ht.yelPine", "tot.ht.pine", "tot.ht.shadeTol", "tot.ht.conif", "tot.ht.planted", "ave.mass.all",
    "ave.mass.yelPine", "ave.mass.pine", "ave.mass.shadeTol", "ave.mass.conif", "ave.mass.planted", "tot.mass.all",
    "tot.mass.yelPine", "tot.mass.pine", "tot.mass.shadeTol", "tot.mass.conif", "tot.mass.planted"
    )][is.na(plot_dhm[c("dens.all", "dens.yelPine", "dens.pine", "dens.shadeTol", "dens.conif", "dens.planted", "ave.ht.all",
                 "ave.ht.yelPine", "ave.ht.pine",  "ave.ht.shadeTol", "ave.ht.conif", "ave.ht.planted", "tot.ht.all",
                 "tot.ht.yelPine", "tot.ht.pine", "tot.ht.shadeTol", "tot.ht.conif", "tot.ht.planted", "ave.mass.all",
                 "ave.mass.yelPine", "ave.mass.pine", "ave.mass.shadeTol", "ave.mass.conif", "ave.mass.planted", "tot.mass.all",
                 "tot.mass.yelPine", "tot.mass.pine", "tot.mass.shadeTol", "tot.mass.conif", "tot.mass.planted")])] <- 0 #replace NAs with 0s for the plots that were not in the seedling_plots df.

plot_dhm <- plot_dhm %>% 
  mutate(ln.dens.planted = log(dens.planted+24.99)) %>%
  filter(Type != "internal") %>% 
  mutate(ln.dens.conif = log(dens.conif+24.99)) %>%
  mutate(fsplanted = as.factor(fsplanted)) %>%
  mutate(facts.released = as.factor(facts.released)) %>%
  mutate(GrassHt = ifelse(is.na(GrassHt), 0, GrassHt)) %>%
  mutate(ShrubHt = ifelse(is.na(ShrubHt), 0, ShrubHt)) %>%
  mutate(ForbHt = ifelse(is.na(ForbHt), 0, ForbHt)) %>%
  mutate(SeedWallConifer = ifelse(is.na(SeedWallConifer), 500, SeedWallConifer)) %>%
  mutate(log10SeedWallConifer = log10(SeedWallConifer)) %>%
  mutate(totalCov = Shrubs + Grasses + Forbs) %>%
  mutate(totalCovxHt = (Shrubs*ShrubHt + Grasses*GrassHt + Forbs*ForbHt)) %>%
  mutate(LitDuff = LitterDepth + DuffDepth) %>%
  mutate(ShrubHt2 = ifelse(ShrubHt == 0, ShrubErectHt, ShrubHt))


## make it long form so we can plot them as panels

plot_dhm_long <- plot_dhm %>%
  gather(key="metric",value="value", colnames(seedl_dhm) %>% .[.!= "PlotID"]) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>% #assign 0 to na values
  mutate(met.type = str_replace(metric, "\\.[^.]+$" , ""))

#save(plot_dhm_long, plot_dhm, file= "output/plotSeedlingData.RData")



##### Check to see if shrub cover around each tree is similar to plot level ------------

#seedl_ave.shr <- seedlings_plot %>%
#  filter(CompType %in% c("S", "SC", "SH", "C", "H", "CH", "HC", "HS", "CS")) %>%
#  group_by(PlotID) %>%
#  summarize(ave.sur.shrub = mean(CompCover))

#plots.ave.shr <- plots %>% 
#  left_join(seedl_ave.shr, by = "PlotID")

#lmtest <- lm(Shrubs ~ ave.sur.shrub, data = plots.ave.shr)
#summary(lmtest)
#plot(allEffects(lmtest))
#cor(plots.ave.shr$Shrubs, plots.ave.shr$ave.sur.shrub, use = "complete.obs", method = "pearson")





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

seedl_ave.shr2 <- seedlings_plot %>%
  filter(CompType %in% c("S")) %>%
  filter(CompCover < 110) %>%
  left_join(plots, by = "PlotID")


lmtest <- lmer(Shrubs ~ CompCover + (1|PlotID), data = seedl_ave.shr2 %>% filter(CompCover <= 110))
summary(lmtest)
plot(allEffects(lmtest))
cor(plots.ave.shr$Shrubs, plots.ave.shr$ave.sur.shrub, use = "complete.obs", method = "pearson")

ggplot(seedl_ave.shr2, aes(x = CompCover, y = Shrubs)) +
  geom_point(position = "jitter")
