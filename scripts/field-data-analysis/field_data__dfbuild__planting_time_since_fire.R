library(lme4)
library(gridExtra)
library(qpcR)
library(tidyverse)

#####  Buildup of dataframes for the folloing analyses ---------------------------------------------

#  dens.all ~ Year since fire 
#  dens.pine  ~ Year since fire
#  dens.PIPO  ~ Year since fire
#  dens.PIJE ~ Year since fire
#  ave.ht.all ~ Year since fire 
#  ave.ht.pine  ~ Year since fire
#  ave.ht.PIPO  ~ Year since fire
#  ave.ht.PIJE ~ Year since fire
#  mass.all ~ Year since fire 
#  mass.pine  ~ Year since fire
#  mass.PIPO  ~ Year since fire
#  mass.PIJE ~ Year since fire


#####  load data -----------------------------------------------------------------------------------

plots = read.csv("data/field-processed/compiled-processed/plots_w_gis_data.csv",stringsAsFactors = FALSE)
subsample_threshold = read.csv("data/field-processed/compiled-processed/subsample_threshold.csv",stringsAsFactors = FALSE)
seedlings_plot = read.csv("data/field-processed/compiled-processed/seedlings_plot.csv",stringsAsFactors = FALSE)
seedlings_transect = read.csv("data/field-processed/compiled-processed/seedlings_transect.csv",stringsAsFactors = FALSE)

seedlings_plot <- seedlings_plot %>%
  mutate(Species = recode(Species, "ACMA" = "ABMA", "AMBA" = "ABMA"))


##### make summaries of each variable --------------------------------------------------------------


# group species
seedlings_plot <- seedlings_plot %>%
  filter(DronePlotTree == FALSE) %>%
  mutate(yelPine = Species %in% c("PIPO", "PIJE", "PIPJ"), # combine yellowpines.
         pine = Species %in% c("PIPO", "PIJE", "PILA", "PIPJ"), # creates T/F column for Pine
         shadeTol = Species %in% c("ABCO","ABMA","CADE", "FIR"), # creates T/F column for Firs and incense cedar
         oak = Species %in% c("QUKE", "QUCH"),
         conif = Species %in% c("PIPO", "PIJE", "PIPJ", "PILA", "ABCO", "ABMA", "CADE", "FIR", "PSME", "JUOC")) 

# summarize trees by plot
seedl_dhm <- seedlings_plot %>%
  group_by(PlotID) %>%
  summarize(dens.all = sum(DensitySlope),
            dens.yelPine = sum(DensitySlope*yelPine),
            dens.pine = sum(DensitySlope*pine),
            dens.shadeTol = sum(DensitySlope*shadeTol),
            dens.oak = sum(DensitySlope*oak),
            dens.conif = sum(DensitySlope*conif),
            
            ave.ht.all = mean(TotHeight * DensitySlope),
            ave.ht.yelPine = mean(TotHeight * DensitySlope*yelPine),
            ave.ht.pine = mean(TotHeight * DensitySlope*pine),
            ave.ht.shadeTol = mean(TotHeight * DensitySlope*shadeTol),
            ave.ht.oak = mean(TotHeight * DensitySlope*oak),
            ave.ht.conif = mean(TotHeight * DensitySlope*conif),
            
            tot.ht.all = sum(TotHeight * DensitySlope),
            tot.ht.yelPine = sum(TotHeight * DensitySlope*yelPine),
            tot.ht.pine = sum(TotHeight * DensitySlope*pine),
            tot.ht.shadeTol = sum(TotHeight * DensitySlope*shadeTol),
            tot.ht.oak = sum(TotHeight * DensitySlope*oak),
            tot.ht.conif = sum(TotHeight * DensitySlope*conif),
            
            ave.mass.all = mean(TotHeight^2 * DensitySlope),
            ave.mass.yelPine = mean(TotHeight^2 * DensitySlope*yelPine),
            ave.mass.pine = mean(TotHeight^2 * DensitySlope*pine),
            ave.mass.shadeTol = mean(TotHeight^2 * DensitySlope*shadeTol),
            ave.mass.oak = mean(TotHeight^2 * DensitySlope*oak),
            ave.mass.conif = mean(TotHeight^2 * DensitySlope*conif),
            
            tot.mass.all = sum(TotHeight^2 * DensitySlope),
            tot.mass.yelPine = sum(TotHeight^2 * DensitySlope*yelPine),
            tot.mass.pine = sum(TotHeight^2 * DensitySlope*pine),
            tot.mass.shadeTol = sum(TotHeight^2 * DensitySlope*shadeTol),
            tot.mass.oak = sum(TotHeight^2 * DensitySlope*oak),
            tot.mass.conif = sum(TotHeight^2 * DensitySlope*conif))
 


# combine with plot data
plot_dhm <- plots %>%
  mutate(fsplanted = ifelse(!is.na(facts.planting.first.year), "planted", "unplanted")) %>% # create a variable for whether were planted or not
  mutate(PairID = ifelse(Type == "internal" & fire_code == "E", "Eint", PlotID_notype)) %>% #creates new PlotID where all the intenal plots at one site are within one pair
  mutate(PairID = ifelse(Type == "internal" & fire_code == "A", "Aint", PlotID_notype)) %>%         
  left_join(seedl_dhm, by="PlotID")  # join densities to plot data

## make it long form so we can plot them as panels

plot_dhm_long <- plot_dhm %>%
  gather(key="metric",value="value", colnames(seedl_dhm) %>% .[.!= "PlotID"]) %>%
  mutate(value = ifelse(is.na(value), 0, value)) %>% #assign 0 to na values
  mutate(met.type = str_replace(metric, "\\.[^.]+$" , ""))


## plot distribution of response vars by fire and treated status
explore <- ggplot(plot_dhm_long, aes(x=interaction(Type, Fire), y=log(value+1),color=Type)) +
  geom_violin(size = 1) +
  #geom_boxplot(width=0.1) +
  #geom_jitter(width = .02, alpha = .25) +
  geom_dotplot(binaxis='y', method = "histodot", stackdir='center',binwidth = .2, dotsize=.7,  aes(x = interaction(Type, Fire), fill = Type)) +
  #geom_point(position=position_jitterdodge(dodge.width=0.5,jitter.width=0.15),size=3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(12) +
  facet_wrap(~metric,scales="free")
ggsave(explore, file = "C:/Users/Quinn Sorenson/Desktop/lotsOplots.pdf", width = 60, height = 40, limitsize = FALSE)


##### Create function to plot vars and apply to all var combinations -------------------------------------------------

enviroPlots <- function(met, enviro, trans = "none") {
  assign(paste0(enviro, ".plot.", if (trans == "log") { paste0("log.", met) } else { met }), ggplot(plot_dhm_long %>% filter(met.type == met), 
                                                                                           aes(x = get(enviro), y = if (trans == "log") { log(value+1) } 
                                                                                               else { value }, color = fsplanted)) +
  geom_smooth(method = "lm", size = 1.5) +
  geom_point(size = 2) +
  ylab(if (trans == "log") { paste0("log.", met) } else { met }) +
  scale_color_manual(values = c("#D5C332", "#048BA8")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(12) +
  facet_wrap(~Fire*metric, scales="free"))
ggsave(get(paste0(enviro, ".plot.", if (trans == "log") { paste0("log.", met) } else { met })), 
       file = paste0("figures/exploratory/", enviro, ".plot.", if (trans == "log") { paste0("log.", met) } else { met }, ".pdf"), 
       width = 18, height = 11, limitsize = FALSE)
} # Function plotting data by response var = met, environmental variable, and log+1 transformtion or not.


metVars <- plot_dhm_long %>% select(met.type) %>% unique %>% as.matrix() %>% as.vector() #extracting response vars from dataset
enviroVars <- c("elev", "rad_march", "slope_dem") #environmental vars
transVars <- c("log", "none") # with and with log transformation
allVars <- as_tibble(qpcR:::cbind.na(metVars,enviroVars,transVars))
allVars <- allVars %>% tidyr::expand(metVars, enviroVars, transVars) %>% drop_na() # create dataframe with all combination of variables

mapply(enviroPlots, allVars$metVars, allVars$enviroVars, allVars$transVars) #apply all combinations to graphing function... damn, thats a lot of graphs...



########################################################################################
#################################   TRASH   ############################################
########################################################################################


plot_dhm %>%  filter(Type == "internal", fire_code != "E") #search for plots that are coded internal that arent in Cw site

cor(plots %>% dplyr::select(elev, rad_march, slope_dem),  use = "complete.obs", method = "pearson")
