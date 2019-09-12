library(tidyverse)
library(lme4)
library(gridExtra)

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
         shadeTol = Species %in% c("ABCO","ABMA","CADE"), # creates T/F column for Firs and incense cedar
         oak = Species %in% c("QUKE", "QUCH"),
         conif = Species %in% c("PIPO", "PIJE", "PIPJ", "PILA", "ABCO", "ABMA", "CADE", "FIR", "PSME", "JUOC")) 

# summarize trees by plot
seedl_dhm <- seedlings_plot %>%
  group_by(PlotID) %>%
  summarize(dens.all = sum(DensitySlope),
            dens.yelPine = sum(DensitySlope*yelPine),
            dens.pine = sum(DensitySlope*pine),
            dens.shadeTol = sum(DensitySlope),
            dens.oak = sum(DensitySlope*oak),
            dens.conif = sum(DensitySlope*conif),
            ave.ht.all = mean(TotHeight * DensitySlope),
            ave.ht.yelPine = mean(TotHeight * DensitySlope*yelPine),
            ave.ht.pine = mean(TotHeight * DensitySlope*pine),
            ave.ht.shadeTol = mean(TotHeight * DensitySlope),
            ave.ht.oak = mean(TotHeight * DensitySlope*oak),
            ave.ht.conif = mean(TotHeight * DensitySlope*conif),
            tot.ht.all = sum(TotHeight * DensitySlope),
            tot.ht.yelPine = sum(TotHeight * DensitySlope*yelPine),
            tot.ht.pine = sum(TotHeight * DensitySlope*pine),
            tot.ht.shadeTol = sum(TotHeight * DensitySlope),
            tot.ht.oak = sum(TotHeight * DensitySlope*oak),
            tot.ht.conif = sum(TotHeight * DensitySlope*conif),
            ave.mass.all = mean(TotHeight^2 * DensitySlope),
            ave.mass.yelPine = mean(TotHeight^2 * DensitySlope*yelPine),
            ave.mass.pine = mean(TotHeight^2 * DensitySlope*pine),
            ave.mass.shadeTol = mean(TotHeight^2 * DensitySlope),
            ave.mass.oak = mean(TotHeight^2 * DensitySlope*oak),
            ave.mass.conif = mean(TotHeight^2 * DensitySlope*conif),
            tot.mass.all = sum(TotHeight^2 * DensitySlope),
            tot.mass.yelPine = sum(TotHeight^2 * DensitySlope*yelPine),
            tot.mass.pine = sum(TotHeight^2 * DensitySlope*pine),
            tot.mass.shadeTol = sum(TotHeight^2 * DensitySlope),
            tot.mass.oak = sum(TotHeight^2 * DensitySlope*oak),
            tot.mass.conif = sum(TotHeight^2 * DensitySlope*conif))


# combine with plot data
plot_dhm <- left_join(plots, seedl_dhm, by="PlotID")  # join densities to plot data

## make it long form so we can plot them as panels

plot_dhm_long <- plot_dhm %>%
  gather(key="metric",value="value", colnames(seedl_dhm) %>% .[.!= "PlotID"])


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

## specifically plot density of pines that are above shrubs
d_foc = d_long %>%
  filter(metric == "pine_density_over") %>%
  mutate(Type = recode(Type,control="unplanted",treatment="planted"),
         Type = factor(Type,levels=c("unplanted","planted")),
         value = ifelse(value == 0,value,value + runif(n=length(value),-15,15)) )

ggplot(d_foc,aes(x=interaction(Type, Fire),y=value/2.47,color=Type)) +
  #geom_point(position=position_jitterdodge(dodge.width=0.5,jitter.width=0.25),size=4) +
  geom_violin(size = 1) +
  geom_dotplot(binaxis='y', method = "histodot", stackdir='center',binwidth = 5, dotsize=.7,  aes(x = interaction(Type, Fire), fill = Type)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  lims(y=c(0,250)) +
  labs(x="Fire",y="Seedlings / acre") +
  scale_color_brewer(palette="Set1")



## plot relationship between environmental vars and response vars
d_plotting = d_long %>%
  filter(metric %in% c("pine_density"))#,
# !(Fire %in% "Ctnwd"))

