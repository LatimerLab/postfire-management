setwd("C:/Users/DYoung/Documents/UC Davis/Research Projects/Post-fire management/postfire-management")

library(tidyverse)
library(lme4)

## load data
plots = read.csv("data/field-processed/compiled-processed/plots_w_gis_data.csv",stringsAsFactors = FALSE)
shrubs = read.csv("data/field-processed/compiled-processed/shrubs.csv",stringsAsFactors = FALSE)
seed_trees = read.csv("data/field-processed/compiled-processed/seed_trees.csv",stringsAsFactors = FALSE)
cwd = read.csv("data/field-processed/compiled-processed/cwd.csv",stringsAsFactors = FALSE)
subsample_threshold = read.csv("data/field-processed/compiled-processed/subsample_threshold.csv",stringsAsFactors = FALSE)
seedlings_plot = read.csv("data/field-processed/compiled-processed/seedlings_plot.csv",stringsAsFactors = FALSE)
seedlings_transect = read.csv("data/field-processed/compiled-processed/seedlings_transect.csv",stringsAsFactors = FALSE)
seedlings_dead = read.csv("data/field-processed/compiled-processed/seedlings_dead.csv",stringsAsFactors = FALSE)
prefire_trees = read.csv("data/field-processed/compiled-processed/prefire_trees.csv",stringsAsFactors = FALSE)




#### Analysis of paired plots ####

plots = plots %>%
  filter(Type %in% c("treatment","control"))

# capture species more generally
seedlings_plot <- seedlings_plot %>%
  mutate(Species = recode(Species,PIPO = "PIPJ",PIJE = "PIPJ")) %>%
  mutate(Pine = Species %in% c("PIPJ","PILA"),
         ShadeTol = Species %in% c("ABCO","ABMA","CADE"))

# summarize trees by plot
seedl_summ <- seedlings_plot %>%
  group_by(PlotID) %>%
  summarize(pine_density = sum(DensitySlope * Pine),
            shadetol_density = sum(DensitySlope * ShadeTol),
            tree_density = sum(DensitySlope),
            tot_ht = sum(TotHeight * DensitySlope),
            mean_ht = mean(TotHeight * DensitySlope),
            pine_density_over = sum(DensitySlope * Pine * (OMU=="O")),
            tree_density_over = sum(DensitySlope * (OMU=="O")))

# combine with plot data
d <- left_join(plots,seedl_summ,by="PlotID")

d <- d %>%
  mutate_at(vars(pine_density,tree_density,tot_ht,pine_density_over,tree_density_over), funs(ifelse(is.na(.),0,.)))

## make it long form so we can plot them as panels

d_long <- d %>%
  gather(key="metric",value="value",c("pine_density","shadetol_density","tree_density","tot_ht","mean_ht","pine_density_over","tree_density_over"))


## plot distribution of response vars by fire and treated status
ggplot(d_long,aes(x=Fire,y=value,color=Type)) +
  geom_point(position=position_jitterdodge(dodge.width=0.5,jitter.width=0.15),size=3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(12) +
  facet_wrap(~metric,scales="free")



## plot relationship between environmental vars and response vars
d_plotting = d_long %>%
  filter(metric %in% c("pine_density"),
         !(Fire %in% "Ctnwd"))

ggplot(d_plotting,aes(x=rad_march,y=value,color=Type)) +
  geom_point() +
  theme_bw(16) +
  facet_wrap(~Fire,scales="free") +
  geom_smooth(method="lm")


## fit pressence/absence model
response_var = "value"
pred_vars_num = c("elev","rad_march")
pred_vars_chr = c("Type","Fire")

focal_vars = c(response_var,pred_vars)

d_mod = d_long %>%
  filter(metric %in% "pine_density",
         !(Fire %in% "Ctnwd")) %>%
  select_(.dots=focal_vars) %>%
  mutate_at(pred_vars_num,funs((.-mean(.))/sd(.))) %>%
  mutate(pine_presence = value > 0)
  
m = glmer(pine_presence ~ elev + rad_march + Type + (1+elev|Fire), data=d_mod,family="binomial") # Type is whether treated
summary(m)






  
