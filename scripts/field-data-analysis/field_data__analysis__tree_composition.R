library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(sjPlot)
library(MuMIn)
library(Hmisc)
library(car)
#library(BiodiversityR)

# Load data 
load("./output/plotSeedlingData.RData") 

# Calculate mean seedling densities for each fire 
plot_dhm %>% group_by(Fire) %>% 
  summarise(dens.all.mean = mean(dens.all))

# Make proportion pine data set for analysis 
plot_dhm_pine <- plot_dhm
plot_dhm_pine$p.pine <- cbind(round(plot_dhm$dens.pine/24.94098, 0), round(plot_dhm$dens.conif/24.94098, 0)-round(plot_dhm$dens.pine/24.94098, 0))   
plot_dhm_pine <- plot_dhm_pine %>% 
  filter(dens.conif != 0) %>%
  mutate(ForShr = Shrubs + Forbs)

nrow(plot_dhm_pine) # 119 rows -- this is substantially smaller, so may need to refit a simpler model
nrow(plot_dhm) # 182 rows 

vars_to_test_continuous <- c("normal_annual_precip", "rad_summer", "Forbs", "Shrubs", "Grasses", "ShrubHt", 
                  "LiveOverstory", "log10SeedWallConifer", "twi", "tpi2000", "elev", "CWD_sound")
vars_to_test_factor <- c("fsplanted", "facts.planting.first.year")

# scale continuous variables 
plot_dhm_pine_std <- stdize(plot_dhm_pine[ , vars_to_test_continuous], prefix = FALSE)

# add factor variables
plot_dhm_pine_std <- cbind(plot_dhm_pine_std, plot_dhm_pine[, vars_to_test_factor])

# add the response 
plot_dhm_pine_std <- cbind(plot_dhm_pine_std, plot_dhm_pine[, "p.pine"])
names(plot_dhm_pine_std)[15] <- "seedling_count"
names(plot_dhm_pine_std)[16] <- "pine_count"

# Add the grouping variables 
plot_dhm_pine_std <- cbind(plot_dhm_pine_std, plot_dhm_pine[, c("Fire", "PairID")])

# Reorder planted variable so unplanted is the base
plot_dhm_pine_std <- mutate(plot_dhm_pine_std, fsplanted = relevel(fsplanted, "unplanted"))
#plot_dhm_pine_std$fsplanted <- case_match(plot_dhm_pine_std$fsplanted, "planted" ~ 1, "unplanted" ~ 0)

# remove NAs so model comparison is on par for all variable combinations 
apply(plot_dhm_pine_std, 2, f <- function(x) {return(sum(is.na(x)))}) # only 1 row has missing data
plot_dhm_pine_std <- plot_dhm_pine_std[complete.cases(plot_dhm_pine_std),]

# Full model without interactions 
fm_formula <- as.formula("cbind(seedling_count, pine_count) ~ normal_annual_precip + rad_summer + Forbs + Shrubs + Grasses + ShrubHt + LiveOverstory + 
  log10SeedWallConifer + twi + tpi2000 + elev + CWD_sound + facts.planting.first.year + fsplanted + (1|Fire) + (1|Fire:PairID)")

fm1 <- glmer(fm_formula, data = plot_dhm_pine_std, family = "binomial", na.action = na.fail) # note won't converge 

# Try model selection using MuMin from a base model that has to include "planted" and "timing" 
fm1_dredge <- dredge(fm1, rank = "AICc", fixed = ~ fsplanted + facts.planting.first.year + (1|Fire) + (1|Fire:PairID), m.lim = c(0, 3), trace = TRUE, evaluate = TRUE)
get.models(fm1_dredge, subset = delta < 4)

# Top variable is LiveOverstory -- add to fixed and proceed 
fm1_dredge <- dredge(fm1,rank = "AICc", fixed = ~ LiveOverstory + fsplanted + facts.planting.first.year + (1|Fire) + (1|Fire:PairID), m.lim = c(0, 4), trace = TRUE, evaluate = TRUE)
get.models(fm1_dredge, subset = delta < 4)

# Top variable is Shrubs -- add to fixed and proceed 
fm1_dredge <- dredge(fm1, rank = "AICc", fixed = ~ Shrubs + LiveOverstory + fsplanted + facts.planting.first.year + (1|Fire) + (1|Fire:PairID), m.lim = c(0, 5), trace = TRUE, evaluate = TRUE)
get.models(fm1_dredge, subset = delta < 4)

# Top variable is Shrub Height -- add to fixed and proceed 
fm1_dredge <- dredge(fm1, rank = "AICc", fixed = ~ ShrubHt + Shrubs + LiveOverstory + fsplanted + facts.planting.first.year + (1|Fire) + (1|Fire:PairID), m.lim = c(0, 6), trace = TRUE, evaluate = TRUE)
 get.models(fm1_dredge, subset = delta < 4)
 
 # Top variable is normal precip -- add to fixed and proceed 
 fm1_dredge <- dredge(fm1, rank = "AICc", fixed = ~ normal_annual_precip + ShrubHt + Shrubs + LiveOverstory + fsplanted + facts.planting.first.year + (1|Fire) + (1|Fire:PairID), m.lim = c(0, 7), trace = TRUE, evaluate = TRUE)
 get.models(fm1_dredge, subset = delta < 4)
 
 ### STOP: model won't converge. Go back to previous model and use that as the full set of fixed effects 
 
fm1 <- glmer(cbind(seedling_count, pine_count) ~ facts.planting.first.year +  
                fsplanted + LiveOverstory + ShrubHt +  
                Shrubs + (1 | Fire) + (1 | Fire:PairID), data = plot_dhm_pine_std, 
                family = "binomial")
summary(fm1)

#### Next test interactions between planted and other fixed effects, testing one interaction at a time.  

# facts.planting.first.year
fm2 <- glmer(cbind(seedling_count, pine_count) ~ (1|Fire) + (1|Fire:PairID) + Shrubs + LiveOverstory + 
               ShrubHt + fsplanted + facts.planting.first.year + 
               fsplanted:facts.planting.first.year,
                data = plot_dhm_pine_std, 
                family = "binomial")
AIC(fm1, fm2) # interaction added
summary(fm2)

# Shrubs
fm3 <- glmer(cbind(seedling_count, pine_count) ~ (1|Fire) + (1|Fire:PairID) + Shrubs + LiveOverstory + 
               ShrubHt + fsplanted + facts.planting.first.year + 
               fsplanted:facts.planting.first.year + fsplanted:Shrubs,
             data = plot_dhm_pine_std, 
             family = "binomial")
AICc(fm3 , fm2) # interaction rejected 

# LiveOverstory
fm4 <- glmer(cbind(seedling_count, pine_count) ~ (1|Fire) + (1|Fire:PairID) + Shrubs + LiveOverstory + 
               ShrubHt + fsplanted + facts.planting.first.year + 
               fsplanted:facts.planting.first.year + fsplanted:LiveOverstory,
             data = plot_dhm_pine_std, 
             family = "binomial")
AICc(fm4 , fm2) # interaction rejected 

# ShrubHt
fm5 <- glmer(cbind(seedling_count, pine_count) ~ (1|Fire) + (1|Fire:PairID) + Shrubs + LiveOverstory + 
               ShrubHt + fsplanted + facts.planting.first.year + 
               fsplanted:facts.planting.first.year + fsplanted:ShrubHt,
             data = plot_dhm_pine_std, 
             family = "binomial")
AICc(fm5 , fm2) # interaction rejected 


# test 3-way interaction with shrubs
fm6 <- glmer(cbind(seedling_count, pine_count) ~ (1|Fire) + (1|Fire:PairID) + Shrubs + LiveOverstory + 
               fsplanted + facts.planting.first.year + 
               fsplanted:facts.planting.first.year + fsplanted:facts.planting.first.year:Shrubs,
             data = plot_dhm_pine_std, 
             family = "binomial")
AICc(fm6 , fm2) # interaction rejected -- also convergence failure 

# FINAL MODEL 
pine.comp <- fm2

summary(pine.comp)
plot(allEffects(pine.comp))

# Check correlation of the main effect variables
cor(plot_dhm_pine_std[, c("Shrubs", "ShrubHt", "LiveOverstory", "facts.planting.first.year")])

##### Next step will be to modify plots code from field_data_plots_planting_time to handle the proportion pine model. 

# Save the model object for use with plots. 
save(pine.comp, file = "./output/pine_composition_model_object.Rdata")


########################
######## Old code ###### 
########################

fm1 <- glmer(p.pine ~                      
               
               (1|Fire) +
               (1|Fire:PairID),
             family = binomial,
             data = plot_dhm_pine)   




pine.comp <- glmer(p.pine ~ 
### climate
                     scale(tmean) + #interact with planting in end
                     #scale(tmax) +
                     scale(normal_annual_precip) + 
                     #scale(tmin) +
                     scale(rad_summer) +
### competition
                     scale(Forbs) +
                    scale(Shrubs) +
                     scale(Grasses) +
                     scale(ShrubHt)  +
                     
  
                     scale(LiveOverstory) +

### dispersal
                     scale(log10SeedWallConifer) +
###Topography
                     #scale(tpi500) +
                     scale(twi) +
                     scale(tpi2000) + 
                      scale(elev) +   
                     #scale(LitDuff)*fsplanted +
                     scale(CWD_sound) +
###planting  
                      facts.planting.first.year +
                      fsplanted +
  
                     (1|Fire) +
                     (1|Fire:PairID),
                   family = binomial,
                   data = plot_dhm_pine)               
AIC(pine.comp)
summary(pine.comp)
plot(allEffects(pine.comp))

comp <- spp_matrix %>%
  mutate(PIPJ = PIPJ + PIPO + PIJE,
         ABCO = ABCO + FIR) %>%
  select(-PIPO, -PIJE, -FIR, -JUOC, -QUCH, -QUKE) %>%
  filter(0 != ABCO + ABMA + CADE + PILA + PIPJ + PSME) 
spp_comp <- comp %>%
  select(-PlotID, -Fire, - fsplanted)
id_comp <-  comp %>% select(PlotID, Fire, fsplanted)
spp_comp <- as.data.frame(spp_comp)
id_comp <- as.data.frame(id_comp)
id_comp$Fire <- as.factor(id_comp$Fire)

cap_1 <- CAPdiscrim(spp_comp ~ Fire, data = id_comp, dist = "bray", axes = 2, m = 0, add = FALSE)

nmds_1 <- metaMDS(spp_comp, distance  = "bray")

cap_plot <- cbind(cap_1$PCoA, id_comp)
cap_plot <- cap_plot %>% rename("CAP1" = 1, "CAP2" = 2)

nmds_plot <- cbind(nmds_1$points, id_comp)

ggplot(cap_plot, aes(x = CAP1, y = CAP2, shape = fsplanted)) +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 21)) +
  #facet_wrap(~Fire)

ggplot(cap_plot, aes(x = CAP1, y = CAP2, color = Fire, shape = fsplanted)) +
  geom_point(size = 3) +
  scale_shape_manual(values=c(16, 21))
#facet_wrap(~Fire)

ggplot(nmds_plot, aes(x = MDS1, y = MDS2)) +
  geom_point(size = 3, aes(x = MDS1, y = MDS2, color = Fire, shape = fsplanted)) +
  geom_text(data = as.data.frame(nmds_1$species) %>% 
              mutate(spp = row.names(as.data.frame(nmds_1$species))), 
            aes(x = MDS1, y = MDS2, label = spp)) +
  stat_ellipse(aes(x = MDS1, y = MDS2, linetype = fsplanted), type = "t") +
  scale_shape_manual(values=c(16, 21)) 

nmdsFire <- ggplot(nmds_plot %>% 
                     mutate(Fire = as.character(Fire)) %>%
                     mutate(Fire = ifelse(Fire == "AmRiv", "American River", 
                                          ifelse(Fire == "Ctnwd", "Cottonwood", 
                                          ifelse(Fire == "MoonAnt", "Moonlight/Antelope", Fire)))), 
                            aes(x = MDS1, y = MDS2)) +
  geom_point(size = 1.8, aes(x = MDS1, y = MDS2, shape = fsplanted)) +
  #geom_point(data = as.data.frame(nmds_1$species), aes(x = MDS1, y = MDS2)) +
  scale_shape_manual(values=c(16, 21)) +
  stat_ellipse(aes(x = MDS1, y = MDS2, linetype = fsplanted), type = "norm", level = .8)+
  geom_label(data = as.data.frame(nmds_1$species) %>% mutate(spp = row.names(as.data.frame(nmds_1$species))), 
             aes(x = MDS1, y = MDS2, label = spp), size =2.5, label.padding = unit(0.12, "lines")) +
  facet_wrap(~Fire) +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "transparent", color = "transparent"), 
        #strip.text.y = element_text(margin = margin(0,0,0,1)),
        text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top", 
        panel.spacing.x = unit(.11, "lines"),
        panel.spacing.y = unit(.6, "lines"))
ggsave(nmdsFire, file="figures/manuscript/ndmsFire.pdf", width=6.5, height= 5.5)



splot1 <- ordiplot(cap_1, type="none")
ordisymbol(plot1, id_comp, "Fire", legend=TRUE)


