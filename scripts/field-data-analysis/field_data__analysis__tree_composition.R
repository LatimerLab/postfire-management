library(tidyverse)
library(lme4)
library(lmerTest)
library(effects)
library(sjPlot)
library(MuMIn)
library(Hmisc)
library(car)
library(BiodiversityR)

plot_dhm$p.pine <- cbind(round(plot_dhm$dens.pine/24.94098, 0), round(plot_dhm$dens.conif/24.94098, 0)-round(plot_dhm$dens.pine/24.94098, 0))
                         
pine.comp <- glmer(p.pine ~ 
### climate
                     #scale(tmean) + #interact with planting in end
                     #scale(tmax) +
                     scale(normal_annual_precip) * scale(tmin) +
                     #scale(rad_summer) +
### competition
                     #scale(Forbs)*fsplanted +
  scale(Shrubs) * facts.planting.first.year*fsplanted +
                     #scale(Grasses) +
                     #scale(ShrubHt)  +
                     
  
                     #scale(LiveOverstory) +

### dispersal
                     scale(log10SeedWallConifer) +
###Topography
                     #scale(tpi500) +
                     #scale(twi) +
                     scale(tpi2000) * scale(elev) +   
                     #scale(LitDuff)*fsplanted +
                     #scale(CWD_sound) +
###planting  
                     # *facts.planting.first.year* +
                     # fsplanted +
                     (1|Fire),
                     #(1|Fire:PairID),
                   family = binomial,
                   data = plot_dhm %>% filter(dens.conif != 0) %>% mutate(ForShr = Shrubs + Forbs))
                                  #%>% mutate(facts.planting.first.year = as.factor(facts.planting.first.year)))                          
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


