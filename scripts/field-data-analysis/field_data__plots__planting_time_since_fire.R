library(AICcmodavg)
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)
library(sjPlot)
library(MuMIn)

load(file = "output/modelDataForPlots.RData")
load("output/plotSeedlingData.RData") #load R object: plot_dhm_long


fmt_dcimals <- function(decimals=0){
  function(x) format(x,nsmall = decimals,scientific = FALSE)
}

rotatedAxisElementText = function(angle,position='x',Size,Color){
  angle     = angle[1]; 
  position  = position[1]
  positions = list(x=0,y=90,top=180,right=270)
  if(!position %in% names(positions))
    stop(sprintf("'position' must be one of [%s]",paste(names(positions),collapse=", ")),call.=FALSE)
  if(!is.numeric(angle))
    stop("'angle' must be numeric",call.=FALSE)
  rads  = (angle - positions[[ position ]])*pi/180
  hjust = 0.5*(1 - sin(rads))
  vjust = 0.5*(1 + cos(rads))
  element_text(angle=angle,vjust=vjust,hjust=hjust, size = Size, color = Color)
}


#ln.dens.planted ~ scale(tpi2000)*facts.planting.first.year +
#  scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
#  scale(tmean)*scale(normal_annual_precip) +
#  log10SeedWallConifer + scale(ShrubHt) 

##### year by planted by shrub -----------------------------------------------

obs_pltd.pys <- plot_dhm %>% select(fsplanted, Shrubs, facts.planting.first.year)
obs_pltd.pys$tmin <- mean(plot_dhm$tmin)
obs_pltd.pys$normal_annual_precip <- mean(plot_dhm$normal_annual_precip)
obs_pltd.pys$log10SeedWallConifer <- mean(plot_dhm$log10SeedWallConifer)
obs_pltd.pys$ShrubHt <- mean(plot_dhm$ShrubHt)
obs_pltd.pys$tpi2000 <- mean(plot_dhm$tpi2000)
obs_pltd.pys$LitDuff <- mean(plot_dhm$LitDuff)

obs.pys <- predict(pltd, newdata = obs_pltd.pys, re.form=NA)
obs_resid.pys <- (obs.pys + resid(pltd,re.form=NA)) 
plot_dhm$obs_resid.pys <- exp(obs_resid.pys) -24.99





# average value for other variables in the model
#Shrubs_mean <- mean(plot_dhm$Shrubs)
#ShrubHt_mean <- mean(plot_dhm$ShrubHt)
log10SeedWallConifer_mean = -0.6 # mean(plot_dhm$log10SeedWallConifer)
tmin_mean <- mean(plot_dhm$tmin)
normal_annual_precip_mean <- mean(plot_dhm$normal_annual_precip)
LitDuff_mean <- mean(plot_dhm$LitDuff)
tpi2000_mean <- mean(plot_dhm$tpi2000)

# variables to vary for the plot
Shrubs_levels <- seq(from=0,to=100,length.out=101)
facts.planting.first.year_levels <- c(1,2,3)
fsplanted_levels <- c("planted","unplanted")

predict_pltd.pys <- expand.grid(Shrubs = Shrubs_levels,
                      facts.planting.first.year = facts.planting.first.year_levels,
                      fsplanted = fsplanted_levels,
                      log10SeedWallConifer = log10SeedWallConifer_mean,
                      tmin = tmin_mean,
                      normal_annual_precip = normal_annual_precip_mean,
                      LitDuff = LitDuff_mean,
                      tpi2000 = tpi2000_mean,
                      Fire = "hypothetical",
                      PairID = "hypothetical")





predicted_pltd.pys <- cbind(predict_pltd.pys, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.pys, re.form = NA, level = 0, type="response", se.fit = TRUE)))

year.labs <- c(c("One year", "Two years", "Three years"))
names(year.labs) <- c(1,2,3)


plantedYearShrubs <- ggplot(data = predicted_pltd.pys, aes(y = (exp(fit)-24.99), x = Shrubs, color = fsplanted, fill = fsplanted)) +
  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), alpha = .50, color = NA) +
  geom_smooth(show.legend = FALSE, size = 2, se = F) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.pys, x = Shrubs), show.legend = FALSE) +
  #geom_point(data = plot_dhm, aes(y=dens.planted, x = Shrubs)) +
  scale_color_manual(values = c("#4f5a3a","#807561")) +
  scale_fill_manual(values = c("#98ab8f","#d1cdb6")) +
  ylab("Seedling Density \n(indv/ha)") +
  xlab("Shrub Cover (%)") +
  xlim(20,100) +
  ylim(-10,1200) +
  facet_wrap(~facts.planting.first.year, strip.position = "top", labeller = labeller(facts.planting.first.year = year.labs)) +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "transparent", color = "transparent"), 
        #strip.text.y = element_text(margin = margin(0,0,0,1)),
        text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top", 
        panel.spacing.x = unit(.11, "lines"),
        panel.spacing.y = unit(.6, "lines"))
ggsave(plantedYearShrubs, file="figures/manuscript/plantedYearShrubs.pdf", width=6.5, height=4.5)

  
##### year by planted by shrub -----------------------------------------------

predict_pltd.ty <- plot_dhm %>%
  tidyr::expand(nesting(facts.planting.first.year), tpi2000) 
predict_pltd.ty$tmin <- mean(plot_dhm$tmin)
predict_pltd.ty$normal_annual_precip <- mean(plot_dhm$normal_annual_precip)
predict_pltd.ty$log10SeedWallConifer <- -.6
#predict_pltd.ty$ShrubHt <- mean(plot_dhm$ShrubHt)
predict_pltd.ty$Shrubs <- mean(plot_dhm$Shrubs)
predict_pltd.ty$fsplanted <- "planted"
predict_pltd.ty$fsplanted <- as.factor(predict_pltd.ty$fsplanted)
predict_pltd.ty$LitDuff <- mean(plot_dhm$LitDuff)


predicted_pltd.ty <- cbind(predict_pltd.ty, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.ty, re.form = NA, level = 0, type="response", se.fit = TRUE)))
predicted_pltd.ty$facts.planting.first.year <- as.factor(predicted_pltd.ty$facts.planting.first.year)

tpiYear <-  ggplot(data = predicted_pltd.ty, aes(y = exp(fit)-24.99, 
                                                 x = tpi2000, 
                                                 color = facts.planting.first.year, 
                                                 fill = facts.planting.first.year)) +
  geom_point(data = plot_dhm %>% 
               mutate(facts.planting.first.year = as.factor(facts.planting.first.year)), 
                      aes(y=dens.planted, x = tpi2000, color = facts.planting.first.year, 
                          fill = facts.planting.first.year), show.legend = FALSE) +
  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), 
              alpha = .15, color = NA, show.legend = FALSE) +
  geom_smooth(show.legend = FALSE, size = 2, se = F) +
  scale_color_manual(values = c("#8c8863","#756156", "#48553b")) +
  scale_fill_manual(values = c("#8c8863","#756156", "#48553b")) +
  ylab("Seedling Density \nindv./ha)") +
  xlab("Topographic position index") +
  ylim(-10, 1300) +
  theme( text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top")
ggsave(tpiYear , file="figures/manuscript/tpiYear .pdf", width=3.25, height=3.45)


##### tmean by annual precip -----------------------------------------------

predict_pltd.tp <- as.data.frame(plot_dhm$tmin)
colnames(predict_pltd.tp)[1] <- c("tmin")
predict_pltd.tp$tpi2000 <- mean(plot_dhm$tpi2000)
predict_pltd.tp$facts.planting.first.year <- 2
predict_pltd.tp$log10SeedWallConifer <- mean(plot_dhm$log10SeedWallConifer)
#predict_pltd.tp$ShrubHt <- mean(plot_dhm$ShrubHt)
predict_pltd.tp$Shrubs <- mean(plot_dhm$Shrubs)
predict_pltd.tp$fsplanted <- "planted"
predict_pltd.tp$fsplanted <- as.factor(predict_pltd.tp$fsplanted)
predict_pltd.tp$LitDuff <- mean(plot_dhm$LitDuff)
pre6 <- as.data.frame(rep(600, nrow(predict_pltd.tp)))
pre12 <- as.data.frame(rep(1200, nrow(predict_pltd.tp)))
pre18 <- as.data.frame(rep(1800,  nrow(predict_pltd.tp)))
colnames(pre6)[1] <- c("normal_annual_precip")
colnames(pre12)[1] <- c("normal_annual_precip")
colnames(pre18)[1] <- c("normal_annual_precip")
predict_pltd.tpp <- rbind(cbind(predict_pltd.tp, pre6),
                          cbind(predict_pltd.tp, pre12),
                          cbind(predict_pltd.tp, pre18))

precip.labs <- c(c("600mm", "1200mm", "1800mm"))
names(precip.labs) <- c(600, 1200, 1800)


predicted_pltd.tpp <- cbind(predict_pltd.tpp, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.tpp, re.form = NA, level = 0, type="response", se.fit = TRUE)))
predicted_pltd.tpp$facts.planting.first.year <- as.factor(predicted_pltd.tpp$facts.planting.first.year)

tminPrecip <-  ggplot(data = predicted_pltd.tpp %>% mutate(normal_annual_precip = as.factor(normal_annual_precip)), aes(y = exp(fit)-24.99, x = tmin, color = normal_annual_precip, fill = normal_annual_precip)) +
  geom_ribbon(aes(ymax = ifelse((exp(fit + se.fit)-24.99) < 1600,(exp(fit + se.fit)-24.99), 1600), ymin=(exp(fit - se.fit)-24.99), color = normal_annual_precip, fill = normal_annual_precip), 
                  alpha = .15, color = NA, show.legend = FALSE) +
  geom_smooth(show.legend = FALSE, size = 1.5, se = F, color = "#6D7274") +
  #geom_point(data = plot_dhm %>% 
   #            mutate(normal_annual_precip = as.factor(ifelse(normal_annual_precip < 900, 600, 
    #                                                ifelse(normal_annual_precip > 900 & 
     #                                                        normal_annual_precip < 1500, 1200, 1800)))), 
      #                aes(y=dens.planted, x = tmin, color = normal_annual_precip, fill = normal_annual_precip), show.legend = FALSE) +
  scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Seedling Density \nindv./ha)") +
  xlab("Yearly average minimum temperature (F)") +
  ylim(-40, 1600) +
  facet_wrap(~normal_annual_precip, strip.position = "top", labeller = labeller(normal_annual_precip = precip.labs)) +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "transparent", color = "transparent"), 
        #strip.text.y = element_text(margin = margin(0,0,0,1)),
        text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top", 
        panel.spacing.x = unit(.11, "lines"),
        panel.spacing.y = unit(.6, "lines"))
ggsave(tminPrecip , file="figures/manuscript/tminPrecip.pdf", width=6.5, height=4.5)

plot(plot_dhm$normal_annual_precip, plot_dhm$tmin)


##### Seed wall  -----------------------------------------------


log10SeedWallConifer_levels <- plot_dhm$log10SeedWallConifer#seq(from = min(plot_dhm$log10SeedWallConifer), to = max(plot_dhm$log10SeedWallConifer), length.out = 100)
tpi2000_mean <- mean(plot_dhm$tpi2000)
facts.planting.first.year_mean <- 1
tmin_mean <- mean(plot_dhm$tmin)
normal_annual_precip_mean <- mean(plot_dhm$normal_annual_precip)
Shrubs_mean <- mean(plot_dhm$Shrubs)
fsplanted_mean <- "unplanted"
fsplanted_mean <- as.factor(fsplanted_mean)
LitDuff_mean <- mean(plot_dhm$LitDuff)

predict_pltd.sw <- expand.grid(log10SeedWallConifer = log10SeedWallConifer_levels,
                               tpi2000 = tpi2000_mean,
                               facts.planting.first.year = facts.planting.first.year_mean, 
                               tmin = tmin_mean,
                               normal_annual_precip = normal_annual_precip_mean,
                               Shrubs = Shrubs_mean,
                               fsplanted = fsplanted_mean,
                               LitDuff = LitDuff_mean)


predicted_pltd.sw <- cbind(predict_pltd.sw, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.sw, re.form = NA, level = 0, type="response", se.fit = TRUE)))

obs.sw <- predict(pltd, newdata = predict_pltd.sw, re.form=NA)
obs_resid.sw <- (obs.sw + resid(pltd,re.form=NA)) 
plot_dhm$obs_resid.sw <- exp(obs_resid.sw) -24.99



seedw <-  ggplot(data = predicted_pltd.sw, aes(y = exp(fit)-24.99, 
                                               x = 10^(log10SeedWallConifer))) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), 
              alpha = .15, color = NA, fill = "#756156", show.legend = FALSE) +
  geom_smooth(show.legend = FALSE, size = 2, color = "#756156", se = F) +
  #scale_color_manual(values = c("#8c8863","#756156", "#48553b")) +
  #scale_fill_manual(values = c("#8c8863","#756156", "#48553b")) +
  ylab("Seedling Density \n(indv./ha)") +
  xlab("Minimum distance from seed source (m)") +
  ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(seedw , file="figures/manuscript/seedw.pdf", width=3.25, height=3.45)

##### Litter  -----------------------------------------------


LitDuff_levels <- plot_dhm$LitDuff#seq(from = min(plot_dhm$log10SeedWallConifer), to = max(plot_dhm$log10SeedWallConifer), length.out = 100)
tpi2000_mean <- mean(plot_dhm$tpi2000)
facts.planting.first.year_mean <- 1
tmin_mean <- mean(plot_dhm$tmin)
normal_annual_precip_mean <- mean(plot_dhm$normal_annual_precip)
Shrubs_mean <- mean(plot_dhm$Shrubs)
fsplanted_mean <- "unplanted"
fsplanted_mean <- as.factor(fsplanted_mean)
log10SeedWallConifer_mean <- mean(plot_dhm$log10SeedWallConifer)

predict_pltd.ld <- expand.grid(LitDuff = LitDuff_levels,
                               log10SeedWallConifer = log10SeedWallConifer_mean,
                               tpi2000 = tpi2000_mean,
                               facts.planting.first.year = facts.planting.first.year_mean, 
                               tmin = tmin_mean,
                               normal_annual_precip = normal_annual_precip_mean,
                               Shrubs = Shrubs_mean,
                               fsplanted = fsplanted_mean)


predicted_pltd.ld <- cbind(predict_pltd.ld, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.ld, re.form = NA, level = 0, type="response", se.fit = TRUE)))

obs.sw <- predict(pltd, newdata = predict_pltd.sw, re.form=NA)
obs_resid.sw <- (obs.sw + resid(pltd,re.form=NA)) 
plot_dhm$obs_resid.sw <- exp(obs_resid.sw) -24.99



litduf <-  ggplot(data = predicted_pltd.ld, aes(y = exp(fit)-24.99, 
                                               x = LitDuff)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), 
              alpha = .15, color = NA, fill = "#291711", show.legend = FALSE) +
  geom_smooth(show.legend = FALSE, size = 2, color = "#291711", se = F) +
  #scale_color_manual(values = c("#8c8863","#756156", "#48553b")) +
  #scale_fill_manual(values = c("#8c8863","#756156", "#48553b")) +
  ylab("Seedling Density \n(indv./ha)") +
  xlab("Litter + Duff (cm)") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(litduf, file="figures/manuscript/litduf.pdf", width=3.25, height=3.45)






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



ggplot(data = predicted_ver, 
       aes(x = cc, y = 1-fit, color = cc, fill= cc)) + 
  geom_errorbar(aes(ymax = 1-(fit + se.fit), ymin=1-(fit -se.fit)), color = c("#483D3F","#4A5043"), width = 0.1) +
  geom_point(size = 10, shape = 21, color = c("#A44200","#4A7145"), fill = c("#D29715","#9CA04B"), stroke = 2) +
  guides(fill = guide_legend(title = NULL), color = NULL ) +
  labs(title = NULL, x = NULL, y = NULL) +
  scale_x_discrete(labels = NULL) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.50,.75, 1.00), limits = c(-.01, 1.01)) +
  #scale_fill_manual(values=c("Grey50", "white")) +
  #guides(fill=guide_legend(title="Belowground \ncompetition", keywidth=0.4,
  #                        keyheight=0.6,
  #                       default.unit="inch")) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_rect(colour = 'Grey20', fill = 'white', linetype='solid'), 
        legend.key = element_blank(), legend.title.align = .5, aspect.ratio =  1,
        text = element_text(size = 24), axis.text.x = element_text(size = 24, color = "black"),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 14,  Color = "black"), 
        legend.position="none") 
ggsave(pver, file="plots/seed_pred_ver.pdf", width=3.2, height=3.16)

psm <- ggplot(sm.melt, aes(x = lnsm, y = pa, color = factor(interaction(LUH,CT), labels = c("Post-ag. thinned", "Remnant thinned", "Post-ag. unthinned", "Remnant unthinned")))) +
  labs(#title = expression(italic("Coreopsis major")), 
    #colour = "Legend", 
    y = NULL,#"Log total biomass (mg)", 
    x = "Seed mass") +
  geom_line(data = predict_sm.melt, size = 2) +
  scale_colour_manual(values=c("gold2","darkolivegreen3", "gold4", "darkolivegreen")) +
  #scale_linetype_manual(values = c("solid", "twodash"), name = "Legend") +
  scale_y_log10(breaks = c(0.001, .020)) +
  theme(panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(),
        legend.background = element_blank(), 
        legend.title=element_blank(),
        legend.key = element_blank(), legend.title.align = .5,
        text = element_text(size = 30), axis.text.x = element_text(size = 30, color = "black"),
        axis.title = element_text(size = 36),
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 30,  Color = "black"), 
        legend.position=c(.74,.12), legend.box = "horizontal", legend.key.width = unit(3,"line"))


fig <- plot.data %>% 
  ggplot(aes(x = pep, y = value, group = type, color = type)) +
  geom_point(show.legend = FALSE) +
  stat_smooth(method = "lm", aes(fill = type), show.legend = FALSE) +
  facet_wrap(~trait, scales = "free_y", strip.position = "left", labeller = label_parsed) +
  theme_classic() +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "transparent", color = "transparent"), 
        strip.text.y = element_text(margin = margin(0,0,0,1)),
        text = element_text(size = 10), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 7,  Color = "black", Margin = margin(0,0,0,0)), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = "white", color = "black"), 
        legend.position = "top", 
        panel.spacing.x = unit(.11, "lines"),
        panel.spacing.y = unit(.6, "lines")) +
  ylab(NULL) +
  xlab("Precipitation/Pan Evaporation (mm/mm)") +
  scale_color_manual(values = c("#a2aa43", "#6c7f8d"), name = NULL) +
  scale_fill_manual(values = c("#E4E6C3", "#D7E2F0"))