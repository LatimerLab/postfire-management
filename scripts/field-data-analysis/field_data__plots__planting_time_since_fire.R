library(tidyverse)
library(AICcmodavg)

load(file = "output/modelDataForPlots.RData")
load("output/plotSeedlingData.RData") #load R object: plot_dhm_long
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
  mutate(neglog5SeedWallConifer = -logb(SeedWallConifer, base = exp(5))) %>%
  mutate(totalCov = Shrubs + Grasses + Forbs) %>%
  mutate(totalCovxHt = (Shrubs*ShrubHt + Grasses*GrassHt + Forbs*ForbHt))


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
#  neglog5SeedWallConifer + scale(ShrubHt) 

##### year by planted by shrub -----------------------------------------------

predict_pltd.pys <- plot_dhm %>%
  expand(nesting(fsplanted, facts.planting.first.year),Shrubs) 
predict_pltd.pys$tmean <- mean(plot_dhm$tmean)
predict_pltd.pys$normal_annual_precip <- mean(plot_dhm$normal_annual_precip)
predict_pltd.pys$neglog5SeedWallConifer <- mean(plot_dhm$neglog5SeedWallConifer)
predict_pltd.pys$ShrubHt <- mean(plot_dhm$ShrubHt)
predict_pltd.pys$tpi2000 <- mean(plot_dhm$tpi2000)

predicted_pltd.pys <- cbind(predict_pltd.pys, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.pys, re.form = NA, level = 0, type="response", se.fit = TRUE)))

year.labs <- c(c("One year", "Two years", "Three years"))
names(year.labs) <- c(1,2,3)

plantedYearShrubs <- ggplot(data = predicted_pltd.pys, aes(y = fit, x = asin(sqrt(Shrubs/100)), color = fsplanted, fill = fsplanted)) +
  geom_ribbon(aes(ymax = (fit + se.fit), ymin=(fit - se.fit)), alpha = .50, color = NA) +
  geom_smooth(method = "lm", show.legend = FALSE, size = 2) +
  geom_point(data = plot_dhm, aes(y = ln.dens.planted, x = asin(sqrt(Shrubs/100)))) +
  scale_color_manual(values = c("#4f5a3a","#807561")) +
  scale_fill_manual(values = c("#98ab8f","#d1cdb6")) +
  ylab("Seedling Density \nln(indv.+1/m)") +
  xlab("Shrub Cover \nasin(sqrt(%))") +
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
  expand(nesting(facts.planting.first.year), tpi2000) 
predict_pltd.ty$tmean <- mean(plot_dhm$tmean)
predict_pltd.ty$normal_annual_precip <- mean(plot_dhm$normal_annual_precip)
predict_pltd.ty$neglog5SeedWallConifer <- mean(plot_dhm$neglog5SeedWallConifer)
predict_pltd.ty$ShrubHt <- mean(plot_dhm$ShrubHt)
predict_pltd.ty$Shrubs <- mean(plot_dhm$Shrubs)
predict_pltd.ty$fsplanted <- "planted"
predict_pltd.ty$fsplanted <- as.factor(predict_pltd.ty$fsplanted)


predicted_pltd.ty <- cbind(predict_pltd.ty, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.ty, re.form = NA, level = 0, type="response", se.fit = TRUE)))
predicted_pltd.ty$facts.planting.first.year <- as.factor(predicted_pltd.ty$facts.planting.first.year)

tpiYear <-  ggplot(data = predicted_pltd.ty, aes(y = fit, x = tpi2000, color = facts.planting.first.year, fill = facts.planting.first.year)) +
  geom_ribbon(aes(ymax = (fit + se.fit), ymin = (fit - se.fit)), alpha = .15, color = NA, , show.legend = FALSE) +
  geom_smooth(method = "lm", size = 2) +
  #geom_point(data = plot_dhm, aes(y = ln.dens.planted, x = tpi2000)) +
  scale_color_manual(values = c("#8c8863","#756156", "#48553b")) +
  scale_fill_manual(values = c("#8c8863","#756156", "#48553b")) +
  ylab("Seedling Density \nln(indv.+1/m)") +
  xlab("Topographic position index") +
  theme( text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top")
ggsave(tpiYear , file="figures/manuscript/tpiYear .pdf", width=3.25, height=3.45)


##### year by tpi -----------------------------------------------

predict_pltd.ty <- plot_dhm %>%
  expand(nesting(facts.planting.first.year), tpi2000) 
predict_pltd.ty$tmean <- mean(plot_dhm$tmean)
predict_pltd.ty$normal_annual_precip <- mean(plot_dhm$normal_annual_precip)
predict_pltd.ty$neglog5SeedWallConifer <- mean(plot_dhm$neglog5SeedWallConifer)
predict_pltd.ty$ShrubHt <- mean(plot_dhm$ShrubHt)
predict_pltd.ty$Shrubs <- mean(plot_dhm$Shrubs)
predict_pltd.ty$fsplanted <- "planted"
predict_pltd.ty$fsplanted <- as.factor(predict_pltd.ty$fsplanted)


predicted_pltd.ty <- cbind(predict_pltd.ty, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.ty, re.form = NA, level = 0, type="response", se.fit = TRUE)))
predicted_pltd.ty$facts.planting.first.year <- as.factor(predicted_pltd.ty$facts.planting.first.year)

tpiYear <-  ggplot(data = predicted_pltd.ty, aes(y = fit, x = tpi2000, color = facts.planting.first.year, fill = facts.planting.first.year)) +
  geom_ribbon(aes(ymax = (fit + se.fit), ymin = (fit - se.fit)), alpha = .15, color = NA, , show.legend = FALSE) +
  geom_smooth(method = "lm", size = 2) +
  #geom_point(data = plot_dhm, aes(y = ln.dens.planted, x = tpi2000)) +
  scale_color_manual(values = c("#8c8863","#756156", "#48553b")) +
  scale_fill_manual(values = c("#8c8863","#756156", "#48553b")) +
  ylab("Seedling Density \nln(indv.+1/m)") +
  xlab("Topographic position index") +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(tpiYear , file="figures/manuscript/tpiYear .pdf", width=3.25, height=3.45)


##### tmean by annual precip -----------------------------------------------

predict_pltd.tp <- plot_dhm %>%
  expand(tmean) 
predict_pltd.tp$tpi2000 <- mean(plot_dhm$tpi2000)
predict_pltd.tp$facts.planting.first.year <- 2
predict_pltd.tp$neglog5SeedWallConifer <- mean(plot_dhm$neglog5SeedWallConifer)
predict_pltd.tp$ShrubHt <- mean(plot_dhm$ShrubHt)
predict_pltd.tp$Shrubs <- mean(plot_dhm$Shrubs)
predict_pltd.tp$fsplanted <- "planted"
predict_pltd.tp$fsplanted <- as.factor(predict_pltd.tp$fsplanted)
first <- as.data.frame(rep(600, 182))
colnames(first)[1] < - c("normal_annual_precipitation")
second <-rep(1200, 182)
third <-rep(1800, 182)
predict_pltd.tpp <- rbind(cbind(predict_pltd.tp, as.data.frame(rep(600, 182)),cbind(predict_pltd.tp, rep(1200, 182)),cbind(predict_pltd.tp, rep(1800, 182)))


predicted_pltd.tp <- cbind(predict_pltd.tp, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.tp, re.form = NA, level = 0, type="response", se.fit = TRUE)))
predicted_pltd.tp$facts.planting.first.year <- as.factor(predicted_pltd.tp$facts.planting.first.year)

tpiYear <-  ggplot(data = predicted_pltd.ty, aes(y = fit, x = tpi2000, color = facts.planting.first.year, fill = facts.planting.first.year)) +
  geom_ribbon(aes(ymax = (fit + se.fit), ymin = (fit - se.fit)), alpha = .15, color = NA, , show.legend = FALSE) +
  geom_smooth(method = "lm", size = 2) +
  #geom_point(data = plot_dhm, aes(y = ln.dens.planted, x = tpi2000)) +
  scale_color_manual(values = c("#8c8863","#756156", "#48553b")) +
  scale_fill_manual(values = c("#8c8863","#756156", "#48553b")) +
  ylab("Seedling Density \nln(indv.+1/m)") +
  xlab("Topographic position index") +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(tpiYear , file="figures/manuscript/tpiYear .pdf", width=3.25, height=3.45)


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