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
} # Funciont for controling decimal number

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
} # function for rotating axis labels


#ln.dens.planted ~ scale(tpi2000)*facts.planting.first.year +
#  scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
#  scale(tmean)*scale(normal_annual_precip) +
#  log10SeedWallConifer + scale(ShrubHt) 

##### year by planted by shrub -----------------------------------------------

#obs_pltd.pys <- plot_dhm %>% select(fsplanted, Shrubs, facts.planting.first.year)
#obs_pltd.pys$tmin <- median(plot_dhm$tmin)
#obs_pltd.pys$normal_annual_precip <- median(plot_dhm$normal_annual_precip)
#obs_pltd.pys$log10SeedWallConifer <- 1.69 #median(plot_dhm$log10SeedWallConifer)
#obs_pltd.pys$ShrubHt <- median(plot_dhm$ShrubHt)
#obs_pltd.pys$tpi2000 <- median(plot_dhm$tpi2000)
#obs_pltd.pys$LitDuff <- median(plot_dhm$LitDuff)

#obs.pys <- predict(pltd, newdata = obs_pltd.pys, re.form=NA)
#obs_resid.pys <- (obs.pys + resid(pltd,re.form=NA)) 
#plot_dhm$obs_resid.pys <- exp(obs_resid.pys) -24.99





# average value for other variables in the model
#Shrubs_mean <- mean(plot_dhm$Shrubs)
#ShrubHt_mean <- mean(plot_dhm$ShrubHt)
log10SeedWallConifer_mean = median(plot_dhm$log10SeedWallConifer)
tmin_mean <- median(plot_dhm$tmin)
normal_annual_precip_mean <- median(plot_dhm$normal_annual_precip)
LitDuff_mean <- median(plot_dhm$LitDuff)
tpi2000_mean <- median(plot_dhm$tpi2000)
elev_mean <- median(plot_dhm$elev)

# variables to vary for the plot
Shrubs_levels <- seq(from=0,to=100,length.out=701)
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
                      elev = elev_mean,
                      Fire = "hypothetical",
                      PairID = "hypothetical")


year.labs <- c(c("One year", "Two years", "Three years"))
names(year.labs) <- c(1,2,3)


predicted_pltd.pys <- cbind(predict_pltd.pys, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.pys, re.form = NA, level = 0, type="response", se.fit = TRUE)))


plantedYearShrubs <- ggplot(data = predicted_pltd.pys, 
                            aes(y = (exp(fit)-24.99), x = Shrubs, color = fsplanted, fill = fsplanted)) +
                            #aes(y = fit*24.99, x = Shrubs, color = fsplanted, fill = fsplanted)) +

  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), alpha = .50, color = NA) +
  #geom_ribbon(aes(ymax = (fit + se.fit)*24.99, ymin= (fit - se.fit)*24.99), alpha = .50, color = NA) +
  geom_line(show.legend = FALSE, size = 2, se = F) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.pys, x = Shrubs), show.legend = FALSE) +
  #geom_point(data = plot_dhm, aes(y=dens.planted, x = Shrubs)) +
  scale_color_manual(values = c("#4f5a3a","#807561")) +
  scale_fill_manual(values = c("#98ab8f","#d1cdb6")) +
  ylab("Seedling Density \n(indv/ha)") +
  xlab("Shrub Cover (%)") +
  xlim(20,100) +
  ylim(-10,325) +
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

  

##### tmin by annual precip -----------------------------------------------


# median value for other variables in the model
Shrubs_median <- median(plot_dhm$Shrubs)
facts.planting.first.year_median <- 2
log10SeedWallConifer_median = median(plot_dhm$log10SeedWallConifer)
#LitDuff_median <- median(plot_dhm$LitDuff)
tpi2000_median <- median(plot_dhm$tpi2000)
elev_median <- median(plot_dhm$elev)

# variables to vary for the plot
tmin_levels <- seq(from=min(plot_dhm$tmin),to=max(plot_dhm$tmin),length.out=701)
normal_annual_precip_levels <- c(600,1200,1800)
fsplanted_levels <- c("planted","unplanted")

predict_pltd.tp <- expand.grid(Shrubs = Shrubs_median,
                                facts.planting.first.year = facts.planting.first.year_median,
                                fsplanted = fsplanted_levels,
                                log10SeedWallConifer = log10SeedWallConifer_median,
                                tmin = tmin_levels,
                                normal_annual_precip = normal_annual_precip_levels,
                                #LitDuff = LitDuff_median,
                                tpi2000 = tpi2000_median,
                                elev = elev_median,
                                Fire = "hypothetical",
                                PairID = "hypothetical")



precip.labs <- c(c("600mm", "1200mm", "1800mm"))
names(precip.labs) <- c(600, 1200, 1800)


predicted_pltd.tp <- cbind(predict_pltd.tp, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.tp, re.form = NA, level = 0, type="response", se.fit = TRUE)))
predicted_pltd.tp$facts.planting.first.year <- as.factor(predicted_pltd.tp$facts.planting.first.year)

tminPrecip <-  
  ggplot(data = predicted_pltd.tp %>% mutate(normal_annual_precip = as.factor(normal_annual_precip)), 
                      aes(y = exp(fit)-24.99, x = tmin, color = normal_annual_precip, fill = normal_annual_precip, linetype = fsplanted)) +
                      #aes(y = fit*24.99, x = tmin, color = normal_annual_precip, fill = normal_annual_precip, linetype = fsplanted)) + #for Poisson
    geom_ribbon(aes(ymax = ifelse((exp(fit + se.fit)-24.99) < 1600,(exp(fit + se.fit)-24.99), 1600), ymin=(exp(fit - se.fit)-24.99), color = normal_annual_precip, fill = normal_annual_precip), 
    #geom_ribbon(aes(ymax = ifelse((fit + se.fit)*24.99 < 1600, (fit + se.fit)*24.99, 1600), ymin=(fit - se.fit)*24.99, color = normal_annual_precip, fill = normal_annual_precip), 
                  alpha = .15, color = NA, show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 1.5, se = F, color = "#6D7274") +
  #geom_point(data = plot_dhm %>% #for Poisson
   #            mutate(normal_annual_precip = as.factor(ifelse(normal_annual_precip < 900, 600, 
    #                                                ifelse(normal_annual_precip > 900 & 
     #                                                        normal_annual_precip < 1500, 1200, 1800)))), 
      #                aes(y=dens.planted, x = tmin, color = normal_annual_precip, fill = normal_annual_precip), show.legend = FALSE) +
  scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Seedling Density \n(indv./ha)") +
  xlab("Yearly average minimum temperature (F)") +
  ylim(-40, 1600) +
  xlim(0, 5.5) +
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

#plot(plot_dhm$normal_annual_precip, plot_dhm$tmin)


##### Seed wall  -----------------------------------------------

  
  
# median value for other variables in the model
tmin_median <- median(plot_dhm$tmin)
normal_annual_precip_median <- median(plot_dhm$normal_annual_precip)
Shrubs_median <- median(plot_dhm$Shrubs)
facts.planting.first.year_median <- 2
LitDuff_median <- median(plot_dhm$LitDuff)
tpi2000_median <- median(plot_dhm$tpi2000)
elev_median <- median(plot_dhm$elev)
  
# variables to vary for the plot
log10SeedWallConifer_levels = log10(seq(from=min(10^plot_dhm$log10SeedWallConifer),to=max(10^plot_dhm$log10SeedWallConifer),length.out=701))
fsplanted_levels <- c("planted","unplanted")
  
predict_pltd.sw <- expand.grid(Shrubs = Shrubs_median,
                                facts.planting.first.year = facts.planting.first.year_median,
                                fsplanted = fsplanted_levels,
                                log10SeedWallConifer = log10SeedWallConifer_levels,
                                tmin = tmin_median,
                                normal_annual_precip = normal_annual_precip_median,
                                LitDuff = LitDuff_median,
                                tpi2000 = tpi2000_median,
                                elev = elev_median,
                                Fire = "hypothetical",
                                PairID = "hypothetical")


predicted_pltd.sw <- cbind(predict_pltd.sw, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.sw, re.form = NA, level = 0, type="response", se.fit = TRUE)))

#obs.sw <- predict(pltd, newdata = predict_pltd.sw, re.form=NA)
#obs_resid.sw <- (obs.sw + resid(pltd,re.form=NA)) 
#plot_dhm$obs_resid.sw <- exp(obs_resid.sw) -24.99



seedw <- ggplot(data = predicted_pltd.sw, 
                 aes(y = exp(fit)-24.99, x = 10^(log10SeedWallConifer),linetype = fsplanted)) +
                 #aes(y = fit*24.99, x = 10^(log10SeedWallConifer),linetype = fsplanted)) + #poisson
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), 
  #geom_ribbon(aes(ymax = (fit + se.fit)*24.99, ymin = (fit - se.fit)*24.99), #Poisson
              alpha = .15, color = NA, fill = "#756156", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#756156", se = F) +
  #scale_color_manual(values = c("#8c8863","#756156", "#48553b")) +
  #scale_fill_manual(values = c("#8c8863","#756156", "#48553b")) +
  ylab("Seedling Density \n(indv./ha)") +
  xlab("Minimum distance from seed source (m)") +
  #ylim(-10, 230) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(seedw , file="figures/manuscript/seedw.pdf", width=3.25, height=3.45)

##### Litter  -----------------------------------------------
### FOR FEM REVISION 1, REMOVED FROM THE MODEL, NOT NEEDED 

# median value for other variables in the model
#tmin_median <- median(plot_dhm$tmin) 
#normal_annual_precip_median <- median(plot_dhm$normal_annual_precip) 
#Shrubs_median <- median(plot_dhm$Shrubs)
#facts.planting.first.year_median <- 2
#log10SeedWallConifer_median <- median(plot_dhm$LitDuff)
#tpi2000_median <- median(plot_dhm$tpi2000)
#elev_median <- median(plot_dhm$elev)

# variables to vary for the plot
#LitDuff_levels = (seq(from=min(plot_dhm$LitDuff),to=max(plot_dhm$LitDuff),length.out=701))
#fsplanted_levels <- c("planted","unplanted")

#predict_pltd.ld <- expand.grid(Shrubs = Shrubs_median,
#                               facts.planting.first.year = facts.planting.first.year_median,
#                               fsplanted = fsplanted_levels,
#                               log10SeedWallConifer = log10SeedWallConifer_median,
#                               tmin = tmin_median,
#                               normal_annual_precip = normal_annual_precip_median,
#                               LitDuff = LitDuff_levels,
#                               tpi2000 = tpi2000_median,
#                              elev = elev_median,
#                              Fire = "hypothetical",
#                              PairID = "hypothetical")


#predicted_pltd.ld <- cbind(predict_pltd.ld, as.data.frame(
#  predictSE(pltd, newdata = predict_pltd.ld, re.form = NA, level = 0, type="response", se.fit = TRUE)))

#obs.sw <- predict(pltd, newdata = predict_pltd.sw, re.form=NA)
#obs_resid.sw <- (obs.sw + resid(pltd,re.form=NA)) 
#plot_dhm$obs_resid.sw <- exp(obs_resid.sw) -24.99



#litduf <- ggplot(data = predicted_pltd.ld, aes(y = 
#                                                     exp(fit)-24.99,
                                                     #(fit)*24.99,
#                                               x = LitDuff, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
#  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), 
  #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
#              alpha = .15, color = NA, fill = "#291711", show.legend = FALSE) +
#  geom_line(show.legend = FALSE, size = 2, color = "#291711", se = F) +
  #scale_color_manual(values = c("#8c8863","#756156", "#48553b")) +
  #scale_fill_manual(values = c("#8c8863","#756156", "#48553b")) +
#  ylab("Seedling Density \n(indv./ha)") +
#  xlab("Litter + Duff (cm)") +
  #ylim(-10, 75) +
#  theme( text = element_text(size = 12), 
#         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
#         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
#         legend.position = "top")
#ggsave(litduf, file="figures/manuscript/litduf.pdf", width=3.25, height=3.45)

##### tpi elevation -----------------------------------------------

tmin_median <- median(plot_dhm$tmin) 
normal_annual_precip_median <- median(plot_dhm$normal_annual_precip) 
Shrubs_median <- median(plot_dhm$Shrubs)
facts.planting.first.year_median <- 2
log10SeedWallConifer_median <- median(plot_dhm$LitDuff)
LitDuff_levels <- median(plot_dhm$LitDuff)

# variables to vary for the plot
tpi2000_levels = (seq(from=min(plot_dhm$tpi2000),to=max(plot_dhm$tpi2000),length.out=701))
fsplanted_levels <- c("planted","unplanted")
elev_levels <- c(1200, 1700, 2200)

predict_pltd.te <- expand.grid(Shrubs = Shrubs_median,
                               facts.planting.first.year = facts.planting.first.year_median,
                               fsplanted = fsplanted_levels,
                               log10SeedWallConifer = log10SeedWallConifer_median,
                               tmin = tmin_median,
                               normal_annual_precip = normal_annual_precip_median,
                               LitDuff = LitDuff_median,
                               tpi2000 = tpi2000_levels,
                               elev = elev_levels,
                               Fire = "hypothetical",
                               PairID = "hypothetical")


elev.labs <- c(c("1200m", "1700m", "2200m"))
names(elev.labs) <- c(1200, 1700, 2200)


predicted_pltd.te <- cbind(predict_pltd.te, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.te, re.form = NA, level = 0, type="response", se.fit = TRUE)))

tpiElev <-  ggplot(data = predicted_pltd.te %>% mutate(elev = as.factor(elev)), aes(y = exp(fit)-24.99, x = tpi2000, color = elev, fill = elev, linetype = fsplanted)) +
  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99), color = elev, fill = elev), 
              alpha = .15, color = NA, show.legend = FALSE) +
  geom_smooth(show.legend = FALSE, size = 1.5, se = F) +
  #geom_point(data = plot_dhm %>% 
  #            mutate(normal_annual_precip = as.factor(ifelse(normal_annual_precip < 900, 600, 
  #                                                ifelse(normal_annual_precip > 900 & 
  #                                                        normal_annual_precip < 1500, 1200, 1800)))), 
  #                aes(y=dens.planted, x = tmin, color = normal_annual_precip, fill = normal_annual_precip), show.legend = FALSE) +
  scale_color_manual(values = c("#48281E","#4C6B6A", "#C8A104")) +
  scale_fill_manual(values = c("#48281E","#4C6B6A", "#C8A104")) +
  ylab("Seedling Density \nindv./ha)") +
  xlab("Topographic position index") +
  #ylim(-40, 1600) +
  facet_wrap(~elev, strip.position = "top", labeller = labeller(elev = elev.labs)) +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "transparent", color = "transparent"), 
        #strip.text.y = element_text(margin = margin(0,0,0,1)),
        text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top", 
        panel.spacing.x = unit(.11, "lines"),
        panel.spacing.y = unit(.6, "lines"))
ggsave(tpiElev , file="figures/manuscript/tpiElev.pdf", width=3.25, height=3.45)

plot(plot_dhm$normal_annual_precip, plot_dhm$tmin)


predict_pltd.ty <- plot_dhm %>%
  tidyr::expand(nesting(facts.planting.first.year), tpi2000) 
predict_pltd.ty$tmin <- mean(plot_dhm$tmin)
predict_pltd.ty$normal_annual_precip <- mean(plot_dhm$normal_annual_precip)
predict_pltd.ty$log10SeedWallConifer <- -.6
#predict_pltd.ty$ShrubHt <- mean(plot_dhm$ShrubHt)
predict_pltd.ty$elev <- mean(plot_dhm$elev)
predict_pltd.ty$Shrubs <- mean(plot_dhm$Shrubs)
predict_pltd.ty$fsplanted <- "planted"
predict_pltd.ty$fsplanted <- as.factor(predict_pltd.ty$fsplanted)
#predict_pltd.ty$LitDuff <- mean(plot_dhm$LitDuff)


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
# NOTE THIS ONE COMES OUT DIFFERENTLY FROM THE OLD VERSION 

##### Fire table ----------------------------------------------------------------------------------

Fire.Table.Median <- plot_dhm %>% 
  group_by(Fire) %>%
  summarise(ln.dens.planted = median(ln.dens.planted),
            fire_year = median(fire_year),
            tpi100 =  median(tpi100),
            tpi500 =  median(tpi500),
            tpi2000 =  median(tpi2000),
            tpi5000 =  median(tpi5000),
            elev =  median(elev),
            Forbs =  median(Forbs),
            Grasses =  median(Grasses),
            ShrubHt =  median(ShrubHt),
            Shrubs =  median(Shrubs),
            facts.planting.first.year =  median(facts.planting.first.year),
            #facts.release =  median(facts.released),
            tmean =  median(tmean),
            tmax =  median(tmax),
            rad_winter =  median(rad_winter),
            rad_winter_spring = median(rad_winter_spring),
            rad_spring =  median(rad_spring),
            rad_spring_summer =  median(rad_spring_summer),
            rad_summer =  median(rad_summer),
            normal_annual_precip =  median(normal_annual_precip),
            tmin =  median(tmin),
            bcm_snowpack =  median(bcm_snowpack),
            twi =  median(twi),
            log10SeedWall =  median(log10SeedWallConifer),
            LitterDepth =  median(LitterDepth),
            DuffDepth =  median(DuffDepth),
            LitDuff =  median(LitDuff),
            CWD_rotten =  median(CWD_rotten),
            CWD_sound =  median(CWD_sound),
            LiveOverstory =  median(LiveOverstory),
            aspect_dem =   median(aspect_dem),
            slope_dem =  median(slope_dem)) %>%
  mutate(Type = "Median")

Fire.Table.Minimum <- plot_dhm %>% 
  group_by(Fire) %>%
  summarise(ln.dens.planted = min(ln.dens.planted),
            fire_year = median(fire_year),              
            tpi100 =  min(tpi100),
            tpi500 =  min(tpi500),
            tpi2000 =  min(tpi2000),
            tpi5000 =  min(tpi5000),
            elev =  min(elev),
            Forbs =  min(Forbs),
            Grasses =  min(Grasses),
            ShrubHt =  min(ShrubHt),
            Shrubs =  min(Shrubs),
            facts.planting.first.year =  min(facts.planting.first.year),
            #facts.release =  min(facts.released),
            tmean =  min(tmean),
            tmax =  min(tmax),
            rad_winter =  min(rad_winter),
            rad_winter_spring = min(rad_winter_spring),
            rad_spring =  min(rad_spring),
            rad_spring_summer =  min(rad_spring_summer),
            rad_summer =  min(rad_summer),
            normal_annual_precip =  min(normal_annual_precip),
            tmin =  min(tmin),
            bcm_snowpack =  min(bcm_snowpack),
            twi =  min(twi),
            log10SeedWall =  min(log10SeedWallConifer),
            LitterDepth =  min(LitterDepth),
            DuffDepth =  min(DuffDepth),
            LitDuff =  min(LitDuff),
            CWD_rotten =  min(CWD_rotten),
            CWD_sound =  min(CWD_sound),
            LiveOverstory =  min(LiveOverstory),
            aspect_dem =   min(aspect_dem),
            slope_dem =  min(slope_dem)
  ) %>%
  mutate(Type = "Minimum")

Fire.Table.Maximum <- plot_dhm %>% 
  group_by(Fire) %>%
  summarise(ln.dens.planted = max(ln.dens.planted),
            fire_year = median(fire_year),           
            tpi100 =  max(tpi100),
            tpi500 =  max(tpi500),
            tpi2000 =  max(tpi2000),
            tpi5000 =  max(tpi5000),
            elev =  max(elev),
            Forbs =  max(Forbs),
            Grasses =  max(Grasses),
            ShrubHt =  max(ShrubHt),
            Shrubs =  max(Shrubs),
            facts.planting.first.year =  max(facts.planting.first.year),
            #facts.release =  max(facts.released),
            tmean =  max(tmean),
            tmax =  max(tmax),
            rad_winter =  max(rad_winter),
            rad_winter_spring = max(rad_winter_spring),
            rad_spring =  max(rad_spring),
            rad_spring_summer =  max(rad_spring_summer),
            rad_summer =  max(rad_summer),
            normal_annual_precip =  max(normal_annual_precip),
            tmin =  max(tmin),
            bcm_snowpack =  max(bcm_snowpack),
            twi =  max(twi),
            log10SeedWall =  max(log10SeedWallConifer),
            LitterDepth =  max(LitterDepth),
            DuffDepth =  max(DuffDepth),
            LitDuff =  max(LitDuff),
            CWD_rotten =  max(CWD_rotten),
            CWD_sound =  max(CWD_sound),
            LiveOverstory =  max(LiveOverstory),
            aspect_dem =   max(aspect_dem),
            slope_dem =  max(slope_dem)
  ) %>%
  mutate(Type = "Maximum")

Fire.Table <- as.data.frame(rbind(Fire.Table.Median, Fire.Table.Maximum, Fire.Table.Minimum))
Fire.Table[nrow(Fire.Table) + 1,] <- colnames(Fire.Table) 

Fire.Table <- Fire.Table %>% 
  dplyr::select(Fire, Type, ln.dens.planted, fire_year, normal_annual_precip, tmin, tmean, tmax, bcm_snowpack, rad_winter, rad_winter_spring, rad_spring, rad_spring_summer, rad_summer, aspect_dem, slope_dem, elev, tpi100, tpi500, tpi2000, tpi5000, twi, log10SeedWall, Forbs, Grasses, Shrubs, ShrubHt, CWD_sound, CWD_rotten, LitterDepth, DuffDepth, LitDuff,LiveOverstory, facts.planting.first.year) %>%
  mutate(ln.dens.planted           = str_replace( ln.dens.planted            , pattern = "ln.dens.planted", replacement = "Seedling Density")) %>% 
  mutate(tpi100                    = str_replace( tpi100                     , pattern = "tpi100", replacement = "Topographic Position Index 100")) %>% 
  mutate(tpi500                    = str_replace( tpi500                     , pattern = "tpi500", replacement = "Topographic Position Index 500")) %>% 
  mutate(tpi2000                   = str_replace( tpi2000                    , pattern = "tpi2000", replacement = "Topographic Position Index 2000")) %>% 
  mutate(tpi5000                   = str_replace( tpi5000                    , pattern = "tpi5000", replacement = "Topographic Position Index 5000")) %>% 
  mutate(elev                      = str_replace( elev                       , pattern = "elev", replacement = "Elevation")) %>% 
  mutate(Forbs                     = str_replace( Forbs                      , pattern = "Forbs", replacement = "%Forb")) %>% 
  mutate(Grasses                   = str_replace( Grasses                    , pattern = "Grasses", replacement = "%Grasses")) %>% 
  mutate(ShrubHt                   = str_replace( ShrubHt                    , pattern = "ShrubHt", replacement = "Ave. Shrub Ht")) %>% 
  mutate(Shrubs                    = str_replace( Shrubs                     , pattern = "Shrubs", replacement = "%Shrubs")) %>% 
  mutate(facts.planting.first.year = str_replace( facts.planting.first.year  , pattern = "facts.planting.first.year", replacement = "Year Planted")) %>% 
  mutate(tmean                     = str_replace( tmean                      , pattern = "tmean", replacement = "Mean Annual Temp.")) %>% 
  mutate(tmax                      = str_replace( tmax                       , pattern = "tmax", replacement = "Max. Annual Temp.")) %>% 
  mutate(rad_winter                = str_replace( rad_winter                 , pattern = "rad_winter", replacement = "Winter Solar Insolation")) %>% 
  mutate(rad_winter_spring         = str_replace( rad_winter_spring          , pattern = "rad_winter_spring", replacement = "Winter/Spring Solar Insolation")) %>% 
  mutate(rad_spring                = str_replace( rad_spring                 , pattern = "rad_spring", replacement = "Spring Solar Insolation")) %>%  
  mutate(rad_spring_summer         = str_replace( rad_spring_summer          , pattern = "rad_spring_summer", replacement = "Spring/Summer Solar Insolation")) %>%  
  mutate(rad_summer                = str_replace( rad_summer                 , pattern = "rad_summer", replacement = "Summer Solar Insolation")) %>%  
  mutate(normal_annual_precip      = str_replace( normal_annual_precip       , pattern = "normal_annual_precip", replacement = "Normal Annual Precipitation")) %>%  
  mutate(tmin                      = str_replace( tmin                       , pattern = "tmin", replacement = "Min. Annual Temp.")) %>%  
  mutate(bcm_snowpack              = str_replace( bcm_snowpack               , pattern = "bcm_snowpack", replacement = "Snowpack")) %>%  
  mutate(twi                       = str_replace( twi                        , pattern = "twi", replacement = "Total Water Index")) %>%  
  mutate(log10SeedWall             = str_replace( log10SeedWall              , pattern = "log10SeedWall", replacement = "Seed Source")) %>%  
  mutate(LitterDepth               = str_replace( LitterDepth                , pattern = "LitterDepth", replacement = "Litter Depth")) %>% 
  mutate(DuffDepth                 = str_replace( DuffDepth                  , pattern = "DuffDepth", replacement = "Duff Depth")) %>% 
  mutate(LitDuff                   = str_replace( LitDuff                    , pattern = "LitDuff", replacement = "Litter + Duff Depth")) %>% 
  mutate(CWD_rotten                = str_replace( CWD_rotten                 , pattern = "CWD_rotten", replacement = "Rotten Coarse Woody Debris")) %>% 
  mutate(CWD_sound                 = str_replace( CWD_sound                  , pattern = "CWD_sound", replacement = "Coarse Woody Debris")) %>% 
  mutate(LiveOverstory             = str_replace( LiveOverstory              , pattern = "LiveOverstory", replacement = "%Overstory")) %>% 
  mutate(aspect_dem                = str_replace( aspect_dem                 , pattern = "aspect_dem", replacement = "Aspect"))  %>% 
  mutate(slope_dem                 = str_replace( slope_dem                  , pattern = "slope_dem", replacement = "Slope")) %>%
  arrange(Fire, Type)

                                              

write.csv(Fire.Table, file = "output/Fire.Table.csv")

Fire.Table.1 <- Fire.Table %>% dplyr::select(Fire, fire_year, elev, facts.planting.first.year)

plantinglisttable <- plot_dhm %>% dplyr::select(Fire, plantingList) %>% distinct()


##########################################################################################
#                              Composition figures for Report
##########################################################################################

plot_comp <- plot_dhm %>% filter(dens.conif != 0) %>% filter(!is.na(LiveOverstory))

normal_annual_precip_median <- median(plot_comp$normal_annual_precip) 
LiveOverstory_median <- median(plot_comp$LiveOverstory) 
twi_median <- median(plot_comp$twi)
facts.planting.first.year_median <- 2
Forbs_median <- median(plot_comp$Forbs)
Shrubs_median <- median(plot_comp$Shrubs)
ShrubHt_median <- median(plot_comp$ShrubHt)
LitDuf_median <- median(plot_comp$LitDuff)

normal_annual_precip_levels <- (seq(from=min(plot_comp$normal_annual_precip),to=max(plot_comp$normal_annual_precip),length.out=701))
LiveOverstory_levels <- (seq(from=min(plot_comp$LiveOverstory, na.rm = TRUE),to=max(plot_comp$LiveOverstory, na.rm = TRUE),length.out=701))
twi_levels <- (seq(from=min(plot_comp$twi),to=max(plot_comp$twi),length.out=701))
facts.planting.first.year_levels <- (seq(from=1,to=3,length.out=701))
Forbs_levels <- (seq(from=min(plot_comp$Forbs),to=max(plot_comp$Forbs),length.out=701))
Shrubs_levels <- (seq(from=min(plot_comp$Shrubs),to=max(plot_comp$Shrubs),length.out=701))
ShrubHt_levels <- c(50, 150, 250)
LitDuff_levels <- (seq(from=min(plot_comp$LitDuff),to=max(plot_comp$LitDuff),length.out=701))
fsplanted_levels <- c("planted","unplanted")

##### Normal Annual Precipitation -----------------------------------------------------------------


predict_comp.precip <- expand.grid(normal_annual_precip = normal_annual_precip_levels,
                               LiveOverstory = LiveOverstory_median, 
                               twi = twi_median, 
                               facts.planting.first.year = facts.planting.first.year_median,
                               Forbs = Forbs_median,
                               Shrubs = Shrubs_median,
                               ShrubHt = ShrubHt_median,
                               LitDuff = LitDuf_median,
                               fsplanted = fsplanted_levels,
                               Fire = "hypothetical",
                               PairID = "hypothetical")



predicted_comp.precip <- cbind(predict_comp.precip, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.precip, re.form = NA, level = 0, type="response", se.fit = TRUE)))


precip_comp <- ggplot(data = predicted_comp.precip, aes(y =  fit, x = normal_annual_precip, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .15, color = NA, fill = "#508AA8", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#6D7274") +
  #scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  #scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Proportion Pine") +
  xlab("Normal Annual Precipitation (mm)") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(precip_comp, file="figures/manuscript/precip_comp.pdf", width=3.25, height=3.45)


##### LiveOverstory -----------------------------------------------------------------



predict_comp.over <- expand.grid(normal_annual_precip = normal_annual_precip_median,
                                   LiveOverstory = LiveOverstory_levels, 
                                   twi = twi_median, 
                                   facts.planting.first.year = facts.planting.first.year_median,
                                   Forbs = Forbs_median,
                                   Shrubs = Shrubs_median,
                                   ShrubHt = ShrubHt_median,
                                   LitDuff = LitDuf_median,
                                   fsplanted = fsplanted_levels,
                                   Fire = "hypothetical",
                                   PairID = "hypothetical")



predicted_comp.over <- cbind(predict_comp.over, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.over, re.form = NA, level = 0, type="response", se.fit = TRUE)))


over_comp <- ggplot(data = predicted_comp.over, aes(y =  fit, x = LiveOverstory, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .15, color = NA, fill = "#59881B", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#59881B") +
  #scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  #scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Proportion Pine") +
  xlab("Canopy Cover (%)") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(over_comp, file="figures/manuscript/over_comp.pdf", width=3.25, height=3.45)


##### twi -----------------------------------------------------------------------------------------



predict_comp.twi <- expand.grid(normal_annual_precip = normal_annual_precip_median,
                                 LiveOverstory = LiveOverstory_median, 
                                 twi = twi_levels, 
                                 facts.planting.first.year = facts.planting.first.year_median,
                                 Forbs = Forbs_median,
                                 Shrubs = Shrubs_median,
                                 ShrubHt = ShrubHt_median,
                                 LitDuff = LitDuf_median,
                                 fsplanted = fsplanted_levels,
                                 Fire = "hypothetical",
                                 PairID = "hypothetical")



predicted_comp.twi <- cbind(predict_comp.twi, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.twi, re.form = NA, level = 0, type="response", se.fit = TRUE)))


twi_comp <- ggplot(data = predicted_comp.twi, aes(y =  fit, x = twi, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .15, color = NA, fill = "#1C629C", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#1C629C") +
  #scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  #scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Proportion Pine") +
  xlab("Topographic Water Index") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(twi_comp, file="figures/manuscript/twi_comp.pdf", width=3.25, height=3.45)


##### Forbs ---------------------------------------------------------------------------------------

predict_comp.forb <- expand.grid(normal_annual_precip = normal_annual_precip_median,
                                LiveOverstory = LiveOverstory_median, 
                                twi = twi_median, 
                                facts.planting.first.year = facts.planting.first.year_median,
                                Forbs = Forbs_levels,
                                Shrubs = Shrubs_median,
                                ShrubHt = ShrubHt_median,
                                LitDuff = LitDuf_median,
                                fsplanted = fsplanted_levels,
                                Fire = "hypothetical",
                                PairID = "hypothetical")



predicted_comp.forb <- cbind(predict_comp.forb, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.forb, re.form = NA, level = 0, type="response", se.fit = TRUE)))


forb_comp <- ggplot(data = predicted_comp.forb, aes(y =  fit, x = Forbs, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .15, color = NA, fill = "#721D34", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#721D34") +
  #scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  #scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Proportion Pine") +
  xlab("Forb Cover (%)") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(forb_comp, file="figures/manuscript/forb_comp.pdf", width=3.25, height=3.45)


##### planting year -------------------------------------------------------------------------------

predict_comp.year <- expand.grid(normal_annual_precip = normal_annual_precip_median,
                                 LiveOverstory = LiveOverstory_median, 
                                 twi = twi_median, 
                                 facts.planting.first.year = facts.planting.first.year_levels,
                                 Forbs = Forbs_median,
                                 Shrubs = Shrubs_median,
                                 ShrubHt = ShrubHt_median,
                                 LitDuff = LitDuf_median,
                                 fsplanted = fsplanted_levels,
                                 Fire = "hypothetical",
                                 PairID = "hypothetical")



predicted_comp.year <- cbind(predict_comp.year, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.year, re.form = NA, level = 0, type="response", se.fit = TRUE)))


year_comp <- ggplot(data = predicted_comp.year, aes(y =  fit, x = facts.planting.first.year, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .15, color = NA, fill = "#514A3D", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#514A3D") +
  #scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  #scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Proportion Pine") +
  xlab("Years After Fire") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(year_comp, file="figures/manuscript/year_comp.pdf", width=3.25, height=3.45)


##### Shrub and Shrub height ----------------------------------------------------------------------

predict_comp.ShrubxHt <- expand.grid(normal_annual_precip = normal_annual_precip_median,
                                 LiveOverstory = LiveOverstory_median, 
                                 twi = twi_median, 
                                 facts.planting.first.year = facts.planting.first.year_median,
                                 Forbs = Forbs_median,
                                 Shrubs = Shrubs_levels,
                                 ShrubHt = ShrubHt_levels,
                                 LitDuff = LitDuf_median,
                                 fsplanted = fsplanted_levels,
                                 Fire = "hypothetical",
                                 PairID = "hypothetical")



predicted_comp.ShrubxHt <- cbind(predict_comp.ShrubxHt, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.ShrubxHt, re.form = NA, level = 0, type="response", se.fit = TRUE)))

ShrubHt.labs <- c("50cm", "150cm", "250cm")
names(ShrubHt.labs) <- c(50, 150, 250)
ShrubxHt_comp <- ggplot(data = predicted_comp.ShrubxHt, 
                        aes(y =  fit, x = Shrubs, linetype = fsplanted, color = fsplanted, fill = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .5, color = NA, show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2) +
  scale_color_manual(values = c("#4f5a3a","#807561")) +
  scale_fill_manual(values = c("#98ab8f","#d1cdb6")) +
  ylab("Proportion Pine") +
  xlab("Shrub Cover (%)") +
  xlim(20,100) +
  facet_wrap(~ShrubHt, strip.position = "top", labeller = labeller(ShrubHt = ShrubHt.labs)) +
  #ylim(-10, 75) +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "transparent", color = "transparent"), 
        #strip.text.y = element_text(margin = margin(0,0,0,1)),
        text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top", 
        panel.spacing.x = unit(.11, "lines"),
        panel.spacing.y = unit(.6, "lines"))
ggsave(ShrubxHt_comp, file="figures/manuscript/ShrubxHt_comp.pdf",  width=4.33, height=3.45)


##### LitDuff -------------------------------------------------------------------------------------

predict_comp.litDuff <- expand.grid(normal_annual_precip = normal_annual_precip_median,
                                 LiveOverstory = LiveOverstory_median, 
                                 twi = twi_median, 
                                 facts.planting.first.year = facts.planting.first.year_median,
                                 Forbs = Forbs_median,
                                 Shrubs = Shrubs_median,
                                 ShrubHt = ShrubHt_median,
                                 LitDuff = LitDuff_levels,
                                 fsplanted = fsplanted_levels,
                                 Fire = "hypothetical",
                                 PairID = "hypothetical")



predicted_comp.litDuff <- cbind(predict_comp.litDuff, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.litDuff, re.form = NA, level = 0, type="response", se.fit = TRUE)))


litDuff_comp <- ggplot(data = predicted_comp.litDuff, aes(y =  fit, x = LitDuff, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .15, color = NA, fill = "#291711", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#291711") +
  #scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  #scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Proportion Pine") +
  xlab("Litter + Duff (cm)") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(litDuff_comp, file="figures/manuscript/litDuff_comp.pdf", width=3.25, height=3.45)


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



ln.dens.plant
tpi100
tpi500
tpi2000
tpi5000
elev
Forbs
Grasses
ShrubHt
Shrubs
facts.plantin
fsplanted
facts.release
tmean
tmax
rad_winter
rad_winter_sp
rad_spring
rad_spring_su
rad_summer
normal_annual
tmin
bcm_snowpack
twi
log10SeedWall
LitterDepth
DuffDepth
LitDuff
CWD_rotten
CWD_sound
LiveOverstory
aspect_dem 
slope_dem






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

##### tpi elevation -----------------------------------------------

predict_pltd.ty <- plot_dhm %>%
  tidyr::expand(nesting(facts.planting.first.year), tpi2000) 
predict_pltd.ty$tmin <- median(plot_dhm$tmin)
predict_pltd.ty$normal_annual_precip <- median(plot_dhm$normal_annual_precip)
predict_pltd.ty$log10SeedWallConifer <- -.6
#predict_pltd.ty$ShrubHt <- median(plot_dhm$ShrubHt)
predict_pltd.ty$Shrubs <- median(plot_dhm$Shrubs)
fsplanted_levels <- c("planted","unplanted")
predict_pltd.ty$fsplanted <- as.factor(predict_pltd.ty$fsplanted)
predict_pltd.ty$LitDuff <- median(plot_dhm$LitDuff)


predicted_pltd.ty <- cbind(predict_pltd.ty, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.ty, re.form = NA, level = 0, type="response", se.fit = TRUE)))
predicted_pltd.ty$facts.planting.first.year <- as.factor(predicted_pltd.ty$facts.planting.first.year)

tpiYear <-  ggplot(data = predicted_pltd.ty, aes(y = exp(fit)-24.99, 
                                                 x = tpi2000, 
                                                 color = facts.planting.first.year, 
                                                 fill = facts.planting.first.year,
                                                 linetype = fsplanted)) +
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


predict_pltd.tp$tmin <- as.data.frame(rep(600, nrow(predict_pltd.tp)))plot_dhm %>% select(tmin)
predict_pltd.tp$tpi2000 <- median(plot_dhm$tpi2000)
predict_pltd.tp$elev <- median(plot_dhm$elev)
predict_pltd.tp$facts.planting.first.year <- 2
predict_pltd.tp$log10SeedWallConifer <- median(plot_dhm$log10SeedWallConifer)
#predict_pltd.tp$ShrubHt <- median(plot_dhm$ShrubHt)
predict_pltd.tp$Shrubs <- median(plot_dhm$Shrubs)
predict_pltd.tp$LitDuff <- median(plot_dhm$LitDuff)
pre6 <- as.data.frame(rep(600, nrow(predict_pltd.tp)))
pre12 <- as.data.frame(rep(1200, nrow(predict_pltd.tp)))
pre18 <- as.data.frame(rep(1800,  nrow(predict_pltd.tp)))
colnames(pre6)[1] <- c("normal_annual_precip")
colnames(pre12)[1] <- c("normal_annual_precip")
colnames(pre18)[1] <- c("normal_annual_precip")
predict_pltd.tpp <- rbind(cbind(predict_pltd.tp, pre6),
                          cbind(predict_pltd.tp, pre12),
                          cbind(predict_pltd.tp, pre18))
predict_pltd.tppp <- predict_pltd.tpp
predict_pltd.tppu <- predict_pltd.tpp
predict_pltd.tppp$fsplanted <- "planted"
predict_pltd.tppu$fsplanted <- "unplanted"
predict_pltd.tpp <- rbind(cbind(predict_pltd.tpp, predict_pltd.tppp %>% select(fsplanted)),
                          cbind(predict_pltd.tpp, predict_pltd.tppu %>% select(fsplanted)))
predict_pltd.tpp <- predict_pltd.tpp %>% mutate(fsplanted = as.factor(fsplanted))


