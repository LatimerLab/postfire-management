library(AICcmodavg)
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggplot2)
library(gridExtra)
library(effects)
library(sjPlot)
library(MuMIn)

#load(file = "output/modelDataForPlots.RData") # loads Quinn's old model versions
load("output/plotSeedlingData.RData") #load dataframe for prediction and plotting: plot_dhm_long

# Load final model for seedling density 
load("./output/seedling_density_final_model.Rdata")

# Refit this model using the untransformed data for plotting
pltd <- lmer(formula(seedling_density_final_model), data = plot_dhm) # assign model name for compatibilit with plotting code


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


##### year by planted by shrub -----------------------------------------------

# average value for other variables in the model
#Shrubs_mean <- mean(plot_dhm$Shrubs)
#ShrubHt_mean <- mean(plot_dhm$ShrubHt)
log10SeedWallConifer_mean = median(plot_dhm$log10SeedWallConifer)
tmin_mean <- median(plot_dhm$tmin)
normal_annual_precip_mean <- median(plot_dhm$normal_annual_precip)
#LitDuff_mean <- median(plot_dhm$LitDuff)
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
                      #LitDuff = LitDuff_mean,
                      tpi2000 = tpi2000_mean,
                      elev = elev_mean,
                      Fire = "hypothetical",
                      PairID = "hypothetical")


year.labs <- c(c("One year", "Two years", "Three years"))
names(year.labs) <- c(1,2,3)


predicted_pltd.pys <- cbind(predict_pltd.pys, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.pys, re.form = NA, level = 0, type="response", se.fit = TRUE)))


plantedYearShrubs <- ggplot(data = predicted_pltd.pys, 
                            aes(y = (exp(fit)-24.99), x = Shrubs, 
                            color = fsplanted, fill = fsplanted)) +
        
  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), alpha = .50, color = NA) +
  geom_line(show.legend = FALSE, linewidth = 2) +
  scale_color_manual(values = c("#4f5a3a","#807561")) +
  scale_fill_manual(values = c("#98ab8f","#d1cdb6")) +
  ylab("Seedling Density \n(indv/ha)") +
  xlab("Shrub Cover (%)") +
  xlim(20,100) +
  ylim(-10,325) +
  facet_wrap(~facts.planting.first.year, strip.position = "top", labeller = labeller(facts.planting.first.year = year.labs)) +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "transparent", color = "transparent"), 
        text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top", legend.title=element_blank(),
        panel.spacing.x = unit(.11, "lines"),
        panel.spacing.y = unit(.6, "lines"))
ggsave(plantedYearShrubs, file="figures/manuscript_resub/plantedYearShrubs.pdf", width=6.5, height=4.5)

  

##### tmin by annual precip -----------------------------------------------


# median value for other variables in the model
Shrubs_median <- median(plot_dhm$Shrubs)
facts.planting.first.year_median <- 2
log10SeedWallConifer_median = median(plot_dhm$log10SeedWallConifer)
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
  geom_line(show.legend = FALSE, linewidth = 1.5, color = "#6D7274") +
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
ggsave(tminPrecip , file="figures/manuscript_resub/tminPrecip.pdf", width=6.5, height=4.5)


##### Seed wall  -----------------------------------------------
  
# median value for other variables in the model
tmin_median <- median(plot_dhm$tmin)
normal_annual_precip_median <- median(plot_dhm$normal_annual_precip)
Shrubs_median <- median(plot_dhm$Shrubs)
facts.planting.first.year_median <- 2
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
                                tpi2000 = tpi2000_median,
                                elev = elev_median,
                                Fire = "hypothetical",
                                PairID = "hypothetical")


predicted_pltd.sw <- cbind(predict_pltd.sw, as.data.frame(
  predictSE(pltd, newdata = predict_pltd.sw, re.form = NA, level = 0, type="response", se.fit = TRUE)))

seedw <- ggplot(data = predicted_pltd.sw, 
                 aes(y = exp(fit)-24.99, x = 10^(log10SeedWallConifer),linetype = fsplanted)) +
  geom_ribbon(aes(ymax = (exp(fit + se.fit)-24.99), ymin=(exp(fit - se.fit)-24.99)), 
              alpha = .15, color = NA, fill = "#756156", show.legend = FALSE) +
  geom_line(show.legend = FALSE, linewidth = 2, color = "#756156") +
  ylab("Seedling Density \n(indv./ha)") +
  xlab("Minimum distance from seed source (m)") +
  #ylim(-10, 230) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(seedw , file="figures/manuscript_resub/seedw.pdf", width=3.25, height=3.45)

##### tpi elevation -----------------------------------------------

tmin_median <- median(plot_dhm$tmin) 
normal_annual_precip_median <- median(plot_dhm$normal_annual_precip) 
Shrubs_median <- median(plot_dhm$Shrubs)
facts.planting.first.year_median <- 2
log10SeedWallConifer_median = median(plot_dhm$log10SeedWallConifer)

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
  scale_color_manual(values = c("#48281E","#4C6B6A", "#C8A104")) +
  scale_fill_manual(values = c("#48281E","#4C6B6A", "#C8A104")) +
  ylab("Seedling Density \nindv./ha)") +
  xlab("Topographic position index") +
  #ylim(-40, 1600) +
  facet_wrap(~elev, strip.position = "top", labeller = labeller(elev = elev.labs)) +
  theme(strip.placement = "outside", 
        strip.background = element_rect(fill = "transparent", color = "transparent"), 
        text = element_text(size = 12), 
        axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
        panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
        legend.position = "top", 
        panel.spacing.x = unit(.11, "lines"),
        panel.spacing.y = unit(.6, "lines"))

ggsave(tpiElev , file="figures/manuscript_resub/tpiElev.pdf", width=3.25, height=3.45)

plot(plot_dhm$normal_annual_precip, plot_dhm$tmin)


##### Fire table ----------------------------------------------------------------------------------

Fire.Table.Median <- plot_dhm %>% 
  group_by(Fire) %>%
  summarize(ln.dens.planted = median(ln.dens.planted),
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
            slope_dem =  median(slope_dem)
  ) %>%
  mutate(Type = "Median")

Fire.Table.Minimum <- plot_dhm %>% 
  group_by(Fire) %>%
  summarize(ln.dens.planted = min(ln.dens.planted),
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
  summarize(ln.dens.planted = max(ln.dens.planted),
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
  arrange(Fire, Type) %>%
  select(Fire, Type, ln.dens.planted, fire_year, normal_annual_precip, tmin, tmean, tmax, bcm_snowpack, rad_winter, rad_winter_spring, 
         rad_spring, rad_spring_summer, rad_summer, aspect_dem, slope_dem, elev, tpi100, tpi500, tpi2000, tpi5000, twi, 
         log10SeedWall, Forbs, Grasses, Shrubs, ShrubHt, CWD_sound, CWD_rotten, LitterDepth, DuffDepth, LitDuff,
         facts.planting.first.year)
                                              

write.csv(Fire.Table, file = "./tables/Fire_Table.csv")

Fire.Table.1 <- Fire.Table %>% select(Fire, fire_year, elev, facts.planting.first.year)

plantinglisttable <- plot_dhm %>% select(Fire, plantingList) %>% distinct()


##########################################################################################
#                              Composition Figures 
##########################################################################################

# Filter data to only plots containing conifers
plot_comp <- plot_dhm %>% filter(dens.conif != 0)

# Create count columns (convert tree density to counts)
plot_comp <- mutate(plot_comp, 
                            seedling_count = round(plot_comp$dens.conif/24.94098, 0), 
                            pine_count = round(plot_comp$dens.pine/24.94098, 0))


# Load the final proportion shrubs model 
load("./output/proportion_pines_final_model.Rdata")

# Refit this model using the untransformed data for plotting
pine.comp <- glmer(formula(pines_final_model), data = plot_comp, family = "binomial") # assign model name for compatibilit with plotting code


#normal_annual_precip_median <- median(plot_comp$normal_annual_precip) 
twi_median <- median(plot_comp$twi)
facts.planting.first.year_median <- 2
Shrubs_median <- median(plot_comp$Shrubs)
ShrubHt_median <- median(plot_comp$ShrubHt)

#normal_annual_precip_levels <- (seq(from=min(plot_comp$normal_annual_precip),to=max(plot_comp$normal_annual_precip),length.out=701))
#LiveOverstory_levels <- (seq(from=min(plot_comp$LiveOverstory, na.rm = TRUE),to=max(plot_comp$LiveOverstory, na.rm = TRUE),length.out=701))
twi_levels <- (seq(from=min(plot_comp$twi),to=max(plot_comp$twi),length.out=701))
facts.planting.first.year_levels <- (seq(from=1,to=3,length.out=701))
#Forbs_levels <- (seq(from=min(plot_comp$Forbs),to=max(plot_comp$Forbs),length.out=701))
Shrubs_levels <- (seq(from=min(plot_comp$Shrubs),to=max(plot_comp$Shrubs),length.out=701))
ShrubHt_levels <- seq(from = min(plot_comp$ShrubHt), to = max(plot_comp$ShrubHt))
fsplanted_levels <- c("planted","unplanted")



##### twi -----------------------------------------------------------------------------------------

predict_comp.twi <- expand.grid(twi = twi_levels, 
                                 facts.planting.first.year = facts.planting.first.year_median,
                                 Shrubs = Shrubs_median,
                                 ShrubHt = ShrubHt_median,
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
ggsave(twi_comp, file="figures/manuscript_resub/twi_comp.pdf", width=3.25, height=3.45)

##### Shrubs -----------------------------------------------------------------------------------------

predict_comp.shrubs <- expand.grid(Shrubs = Shrubs_levels, 
                                facts.planting.first.year = facts.planting.first.year_median,
                                twi = twi_median,
                                ShrubHt = ShrubHt_median,
                                fsplanted = fsplanted_levels,
                                Fire = "hypothetical",
                                PairID = "hypothetical")



predicted_comp.shrubs <- cbind(predict_comp.shrubs, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.shrubs, re.form = NA, level = 0, type="response", se.fit = TRUE)))


Shrubs_comp <- ggplot(data = predicted_comp.shrubs, aes(y =  fit, x = Shrubs, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .15, color = NA, fill = "#98ab8f", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#98ab8f") +
  #scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  #scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Proportion Pine") +
  xlab("Percent Cover of Shrubs") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(twi_comp, file="figures/manuscript_resub/shrubs_comp.pdf", width=3.25, height=3.45)


##### Shrub Height -----------------------------------------------------------------------------------------


predict_comp.shrubht <- expand.grid(ShrubHt = ShrubHt_levels, 
                                   facts.planting.first.year = facts.planting.first.year_median,
                                   twi = twi_median,
                                   Shrubs = Shrubs_median,
                                   fsplanted = fsplanted_levels,
                                   Fire = "hypothetical",
                                   PairID = "hypothetical")



predicted_comp.shrubht <- cbind(predict_comp.shrubht, as.data.frame(
  predictSE(pine.comp, newdata = predict_comp.shrubht, re.form = NA, level = 0, type="response", se.fit = TRUE)))


ShrubHt_comp <- ggplot(data = predicted_comp.shrubht, aes(y =  fit, x = ShrubHt, linetype = fsplanted)) +
  #geom_point(data = plot_dhm, aes(y=obs_resid.sw, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  #geom_point(data = plot_dhm %>% filter(fsplanted == "unplanted"), aes(y=dens.planted, x = 10^(log10SeedWallConifer)), color = "#696030", show.legend = FALSE) +
  geom_ribbon(aes(ymax = fit + se.fit, ymin=fit - se.fit), 
              #geom_ribbon(aes(ymax = ((fit + se.fit)*24.99), ymin=((fit - se.fit)*24.99)), 
              alpha = .15, color = NA, fill = "#4f5a3a", show.legend = FALSE) +
  geom_line(show.legend = FALSE, size = 2, color = "#4f5a3a") +
  #scale_color_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  #scale_fill_manual(values = c("#8FBEDC","#508AA8", "#477998")) +
  ylab("Proportion Pine") +
  xlab("Shrub Height (cm)") +
  #ylim(-10, 75) +
  theme( text = element_text(size = 12), 
         axis.text.y = rotatedAxisElementText(angle = 90, position = "y", Size = 10,  Color = "black"), #requires the function to be run at the head of this script
         panel.background = element_rect(fill = NA, color = "black"), panel.grid = element_blank(), 
         legend.position = "top")
ggsave(twi_comp, file="figures/manuscript_resub/shrubht_comp.pdf", width=3.25, height=3.45)



##### planting year -------------------------------------------------------------------------------

predict_comp.year <- expand.grid(normal_annual_precip = normal_annual_precip_median,
                                 LiveOverstory = LiveOverstory_median, 
                                 twi = twi_median, 
                                 facts.planting.first.year = facts.planting.first.year_levels,
                                 Shrubs = Shrubs_median,
                                 ShrubHt = ShrubHt_median,
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
ggsave(year_comp, file="figures/manuscript_resub/year_comp.pdf", width=3.25, height=3.45)

