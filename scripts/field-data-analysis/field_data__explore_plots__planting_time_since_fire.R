library(tidyverse)
library(lme4)
library(ggplot2)
library(gridExtra)



load("output/plotSeedlingData.RData") #load R object: plot_dhm_long


##### Create function to plot vars and apply to all var combinations -------------------------------------------------

enviroPlots <- function(met, enviro, trans = "none") {
  assign(paste0(enviro, ".plot.", if (trans == "log") { paste0("log.", met) } else { met }), 
         ggplot(plot_dhm_long %>% filter(met.type == met), 
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
enviroVars <- c("elev", "rad_march", "slope_dem", "normal_annual_precip") #environmental vars
transVars <- c("log", "none") # with and with log transformation
allVars <- as_tibble(qpcR:::cbind.na(metVars,enviroVars,transVars))
allVars <- allVars %>% tidyr::expand(metVars, enviroVars, transVars) %>% drop_na() # create dataframe with all combination of variables

mapply(enviroPlots, allVars$metVars, allVars$enviroVars, allVars$transVars) #apply all combinations to graphing function... damn, thats a lot of graphs...

cor(plots %>% dplyr::select(elev, rad_march, slope_dem, normal_annual_precip),  use = "complete.obs", method = "pearson")




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

## plot distribution of response vars by fire and treated status
explore <- ggplot(plot_dhm_long, aes(x=interaction(fsplanted, Fire), y=log(value+1),color=fsplanted)) +
  geom_violin(size = 1) +
  #geom_boxplot(width=0.1) +
  #geom_jitter(width = .02, alpha = .25) +
  geom_dotplot(binaxis='y', method = "histodot", stackdir='center',binwidth = .2, dotsize=.7,  aes(x = interaction(fsplanted, Fire), fill = Type)) +
  #geom_point(position=position_jitterdodge(dodge.width=0.5,jitter.width=0.15),size=3) +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw(12) +
  facet_wrap(~metric,scales="free")
ggsave(explore, file = "C:/Users/Quinn Sorenson/Desktop/lotsOplots.pdf", width = 60, height = 40, limitsize = FALSE)

