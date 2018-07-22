setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(sf)
library(tidyverse)

source("scripts/site-selection/st_rbind_all.R")

## open the relevant sets of selected plots

a <- st_read("data/site-selection/output/selected-plots/moontelope_v2.gpkg")
b <- st_read("data/site-selection/output/selected-plots/moontelope_v3_priHomogeneity.gpkg")

done <- read.csv("data/site-selection/analysis-parameters/plots_surveyed.csv",header=TRUE,stringsAsFactors=FALSE)

## open the list of completed plots

p <- st_rbind_all(a,b)

p = p %>%
  filter(plot.id.gps %in% done$plot)

## save to file
st_write(p,"data/site-selection/output/surveyed-plots/moontelope.gpkg")

## plot them

yr.colors <- c("0" = "black","1" = "darkolivegreen3", "2" = "cornflowerblue", "3" = "darkorange1", "4+" = "brown3")
ggplot(p,aes(x=elev,y=rad,color=yr.pltd,shape=dist.nonhigh)) +
  geom_point(size=1.5) +
  ggtitle("2007MOONTELOPE") +
  theme_bw(16) +
  theme(plot.title = element_text(size=12)) +
  facet_wrap(~mgmt.factorial.nofire) +
  scale_shape_manual(values=c(16,1)) +
  scale_colour_manual(values=yr.colors) +
  theme(strip.text.x = element_text(size = 12)) +
  labs(color="Yr planted",shape="Seed dist")
