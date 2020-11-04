library(tidyverse)

plots = read.csv("data/field-processed/compiled-processed/plots_w_gis_data_newTWI.csv",stringsAsFactors=FALSE)

density = read.csv("data/intermediate/density_per_plot.csv", stringsAsFactors=FALSE) %>%
  rename("planted_density_tpa" = mean_planted_density)

plots = left_join(plots,density,by="PlotID")

plots[plots$Type == "control","planted_density_tpa"] = 0

write.csv(plots,"data/field-processed/compiled-processed/plots_w_gis_data_newTWI.csv", row.names=FALSE)

plots_check = plots %>%
  select(PlotID,Fire,planted_density_tpa)
