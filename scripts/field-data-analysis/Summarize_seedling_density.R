# Calculate seedling densities by functional group and treatment

library(tidyverse)

# Load R object: plot_dhm_long
load("output/plotSeedlingData.RData") 

names(plot_dhm)

# Summarize seedling densities by functional group

density_summary_allfires <- plot_dhm %>% 
  select(Fire, fsplanted, dens.pine, dens.shadeTol, dens.yelPine) %>%
  split(.$fsplanted) %>% 
  map(summarise,
      n = n(), 
      treatment = first(fsplanted), 
      mean.dens.pine = mean(dens.pine),
      sd.dens.pine = sd(dens.pine),
      mean.dens.shadeTol = mean(dens.shadeTol),
      sd.dens.shadeTol = sd(dens.shadeTol),
  ) %>% # combine the results
  bind_rows()

names(density_summary_allfires)[1] <- "Fire"

# Summarize seedling densities by functional group and Fire

density_summary <- plot_dhm %>% 
  select(Fire, fsplanted, dens.pine, dens.shadeTol, dens.yelPine) %>%
  group_by(.$Fire) %>%
  split(.$fsplanted) %>% 
  map(summarise,
    n = n(), 
    treatment = first(fsplanted), 
    mean.dens.pine = mean(dens.pine),
    sd.dens.pine = sd(dens.pine),
    q10.dens.pine = quantile(dens.pine, 0.1),
    q90.dens.pine = quantile(dens.pine, 0.9),
    mean.dens.shadeTol = mean(dens.shadeTol),
    sd.dens.shadeTol = sd(dens.shadeTol),
    q10.dens.shadeTol = quantile(dens.shadeTol, 0.1),
    q90.dens.shadeTol = quantile(dens.shadeTol, 0.9),
  ) %>% # combine the results
  bind_rows()

names(density_summary)[1] <- "Fire"


# Make plot from the data frame density_summary, showing the mean density plus error bars for the quantiles


pine_density_plot <- ggplot() + 
  stat_summary(data = density_summary, aes(x = Fire, y = mean.dens.pine, fill = treatment), fun = mean, geom = "bar", position = "dodge") +
  geom_errorbar(data = density_summary, aes(x = Fire, group = treatment, ymin = q10.dens.pine, ymax = q90.dens.pine), position = position_dodge(width = 0.9), width = 0.25) + 
  labs(title = "a) Pines",
       x = "Fire",
       y = "Density (seedlings per hectare)") +
  theme_minimal() +  
  theme(legend.position = "none", legend.title = element_blank()) + 
  scale_fill_manual(values = c("darkgreen", "gold"))

shadeTol_density_plot <- ggplot() + 
  stat_summary(data = density_summary, aes(x = Fire, y = mean.dens.shadeTol, fill = treatment), fun = mean, geom = "bar", position = "dodge") +
  geom_errorbar(data = density_summary, aes(x = Fire, group = treatment, ymin = q10.dens.shadeTol, ymax = q90.dens.pine), position = position_dodge(width = 0.9), width = 0.25) + 
  labs(title = "b) Shade-tolerant trees",
       x = "Fire",
       y = "Density (seedlings per hectare)") +
  theme_minimal() +  
  theme(legend.position = "none", legend.title = element_blank()) + 
  scale_fill_manual(values = c("darkgreen", "gold"))

grid.arrange(pine_density_plot, shadeTol_density_plot, ncol = 1)




  