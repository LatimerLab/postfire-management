library(tidyverse)

load("output/plotSeedlingData.RData") #load R object: plot_dhm_long
#old not downscaled data here: plotSeedlingData_old_not_downscaled.RData

plot_plot = plot_dhm_long %>%
  filter(Type == "treatment",
         metric == "dens.all") %>%
  mutate(salv = (facts.salvage == "yes"),
         prep = (facts.prep.nyears > 0),
         thin = facts.thin.years.post != "",
         release = facts.release.years.post != "",
         replant = facts.release.years.post != "",
         plt_yrs_post = facts.planting.first.year) %>%
  mutate(salv = ifelse(salv,"Salvaged","Unsalvaged"),
         release = ifelse(release,"Released","Unreleased"),
         replant = ifelse(replant,"Replanted","Not replanted")) %>%
  mutate(Fire = recode(Fire,Ctnwd = "Cottonwood",
                       MoonAnt = "Moonlight-Antelope",
                       AmRiv = "American River")) %>%
  mutate(FireSalvRel = paste(Fire,salv,release,replant,sep="\n")) %>%
  mutate(plt_yrs_post = as.factor(plt_yrs_post))


p = ggplot(plot_plot,aes(x = normal_annual_precip,
                     y = rad_spring,
                     color = plt_yrs_post)) +
  geom_point() +
  facet_wrap(~FireSalvRel,scales="free",ncol=2) +
  scale_color_discrete(name="Years following\nfire planted") +
  labs(x = "Normal annual precipitation (mm)",
       y = "Spring solstice insolation (Wh m-2 day-1)") +
  theme_bw()

png("figures/manuscript/management_and_env_variation.png",width = 1000,height=1500,res=200)
p
dev.off()

## for summary table

%?%
  group_by(Fire,salv,release,plt_yrs_post) %>%
  summarize(nplots = n()) %>%
  ungroup() %>%


         