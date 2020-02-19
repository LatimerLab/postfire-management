setwd("~/projects/Post-fire management/postfire-management")

library(tidyverse)
library(lme4)
library(sf)
library(raster)

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

test <- plot_dhm %>% dplyr::select(Fire, facts.planting.first.year)
unique(test)

##### final models for planted species and conifers --------------------------------------------------------------------------------

#USE THIS MODEL FOR THE TOOL
pltd <- lmer(ln.dens.planted ~ scale(tpi2000)*facts.planting.first.year + 
               scale(asin(sqrt(Shrubs/100)))*facts.planting.first.year*fsplanted +
               scale(tmean)*scale(normal_annual_precip) +
               neglog5SeedWallConifer + scale(ShrubHt) +
               (1|Fire) + (1|Fire:PairID), data = plot_dhm)

#### histograms of the predictors ####

hist(plot_dhm$neglog5SeedWallConifer,nclass = 20)


#### Predict to a landscape ####

## trial landscape subset ##
region = st_read("management-tool/data/focal-region/focal-region.geojson")

tpi = raster("management-tool/data/non-synced/tpi2000.tif")
ppt = raster("management-tool/data/non-synced/ppt.tif")
tmean = raster("management-tool/data/non-synced/tmean.tif")

env = stack(tpi,ppt,tmean*100)
enc = crop(env,region %>% st_transform(projection(env)))
enc = mask(enc,region %>% st_transform(projection(env)))


env_crop = crop(env,region %>% st_transform(projection(env)))

writeRaster(enc,"management-tool/data/non-synced/preds_stack.tif",overwrite=TRUE, datatype="INT2S", options="COMPRESS=LZW")   ##738

env2 = raster("management-tool/data/non-synced/preds_stack.tif")
env_crop = crop(env,region %>% st_transform(projection(env)))



### Extract raster cells to data frame ###

env_df = as.data.frame(env,xy=TRUE)

env_df = env_df %>%
  mutate(planting_year = 1,
         planted = FALSE,
         seedwall = mean(plot_dhm$neglog5SeedWallConifer),
         Shrubs = asin(sqrt(mean(plot_dhm$Shrubs/100))),
         ShrubHt = mean(plot_dhm$ShrubHt))

env_df = env_df %>%
  rename(tpi2000 = tpi2000,
         normal_annual_precip = ppt,
         tmean = tmean,
         Shrubs = Shrubs,
         ShrubHt = ShrubHt,
         neglog5SeedWallConifer = seedwall,
         facts.planting.first.year = planting_year,
         fsplanted = planted)




####

preds = predict(pltd,env_df,re.form=NA)

env_df$preds = preds

pred_raster = rasterFromXYZ(env_df %>% dplyr::select(x,y,preds))

plot(pred_raster)






