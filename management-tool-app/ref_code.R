### Extract raster cells to data frame ###

env_df = as.data.frame(enc,xy=TRUE)

write.csv(env_df,"management-tool/data/non-synced/preds_stack.csv")


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




#### Make predictions from model ####

preds = predict(pltd,env_df,re.form=NA)

env_df$preds = preds

pred_raster = rasterFromXYZ(env_df %>% dplyr::select(x,y,preds))

plot(pred_raster)

