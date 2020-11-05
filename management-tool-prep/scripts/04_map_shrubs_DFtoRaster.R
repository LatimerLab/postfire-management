setwd("~/repos/postfire-management")

library(tidyverse)
library(raster)
library(sf)
library(lme4)

pred_w_xy = readRDS("management-tool-prep/data/non-synced/intermediate/shrub_pred_df.rds")

pred_raster = rasterFromXYZ(pred_w_xy)
gc()

# Open another raster (env predictor for shrub) to get projection
ppt = raster("management-tool-prep/data/non-synced/intermediate/ppt.tif")

projection(pred_raster) = projection(ppt)

## cap at 1.57 so the continuous predictions of cover that go above 1.57 don't result in < 100% cover
pred_raster[pred_raster > 1.57] = 1.57

pred_raster = ((pred_raster %>% sin())^2)*100
gc()

writeRaster(pred_raster,"management-tool-prep/data/non-synced/intermediate/shrub.tif", overwrite=TRUE)
