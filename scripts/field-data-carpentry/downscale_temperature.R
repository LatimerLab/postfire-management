library(raster)
library(dplyr)
library(sf)
library(purrr)
library(furrr)

# setwd("~/repos/postfire-management")

#### Load data ####

# TopoWx temperature
clim = stack("~/projects/temp_downscale/normals_tmax.nc") %>% mean()
#tmin = stack("data/non-synced/existing-datasets/topowx_temerature/tmin_normal/normals_tmin.nc") %>% mean()

# TopoWx DEM
coarse_dem = raster("~/projects/temp_downscale/dem_prism_800m.tif")

# 30m DEM to downscale to
fine_dem = raster("~/projects/temp_downscale/CAmerged15.tif")

# Focal region
focal_region = st_read("~/projects/temp_downscale/focal-region.geojson") %>% st_transform(crs(fine_dem))


#### Prep data ####

buffer = 2000 # how far beyond focal fine-cell to look for coarse-cells to contribute to lapse rate regression

focal_region_buffer = focal_region %>% st_transform(3310) %>% st_buffer(buffer*1.2)

## Clip rasters to focal region extent (+ buffer dist for coarse layers)

clim = crop(clim,focal_region_buffer)
clim = mask(clim,focal_region_buffer)
clim = projectRaster(clim,coarse_dem)

coarse_dem = crop(coarse_dem,focal_region_buffer)
coarse_dem = mask(coarse_dem,focal_region_buffer)

# tmin = crop(tmin,focal_region)
# tmin = mask(tmin,focal_region)
# tmin = projectRaster(tmin,coarse_dem)

fine_dem = crop(fine_dem,focal_region)
fine_dem = mask(fine_dem,focal_region)
# code NA as -9999
fine_dem[is.na(fine_dem)] = -999



#### Prep for splitting into tiles to parallelize over ####


subset_grid = st_make_grid(focal_region %>% st_transform(3310),
                           cellsize = 10000) %>% st_transform(crs(clim))

############ focal_cell = subset_grid[787] # good one is 798. or 787-789

#### Build lists of tiles to parallelize over ####
## takes about 2 min

coarse_clim_list = list()
coarse_dem_list = list()
fine_dem_list = list()

nlayers = 1

for(i in 1:length(subset_grid)) {

  focal_cell = subset_grid[i]
  focal_cell_w_buffer = focal_cell %>% st_transform(3310) %>% st_buffer(buffer*1.2) %>% st_transform(crs(clim))
  
  # if the cell is completely outside the region of fine points, skip it
  fine_dem_extent = extent(fine_dem)%>% as("SpatialPolygons") %>% as("sf")
  st_crs(fine_dem_extent) = st_crs(fine_dem)
  if(st_intersects(fine_dem_extent %>% st_transform(3310),focal_cell %>% st_transform(3310), sparse=FALSE)[1,1] == FALSE) {
    next()
  }
  
  coarse_clim_focal = crop(clim,focal_cell_w_buffer %>% as("Spatial"))
  coarse_dem_focal = crop(coarse_dem,focal_cell_w_buffer %>% as("Spatial"))
  fine_dem_focal = crop(fine_dem,focal_cell %>% as("Spatial"))
  
  ## if all points are NA (-999) (i.e., outside of focal region), skip it
  if(sum(values(fine_dem_focal) == -999) == ncell(fine_dem_focal)) {
    next()
  }
  
  coarse_clim_list[[nlayers]] = coarse_clim_focal
  coarse_dem_list[[nlayers]] = coarse_dem_focal
  fine_dem_list[[nlayers]] = fine_dem_focal
  
  nlayers = nlayers + 1
  
  cat("\rMade tile",i,"of",length(subset_grid))
  
}




#### Start analysis ####



i = 1

fine_dem_focal = fine_dem_list[[i]]
coarse_clim_focal = coarse_clim_list[[i]]
coarse_dem_focal = coarse_dem_list[[i]]


downscale_tile = function(coarse_clim_focal,coarse_dem_focal,fine_dem_focal) {
  
  ## fine raster to points
  fine_points = rasterToPoints(fine_dem_focal, spatial=TRUE) %>% as("sf") %>% rename(elev = "layer") %>% st_transform(3310)
  
  
  ## if all fine points are empty, return a NA raster tile
  if(sum(fine_points$elev == -999) == nrow(fine_points)) {
    clim_ds_rast_tile = setValues(fine_dem_focal, NA)
    return(clim_ds_rast_tile)
  }
  
  ## stack coarse climate var and coarse elev, then turn to points
  coarse_clim_elev = stack(coarse_clim_focal, coarse_dem_focal)
  coarse_clim_elev_points = rasterToPoints(coarse_clim_elev, spatial=TRUE) %>% as("sf") %>% st_transform(3310)
  names(coarse_clim_elev_points) = c("clim","elev","geometry")
  
  ## which coarse points are in the buffer of each fine point?
  fine_points_buffer =  st_buffer(fine_points,buffer)
  coarse_intersecting_fine = st_intersects(fine_points_buffer,coarse_clim_elev_points) ## first index is for each fine point; second is for each coarse point
  
  ## distance from each fine point to each coarse point
  fine_to_coarse_dist = st_distance(fine_points,coarse_clim_elev_points)
  
  ## good fine point for 798: 98000
  
  ## For each fine point
  fine_points$clim = NA
  
  for(i in 1:nrow(fine_points)) {
    
    cat("\rDownscaling fine point",i,"of",nrow(fine_points))
    
    # if fine point elev is NA (outside climate zone), return NA cliamte
    if(fine_points[i,]$elev == -999) {
      fine_points[i,"clim"] = NA
      next()
    }
    
    # get elevation of fine point
    fine_point_elev = fine_points[i,"elev"]
    fine_point_coords = st_coordinates(fine_points[i,])
    st_geometry(fine_point_elev) = NULL
    fine_point_elev = fine_point_elev %>% as.numeric()
    fine_point_x = fine_point_coords[,1]
    fine_point_y = fine_point_coords[,2]
    
    # get the contributing coarse points
    coarse_within_buffer = coarse_clim_elev_points[coarse_intersecting_fine[[i]],]
    coarse_within_buffer_coords = st_coordinates(coarse_within_buffer)
    coarse_within_buffer$x = coarse_within_buffer_coords[,1]
    coarse_within_buffer$y = coarse_within_buffer_coords[,2]
  
    ## fit regression model and extract coefs (lapse rate)
    m = lm(clim ~ elev + x + y, data = coarse_within_buffer)
    lapse_rate = coef(m)["elev"]
    x_coef = coef(m)["x"]
    y_coef = coef(m)["y"]
    
    ## for each coarse cell, predict climate at elevation of fine point
    # elev diff between fine point and each coarse point
    elev_diff = fine_point_elev - coarse_within_buffer$elev
    x_diff = fine_point_x - coarse_within_buffer$x
    y_diff = fine_point_y - coarse_within_buffer$y
    # climate difference
    clim_diff = lapse_rate * elev_diff + x_diff * x_coef + y_diff * y_coef
    # climate prediction based on each coarse point
    clim_pred = coarse_within_buffer$clim + clim_diff
    # distance to each coarse point
    coarse_dist = fine_to_coarse_dist[i,coarse_intersecting_fine[[i]]] %>% as.numeric()
    #old code; distance now computed outside of loop then looked up with the code on the previous line.    dists = st_distance(fine_points[i,], coarse_within_buffer) %>% as.numeric()
    # create inverse-distance-squared-weighted average of clim preds
    ids = 1/(coarse_dist^2)
    mean_clim_pred = weighted.mean(clim_pred,ids)
  
    ##!!! Potentially need Flint bullseye reduction
  
    fine_points[i,"clim"] = mean_clim_pred
    
  }
  
  cat(" -- finished tile\n")
  
  clim_ds_rast_tile = setValues(fine_dem_focal, fine_points$clim)
  
  return(clim_ds_rast_tile)
  
}


plan(multiprocess)
tiles = future_pmap(.l = list(coarse_clim_list[1:8],coarse_dem_list[1:8],fine_dem_list[1:8]), .f = downscale_tile)

merged = do.call(merge, tiles)

writeRaster(merged,"~/projects/temp_downscale/merged_raster_output.tif")