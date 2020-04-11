library(raster)
library(dplyr)
library(sf)

setwd("~/repos/postfire-management")


#### Load data ####

# TopoWx temperature
tmax = stack("data/non-synced/existing-datasets/topowx_temerature/tmax_normal/normals_tmax.nc") %>% mean()
tmin = stack("data/non-synced/existing-datasets/topowx_temerature/tmin_normal/normals_tmin.nc") %>% mean()

# TopoWx DEM
coarse_dem = raster("data/non-synced/existing-datasets/DEM/dem_prism_800m.tif")

# 30m DEM
fine_dem = raster("data/non-synced/existing-datasets/DEM/CAmerged15.tif")

# Focal region
focal_region = st_read("management-tool-prep/data/focal-region/focal-region.geojson") %>% st_transform(3310) %>% st_buffer(10000) %>% st_transform(crs(fine_dem))

#### Prep data ####

## Clip rasters to focal region extent + 10 km

tmax = crop(tmax,focal_region)
tmax = mask(tmax,focal_region)
tmax = projectRaster(tmax,coarse_dem)

tmin = crop(tmin,focal_region)
tmin = mask(tmin,focal_region)
tmin = projectRaster(tmin,coarse_dem)

tmim_proj = projectRaster(tmin,coarse_dem)

fine_dem = crop(fine_dem,focal_region)
fine_dem = mask(fine_dem,focal_region)

coarse_dem = crop(coarse_dem,focal_region)
coarse_dem = mask(coarse_dem,focal_region)


#### Clip to small test region ####

subset_grid = st_make_grid(focal_region %>% st_transform(3310),
                           cellsize = 10000) %>% st_transform(crs(tmax))

focal_cell = subset_grid[798] 

tmax_focal = crop(tmax,focal_cell %>% as("Spatial"))
tmin_focal = crop(tmin,focal_cell %>% as("Spatial"))
fine_dem_focal = crop(fine_dem,focal_cell %>% as("Spatial"))

###!!! the coarse one needs to be buffered wider: at least the set buffer distance
coarse_dem_focal = crop(coarse_dem,focal_cell %>% as("Spatial"))


#### Start analysis ####

buffer = 2000

## fine raster to points
fine_points = rasterToPoints(fine_dem_focal, spatial=TRUE) %>% as("sf") %>% rename(elev = "CAmerged15") %>% st_transform(3310)

## stack coarse climate var and coarse elev, then turn to points
coarse_clim_elev = stack(tmax_focal, coarse_dem_focal)
coarse_clim_elev_points = rasterToPoints(coarse_clim_elev, spatial=TRUE) %>% as("sf") %>% st_transform(3310)
names(coarse_clim_elev_points) = c("clim","elev","geometry")

## which coarse points are in the buffer of each fine point?
fine_points_buffer =  st_buffer(fine_points,buffer)
coarse_intersecting_fine = st_intersects(fine_points_buffer,coarse_clim_elev_points) ## first index is for each fine point; second is for each coarse point


## good fine point: 98000

## For each fine point

fine_points$clim = NA

for(i in 1:nrow(fine_points)) {

  #get the contributing coarse points

  coarse_within_buffer = coarse_clim_elev_points[coarse_intersecting_fine[[i]],]
  
  
  ## fit regression model
  m = lm(clim ~ elev, data = coarse_within_buffer)
  
  
  ##!!! Potentially need Flint bullseye reduction here
  
  clim_pred = predict(m,newdata = data.frame(fine_points[i,"elev"]))
  
  fine_points[i,"clim"] = clim_pred
  
  cat("\r   Finished point:",i)

}

fine_points_coords = st_coordinates(fine_points)

fine_points_xyz = data.frame(x = fine_points_coords[,1], y = fine_points_coords[,2], clim = fine_points$clim)

fine_points_rast = fine_dem_focal
a = setValues(fine_dem_focal, fine_points$clim)
  
  raster(fine_dem_focal,vals=fine_points$clim)
  
  
  rasterFromXYZ(fine_points_xyz)



