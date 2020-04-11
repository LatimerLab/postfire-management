library(raster)
library(rgeos)
library(dplyr)
library(sf)
library(rgeos)

setwd("~/repos/postfire-management")

default.proj <- CRS("+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# layer is the climate layer you want to downscale (a rasterLayer)
# dem is the elevation layer that the climate layer was built on (a rasterLayer), with elevation in units that are the same as the map units of "proj"
# points is a SpatialPointsDataFrame of the points you want downscaled values for, and which includes the attribute "elev" in units that are the same as the map units of "proj"
# res is the resolution of the supplied climate layer
gids.spatial <- function(layer,dem,dem_highres,points,res,proj=default.proj) {

  cat("\nDownscaling layer ",names(layer),".\n",sep="")
  
  # search radius for cells to include in the regression. This radius is approx. equal to that used by Flint et al. 2013
  radius <- 2*res
  
  if(projection(dem) != projection (layer)) {
    stop("The supplied climate layer is not in the same projection as the supplied DEM. Supply these layers in the same projection.")
  }
  
  resdiff <- mean(res(dem)) / mean(res(layer))
  if((resdiff > 1.01) | (resdiff < 0.99)) {
    print(resdiff)
    stop("The supplied climate layer is not the same resolution as the supplied DEM. These layers should be from the same dataset.")
  }
  
  layer <- crop(layer,dem) ## this line is new
  dem <- crop(dem,layer) ## this line is new
  dem <- mask(dem,layer)
  stack <- stack(dem,layer)
  names(stack) <- c("dem","layer")
  
  
  points.proj <- spTransform(points,proj)
  
  if(is.null(points.proj$elev)) {
    points.proj$elev = extract(dem_highres,points.proj,method="bilinear")
  }
  
  points.proj.buffer <- gBuffer(points.proj,byid=TRUE,width=radius)
  
  points.proj.buffer.geo = spTransform(points.proj.buffer,proj4string(dem))
  
  npts = nrow(points.proj)
  
  ds.vals = rep(NA,npts)
  
  for(i in 1:npts) {

    point.proj <- points.proj[i,]
    point <- points[i,]
    point.proj.buffer <- points.proj.buffer[i,]
    point.proj.buffer.geo <- points.proj.buffer.geo[i,]
    
    stack.crop = crop(stack,point.proj.buffer.geo)
    
    
    layer.pts <- rasterToPoints(stack.crop,spatial=TRUE) #turn raster into points
    layer.pts.proj <- spTransform(layer.pts,proj)
    
    
    
    
    
    layer.pts.near <- raster::intersect(layer.pts.proj,point.proj.buffer)
    
    layer.pts.near.df = as.data.frame(layer.pts.near)
    
    ## fit regression model
    m <- lm(layer~dem+x+y,data=layer.pts.near.df)
    c.dem <- coef(m)["dem"]
    c.x <- coef(m)["x"]
    c.y <- coef(m)["y"]
    
    layer.pts.near$dist <- gDistance(point.proj,layer.pts.near,byid=TRUE)
    
    # This is a modification of the original GIDS by the Flints to reduce "bullseye" patterns where a point is near the center of one cell.
    layer.pts.near[which(as.numeric(layer.pts.near$dist) < res),"dist"] <- res
    
    elev.diff <- point.proj$elev-layer.pts.near$dem
    point.proj.coord <- coordinates(point.proj)
    point.proj.x <- point.proj.coord[1]
    point.proj.y <- point.proj.coord[2]
    
    x.diff <- point.proj.x -layer.pts.near$x
    y.diff <-point.proj.y-layer.pts.near$y
    
    layer.pts.near$z <- layer.pts.near$layer + c.dem * elev.diff + c.x * x.diff + c.y * y.diff
    layer.pts.near$z.div.d <- layer.pts.near$z/(layer.pts.near$dist^2)
    
    z <- sum(layer.pts.near$z.div.d)/sum(1/(layer.pts.near$dist^2))    
    
    ds.vals[i] <- z
    
    cat("\r   Finished point:",i)
  }
  
  return(ds.vals)
}





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
coarse_dem_focal = crop(coarse_dem,focal_cell %>% as("Spatial"))

#### Fine dem to points? ####

fine_dem_points = rasterToPoints(fine_dem_focal, spatial=TRUE)

#### Run GIDS ####

out = gids.spatial(layer = tmax_focal,
                   dem = coarse_dem_focal,
                   dem_highres = fine_dem_focal,
                   points = fine_dem_points,
                   res = 800,
                   proj = default.proj)






(layer,dem,dem_highres,points,res,proj=default.proj)


