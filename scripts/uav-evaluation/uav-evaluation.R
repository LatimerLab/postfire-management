setwd("~/UC Davis/Research Projects/Post-fire management/postfire-management")

library(raster)
library(sf)
library(tidyverse)

#### convenience functions ####

# convert sf objects to data.frame and drop geometry
st_drop_geometry <- function(x) {
  if(inherits(x,"sf")) {
    x <- st_set_geometry(x, NULL)
    class(x) <- 'data.frame'
  }
  return(x)
}

#### load necessary layers ####

## load tree canopy perimeters
trees <- st_read("data/non-synced/uav-evaluation/freds-snag-high/manual-tree-identification/tree outlines.shp")
#assign each tree a unique id
trees$tree.id <- 1:nrow(trees)

## load tree canopy perimeters (version that is aligned with rgb image)
trees.rgb <- st_read("data/non-synced/uav-evaluation/freds-snag-high/manual-tree-identification/tree outlines_shifted_rgb.shp")
#assign each tree a unique id
trees.rgb$tree.id <- 1:nrow(trees.rgb)

## load and prep multispectral layers
ms.files <- list.files("data/non-synced/uav-evaluation/freds-snag-high/multispectral/individual-bands/",pattern="\\.tif$",full.names=TRUE)
ms <- stack(ms.files)

## load RGB image
rgb <- stack("data/non-synced/uav-evaluation/freds-snag-high/rgb/ursell-young-latimer_snag-retention-5-ha_transparent_mosaic_group1.tif")
rgb <- rgb[[1:3]]
names(rgb) <- c("red","green","blue")



# we only need the first layer from each file (they all have a second layer that is a coverage mask)
ms <- ms[[seq(from=1,to=(nlayers(ms)-1),by=2)]]

# remove the ".1" from each layer name
names(ms) <- str_sub(names(ms),1,-3)

# mask out non-tree pixels from multispectral raster stack
trees.sp <- as(trees,"Spatial")
ms <- raster::crop(ms,trees.sp)
ms.full <- ms # save it before masking
ms <- mask(ms,trees.sp)

# and from RGB stack
trees.rgb.sp <- as(trees.rgb,"Spatial")
rgb <- raster::crop(rgb,trees.rgb.sp)
rgb.full <- rgb # save it before masking
rgb <- mask(rgb,trees.rgb.sp)
rgb <- raster::aggregate(rgb,fact=2)

##!! temporary hack: to run analysis for rgb data rather than multispectral, overwrite the objects
# in the future, will want to create new generic variables for the steps that follow rather than using overwritten multispectral
trees <- trees.rgb
ms <- rgb
ms.full <- rgb.full

#### extract multispectral reflectance values at each tree canopy ####

# make a grid of points, a point every 5 cm
grid.sfc <- st_make_grid(trees,cellsize=0.05,what="centers")
grid <- st_sf(data.frame(point.id=1:length(grid.sfc),geom=grid.sfc))

# restrict grid to the tree canopies
grid <- st_intersection(grid,trees)

# extract reflectance values at points (need to convert to SpatialPointsDataFrame so raster::extract works)
grid.sp <- as(grid,"Spatial")
grid.ms <- raster::extract(ms,grid.sp) %>% as.data.frame() # this maks a matrix
d <- bind_cols(grid,grid.ms) #data to use for analysis

# restart tree ID numbering within each species
d$tree.id.internal <- ave(d$tree.id,d$species,FUN=function(x) as.numeric(factor(x)))
d$tree.id.internal <- as.character(d$tree.id.internal)



#### quality filtering of extracted reflectance values ####

## within each species and reflectance category (independent of individual trees), determine quantiles of reflectance (to filter out shade pixels)
# # multispectral version
# d <- d %>%
#   group_by(species) %>%
#   mutate(blue.percentile = cume_dist(blue)) %>%
#   mutate(red.percentile = cume_dist(red)) %>%
#   mutate(green.percentile = cume_dist(green)) %>%
#   mutate(nir.percentile = cume_dist(nir)) %>%
#   mutate(red_edge.percentile = cume_dist(red_edge)) %>%
#   ungroup()

# rgb version
d <- d %>%
  group_by(species) %>%
  mutate(blue.percentile = cume_dist(blue)) %>%
  mutate(red.percentile = cume_dist(red)) %>%
  mutate(green.percentile = cume_dist(green)) %>%
  ungroup()


# make sure the percentile-making was done properly--by species--(so percentile 0.1 reflectance for species A can be diff than for species B)
ggplot(d,aes(y=blue,x=blue.percentile,color=species)) +
  geom_point()
# it worked

# compute the average percentile across all bands
# # multispectral version
# d <- d %>%
#   mutate(mean.percentile = (blue.percentile+red.percentile+green.percentile+nir.percentile+red_edge.percentile)/5)

#rgb version
d <- d %>%
  mutate(mean.percentile = (blue.percentile+red.percentile+green.percentile)/3)


# only keep the 25% of points with highest reflectance--thinking it might be a conservative (liberal?) way to remove all shade pixels (and then some)
d <- d %>%
  filter(mean.percentile > .75)



#### compute transofmraitons for each pixel (e.g. PCA values) ####

# need to remove spatial columns from df
d <- st_drop_geometry(d) ####

# # multispectral version
# refl.cols <- c("red","green","blue","nir","red_edge")

# rgb version
refl.cols <- c("red","green","blue")

pc <- prcomp(as.matrix(d[,refl.cols]),center=TRUE,scale.=TRUE)
# nice, the first two axes capture 92% of the variance

pc <- as.data.frame(pc$x)

d[,names(pc)] <- pc


#### plot the reflectance values ####

# randomize the points so one species doesn't overlap the others
d <- d[sample(1:nrow(d),size=nrow(d),replace=TRUE),]

#make shape scale
shape.codes <- 19:0
shape.names <- 1:length(shape.codes)

shape.scale <- shape.codes
names(shape.scale) <- shape.names

# remove outlier (multispectral only)
# d <- d[d$PC2>-7,]

library(viridis)
ggplot(d,aes(x=PC1,y=PC2,color=species,fill=species,shape=tree.id.internal)) +
  geom_point(size=2) +
  scale_color_brewer(palette="Set1") +
  scale_shape_manual(values=shape.scale) +
  theme_bw()


#### try to classify with random forest ####
library(randomForest)

# sample random training and validation datasets: take half of the points of each species randomly
# d is already randomized, so now sort by species and take every other record
d.ord <- d %>%
  arrange(species)
index <- rep(1:2,nrow(d.ord))[1:nrow(d.ord)] # vector of 1 2 1 2 etc. the same length as the data frame
d.ord$index <- index

d.train <- d.ord[d.ord$index == 1,]
d.val <- d.ord[d.ord$index == 2,]

# multispectral version
# rf.mod <- randomForest(species~blue+green+nir+red_edge+red,d.train,ntree=500,importance=TRUE)

# rgb version
rf.mod <- randomForest(species~blue+green+red,d.train,ntree=500,importance=TRUE)

varImpPlot(rf.mod,sort=TRUE)

# predict for validation data
d.val$predicted.species <- predict(rf.mod,d.val)

table(d.val$species,d.val$predicted.species)
# this classification works phenomenally!!!! is it overfitting because we have multiple points per tree?


### Try it by withholding entire trees (as opposed to half of the points from each tree)
#how many trees of each species?
d.summ <- d %>%
  group_by(species) %>%
  summarise(count=length(unique(tree.id)))

# we probably have enough PIPO, PILA, GRAM, and CEIN. when there are 5 of a species, use 3 for training and 2 for validation

## for training, take three trees of PILA, GRAM, and CEIN and 6 trees of PIPO
d.train <- d %>%
  filter(((species %in% c("PILA","GRAM","CEIN")) & (tree.id.internal %in% 1:3)) | ((species %in% c("PIPO")) & (tree.id.internal %in% 1:6))  )

## for validation, take the rest
d.val <- d %>%
  filter(((species %in% c("PILA","GRAM","CEIN")) & !(tree.id.internal %in% 1:3)) | ((species %in% c("PIPO")) & !(tree.id.internal %in% 1:6))  )

d.train$species <- droplevels(d.train$species)

# multispectral version
# rf.mod <- randomForest(species~blue+green+nir+red_edge+red,d.train,ntree=500,importance=TRUE)

rf.mod <- randomForest(species~blue+green+red,d.train,ntree=500,importance=TRUE)

varImpPlot(rf.mod,sort=TRUE)

# predict for validation data
d.val$predicted.species <- predict(rf.mod,d.val)
table(d.val$species,d.val$predicted.species)


#### try to classify with quadratic discriminant analysis ####
library(MASS)

# need to remvove the small groups--so what are they?
d.summ <- d %>%
  group_by(species) %>%
  summarise(count=n())
# just need to remove abco
d.qda <- d %>%
  filter(species != "ABCO")


# sample random training and validation datasets: take half of the points of each species randomly
# d is already randomized, so now sort by species and take every other record
d.ord <- d.qda %>%
  arrange(species)
index <- rep(1:2,nrow(d.ord))[1:nrow(d.ord)] # vector of 1 2 1 2 etc. the same length as the data frame
d.ord$index <- index

d.train <- d.ord[d.ord$index == 1,]
d.val <- d.ord[d.ord$index == 2,]

d.train$species <- droplevels(d.train$species)

# multispectral version
# qda.mod <- qda(species~blue+green+nir+red_edge+red,d.train)

# rgb version
qda.mod <- qda(species~blue+green+red,d.train)

# predict for validation data
preds <- predict(qda.mod,d.val)
d.val$predicted.species <- preds$class

table(d.val$species,d.val$predicted.species)
# this classification works phenomenally!!!! is it overfitting because we have multiple points per tree?


### Try it by withholding entire trees (as opposed to half of the points from each tree)
#how many trees of each species?
d.summ <- d %>%
  group_by(species) %>%
  summarise(count=length(unique(tree.id)))

# just abco
d.qda <- d %>%
  filter(species != "ABCO")

# we probably have enough PIPO, PILA, GRAM, and CEIN. when there are 5 of a species, use 3 for training and 2 for validation

## for training, take three trees of PILA, GRAM, and CEIN and 6 trees of PIPO
d.train <- d.qda %>%
  filter(((species %in% c("PILA","GRAM","CEIN")) & (tree.id.internal %in% 1:3)) | ((species %in% c("PIPO")) & (tree.id.internal %in% 1:6))  )

## for validation, take the rest
d.val <- d.qda %>%
  filter(((species %in% c("PILA","GRAM","CEIN")) & !(tree.id.internal %in% 1:3)) | ((species %in% c("PIPO")) & !(tree.id.internal %in% 1:6))  )

d.train$species <- droplevels(d.train$species)

# multispectral version
# qda.mod <- qda(species~blue+green+nir+red_edge+red,d.train)

# rgb version
qda.mod <- qda(species~blue+green+red,d.train)


# predict for validation data
preds <- predict(qda.mod,d.val)
d.val$predicted.species <- preds$class

table(d.val$species,d.val$predicted.species)
# this classification works phenomenally!!!! is it overfitting because we have multiple points per tree?



#### Apply a QDA model across the whole landscape to categorize pixels ####

vals <- getValues(ms.full)
vals.df <- as.data.frame(vals)
preds <- predict(qda.mod,vals.df)
post <- as.data.frame(preds$post) # extract the posterior probabilities by class

#compute the max posterior across all classes for each pixel to get a sense of how confident the model is of a classification
post <- post %>%
  mutate(max.post = pmax(CEIN,GRAM,PILA,PIPO,QUKE,SALIX,SHADOW))

pred.class <- preds$class

# # random forest version
# pred.class <- preds

# set all predictions that have low posterior to NA
 low.posterior <- which(post$max.post < 0.8)
 pred.class[low.posterior] <- NA

ms.full$preds <- pred.class

## convert the rasterstack into a data frame with x-y coords for ggplotting
ms.sg <- as(ms.full,"SpatialGridDataFrame") # for some reason we need to go to SGDF before we can coerce to SPDF
ms.sp <- as(ms.sg,"SpatialPixelsDataFrame")
ms.df <- as.data.frame(ms.sp)

# multispectral version
# names(ms.df) <- c("blue","green","nir","red_edge","red","pred","x","y")

# rgb version
names(ms.df) <- c("red","green","blue","pred","x","y")

ggplot(ms.df,aes(x=x,y=y,fill=pred)) +
  geom_tile() +
  scale_fill_brewer(palette="Set1")
