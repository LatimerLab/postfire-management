library(shiny)
library(ggplot2)
library(raster)
library(sf)
library(dplyr)
library(rgdal)
library(lme4)
library(fasterize)
library(viridis)

options(shiny.sanitize.errors = FALSE)

#### Globals ####

perim = st_read("data/power_perim.geojson") %>% st_transform(3310)
sev = st_read("data/power_sev.geojson") %>% st_transform(3310)
env = brick("data/env_raster_stack.tif")

#### Compute seed distance ####

## get all the non-high-sev area
sev_nonhigh = sev %>%
  filter(BURNSEV < 7)

## buffer out the fire perim 100 m to use that as non-high-sev
perim_buffer = st_buffer(perim,100)
perim_ring = st_difference(perim_buffer,perim)

## merge the ring with the non-high-sev to get all areas to measure seed distance from
seed_source = st_union(sev_nonhigh,perim_ring) %>% st_buffer(0) #%>% st_union

## turn into raster
raster_template = raster(seed_source %>% as("Spatial"),resolution=30,crs=3310)
raster_template = env[[1]] %>% crop(seed_source %>% as("Spatial")) #! could do this after cropping the env stack to avoid cropping it twice

seed_rast = fasterize(seed_source %>% st_as_sf,raster_template,fun="count")

## comp distance to non-high sev
seed_dist = distance(seed_rast)

## crop to the fire footprint
seed_dist = crop(seed_dist,perim)
seed_dist = mask(seed_dist,perim)

## Make a seed dist (0 m) mask layer
non_high_sev_mask = seed_dist
non_high_sev_mask[seed_dist == 0] = 1
non_high_sev_mask[seed_dist != 0] = 0



#### Load and assemble env predictor data ####

env = crop(env,perim %>% st_transform(projection(env)))
env = mask(env,perim %>% st_transform(projection(env)))
env = stack(env,seed_dist,non_high_sev_mask)

env_df = as.data.frame(env,xy=TRUE)
names(env_df) = c("x","y","tpi","ppt","tmin","shrub","seed_dist","non_high_sev_mask")
env_df = env_df %>%
  rename(normal_annual_precip = ppt,
         tpi2000 = "tpi",
         Shrubs = "shrub") %>%
  ## undo the raster scaling that was done to be able to save layers as int
  mutate(tmin = tmin/100,
         tpi2000 = tpi2000/10,
         Shrubs = Shrubs/100) %>%
  # apply transformation needed by stat model
  mutate(seed_dist = ifelse(seed_dist == 0,15,seed_dist)) %>% # correct for a limitation of using remotely sensed data: no plot center is exactly 0 m from a tree. Use 15 since we are focused on high-severity areas, so the closest a tree could be is half the width of the pixel. Also conveniently 15 m was the closest a tree was in our plot dataset.
  mutate(seed_dist = ifelse(seed_dist >= 200,200,seed_dist)) %>% # cap it at 200 since our field data only go that far and we know it tends to level off by then
  mutate(neglog5SeedWallConifer = -logb(seed_dist, base = exp(5)) )
  #%>%
  # ## do arcsin sqrt transf for shrubs (apparently not needed because redone by scale)
  # mutate(Shrubs = Shrubs/100 %>% sqrt %>% asin)

color_pal <- c("too low (even with planting)" = "#fde725", "good if planted" = "#7ad151", "good regardless of planting" = "#22a884",  "not possible" = "#2a788e", "good if unplanted" = "#414487", "too high (even without planting)" = "#440154")




mod = readRDS("data/model.rds")

# 
# summary(env_df$tmean)
# summary(env_df$tpi2000)
# summary(env_df$normal_annual_precip)
# 
# 
# 
dat = readRDS("data/data.rds")
# 
# dat = dat %>%
#   select(tpi2000,facts.planting.first.year,Shrubs,fsplanted,tmin,normal_annual_precip,neglog5SeedWallConifer,ShrubHt) %>%
#   mutate(fsplanted = ifelse(fsplanted == "planted",1,0))
# 
# summary(dat$tmean)
# summary(dat$tpi2000)
# summary(dat$normal_annual_precip)


#### Function for making maps based on inputs ####
# This function relies on some globals from above
make_maps = function(plant_year, density_low, density_high, mask_non_high_sev) {
  
  env_df = env_df %>%
    mutate(#neglog5SeedWallConifer = input$seedwall,
      facts.planting.first.year = as.numeric(plant_year))
  # fsplanted = input$planted)
  #  Shrubs = input$shrub_cover,  
  #ShrubHt = input$shrub_height,
  #LitDuff = input$lit_duff)
  
  # env_df = env_df %>%
  #   mutate(normal_annual_precip = 997.3,
  #          tpi2000 = 14.6,
  #          tmean = 2.06)
  
  # env_df = env_df %>%
  #   mutate(Shrubs = asin(sqrt(Shrubs/100)))
  
  ## for predictions of a no-planting scenario
  env_df_noplant = env_df %>%
    mutate(fsplanted = FALSE)
  
  ## for predictions of a planting scenario
  env_df_plant = env_df %>%
    mutate(fsplanted = TRUE)
  
  # predict seedlings per ha (incl undoing the response variable trasformation)
  pred_noplant = predict(mod,env_df_noplant,re.form=NA) %>% exp() - 24.99
  pred_plant = predict(mod,env_df_plant,re.form=NA) %>% exp() - 24.99
  
  # convert seedlings/ha to seedlings/acre
  pred_noplant = pred_noplant / 2.47
  pred_plant = pred_plant / 2.47
  
  # any model predictions below 0 should be 0
  pred_noplant[pred_noplant < 0.1] = 0.1
  pred_plant[pred_plant < 0.1] = 0.1
  
  pred_df = env_df
  
  pred_df$pred_noplant = pred_noplant
  pred_df$pred_plant = pred_plant
  
  density_low = density_low
  density_high = density_high
  
  ## classify densities
  pred_df = pred_df %>%
    mutate(noplant_class = cut(pred_noplant,breaks=c(-Inf,density_low,density_high,Inf),labels=c("low","good","high"))) %>%
    mutate(plant_class = cut(pred_plant,breaks=c(-Inf,density_low,density_high,Inf),labels=c("low","good","high"))) %>%
    filter(!is.na(pred_noplant))
  
  
  
  pred_df$overall = NA
  ##too low even with planting
  pred_df[which(pred_df$plant == "low"),"overall"] = "too low (even with planting)"
  ##too high even without planting
  pred_df[which(pred_df$noplant == "high"),"overall"] = "too high (even without planting)"
  ## good with planting only
  pred_df[which(pred_df$noplant != "good" & pred_df$plant == "good"),"overall"] = "good if planted"
  ## good with natural only
  pred_df[which(pred_df$noplant == "good" & pred_df$plant != "good"), "overall"] = "good if unplanted"
  ## good regardless of planting
  pred_df[which(pred_df$noplant == "good" & pred_df$plant == "good"), "overall"] = "good regardless of planting"
  ## too low when unplanted; too high when planted
  pred_df[which(pred_df$noplant == "low" & pred_df$plant == "high"), "overall"] = "not possible"
  
  # ## maybe don't need the following?
  # pred_raster_noplant = rasterFromXYZ(pred_df %>% dplyr::select(x,y,noplant_class))
  # pred_raster_plant = rasterFromXYZ(pred_df %>% dplyr::select(x,y,plant_class))
  
  df_plot = pred_df %>%
    mutate(overall = factor(overall, levels = rev(names(color_pal)))) %>%
    mutate(pred_noplant = ifelse(pred_noplant > 400, 400, pred_noplant)) %>%
    mutate(pred_plant = ifelse(pred_plant > 400, 400, pred_plant))

  ## Drop rows of masked-out values
  if(mask_non_high_sev) {
    df_plot = df_plot[df_plot$non_high_sev_mask != 1,]
  }
  
  main_map = ggplot(data=df_plot,aes(x=x,y=y,fill=overall)) +
    geom_raster() +
    geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
    theme_void(20) +
    scale_fill_manual(values = color_pal) +
    theme(legend.position="bottom", legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
    labs(title = "Seedling density") +
    guides(fill = guide_legend(nrow = 6))
  
  density_unplanted = ggplot(df_plot,aes(x=x,y=y,fill=pred_noplant)) +
    geom_raster() +
    coord_fixed() +
    theme_void(20) +
    scale_fill_viridis(breaks=c(0,200,400),
                         labels=c(0,200,"400+"),
                         limits=c(0,400),
                         direction = -1,
                       option = "inferno",
                       begin = 0.2,
                       end = 0.9) +
    theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
    labs(title = "Seedling density: Natural", fill = "Seedlings / acre") #+
    #guides(fill = guide_legend(reverse=FALSE))
    #guides(fill = guide_legend(nrow = 2))
  
  density_planted = ggplot(df_plot,aes(x=x,y=y,fill=pred_plant)) +
    geom_raster() +
    coord_fixed() +
    theme_void(20) +
    scale_fill_viridis(breaks=c(0,200,400),
                       labels=c(0,200,"400+"),
                       limits=c(0,400),
                       direction = -1,
                       option = "inferno",
                       begin = 0.2,
                       end = 0.9) +
    theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
    labs(title = "Seedling density: Natural + planted", fill = "Seedlings / acre") #+
  
  cover_shrub = ggplot(df_plot,aes(x=x,y=y,fill=Shrubs)) +
    geom_raster() +
    coord_fixed() +
    theme_void(20) +
    scale_fill_viridis(direction = -1) +
    theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
    labs(title = "Modeled shrub cover", fill = "Percent cover")
  
  seed_distance = ggplot(df_plot,aes(x=x,y=y,fill=seed_dist)) +
    geom_raster() +
    coord_fixed() +
    theme_void(20) +
    scale_fill_viridis() +
    theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
    labs(title = "Seed tree distance", fill = "Distance (m)")
  
  tmin = ggplot(df_plot,aes(x=x,y=y,fill=tmin)) +
    geom_raster() +
    coord_fixed() +
    theme_void(20) +
    scale_fill_viridis(option = "inferno") +
    theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
    labs(title = "Annual average of daily minimum temperature", fill = "°C")
  
  precip = ggplot(df_plot,aes(x=x,y=y,fill=normal_annual_precip)) +
    geom_raster() +
    coord_fixed() +
    theme_void(20) +
    scale_fill_viridis(option = "viridis",
                       direction = -1) +
    theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
    labs(title = "Annual average precipitation", fill = "mm")
  
  tpi = ggplot(df_plot,aes(x=x,y=y,fill=tpi2000)) +
    geom_raster() +
    coord_fixed() +
    theme_void(20) +
    scale_fill_viridis(option = "inferno",
                       direction = 1) +
    theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
    labs(title = "Topographic position index", fill = "TPI")
  
  ret = list("main" = main_map, "density_unplanted" = density_unplanted, "density_planted" = density_planted,
             "cover_shrub" = cover_shrub,
             "seed_distance" = seed_distance,
             tmin = tmin,
             precip = precip,
             tpi = tpi)
  
  return(ret)
  
}






# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PRESET - Post-fire reforestation success estimation tool"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      # sliderInput(inputId = "seedwall",
      #             label = "Seed wall:",
      #             min = -1.2,
      #             max = -0.6,
      #             value = -0.9),
      sliderInput(inputId = "density_range",
                  label = "Acceptable seedling density range (seedlings/acre):",
                  min = 0,
                  max = 600,
                  value = c(50,250)),
      # sliderInput(inputId = "max_density",
      #             label = "Maximum seedling density:",
      #             min = 0,
      #             max = 300,
      #             value = 50),
      radioButtons(inputId = "planted_year",
                  label = "Planting year:",
                  choices = list("1 year post-fire" = 1, "2 years post-fire" = 2, "3 years post-fire" = 3),
                  selected = 1),
      checkboxInput(inputId = "mask_non_high_sev",
                    label = "Show high-severity area only",
                    value = FALSE),
      checkboxGroupInput(inputId = "map_selection",
                    label = "Layers to display:",
                    choices = list("Main predictions" = "main",
                                   "Natural seedling density" = "density_unplanted",
                                   "Planted + natural seedling density" = "density_planted",
                                   "Modeled shrub cover" = "cover_shrub",
                                   "Seed tree distance" = "seed_distance",
                                   "Minimum temperature" = "tmin",
                                   "Annual precipitation" = "precip",
                                   "Topographic position index" = "tpi"
                                   ),
                    selected = "main")
      # sliderInput(inputId = "lit_duff",
      #             label = "Litter and duff:",
      #             min = 0,
      #             max = 10,
      #             value = 2),
      # radioButtons("planted", label = "Planted",
      #              choices = list("Yes" = TRUE, "No" = FALSE), 
      #              selected = "planted")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      ## Main map
      conditionalPanel(
          condition = "input.map_selection.includes('main') === true",      
        plotOutput(outputId = "distPlot"),
        strong("To do:"),
        tags$ul(
          tags$li("Mask out non-high-severity"),
          tags$li("Mask out model extrapolation areas")
        ),
        tags$br()
      ),
      
      ## Unplanted density
      conditionalPanel(
        condition = "input.map_selection.includes('density_unplanted') === true",
        plotOutput(outputId = "densityUnplantedPlot"),
        tags$br(" ")
      ),
      
      ## Planted density
      conditionalPanel(
        condition = "input.map_selection.includes('density_planted') === true",
        plotOutput(outputId = "densityPlantedPlot"),
        tags$br()
      ),
      
      ## Shrub cover
      conditionalPanel(
        condition = "input.map_selection.includes('cover_shrub') === true",
        plotOutput(outputId = "coverShrubPlot"),
        tags$br()
      ),
      
      ## Seed distance
      conditionalPanel(
        condition = "input.map_selection.includes('seed_distance') === true",
        plotOutput(outputId = "seedDistancePlot"),
        tags$br()
      ),
      
      ## Tmin
      conditionalPanel(
        condition = "input.map_selection.includes('tmin') === true",
        plotOutput(outputId = "tminPlot"),
        tags$br()
      ),
      
      ## Precip
      conditionalPanel(
        condition = "input.map_selection.includes('precip') === true",
        plotOutput(outputId = "precipPlot"),
        tags$br()
      ),
      
      ## TPI
      conditionalPanel(
        condition = "input.map_selection.includes('tpi') === true",
        plotOutput(outputId = "tpiPlot"),
        tags$br()
      )
      


    )
  )
)





# Define server logic required to draw a histogram ----
server <- function(input, output) {
  

  maps = reactive({ make_maps(input$planted_year, input$density_range[1], input$density_range[2], input$mask_non_high_sev) })
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    # x    <- faithful$waiting
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 

    ## Make model predictions

    maps_list = maps()
    plot(maps_list$main)
    
    
    
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
  })
  
  
  
  output$densityUnplantedPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$density_unplanted)
  })
  
  output$densityPlantedPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$density_planted)
  })
  
  output$coverShrubPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$cover_shrub)
  })
  
  output$seedDistancePlot = renderPlot({
    maps_list = maps()
    plot(maps_list$seed_distance)
  })
  
  output$tminPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$tmin)
  })
  
  output$precipPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$precip)
  })
  
  output$tpiPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$tpi)
  })
  
}


shinyApp(ui = ui, server = server)
