library(shiny)
library(ggplot2)
library(raster)
library(sf)
library(dplyr)
library(rgdal)
library(lme4)
library(fasterize)
library(viridis)
library(smoothr)
library(shinyjs)
library(V8)

options(shiny.sanitize.errors = FALSE)
options(shiny.maxRequestSize = 100*1024^2)

#### Globals ####

var_lims = read.csv("data/var_lims.csv", header=TRUE)
color_pal <- c("too low (even with planting)" = "#fde725", "good if planted" = "#7ad151", "good regardless of planting" = "#22a884",  "too low when unplanted; too high when planted" = "#2a788e", "good if unplanted" = "#414487", "too high (even without planting)" = "#440154")
mod = readRDS("data/model.rds")
dat = readRDS("data/data.rds")

maps_loaded = reactiveVal(FALSE) # Holds status of whether maps for display are fully computed (so can disable upload box)
severity_uploaded = reactiveVal(FALSE)

df_plot_export = reactiveVal(NULL) # for exporting raster maps in file download handler

jsResetCode <- "shinyjs.reset = function() {history.go(0)}" # Define the js method that resets the page

albers = "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs "


### A blank ggplot that says "Calculating..."

d = data.frame(x = c(0,0,100,100),
               y = c(0,100,0,100),
               z = c(1,1,1,1))
blank_plot = ggplot(d,aes(x=x,y=y)) +
  theme_void() +
  geom_blank() +
  theme(plot.background = element_rect(fill="grey90",color=NA)) +
  annotate("text",x=0,y=97,label="Calculating...",hjust=0)



#### Fire perim and sev ####

# perim = st_read("data/power_perim.geojson") %>% st_transform(3310)
# sevtmp= st_read("data/rim_sev.gpkg") %>% st_transform(3310)

#### Get predictors and predictions for a given fire ####

prep_mapping_vars = function(perim,sev,input) {
  
  if(is.null(sev)) return(NULL)
  
  if(input$resolution_input == 1) {
    env = brick("data/env_raster_stack.tif")
  } else {
    env = brick("data/env_raster_stack_coarse.tif")
  }
  
  env = crop(env,perim %>% st_transform(projection(env)))
  env = mask(env,perim %>% st_transform(projection(env)))

  
  # get a fire perimeter shapefile and severity raster
  # temp sev = raster("/home/derek/Desktop/ca4005312066920190904_20181007_20191010_rdnbr_ba7.tif")
  #temp perim = st_read("/home/derek/Downloads/ca4005312066920190904_20181007_20191010_ravg_data/bdy.gpkg") %>% st_transform(3310)
  
  # # Get a (smoothed) perimeter from the severity shapefile
  # perim = st_union(sev) %>% st_as_sf() %>% st_simplify(dTolerance = 30) %>% st_buffer(0) %>% smooth(method="ksmooth") %>% st_buffer(0)
  # 
  
  ### Compute seed distance ###
  
  ## get all the non-high-sev area
  sev_nonhigh = sev
  sev_nonhigh = projectRaster(sev_nonhigh,env,method="ngb")
  sev_nonhigh[sev_nonhigh >= 7] = NA
  
  
  ## comp distance to non-high-sev
  seed_dist = distance(sev_nonhigh)
  

  ## crop to the fire footprint
  seed_dist = crop(seed_dist,perim %>% st_transform(projection(seed_dist)))
  seed_dist = mask(seed_dist,perim %>% st_transform(projection(seed_dist)))
  
  ## Make a seed dist (0 m) mask layer
  non_high_sev_mask = seed_dist
  non_high_sev_mask[seed_dist == 0] = 1
  non_high_sev_mask[seed_dist != 0] = 0
  
  
  
  #### Load and assemble env predictor data for focal area ####
  
  env = stack(env,seed_dist,non_high_sev_mask)
  
  env_df = as.data.frame(env,xy=TRUE)
  names(env_df) = c("x","y","tpi","ppt","tmean","shrub","elev","seed_dist","non_high_sev_mask")
  env_df = env_df %>%
    rename(normal_annual_precip = ppt,
           tpi2000 = "tpi",
           Shrubs = "shrub") %>%
    ## undo the raster scaling that was done to be able to save layers as int
    mutate(tmean = tmean/100,
           tpi2000 = tpi2000/10,
           Shrubs = Shrubs/100) %>%
    # apply transformation needed by stat model
    mutate(seed_dist = ifelse(seed_dist == 0,15,seed_dist)) %>% # correct for a limitation of using remotely sensed data: no plot center is exactly 0 m from a tree. Use 15 since we are focused on high-severity areas, so the closest a tree could be is half the width of the pixel. Also conveniently 15 m was the closest a tree was in our plot dataset.
    mutate(seed_dist = ifelse(seed_dist >= 200,200,seed_dist)) %>% # cap it at 200 since our field data only go that far and we know it tends to level off by then
    mutate(log10SeedWallConifer = log10(seed_dist))
  
  ## make an extrapolation column
  env_df = env_df %>%
    dplyr::mutate(extrap = !(tpi2000 %>% between(var_lims$tpi_min,var_lims$tpi_max)) |
             !(normal_annual_precip %>% between(var_lims$ppt_min,var_lims$ppt_max)) |
             !(tmean %>% between(var_lims$tmean_min,var_lims$tmean_max)) |
               !(elev %>% between(var_lims$elev_min,var_lims$elev_max))   )
  
  mapping_vars = list(env_df = env_df,
                      perim = perim,
                      sev = sev)
  
  return(mapping_vars)
  
}






# 
# summary(env_df$tmean)
# summary(env_df$tpi2000)
# summary(env_df$normal_annual_precip)
# 
# 
# 

# 
# dat = dat %>%
#   select(tpi2000,facts.planting.first.year,Shrubs,fsplanted,tmin,normal_annual_precip,neglog5SeedWallConifer,ShrubHt) %>%
#   mutate(fsplanted = ifelse(fsplanted == "planted",1,0))
# 
# summary(dat$tmean)
# summary(dat$tpi2000)
# summary(dat$normal_annual_precip)


#### Function to load fire files ####
load_sev = function(input,output) {
  
  # perim = st_read(input$perim_file) %>% st_transform(3310)
  # sev = st_read(input$sev_file) %>% st_transform(3310)
  
  if(is.null(input$sev_file)) {
    sev = NULL
  } else  {
    sev = raster(input$sev_file$datapath)
    severity_uploaded(TRUE)
    cat("Sev raster loaded")
    

    
  }
  
  return(sev)
  
}


#### Function to load fire files ####
load_perim = function(input) {
  
  # perim = st_read(input$perim_file) %>% st_transform(3310)
  # sev = st_read(input$sev_file) %>% st_transform(3310)
  
  if(is.null(input$perim_file)) {
    perim = NULL
  } else  {
    perim = st_read(input$perim_file$datapath)
    cat("Perim loaded")
  }
  
  return(perim)
  
}


#### Function to load fire files ####
test_perim_uploaded = function(input) {
  
  # perim = st_read(input$perim_file) %>% st_transform(3310)
  # sev = st_read(input$sev_file) %>% st_transform(3310)
  
  return(!is.null(input$perim_file))
  

  
  # str(input$perim_file)
  
}


custom_debug = function(input) {
  
  # perim = st_read(input$perim_file) %>% st_transform(3310)
  # sev = st_read(input$sev_file) %>% st_transform(3310)
  
  str(input$sev_file)
  str(file_uploaded)
  
  reset('sev_file')
  
}





#### Function for making maps based on inputs ####
# This function relies on some globals from above
make_maps = function(mapping_vars, plant_year, density_low, density_high, mask_non_high_sev, mask_extrapolation) {
  
  if(is.null(mapping_vars)) {
    
    ret = list("main" = blank_plot, "density_unplanted" = blank_plot, "density_planted" = blank_plot,
               "cover_shrub" = blank_plot,
               "seed_distance" = blank_plot,
               tmean = blank_plot,
               precip = blank_plot,
               tpi = blank_plot)
    
    
  } else {
    
    perim = mapping_vars$perim
    
    env_df = mapping_vars$env_df %>%
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
      mutate(fsplanted = "unplanted")
    
    ## for predictions of a planting scenario
    env_df_plant = env_df %>%
      mutate(fsplanted = "planted")
    
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
    pred_df[which(pred_df$noplant == "low" & pred_df$plant == "high"), "overall"] = "too low when unplanted; too high when planted"
    
    # make a column with these values as numeric codes
    pred_df = pred_df %>%
      mutate(overall_code = recode(overall,
                                   "too low (even with planting)" = 1,
                                   "good if planted" = 2,
                                   "good regardless of planting" = 3,
                                   "too low when unplanted; too high when planted" = 4,
                                   "good if unplanted" = 5,
                                   "too high (even without planting)" = 6))
    
    df_plot = pred_df %>%
      mutate(overall = factor(overall, levels = rev(names(color_pal)))) %>%
      mutate(pred_noplant_trunc = ifelse(pred_noplant > 400, 400, pred_noplant)) %>%
      mutate(pred_plant_trunc = ifelse(pred_plant > 400, 400, pred_plant))
  
    ## Drop rows of masked-out values
    if(mask_non_high_sev) {
      df_plot = df_plot[df_plot$non_high_sev_mask != 1,]
    }
    
    if(mask_extrapolation) {
      df_plot = df_plot[df_plot$extrap != TRUE,]
    }
    
    
    df_plot_export(df_plot)
    cat("Saved plto data table")
    
    perim = perim %>% st_transform(3310)
    
    main_map = ggplot(data=df_plot,aes(x=x,y=y,fill=overall)) +
      geom_raster() +
      geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
      theme_void(20) +
      scale_fill_manual(values = color_pal) +
      theme(legend.position="bottom", legend.title=element_blank(), plot.title = element_text(hjust = 0.5)) +
      labs(title = "Seedling density") +
      guides(fill = guide_legend(nrow = 6))
    
    density_unplanted = ggplot(df_plot,aes(x=x,y=y,fill=pred_noplant_trunc)) +
      geom_raster() +
      geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
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
    
    density_planted = ggplot(df_plot,aes(x=x,y=y,fill=pred_plant_trunc)) +
      geom_raster() +
      geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
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
      geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
      theme_void(20) +
      scale_fill_viridis(direction = -1) +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
      labs(title = "Modeled shrub cover", fill = "Percent cover")
    
    seed_distance = ggplot(df_plot,aes(x=x,y=y,fill=seed_dist)) +
      geom_raster() +
      geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
      theme_void(20) +
      scale_fill_viridis() +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
      labs(title = "Seed tree distance", fill = "Distance (m)")
    
    tmean = ggplot(df_plot,aes(x=x,y=y,fill=tmean)) +
      geom_raster() +
      geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
      theme_void(20) +
      scale_fill_viridis(option = "inferno") +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
      labs(title = "Mean annual temperature", fill = "°C")
    
    precip = ggplot(df_plot,aes(x=x,y=y,fill=normal_annual_precip)) +
      geom_raster() +
      geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
      theme_void(20) +
      scale_fill_viridis(option = "viridis",
                         direction = -1) +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
      labs(title = "Annual average precipitation", fill = "mm")
    
    tpi = ggplot(df_plot,aes(x=x,y=y,fill=tpi2000)) +
      geom_raster() +
      geom_sf(data=perim,color="black",fill = NA, inherit.aes = FALSE) +
      theme_void(20) +
      scale_fill_viridis(option = "inferno",
                         direction = 1) +
      theme(legend.position="bottom", plot.title = element_text(hjust = 0.5), legend.direction = "vertical") +
      labs(title = "Topographic position index", fill = "TPI")
    
    ret = list("main" = main_map, "density_unplanted" = density_unplanted, "density_planted" = density_planted,
               "cover_shrub" = cover_shrub,
               "seed_distance" = seed_distance,
               tmean = tmean,
               precip = precip,
               tpi = tpi)
    
    maps_loaded(TRUE)
    
  }
  
  return(ret)
  
}




# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  useShinyjs(),                                           # Include shinyjs in the UI
  extendShinyjs(text = jsResetCode, functions = c("reset")),                      # Add the js code to the page
  
  # App title ----
  titlePanel("Post-fire reforestation success estimation tool"),
  h4('aka "PRESET"'),
  h6('v 0.0.3 beta'),
  h6("Developed by: Derek Young, Quinn Sorenson, Andrew Latimer"),
  h6("Latimer Lab, UC Davis"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      ### Upload fire perim and severity
      #fileInput("perim_file", "Fire perimeter"),
      
      conditionalPanel(
        condition="output.map_loaded ===false",
        
        radioButtons(inputId = "resolution_input",
                     label = "1. Choose resolution:",
                     choices = list("30 m pixels (slower)" = 1, "60 m pixels (faster)" = 2),
                     selected = 2),
        
        tags$br(),
        
        fileInput("sev_file", "2. Upload severity raster"),
        
        conditionalPanel( condition = "output.severity_uploaded === true",
              textInput("min_high_sev",label="2b. Minimum value to consider high-severity"),
              htmlOutput("severity_text")
 
                          ),
        
        fileInput("perim_file", "3. Upload fire perimeter"),
        
        HTML('&emsp;'),tags$b("Or,"),"use 2019 Caples Fire (Eldorado NF) demo data: ",
        
        actionButton("upload_button","Go")
      ),
      
      conditionalPanel(
        condition="output.map_loaded === true",
        "Fire perimeter/severity map loaded.",
        actionButton("reset_button","Clear")
      ),

      
      # checkboxInput(inputId = "show_map_controls",
      #               label = "Show map controls",
      #               value = FALSE),
      
      
      
      ### Map display controls
      conditionalPanel(
        condition="output.map_loaded === true",
      
        
        # Input: Slider for the number of bins ----
        # sliderInput(inputId = "seedwall",
        #             label = "Seed wall:",
        #             min = -1.2,
        #             max = -0.6,
        #             value = -0.9),
        tags$br(),
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
                    selected = 2),
        checkboxInput(inputId = "mask_non_high_sev",
                      label = "Show high-severity area only",
                      value = FALSE),
        checkboxInput(inputId = "mask_extrap",
                      label = "Hide model extrapolation areas",
                      value = FALSE),
        checkboxGroupInput(inputId = "map_selection",
                      label = "Layers to display:",
                      choices = list("Main predictions" = "main",
                                     "Natural seedling density" = "density_unplanted",
                                     "Planted + natural seedling density" = "density_planted",
                                     "Modeled shrub cover" = "cover_shrub",
                                     "Seed tree distance" = "seed_distance",
                                     "Mean temperature" = "tmean",
                                     "Annual precipitation" = "precip",
                                     "Topographic position index" = "tpi"
                                     ),
                      selected = "main"),
        
        tags$br(),
        selectInput("dataset", "Select a layer to download:",
                    choices = c("Main predictions", "Natural seedling density", "Planted + natural seedling density")),
        downloadButton("downloadData", "Download")
        
        
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Conditional panel to hide all the maps if user has not yet uploaded a perimeter shapefile
      conditionalPanel(
          condition = "output.perim_uploaded === true",
          
        
          
          ## Main map
          conditionalPanel(
            condition = "input.map_selection.includes('main') === true",      
            plotOutput(outputId = "distPlot",height="600px"),
            tags$br()
          ),
          
          ## Unplanted density
          conditionalPanel(
            condition = "input.map_selection.includes('density_unplanted') === true",
            plotOutput(outputId = "densityUnplantedPlot",height="600px"),
            tags$br(" ")
          ),
          
          ## Planted density
          conditionalPanel(
            condition = "input.map_selection.includes('density_planted') === true",
            plotOutput(outputId = "densityPlantedPlot",height="600px"),
            tags$br()
          ),
          
          ## Shrub cover
          conditionalPanel(
            condition = "input.map_selection.includes('cover_shrub') === true",
            plotOutput(outputId = "coverShrubPlot",height="600px"),
            tags$br()
          ),
          
          ## Seed distance
          conditionalPanel(
            condition = "input.map_selection.includes('seed_distance') === true",
            plotOutput(outputId = "seedDistancePlot",height="600px"),
            tags$br()
          ),
          
          ## Tmin
          conditionalPanel(
            condition = "input.map_selection.includes('tmean') === true",
            plotOutput(outputId = "tmeanPlot",height="600px"),
            tags$br()
          ),
          
          ## Precip
          conditionalPanel(
            condition = "input.map_selection.includes('precip') === true",
            plotOutput(outputId = "precipPlot",height="600px"),
            tags$br()
          ),
          
          ## TPI
          conditionalPanel(
            condition = "input.map_selection.includes('tpi') === true",
            plotOutput(outputId = "tpiPlot",height="600px"),
            tags$br()
          )
      )


    )
  )
)





# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  sev = reactiveVal(NULL) # Holds fire seveirty raster once loaded
  perim = reactiveVal(NULL) # Holds fire perimeter sf object once loaded
  
  output$map_loaded = reactive({ maps_loaded() })
  outputOptions(output, 'map_loaded', suspendWhenHidden=FALSE)
  
  output$severity_uploaded = reactive({ severity_uploaded() })
  outputOptions(output, 'severity_uploaded', suspendWhenHidden=FALSE)
  
  # output$max_sev_value = reactive({ max_sev_value() })
  # outputOptions(output, 'max_sev_value', suspendWhenHidden=FALSE)
  
  max_sev_value = reactive({ 
    
        if(!is.null(sev())) {
          return(max(values(sev())))
        } else {
          return(0)
        }
    })
    
  
  output$severity_text = renderUI({ 
    
    if(!is.null(sev())) {
      ## get severity range
      sev_values = values(sev())
      min_sev = min(sev_values)
      max_sev = max(sev_values)
      range_text = paste0(min_sev, " - ", max_sev)
    } else {
      range_text = "[no severity uploaded yet]"
    }
    
    HTML('&emsp;', paste0( "Range of severity values in uploaded raster: ",range_text,tags$br(),tags$br()))
    
    
    })
  



  observeEvent(input$sev_file,
               
               { sev(load_sev(input,output))
                 cat("Max sev value is:",max_sev_value())
                 updateTextInput(session, "min_high_sev", value=max_sev_value())
                 }       
               
               )
  
  observeEvent(input$perim_file,
               
               { perim(load_perim(input)) }       
               
  )
  
  observeEvent(input$reset_button,
               
               {
                    maps_loaded(FALSE)
                 severity_uploaded(FALSE)
                    js$reset()
               }
              )
  
  output$perim_uploaded = reactive({ return(!is.null(perim())) })
  outputOptions(output, 'perim_uploaded', suspendWhenHidden=FALSE)

  observeEvent(input$upload_button,
                       #{custom_debug(input)}
               {
                 caplesfile = NULL
                 caplesfile$sev_file$datapath = "data/caples_sev.tif"
                 sev(load_sev(caplesfile))
                 caplesfile = NULL
                 caplesfile$perim_file$datapath = "data/caples_perim.gpkg"
                 perim(load_perim(caplesfile))
                 
               }
                    )
  
  mapping_vars = reactive({ prep_mapping_vars(perim(),sev(),input) })

  maps = reactive({ make_maps(mapping_vars(), input$planted_year, input$density_range[1], input$density_range[2], input$mask_non_high_sev, input$mask_extrap) })
  

  
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
  
  output$tmeanPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$tmean)
  })
  
  output$precipPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$precip)
  })
  
  output$tpiPlot = renderPlot({
    maps_list = maps()
    plot(maps_list$tpi)
  })
  

  dataset_colname <- reactive({
    switch(input$dataset,
           "Main predictions" = "overall_code",
           "Natural seedling density" = "pred_noplant",
           "Planted + natural seedling density" = "pred_plant")
  })
  
  dataset_filename <- reactive({
    switch(input$dataset,
           "Main predictions" = "main_predictions",
           "Natural seedling density" = "density_natural",
           "Planted + natural seedling density" = "density_planted_plus_natural")
  })

  
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0(dataset_filename(), ".tif")
    },
    content = function(file) {
      
      xyz = df_plot_export() %>%
        select(x,y,!!!dataset_colname())

      rast = rasterFromXYZ(xyz, crs = albers)
      
      writeRaster(rast, file)
    }
  )

  
  
}


shinyApp(ui = ui, server = server)
