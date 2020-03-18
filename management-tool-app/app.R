library(shiny)
library(ggplot2)
library(raster)
library(sf)
library(dplyr)
library(rgdal)
library(lme4)

options(shiny.sanitize.errors = FALSE)

#### Globals: load pred raster, model, clip pred raster to temp focal area ####

region = st_read("data/power_fire_perimeter.geojson")

env = brick("data/env_raster_stack.tif")
env = crop(env,region %>% st_transform(projection(env)))
env = mask(env,region %>% st_transform(projection(env)))

env_df = as.data.frame(env,xy=TRUE)
names(env_df) = c("x","y","tpi","ppt","tmin","shrub")
env_df = env_df %>%
  rename(normal_annual_precip = ppt,
         tpi2000 = "tpi",
         Shrubs = "shrub") %>%
  ## undo the raster scaling that was done to be able to save layers as int
  mutate(tmin = tmin/100,
         tpi2000 = tpi2000/10,
         Shrubs = Shrubs/100) #%>%
  # ## do arcsin sqrt transf for shrubs (apparently not needed because redone by scale)
  # mutate(Shrubs = Shrubs/100 %>% sqrt %>% asin)

mod = readRDS("data/model.rds")


summary(env_df$tmean)
summary(env_df$tpi2000)
summary(env_df$normal_annual_precip)



dat = readRDS("data/data.rds")

dat = dat %>%
  select(tpi2000,facts.planting.first.year,Shrubs,fsplanted,tmin,normal_annual_precip,neglog5SeedWallConifer,ShrubHt) %>%
  mutate(fsplanted = ifelse(fsplanted == "planted",1,0))

summary(dat$tmean)
summary(dat$tpi2000)
summary(dat$normal_annual_precip)


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("PRESET - Post-fire reforestation success estimation tool"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      sliderInput(inputId = "seedwall",
                  label = "Seed wall:",
                  min = -1.2,
                  max = -0.6,
                  value = -0.9),
      sliderInput(inputId = "shrub_cover",
                  label = "Shrub cover:",
                  min = 0,
                  max = 100,
                  value = 50),
      sliderInput(inputId = "shrub_height",
                  label = "Shrub height:",
                  min = 0,
                  max = 300,
                  value = 50),
      sliderInput(inputId = "planted_year",
                  label = "Planting year:",
                  min = 1,
                  max = 3,
                  value = 2),
      sliderInput(inputId = "lit_duff",
                  label = "Litter and duff:",
                  min = 0,
                  max = 10,
                  value = 2),
      radioButtons("planted", label = "Planted",
                   choices = list("Yes" = "planted", "No" = "unplanted"), 
                   selected = "planted")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)





# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
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
    
    
    ## Make model predictions

    env_df = env_df %>%
      mutate(neglog5SeedWallConifer = input$seedwall,
             facts.planting.first.year = input$planted_year,
             fsplanted = input$planted,
             #Shrubs = input$shrub_cover,
             ShrubHt = input$shrub_height,
             LitDuff = input$lit_duff)
    
    # env_df = env_df %>%
    #   mutate(normal_annual_precip = 997.3,
    #          tpi2000 = 14.6,
    #          tmean = 2.06)

    # env_df = env_df %>%
    #   mutate(Shrubs = asin(sqrt(Shrubs/100)))
    
    pred = predict(mod,env_df,re.form=NA) %>% exp() - 24.99

    pred[pred > 600] = 601
    
    env_df$pred = pred
        
    pred_raster = rasterFromXYZ(env_df %>% dplyr::select(x,y,pred))
    
    plot(pred_raster)
    
    # hist(x, breaks = bins, col = "#75AADB", border = "white",
    #      xlab = "Waiting time to next eruption (in mins)",
    #      main = "Histogram of waiting times")
    
  })
  
}


shinyApp(ui = ui, server = server)
