
# attach packages
library(shiny)
library(here)
library(tidyverse)
library(tmap)
library(terra)
library(shinythemes)
library(shinyjs)


### READ IN DATA ###

## Global:
ssp1_global <- rast(here('data','processed','global','ssp1_abandonment_global_50km.tif'))
ssp2_global <- rast(here('data','processed','global','ssp2_abandonment_global_50km.tif'))
ssp3_global <- rast(here('data','processed','global','ssp3_abandonment_global_50km.tif'))
ssp4_global <- rast(here('data','processed','global','ssp4_abandonment_global_50km.tif'))
ssp5_global <- rast(here('data','processed','global','ssp5_abandonment_global_50km.tif'))
carbon_global <- rast(here('data','processed','global','carbon_global_50km.tif'))
bio_global <- rast(here('data','processed','global','biodiversity_global_50km.tif'))

#reading in total abandonment CSV
abandonment_total <- read_csv(here('data','processed','global','total_abandonment.csv')) 
vec <- c("ssp1", "ssp2", "ssp3", "ssp4", "ssp5")



## Brazil:
ssp1_brazil <- rast(here('data','raw','abandoned_crop','SSP1_abandoned_cropland_brazil.tif'))
ssp2_brazil <- rast(here('data','raw','abandoned_crop','SSP2_abandoned_cropland_brazil.tif'))
ssp3_brazil <- rast(here('data','raw','abandoned_crop','SSP3_abandoned_cropland_brazil.tif'))
ssp4_brazil <- rast(here('data','raw','abandoned_crop','SSP4_abandoned_cropland_brazil.tif'))
ssp5_brazil <- rast(here('data','raw','abandoned_crop','SSP5_abandoned_cropland_brazil.tif'))
carbon_brazil <- rast(here('data','processed','brazil','carbon_brazil_noPant.tif'))
bio_brazil <- rast(here('data','processed','brazil','biodiversity_extrisk_brazil_noPant.tif'))





### BEGIN UI ###

ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"),
             tags$head(tags$style(HTML('.navbar-static-top {background-color: 2E86C1;}',
                                       '.navbar-default .navbar-nav>.active>a {background-color: black;}'))),
             title = "Cropland Abandonment",
             
             
             ## FIRST TAB ##
             tabPanel("Introduction", icon = icon("align-left"),
                      titlePanel("Introduction"),
                      mainPanel(width = 10, h5(strong("Authors:"), 
                                               "Max Settineri |", 
                                               "Nickolas McManus |",
                                               "Lucas Boyd", 
                                               "Michelle Geldin |", 
                                               "Shayan Kaveh |")
                      )), #END TAB 1
             
             
             
             ## SECOND TAB ##
             tabPanel("Global", icon = icon("globe"),
                      titlePanel("Major Trends in Projected Global Cropland Abandonment"),
                      sidebarLayout(
                        sidebarPanel(
                          
                          # define alpha sliders for tmap 
                          radioButtons(inputId = "ssp_global_radio", 
                                       label = h3("Abandonment by climate scenario"), 
                                       choices = c("SSP 1" = "ssp1_global", 
                                                   "SSP 2" = "ssp2_global", 
                                                   "SSP 3" = "ssp3_global",
                                                   "SSP 4" = "ssp4_global",
                                                   "SSP 5" = "ssp5_global"),
                                       selected = "ssp1"),
                          sliderInput("abandon_slide", label = h3("Abandonment"), 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.8),
                          sliderInput("carbon_slide", label = h3("Carbon Sequestration Potential"), 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.5),
                          sliderInput("bd_slide", label = h3("Biodiversity"), 
                                      min = 0, 
                                      max = 1, 
                                      value = 0.5) 
                        ), # end sidebar panel
                        
                        # A plot of biodiversity in the main panel
                        mainPanel(strong("Directions"), # small title at the top of the main panel
                                  p("Select your Shared Socioeconomic Pathway of interest, then adjust the carbon and biodiversity sliders to visualize your indicator of interest."),
                                  h3("Carbon Sequestration"),
                                  tmapOutput(outputId = "ab_tmap"),
                                  p(strong("Figure 1:"),"Red indicates projected proportion of agricultural abandonment in a given pixel (square kilometers of abandonment/square kilometers in a pixel). Darker colors signal more abandonment in a given pixel. The blue represents carbon sequestration potential of land for 30 years following human disturbance."),
                                  p("Data Source:", a(href = "https://data.globalforestwatch.org/documents/gfw::carbon-accumulation-potential-from-natural-forest-regrowth-in-forest-and-savanna-biomes/about ", "Carbon Accumulation Potential"), ""),
                                  h3("Biodiversity - Conservation Priorities"),
                                  tmapOutput(outputId = "ab_tmap2"),
                                  p(strong("Figure 1:"),"Red indicates projected proportion of agricultural abandonment in a given pixel (square kilometers of abandonment/square kilometers in a pixel). Darker colors signal more abandonment in a given pixel. The green represents biodiversity, with darker colors indicating a higher level of priority for conservation."),
                                  p("Data Source:", a(href = "http://www.sparc-website.org/", "SPARC Conservation Priorities"), ""),
                                  h3("Total Abandonment by Climate Scenario"),
                                  plotOutput(outputId = "total_abandonment_plot"),
                                  p(strong("Figure 2:"), "Total abandoned cropland globally in 2050 (km^2) by climate scenario. Percentages indicate the proportion of total cropland that is projected to be abandoned.")
                        ) # end main panel tab 1
                      ) # end sidebarlayout
             ), # END TAB 2
             
             
             
             
             ## THIRD TAB ##
             tabPanel("Brazil",  icon = icon("flag"),
                      titlePanel("Abandonment by country"),
                      mainPanel(width = 10, strong("Directions")
                      ) # end main panel of tab 3
             ) # END TAB 3
  ) # end navbarpage
) # end UI





### BEGIN SERVER ###

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ssp_reactive <- reactive({
    switch(input$ssp_global_radio,
           "ssp1_global" = ssp1_global,
           "ssp2_global" = ssp2_global,
           "ssp3_global" = ssp3_global,
           "ssp4_global" = ssp4_global,
           "ssp5_global" = ssp5_global)
  })
  
  # TMAP 1 front page
  output$ab_tmap <- renderTmap({
    req(input$ssp_global_radio)
    message(input$ssp_global_radio)
    tm_shape(shp = ssp_reactive()) + # *** need to find a way to make this reactive to different rasters input$ssp_radio
      tm_raster(title = "Proportion abandoned", 
                col = "global_PFT_2015", 
                palette = "Reds", 
                style = "cont", 
                alpha = input$abandon_slide) +
      tm_shape(carbon_global, raster.downsample = FALSE) +
      tm_raster(title = "C seq. (mg/ha/yr)", 
                col = "sequestration_rate_mean_aboveground_full_extent_Mg_C_ha_yr", 
                palette = "Blues", 
                style = "cont", 
                alpha = input$carbon_slide) # + need to figure out what's going on with this downsampling - abandonment map comes up blank when max.raster is expanded
    #  tmap_options(max.raster = c(plot = 1e10, view = 1e10)) 
  }) # end tmap 1
  
  # TMAP 2 front page
  
  output$ab_tmap2 <- renderTmap({
    req(input$ssp_global_radio)
    message(input$ssp_global_radio)
    tm_shape(shp = ssp1) + # *** need to find a way to make this reactive to different rasters input$ssp_global_radio
      tm_raster(title = "Proportion abandoned", 
                col = "global_PFT_2015", 
                palette = "Reds", 
                style = "cont", 
                alpha = input$abandon_slide) +
      tm_shape(bd, raster.downsample = FALSE) +
      tm_raster(title = "Conservation Priorities",
                col = "sparc_conservationPriorities",
                palette = "Greens",
                style = "cont",
                alpha = input$bd_slide)
    
  }) # end tmap 2
  
  # total abandonment ggplot panel 1
  output$total_abandonment_plot <- renderPlot({
    ggplot(data = abandonment_total, 
           aes(x = ssp, y = abandonment_millions_km2, 
               fill = abandonment_millions_km2), 
           alpha = 0.9) +
      geom_col() +
      theme_minimal(14) + 
      labs(x = element_blank(), y = "Global abandonment (millions km^2)") +
      theme(axis.text.x = element_text(
        vjust = 5, 
        size = 16), 
        axis.text.y = element_text(
          size = 16
        )) + 
      geom_text(aes(x = ssp, 
                    y = abandonment_millions_km2 + .2, 
                    label = paste(percent, "%")), 
                color = "black", 
                size = 7) +
      theme(legend.position = "none") +
      scale_fill_gradientn(colors = c("deepskyblue3", "deepskyblue4"))
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
