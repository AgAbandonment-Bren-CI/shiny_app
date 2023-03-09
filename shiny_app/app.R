
# attach packages
library(shiny)
library(here)
library(tidyverse)
library(tmap)
library(terra)
library(shinythemes)
library(shinyjs)
library(htmltools)
library(cowplot)
library(magick)


### READ IN DATA ###

### Global:
ssp1_global <- rast(here('data','processed','global','ssp1_abandonment_global_50km.tif'))
ssp2_global <- rast(here('data','processed','global','ssp2_abandonment_global_50km.tif'))
ssp3_global <- rast(here('data','processed','global','ssp3_abandonment_global_50km.tif'))
ssp4_global <- rast(here('data','processed','global','ssp4_abandonment_global_50km.tif'))
ssp5_global <- rast(here('data','processed','global','ssp5_abandonment_global_50km.tif'))
carbon_global <- rast(here('data','processed','global','carbon_global_50km.tif'))
bio_global <- rast(here('data','processed','global','biodiversity_global_50km.tif'))

#reading in total abandonment CSV
abandonment_total <- read_csv(here('data','processed','global','total_abandonment.csv')) 
vec <- c("ssp1_global", "ssp2_global", "ssp3_global", "ssp4_global", "ssp5_global")



### Brazil:
ssp1_brazil <- rast(here('data/processed/brazil',
                         'ssp1_abandoned_cropland_brazil.tif'))
ssp2_brazil <- rast(here('data/processed/brazil',
                         'ssp2_abandoned_cropland_brazil.tif'))
ssp3_brazil <- rast(here('data/processed/brazil',
                         'ssp3_abandoned_cropland_brazil.tif'))
ssp4_brazil <- rast(here('data/processed/brazil',
                         'ssp4_abandoned_cropland_brazil.tif'))
ssp5_brazil <- rast(here('data/processed/brazil',
                         'ssp5_abandoned_cropland_brazil.tif'))

carbon_brazil <- rast(here('data/processed/brazil',
                           'carbon_brazil_noPant.tif'))
bio_brazil <- rast(here('data/processed/brazil',
                        'biodiversity_extrisk_brazil_noPant.tif'))

ssp1_solution <- rast(here('data/processed/brazil/prioritizr_outputs',
                           'ssp1_solution_country.tif'))
ssp2_solution <- rast(here('data/processed/brazil/prioritizr_outputs',
                           'ssp2_solution_country.tif'))
ssp3_solution <- rast(here('data/processed/brazil/prioritizr_outputs',
                           'ssp3_solution_country.tif'))
ssp4_solution <- rast(here('data/processed/brazil/prioritizr_outputs',
                           'ssp4_solution_country.tif'))
ssp5_solution <- rast(here('data/processed/brazil/prioritizr_outputs',
                           'ssp5_solution_country.tif'))



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
                                               "Lucas Boyd |", 
                                               "Michelle Geldin |", 
                                               "Shayan Kaveh"),
                                p(HTML("This R Shiny web application presents projected abandoned cropland overlaid with carbon sequestration and biodiversity data to visualize major abandonment trends. First, we visualize global projections of abandoned croplands under five SSP scenarios in 2050 to examine the implications of abandonment to biodiversity and carbon sequestration. This analysis was performed at a global scale with the intent of identifying regions where abandoned lands are projected to overlap with areas of high importance for biodiversity and carbon storage. Next, we focus on Brazil and identify parcels of projected abandonment that could offer the highest benefits to biodiversity and carbon sequestration if actively restored.")),
                                plotOutput('intropic'),
                                )), #END TAB 1
               
             
            ## SECOND TAB ##
            tabPanel("Background/Data", icon = icon("info-circle"),
                     titlePanel("Background"),
                     mainPanel(width = 10,
                               p(HTML("SSP info")),
                               tableOutput('data_table')
                               #                             tags$style(".data_table th, .data_table td {
                               #   border: 1px solid #ddd;
                               #   padding: 8px;
                               #   text-align: left;
                               # }"
                     )), #END TAB 2
             
             
             ## THIRD TAB ##
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
                                       selected = "ssp1_global"),
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
             ), # END TAB 3
             
             
             
             
             ## FOURTH TAB ##
             tabPanel("Brazil",  icon = icon("seedling"),
                      titlePanel("Brazil Abandonment & Restoration"),
                      fluidRow("Here, we investigate which areas of Brazilian cropland abandonment should be prioritized for restoration. This is based on benefits to biodiversity and carbon. More information will go into this section. Follow the instructions on the left side panel."),
                      sidebarLayout(
                        sidebarPanel(h2("Prioritization Model"),
                                     hr(),
                                     ## SSP radio buttons:
                                     radioButtons(inputId = "ssp_brazil_radio", 
                                       label = h3("Step 1: Select a climate scenario"),
                                       choices = c("SSP 1" = "ssp1_brazil", 
                                                   "SSP 2" = "ssp2_brazil", 
                                                   "SSP 3" = "ssp3_brazil",
                                                   "SSP 4" = "ssp4_brazil",
                                                   "SSP 5" = "ssp5_brazil"),
                                       selected = "ssp1_brazil"),
                                     
                                     ## Feature sliders:
                                     h3("Step 2: Select feature weights"),
                                     sliderInput("bd_slide_brazil", 
                                                 label = h4("Biodiversity"), 
                                                 min = 0, 
                                                 max = 3, 
                                                 value = 1),
                                     sliderInput("carbon_slide_brazil", 
                                                 label = h4("Carbon"), 
                                                 min = 0, 
                                                 max = 3, 
                                                 value = 1),
                                     
                                     ## Budget radio buttons:
                                     radioButtons(inputId = "budget",
                                                  label = h3("Step 1: Select a climate scenario"),
                                                  choices = c("Low" = "low_budget", 
                                                              "High" = "high_budget"),
                                                  selected = "ssp1_brazil"),
                        ), # end sidebar panel
                        
                        mainPanel(tmapOutput(outputId = "ab_brazil_tmap", height = 700)
                        ), # end main panel of tab 3
                        position = c('left', 'right'),
                        fluid = TRUE
                      ) #end sidebar layout
             
             ) # END TAB 4
  ) # end navbarpage
) # end UI





### BEGIN SERVER ###

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # START TAB 1
  
  ## intro tab image
  output$intropic <- renderPlot({
    ggdraw ()+
      draw_image(here("ag.jpeg"))
  })
  
  # START SECOND TAB
  
  
  ## Data table:
  
  link_land <- "<a href='https://zenodo.org/record/4584775#.Y_58_uzMJJV'>Chen et al., 2021</a>"
  link_carbon <- "<a href='https://data.globalforestwatch.org/documents/gfw::carbon-accumulation-potential-from-natural-forest-regrowth-in-forest-and-savanna-biomes/about'>Cook-Patton et al., 2020</a>"
  link_bd <- "<a href='http://www.sparc-website.org/'>SPARC Conservation Priorities</a>"
  
  data_info <- data.frame(
    Data_Name = c("Future global land cover", "Carbon accumulation potential", "Biodiversity"),
    Source = c(link_land, link_carbon, link_bd),
    Description = c("Future land cover at 1-km resolution based on the SSP-RCP scenarios, classified by plant functional types (PFTs),  including a “cropland” designation, which was the focus of this analysis.", "Global carbon accumulation potential from natural forest regrowth at 1-km. This dataset was used to visualize carbon sequestration potential from restoration.", "Global spatial dataset at 5km resolution displaying rank-ordered areas of high importance to biodiversity preservation. The rank order of importance was determined by examining current and future ranges of 17,000 vertebrate species and their relative extinction risks.") 
  )
  
  data_info$Source <- as.character(data_info$Source) # convert factor to character
  data_info$Source <- gsub("[\n]", "", data_info$Source) # remove line breaks
  
  output$data_table <- renderTable(data_info, sanitize.text.function = function(x) x)

  # START THIRD TAB
  
  ssp_reactive <- reactive({
    x = switch(input$ssp_global_radio,
           "ssp1_global" = ssp1_global,
           "ssp2_global" = ssp2_global,
           "ssp3_global" = ssp3_global,
           "ssp4_global" = ssp4_global,
           "ssp5_global" = ssp5_global)
    message('in ssp reactive, raster name = ', names(x))
    return(x)
  })
  
  # TMAP 1 front page
  output$ab_tmap <- renderTmap({
    req(input$ssp_global_radio)
    message(input$ssp_global_radio)
    tm_shape(shp = ssp_reactive()) + # *** need to find a way to make this reactive to different rasters input$ssp_radio
      tm_raster(title = "Proportion abandoned", 
                palette = "Reds", 
                style = "cont", 
                alpha = input$abandon_slide) +
      tm_shape(carbon_global) +
      tm_raster(title = "C seq. (mg/ha/yr)", 
                palette = "Blues", 
                style = "cont", 
                alpha = input$carbon_slide) # + need to figure out what's going on with this downsampling - abandonment map comes up blank when max.raster is expanded
    #  tmap_options(max.raster = c(plot = 1e10, view = 1e10)) 
  }) # end tmap 1
  
  # TMAP 2 front page
  
  output$ab_tmap2 <- renderTmap({
    req(input$ssp_global_radio)
    message(input$ssp_global_radio)
    tm_shape(shp = ssp_reactive()) + # *** need to find a way to make this reactive to different rasters input$ssp_global_radio
      tm_raster(title = "Proportion abandoned",
                palette = "Reds", 
                style = "cont", 
                alpha = input$abandon_slide) +
      tm_shape(bio_global, raster.downsample = FALSE) +
      tm_raster(title = "Conservation Priorities",
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
  
  
  
  # START FOURTH TAB
  
  # radio buttons
  ssp_brazil_reactive <- reactive({
    x = switch(input$ssp_brazil_radio,
               "ssp1_brazil" = ssp1_solution$ssp1_highBud_c,
               "ssp2_brazil" = ssp2_solution$ssp2_highBud_c,
               "ssp3_brazil" = ssp3_solution$ssp3_highBud_c,
               "ssp4_brazil" = ssp4_solution$ssp4_highBud_c,
               "ssp5_brazil" = ssp5_solution$ssp5_highBud_c)
    message('in ssp reactive, raster name = ', names(x))
    return(x)
  })
  
  # TMAP Brazil
  output$ab_brazil_tmap <- renderTmap({
    tmap_mode('view')
    
    # req(input$ssp_brazil_radio)
    # message(input$ssp_brazil_radio)
    tm_shape(shp = ssp_brazil_reactive(), raster.downsample = TRUE) + ##make downsample true for now
      tm_raster(title = "Proportion abandoned",
                palette = "Reds", 
                style = "cont") +
    tm_scale_bar(position = c('right', 'bottom'))
      # tm_view(set.view = c(-50, -11.6, 3))
      #tm_view(set.zoom.limits = c(10,20))
      # + need to figure out what's going on with this downsampling - abandonment map comes up blank when max.raster is expanded
    #  tmap_options(max.raster = c(plot = 1e10, view = 1e10)) 
  }) # end tmap 1
  
}



# Run the application 
shinyApp(ui = ui, server = server)
