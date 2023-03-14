
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


##### READ IN DATA #####

### GLOBAL:  --------------------------------------------------------
## Cropland abandonment:
ssp1_global <- rast(here('data/processed/global/ssp1_abandonment_global_50km.tif'))
ssp2_global <- rast(here('data/processed/global/ssp2_abandonment_global_50km.tif'))
ssp3_global <- rast(here('data/processed/global/ssp3_abandonment_global_50km.tif'))
ssp4_global <- rast(here('data/processed/global/ssp4_abandonment_global_50km.tif'))
ssp5_global <- rast(here('data/processed/global/ssp5_abandonment_global_50km.tif'))
ssp_all_global <- rast(here('data/processed/global/ssp_all_abandonment_global_50km.tif'))
## Biodiversitya and carbon:
carbon_global <- rast(here('data/processed/global/carbon_global_50km.tif'))
bio_global <- rast(here('data/processed/global/biodiversity_global_50km.tif'))

## Total abandonment CSV
abandonment_total <- read_csv(here('data/processed/global/total_abandonment.csv')) %>% 
  janitor::clean_names() %>% 
  mutate(total_abandonment_mil_km2 = total_abandonment_km2/1000000,
         new_cropland_mil_km2 = new_cropland_km2/1000000) %>% 
  ## remove variables that aren't plotted
  select(!c(1, 3:7)) %>% 
  ## pivot longer for graphable format
  pivot_longer(2:3, names_to = 'statistic', values_to = 'amount')

## ////I DON'T KNOW WHAT THIS IS FOR?? - NM/////////
vec <- c("ssp1_global", "ssp2_global", "ssp3_global", "ssp4_global", "ssp5_global")


### BRAZIL:  ------------------------------------------------------
## Cropland abandonment
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
ssp_all_brazil <- rast(here('data/processed/brazil',
                         'ssp_all_abandoned_cropland_brazil.tif'))

## Carbon and biodiversity data
carbon_brazil <- rast(here('data/processed/brazil',
                           'carbon_brazil_noPant.tif'))
bio_brazil <- rast(here('data/processed/brazil',
                        'biodiversity_extrisk_brazil_noPant.tif'))

## Prioritizr solution rasters
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
ssp_all_solution <- rast(here('data/processed/brazil/prioritizr_outputs',
                              'ssp_all_solution_country.tif'))





##### BEGIN UI ##### 

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
                                                   "SSP 5" = "ssp5_global",
                                                   "SSP Overlap" = "ssp_all_global"),
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
                                  # h3("Biodiversity - Conservation Priorities"),
                                  # tmapOutput(outputId = "ab_tmap2"),
                                  # p(strong("Figure 1:"),"Red indicates projected proportion of agricultural abandonment in a given pixel (square kilometers of abandonment/square kilometers in a pixel). Darker colors signal more abandonment in a given pixel. The green represents biodiversity, with darker colors indicating a higher level of priority for conservation."),
                                  # p("Data Source:", a(href = "http://www.sparc-website.org/", "SPARC Conservation Priorities"), ""),
                                  h3("Total Abandonment by Climate Scenario"),
                                  plotOutput(outputId = "total_abandonment_plot"),
                                  p(strong("Figure 2:"), "Total abandoned cropland globally in 2050 (km^2) by climate scenario. Percentages indicate the proportion of total cropland that is projected to be abandoned.")
                        ) # end main panel tab 1
                      ) # end sidebarlayout
             ), # END TAB 3
             
             
             
             
             ## FOURTH TAB ##
             tabPanel("Brazil",  icon = icon("seedling"),
                      h1("Brazil Abandonment & Restoration"),
                      fluidRow("Here, we turn our attention to the abandoned cropland in Brazil. As a country, Brazil stands out as a crucial contributor to climate resilience due to its vast carbon storage capacity and significance to biodiversity. To support global efforts aimed at safeguarding critical regions like the Amazon, we have singled out Brazil as the ideal location to pinpoint areas of projected abandonment that hold the potential for maximum benefits in terms of carbon sequestration and biodiversity if actively restored. Furthermore, President Da Silva's commitment to halting deforestation in Brazil by 2030 further amplifies the importance of this tool, as it enables the identification of regions that are most suitable for restoration under budgetary constraints. Follow the steps with the left side panel to generate a map identifying which parcels should be prioritized for restoration."),
                      fluidRow(column = 5, ### to make box align with text above but now header off....
                      headerPanel(""), ## add vertical space
                      sidebarLayout(
                        sidebarPanel(h2("Prioritization Model:"),
                                     hr(style = 'border-top: 2px solid #000000'),
                                     
                                     ## SSP radio buttons:
                                     h3(strong("Step 1: Climate scenario")),
                                     h5("Choose one of the six climate scenarios below. The first five options correspond with SSPs 1 through 5, while the sixth option represents parcels consistently projected to become abandoned in all five SSPs."),
                                     br(),
                                     radioButtons(inputId = "ssp_brazil_radio", 
                                       label = NULL,
                                       choices = c("SSP 1" = "ssp1_brazil", 
                                                   "SSP 2" = "ssp2_brazil", 
                                                   "SSP 3" = "ssp3_brazil",
                                                   "SSP 4" = "ssp4_brazil",
                                                   "SSP 5" = "ssp5_brazil",
                                                   "SSP Overlap" = "ssp_all_brazil"),
                                       selected = "ssp1_brazil"),
                                     hr(style = 'border-top: 1px solid #000000'),
                                     
                                     ## Feature sliders:
                                     h3(strong("Step 2: Feature weights")),
                                     h5(em("Use the sliders to weigh the relative importance of biodiversity and carbon, respectively.")),
                                     br(),
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
                                     hr(style = 'border-top: 1px solid #000000'),
                                     
                                     ## Budget radio buttons:
                                     h3(strong("Step 3: Budget")),
                                     h5(em("Select a low or high end budget scenario for restoration. The low budget is based on historical trend of unused money allocated to Brazil's Ministry of the Environment management budget. The high budget is based on the current balance of the Amazon Fund.")),
                                     br(),
                                     radioButtons(inputId = "budget",
                                                  label = NULL,
                                                  choices = c("Low (455 million BRL)" = "low_budget", 
                                                              "High (3.4 billion BRL)" = "high_budget"),
                                                  selected = "low_budget"),
                        ), # end sidebar panel
                        
                        mainPanel(tmapOutput(outputId = "ab_brazil_tmap", height = 800),
                                  p(strong("Figure 1:"),"Parcels projected to be abandoned between 2020-2050. Orange pixels represent parcels prioritized for restoration under based on user inputs, while blue pixels remain unselected abandoned parcels."),
                                  textOutput('scenario')
                        ), # end main panel of tab 3
                        position = c('left', 'right'),
                        fluid = TRUE
                      )) #end sidebar layout
             
             ) # END TAB 4
  ) # end navbarpage
) # end UI





##### BEGIN SERVER #####

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  ### TAB 1 - Landing page ###
  
  ## intro tab image
  output$intropic <- renderPlot({
    ggdraw ()+
      draw_image(here("ag.jpeg"))
  })
  
  
  
  ### TAB 2 - Background info ###
  
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

  
  
  ### TAB 3 - Global Abandonment ###
  
  ## radio buttons
  ssp_reactive <- reactive({
    x = switch(input$ssp_global_radio,
           "ssp1_global" = ssp1_global,
           "ssp2_global" = ssp2_global,
           "ssp3_global" = ssp3_global,
           "ssp4_global" = ssp4_global,
           "ssp5_global" = ssp5_global,
           "ssp_all_global" = ssp_all_global)
    message('in ssp reactive, raster name = ', names(x))
    return(x)
  })
  
  ## TMAP 1: Carbon
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
                alpha = input$carbon_slide) +
      tm_shape(bio_global, raster.downsample = TRUE) +
      tm_raster(title = "Conservation Priorities",
                palette = "Greens",
                style = "cont",
                alpha = input$bd_slide)
    
    # + need to figure out what's going on with this downsampling - abandonment map comes up blank when max.raster is expanded
    #  tmap_options(max.raster = c(plot = 1e10, view = 1e10)) 
  }) # end tmap 1
  
  
  # ## TMAP 2: Biodiversity
  # output$ab_tmap2 <- renderTmap({
  #   req(input$ssp_global_radio)
  #   message(input$ssp_global_radio)
  #   tm_shape(shp = ssp_reactive()) + # *** need to find a way to make this reactive to different rasters input$ssp_global_radio
  #     tm_raster(title = "Proportion abandoned",
  #               palette = "Reds", 
  #               style = "cont", 
  #               alpha = input$abandon_slide) +
  #     tm_shape(bio_global, raster.downsample = TRUE) +
  #     tm_raster(title = "Conservation Priorities",
  #               palette = "Greens",
  #               style = "cont",
  #               alpha = input$bd_slide)
  #   
  # }) # end tmap 2
  
  # total abandonment ggplot panel 1
  output$total_abandonment_plot <- renderPlot({
    ggplot(data = abandonment_total, aes(x = ssp, y = amount)) +
      geom_bar(aes(fill = statistic), 
               stat = 'identity', position = 'dodge', 
               alpha = 0.8,
               color = 'grey20') +
      theme_minimal() + 
      labs(x = element_blank(),
           y = "Millions km^2)")  +
      # theme(legend.position = "none") +
      scale_fill_manual(values = c("forestgreen", "deepskyblue4"))
  })
  
  
  
  # START FOURTH TAB
  
  # radio buttons
  ssp_brazil_reactive <- reactive({
    switch(input$ssp_brazil_radio,
               "ssp1_brazil" = ssp1_solution$ssp1_highBud_c,
               "ssp2_brazil" = ssp2_solution$ssp2_highBud_c,
               "ssp3_brazil" = ssp3_solution$ssp3_highBud_c,
               "ssp4_brazil" = ssp4_solution$ssp4_highBud_c,
               "ssp5_brazil" = ssp5_solution$ssp5_highBud_c,
               "ssp_all_brazil" = ssp_all_solution$ssp_all_highBud_c)
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
  
  parcels_avail <- reactive({req(input$ssp_brazil_reactive$x)})
  parcels_avail2 <- reactive({terra::global(parcels_avail, fun = 'sum', na.rm = TRUE)})
    
  # return stuff for restoration scenario
  output$scenario <- renderText({
    paste("This scenario contains", parcels_avail)
  })
  
}



# Run the application 
shinyApp(ui = ui, server = server)
