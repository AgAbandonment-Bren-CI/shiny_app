
# attach packages
library(shiny)
library(here)
library(tidyverse)
library(tmap)
library(terra)
library(sf)
library(shinythemes)
library(shinyWidgets)
library(shinyjs)
library(htmltools)
library(magick)
library(kableExtra)
library(rsconnect)


##### READ IN DATA #####

### GLOBAL:  --------------------------------------------------------

## Biodiversity and carbon:
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


### BRAZIL:  --------------------------------------------------------

## Biome raster and vector
biomes_rast <- rast(here('data/processed/brazil/biomes_rast.tif'))
biomes_vect <- read_sf(here('data/processed/brazil/biome_vector/biomes_vect.shp'))

## Biome prioritizr stats (based on 1km res rasters)
biome_stats <- read_csv(here('data/processed/brazil/prioritizr_outputs/biome_stats.csv'))


### Creating banner image
## Load the image
# banner <- magick::image_read("www/deforest.jpg")
# ## Define the crop region
# crop_region <- geometry_area(width = 1414, height = 150, x_off = 0, y_off = 300)
# ## Crop and save the image
# banner_cropped <- image_crop(banner, crop_region)
# image_write(banner_cropped, path = here("shiny_app/www/cropped_image.png"))





##### BEGIN UI ##### 

ui <- fluidPage(
  navbarPage(theme = shinytheme("flatly"),
             ## banner image
             header = HTML('<center><img src="cropped_image.png" style="display: block; margin-left: auto; margin-right: auto; height: 125px; width: 100% "/></center>'),
             ### put in previous line to change font
             # '* {font-family: "Proxima Nova"};'
             title = "Cropland Abandonment",
             
             
             ## FIRST TAB ##
             tabPanel("Introduction", icon = icon("align-left"),
                      mainPanel(width = 15, 
                                br(),
                                
                                ### put text inside colored box
                                tags$div(
                                  style = "background-color: #ecf0f1; padding: 10px; padding-top: 1px; border-radius: 3px;",
                                  h2(strong("Projections of Future Cropland Abandonment:", br(), "Impacts to Biodiversity and Carbon Sequestration")),
                                  p(" "),
                                  h5(strong("Authors:"), 
                                     "Max Settineri |", 
                                     "Nickolas McManus |",
                                     "Lucas Boyd |", 
                                     "Michelle Geldin |", 
                                     "Shayan Kaveh"),
                                  hr(style = 'border-top: 2px solid #2d3e50'),
                                  p("Socioeconomic shifts and environmental change are likely to drive shifts in the distribution of global agriculture, resulting in the large-scale abandonment of croplands. Abandonment is driven by various ecological, socioeconomic, and climatic factors that vary considerably by region. If done haphazardly, abandonment can cause soil erosion, inhibit nutrient cycling, increase wildfire risk, threaten local food security, and negatively impact species that have adapted to human agricultural landscapes."),
                                  p("However, abandoned lands may also be reforested or rewilded, though the best environmental outcomes generally require incentives. If managed strategically, the rewilding of abandoned lands can serve as a powerful natural climate solution, as revegetation sequesters carbon in the form of plant biomass. Moreover, allowing abandoned land to reforest can preserve biodiversity in some regions, including in the tropics."),
                                  p("Patterns of future cropland abandonment, and strategies for making use of abandoned lands are not well researched. Understanding where, and to what extent cropland abandonment will occur in the future can inform conservation strategies and land use planning. This R Shiny web application allows users to visualize projected trends in cropland abandonment globally, and explore the benefits to biodiversity and carbon sequestration that can be realized by conserving abandoned cropland in Brazil."),
                                  p(HTML("<ul><li>The ‘Global’ tab of this Shiny App presents projected abandoned cropland overlaid with carbon sequestration and biodiversity data to visualize major abandonment trends. Users can visualize global projections of abandoned croplands under five SSP scenarios in 2050 to examine the implications of abandonment to biodiversity and carbon sequestration. This analysis was performed at a global scale with the intent of identifying regions where abandoned lands are projected to overlap with areas of high importance for biodiversity and carbon storage.</li> <li>In the ‘Brazil’ tab, we focus our scope and identify parcels of projected abandoned cropland in Brazil that offer the highest benefits to biodiversity and carbon sequestration if actively restored. This tab explores the results of a prioritization analysis that weighs benefits to biodiversity and carbon sequestration against the cost of restoration for individual parcels.</li></ul>"))
                                ) #end colored box
                                ) #end mainPanel
                      ), #END TAB 1
               
             
             
            ## SECOND TAB ##
            tabPanel("Background/Data", icon = icon("info-circle"),
                     mainPanel(width = 10,
                               h2(strong("Background")),
                               h4(strong("Shared Socioeconomic Pathways (SSPs)")),
                               p(HTML("The SSP climate scenarios provide a way to explore varying levels of future greenhouse gas emissions under different global approaches to climate policy through 2100 (O’Neill et al., 2017). The five SSP scenarios outlined in Figure 1 represent distinct narratives describing various challenges to the mitigation of and adaptation to climate change. These narratives cover a wide range of potential futures and incorporate socio-economic, political, demographic, technological, and lifestyle trends. SSP1, “Sustainability”, depicts a gradual global shift towards environmentally friendly, inclusive development emphasizing human well-being instead of economic gain. SSP 5 on the other hand, depicts continued global economic development powered by fossil fuels and resource exploitation. SSP2 represents the “Middle of the Road” scenario, where slow progress is made toward sustainable development goals and income inequality persists (Riahi et al., 2017). This Shiny App uses SSP scenarios to project cropland abandonment under different levels of future climate change.")),
                               plotOutput('ssppic'),
                               p(""),
                               p(strong("Figure 1: "),"The relative mitigation and adaptation challenges for each of the five SSP scenarios (O’Neill et al., 2017)."),
                               br(),
                               
                               h4(strong("Brazil Prioritization Model")),
                               p("The Brazil tab of this Shiny App allows users to examine raster outputs from a prioritization model. The prioritization software requires the input of the features and cost to be evaluated for each planning unit, specific targets for how much of each feature should be represented in the solution, and a primary objective for solving the problem (Figure 2). The spatial restoration prioritization was computed using the prioritizr package (version 7.2.2) in R. This software uses mixed integer linear programming and provides greater flexibility in building and solving spatial planning problems than similar conservation tools, such as Marxan (Beyer et al., 2016).  A separate problem was formulated for each SSP scenario and their associated planning units and features data."),
                               plotOutput('priorpic'),
                               p(" "),
                               p(strong("Figure 2: "), "Here we see the required inputs and process flow for the Prioritizr model."),
                               
                               h2(strong("Data Descriptions")),
                               h4(strong("Projected Land-use:")),
                               p("This analysis used land-use data from Chen et al., 2021. This dataset projects future land cover out to 2100 in 5 year intervals under each SSP scenario. Current (2015) land-use was compared against future projections to determine where cropland abandonment may occur by 2050. For this analysis, any current cropland with a different future land-use classification is classified as 'abandoned', with the exception of land that has been urbanized. Urbanized cropland is excluded as these parcels cannot realistically be restored to their pre-agricultural land cover type."),
                               p(""),
                               h4(strong("Biodiversity and Carbon:")),
                               p("Biodiversity data was sourced from a study mapping global conservation priorities at 5 km resolution based on current and future species distribution models. These models evaluated over 17,000 terrestrial vertebrate species and future bioclimatic variables under the RCP2.6 and RCP8.5 climate scenarios (Roehrdanz et al., 2021). Aggregate extinction risk values were calculated based on the proportion of a species’ range conserved, then summed and normalized across all species (Hannah et al., 2020). To maximize the benefits to present and future biodiversity, and therefore reduce the risk of extinction, parcels with higher extinction risk values were prioritized for restoration. 
"),
                               p("The carbon dataset, originally created by Cook-Patton et al. and updated by Global Forest Watch, estimates the carbon sequestration rate in aboveground and belowground biomass during the first 30 years of natural forest regeneration. Spatial sequestration estimates include all forest and savanna biomes in units of MgC/ha/yr at 1 km resolution. Carbon data are missing for most of the Pantanal biome, a mainly large freshwater wetland in the southwestern portion of Brazil. As a result, this biome was removed from all other feature and planning unit layers."),
                               h4(strong("Brazilian Restoration Budgets:")),
                               p("The restoration prioritization model can be run under two ends of the budget spectrum. The low-end budget was based on the historical trend of unused money allocated to the environmental management budget of Brazil's Ministry of the Environment. By applying this trend to the current (2023) budget, we found that restoration efforst could be financed by the potentialy unused 455 million Brazilian Reals (BRL). The high-end budget scenario of 3.4 billion BRL reflects the current balance of the Amazon Fund. This fund can be utilized by nonprofits, universities, and international and government projects that prevent, monitor, and reverse deforestation in Brazil."),
                               br(),
                               h4(strong("Data Used")),
                               tableOutput('data_table'),
                               br(),
                               h4(strong("References:")),
                               tags$li("Beyer, H. L., Dujardin, Y., Watts, M. E., & Possingham, H. P. (2016). Solving conservation planning problems with integer linear programming. Ecological Modelling, 328, 14–22. https://doi.org/10.1016/j.ecolmodel.2016.02.005"),
                               tags$li("Cook-Patton, S. C., Leavitt, S. M., Gibbs, D. et al. (2020). Mapping carbon accumulation potential from global natural forest regrowth. Nature, 585(7826), 545–550. https://doi.org/10.1038/s41586-020-2686-x"),
                               tags$li("Hannah, L., Roehrdanz, P. R., Marquet, P. A., Enquist, B. J., Midgley, G., Foden, W., Lovett, J. C., Corlett, R. T., Corcoran, D., Butchart, S. H. M., Boyle, B., Feng, X., Maitner, B., Fajardo, J., McGill, B. J., Merow, C., Morueta-Holme, N., Newman, E. A., Park, D. S., … Svenning, J.-C. (2020). 30% land conservation and climate action reduces tropical extinction risk by more than 50%. Ecography, 43(7), 943–953. https://doi.org/10.1111/ecog.05166"),
                               tags$li("O’Neill, B. C., Kriegler, E., Ebi, K. L., Kemp-Benedict, E., Riahi, K., Rothman, D. S., van Ruijven, B. J., van Vuuren, D. P., Birkmann, J., Kok, K., Levy, M., & Solecki, W. (2017). The roads ahead: Narratives for shared socioeconomic pathways describing world futures in the 21st century. Global Environmental Change, 42, 169–180. https://doi.org/10.1016/j.gloenvcha.2015.01.004"),
                               tags$li("Riahi, K., van Vuuren, D. P., Kriegler, E., Edmonds, J., O’Neill, B. C., Fujimori, S., Bauer, N., Calvin, K., Dellink, R., Fricko, O., Lutz, W., Popp, A., Cuaresma, J. C., Kc, S., Leimbach, M., Jiang, L., Kram, T., Rao, S., Emmerling, J., … Tavoni, M. (2017). The Shared Socioeconomic Pathways and their energy, land use, and greenhouse gas emissions implications: An overview. Global Environmental Change, 42, 153–168. https://doi.org/10.1016/j.gloenvcha.2016.05.009"),
                               tags$li("Roehrdanz, P., Hannah, L., Corcoran, D., Corlett, R., Enquist, B., Fajardo, J., Feng, X., Foden, W., Lovett, J., Maitner, B., Marquet, P., Merow, C., & Midgley, G. (2021). Strategic Conservation of Global Vertebrates in Response to Climate Change. SSRN Electronic Journal: Preprints. https://doi.org/10.2139/ssrn.3854499"),
                               tags$li("Yang, Y., Hobbie, S. E., Hernandez, R. R., Fargione, J., Grodsky, S. M., Tilman, D., Zhu, Y.-G., Luo, Y., Smith, T. M., Jungers, J. M., Yang, M., & Chen, W.-Q. (2020). Restoring Abandoned Farmland to Mitigate Climate Change on a Full Earth. One Earth, 3(2), 176–186. https://doi.org/10.1016/j.oneear.2020.07.019"),
                     )), #END TAB 2
             
             
             ## THIRD TAB ##
             tabPanel("Global", icon = icon("globe"),
                      h1("Projected Global Cropland Abandonment for 2050"),
                      p("Abandoned croplands occupy between 385 and 472 million hectares globally, equivalent to roughly 3% of earth’s land area (Yang et al., 2020). This phenomenon is driven by a host of ecological and socioeconomic factors, many of which will be exacerbated in a warming world. The interactive map in this tab visualizes global cropland abandonment in 2050 based on SSP climate scenarios. Additional biodiversity and carbon layers can be toggled on to highlight where projected cropland abandonment overlaps with areas important for biodiversity and carbon sequestration."),
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          # h2("Global Overlays of Cropland Abandonment, Biodiversity, and Carbon Sequestration:"),
                          # h5(em("Map layers can be toggled on and off using the layer icon on the map")),
                          # hr(style = 'border-top: 2px solid #2d3e50'),
                          
                          h3(strong("Climate scenario:")),
                          h5(em("Select one of the six options below to project cropland abandonment under a particular climate scenario. The first five options correspond with SSPs 1 through 5, while the sixth option represents parcels consistently projected to become abandoned in all five SSPs.")),
                          br(),
                          ## select SSP input
                          selectInput(inputId = "ssp_global_select",
                                      label = NULL,
                                      choices = list("SSP1" = "ssp1_abandonment_global_50km", 
                                                     "SSP2" = "ssp2_abandonment_global_50km", 
                                                     "SSP3" = "ssp3_abandonment_global_50km",
                                                     "SSP4" = "ssp4_abandonment_global_50km",
                                                     "SSP5" = "ssp5_abandonment_global_50km",
                                                     "SSP Overlap" = "ssp_all_abandonment_global_50km"),
                                      selected = 'SSP1'),
                          br(),
                          # hr(style = 'border-top: 1px dashed #2d3e50'),
                          
                          ## define alpha sliders for tmap 
                          h3(strong("Layer transparency:")),
                          h5(em("Use the sliders below to adjust the transparency of individual map layers")),
                          sliderInput("abandon_slide", label = h4("Abandonment:"), 
                                      min = 0, 
                                      max = 1, 
                                      step = 0.1,
                                      value = 0.8,
                                      ticks = F),
                          sliderInput("carbon_slide", label = h4("Carbon sequestration potential:"), 
                                      min = 0, 
                                      max = 1, 
                                      step = 0.1,
                                      value = 0.5,
                                      ticks = F),
                          sliderInput("bd_slide", label = h4("Biodiversity:"), 
                                      min = 0, 
                                      max = 1, 
                                      step = 0.1,
                                      value = 0.5,
                                      ticks = F) 
                        ), # end sidebar panel
                        
                        # Map and figure 
                        mainPanel(
                                  ## TMAP
                          h3("Cropland Abandonment, Biodiversity, and Carbon Sequestration:"),
                          h5(em("Map layers can be toggled on and off using the layer icon on the map. Use the left-hand panel to change map inputs.")),
                          # hr(style = 'border-top: 2px solid #2d3e50'),
                                  tmapOutput(outputId = "ab_tmap", height = 600),
                                  p(strong("Figure 1:"),"Red indicates the projected proportion of agricultural abandonment within a given pixel at 50km resolution. Darker colors signal a great proportion of abandonment. The green represents carbon sequestration potential of land for 30 years following human disturbance. The blue represents biodiversity, with darker colors indicating a higher level of priority for conservation."),
                                  
                                  ## Bar Graph
                                  br(),
                                  h3("Total Abandonment by Climate Scenario:"),
                                  plotOutput(outputId = "total_abandonment_plot"),
                                  p(strong("Figure 2:"), "Total amount of abandoned and new cropland globally (millions km",tags$sup("2"),") in 2050 by climate scenario. Total abandonment (red) is greatest under SSP1 and lowest under SSP2. Newly developed cropland (yellow) is the highest under SSP3, while SSP1 depicts the least new cropland.")
                        ), # end main panel tab 1
                        
                      ) # end sidebarlayout
             ), # END TAB 3
             
             
            
             
             
             ## FOURTH TAB ##
             tabPanel("Brazil",  icon = icon("seedling"),
                      h1("Brazil Abandonment & Restoration"),
                      p("Here, we turn our attention to the abandoned cropland in Brazil. As a country, Brazil stands out as a crucial contributor to climate resilience due to its vast carbon storage capacity and significance to biodiversity. To support global efforts aimed at safeguarding critical regions like the Amazon, we have singled out Brazil as the ideal location to pinpoint areas of projected abandonment that hold the potential for maximum benefits in terms of carbon sequestration and biodiversity if actively restored. This tool enables the user to identify the parcels most suitable for restoration under a specific climate scenario and budgetary constraint. Follow the steps on the left side panel to create your own restoration prioritization model."),
                      headerPanel(""), ## add vertical space
                      sidebarLayout(
                        sidebarPanel(h2(strong("Prioritization Model:")),
                                     hr(style = 'border-top: 2px solid #2d3e50'),
                                     
                                     ## SSP Select Input:
                                     h3(strong("Step 1: Climate scenario")),
                                     h5(em("Select one of the six options below to project cropland abandonment under a particular climate scenario. The first five options correspond with SSPs 1 through 5, while the sixth option represents parcels consistently projected to become abandoned in all five SSPs.")),
                                     br(),
                                     selectInput(inputId = "ssp_brazil_select", 
                                       label = NULL,
                                       choices = list("SSP1" = "ssp1", 
                                                   "SSP2" = "ssp2", 
                                                   "SSP3" = "ssp3",
                                                   "SSP4" = "ssp4",
                                                   "SSP5" = "ssp5",
                                                   "SSP Overlap" = "ssp_all"),
                                       selected = "ssp1_"),
                                     hr(style = 'border-top: 1px solid #2d3e50'),
                                     
                                     ## Feature sliders:
                                     h3(strong("Step 2: Feature weights")),
                                     h5(em("Weigh the relative importance of carbon and biodiversity in this restoration model by moving the slider farther left or right, respectively.")),
                                     br(),
                                     ### read in custom slider script
                                     includeScript("slider.js"),
                                     div(class="my_slider", 
                                         sliderInput("feat_weight",
                                                     label = NULL, 
                                                     ticks = TRUE,
                                                     min = 1, max = 5, 
                                                     value = 3)),
                                     hr(style = 'border-top: 1px solid #2d3e50'),
                                     
                                     ## Budget radio buttons:
                                     h3(strong("Step 3: Budget")),
                                     h5(em("Select a low (455 million BRL) or high (3.4 billion BRL) budget scenario for restoration.")),
                                     br(),
                                     radioButtons(inputId = "budget",
                                                  label = NULL,
                                                  choices = c("Low" = "lowBud", 
                                                              "High" = "highBud"),
                                                  selected = "highBud"),
                                     hr(style = 'border-top: 1px solid #2d3e50'),
                                     
                                     ## Resolution buttons:
                                     h3(strong("Step 4: Resolution")),
                                     h5(em("Select the resolution of model results visualized on the map. (WARNING: 1km resolution has significantly longer loading times)")),
                                     br(),
                                     radioButtons(inputId = "resolution",
                                                  label = NULL,
                                                  choices = c("1 km" = "1km", 
                                                              "5 km" = "5km"),
                                                  selected = "5km"),
                        ), # end sidebar panel
                        
                        mainPanel(tmapOutput(outputId = "ab_brazil_tmap", height = 700),
                                  htmlOutput('scenario_text'),
                                  br(),
                                  plotOutput('biome_stats_plot', height = 600),
                                  p(strong("Figure 2:"), "Zonal statistics of cropland abandonment and restoration by Brazilian biome. Orange bars represent the projected amount of abandonment by 2050 in each biome; blue bars represent the number of these parcels selected for restoration based on user inputs.")
                        ), # end main panel of tab 3
                        position = c('left', 'right'),
                        fluid = TRUE
                      ) #end sidebar layout
             
             ) # END TAB 4
  ) # end navbarpage
) # end UI





##### BEGIN SERVER #####

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # intro tab image
  # output$intropic <- renderPlot({
  #   ggdraw ()+
  #     draw_image(here("deforest.jpg"))
  # })

  
  
  
  ### TAB 2 - Background info ###  ---------------------------------------------
  
  ## ssp image
  output$ssppic <- renderPlot({
               ggdraw() + 
                 draw_image(here("ssp.jpg")
    )
  })
  
  ## prioritizer image
  
  output$priorpic <- renderPlot({
    ggdraw() + 
      draw_image(here("prioritizer.png")
      )
  })               

  ## Data table:
  
  data_info <- data.frame(
    Layer = c("Future global land cover", "Carbon accumulation potential", "Biodiversity"),
    Source = c("https://zenodo.org/record/4584775#.Y_58_uzMJJV", "https://data.globalforestwatch.org/documents/gfw::carbon-accumulation-potential-from-natural-forest-regrowth-in-forest-and-savanna-biomes/about", "http://www.sparc-website.org/"),
    Description = c("Future land cover at 1-km resolution based on the SSP-RCP scenarios, classified by plant functional types (PFTs),  including a “cropland” designation, which was the focus of this analysis.", "Global carbon accumulation potential from natural forest regrowth at 1-km. This dataset was used to visualize carbon sequestration potential from restoration.", "Global spatial dataset at 5km resolution displaying rank-ordered areas of high importance to biodiversity preservation. The rank order of importance was determined by examining current and future ranges of 17,000 vertebrate species and their relative extinction risks.")
  )
  
  output$data_table <- function() {
    data_info <- data_info %>% 
      mutate(Source = sprintf("<a href='%s'>%s</a>", Source, Layer))
    data_info %>%
      knitr::kable(format = "html", escape = FALSE) %>%
      kable_styling(bootstrap_options = "striped", full_width = FALSE)
  }

  
  
  
  ### TAB 3 - Global Abandonment ### -------------------------------------------
  
  ## Abandonment raster for tmap
  ssp_reactive <- reactive({
    ## select raster layer by SSP
    x = input$ssp_global_select
    ## read in raster
    file_name = paste0(x, ".tif") 
    raster = rast(here("data/processed/global/", file_name))
    
    return(raster)
  })
  
  
  ## TMAP 1: Global layers for abandonment, carbon, and bio
  output$ab_tmap <- renderTmap({
    tm_shape(shp = ssp_reactive(), name = "Abandoned cropland",
             ##these rasters won't load unless downsampled
             raster.downsample = T) + 
      tm_raster(title = "Proportion abandoned", 
                palette = "Reds", 
                style = "cont", 
                alpha = input$abandon_slide) +
      tm_shape(carbon_global, name = "Carbon",
               raster.downsample = T) +
      tm_raster(title = "C seq. (mg/ha/yr)", 
                palette = "Greens", 
                style = "cont", 
                alpha = input$carbon_slide) +
      tm_shape(bio_global, name = "Biodiversity",
               raster.downsample = T) +
      tm_raster(title = "Conservation Priorities",
                palette = "Blues",
                style = "cont",
                alpha = input$bd_slide) +
      tm_layout(legend.stack = 'vertical') +
      tm_view(set.view = 2)
  }) # End TMAP 1
  

  
  
  ## total abandonment ggplot (Fig 2)
  output$total_abandonment_plot <- renderPlot({
    ggplot(data = abandonment_total, aes(x = ssp, y = amount)) +
      geom_bar(aes(fill = statistic), 
               stat = 'identity', position = 'dodge', 
               alpha = 0.8,
               color = 'grey30') +
      geom_text(aes(label = round(amount,3), group = statistic), vjust = -0.5, size = 4, fontface = 'bold', position = position_dodge(width = 0.9)) +
      theme_minimal() + 
      ylab(bquote(bold('Cropland (millions'~km^2~')'))) +
      labs(x = element_blank()) +
      scale_x_discrete(labels = c('SSP1', 'SSP2', 'SSP3', 'SSP4', 'SSP5')) +
      theme(legend.title = element_blank()) +
      scale_fill_manual(values = c("wheat", "#E6673E"),
                        labels = c("New Cropland", "Total Abandonment")) +
      theme(
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(face = 'bold', size = 13, vjust = 7),
        axis.text.y = element_text(size = 12),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.title.align = 0.5,
        legend.position = c(0.12, 0.89)
      )
      
  })
  
  
  
  ### TAB 4 - Brazilian Restoration ### ----------------------------------------
  
  
  ## Read in 5km raster for TMAP based on "model" user inputs
  raster_layer_5km <- reactive({
    ## assign SSP selection to variable
    x = input$ssp_brazil_select
    ## Load in raster stack based on budget AND ssp (x)
    y = input$budget
    ## assign variable to slider value
    val = input$feat_weight
    ## choose layer in raster stack (y) based on val
    z = if(val == 1) {
      "feat15"
    } else if (val == 2) {
      "feat24"
    } else if (val == 3) {
      "feat33"
    } else if (val == 4) {
      "feat42"
    } else {"feat51"}
    
    ## Create file path/name based on resolution and other inputs
    rez = input$resolution
    file_name = paste(x, y, z, sep = "_") %>% 
      paste0("_", rez, ".tif")
    file_path = paste0("data/processed/brazil/prioritizr_outputs/", rez)
    
    raster = rast(here(file_path, file_name))
    
    return(raster)
  })
  
  
  
  ## TMAP Brazil
  output$ab_brazil_tmap <- renderTmap({
    tmap_mode('view') +
    tm_shape(shp = biomes_vect, name = "Biomes") +
      tm_borders(lwd = 1, col = 'gray40') +
    tm_shape(shp = raster_layer_5km(), name = "Model output",
             raster.downsample = F) + 
      tm_raster(palette = c('#F67D4B', '#1A5EAB'),
                style = "cat",
                title = "Restoration model output:",
                labels = c("Abandoned cropland", "Restoration priority")) +
    tm_scale_bar(position = c('right', 'bottom')) 
  }) # end TMAP Brazil
  
  
  
  ## Filter biome stats according to user inputs
  biome_stats_filtered <- reactive({
    ## assign the inputs
    x = input$ssp_brazil_select
    y = input$budget
    val = input$feat_weight
    z = if(val == 1) {
      "feat15"
    } else if (val == 2) {
      "feat24"
    } else if (val == 3) {
      "feat33"
    } else if (val == 4) {
      "feat42"
    } else {"feat51"}
    
    ## filter df by inputs
    stats_filtered <- biome_stats %>% 
      filter(ssp == x,
             budget == y,
             feat == z)
    
    return(stats_filtered)
  })
  
  
  ## Generate stats for TMAP caption
  
      ## How many parcels are projected to be abandoned?
      parcels_avail <- reactive({
        ## grab specific raster layer
        x = biome_stats_filtered()
        ## filter further to select only planning units
        x_pus <- x %>% 
          filter(category == "pus")
        ## sum and return total count of pus
        pus = sum(x_pus$amount)
        return(pus)
      })
  
      ## How many parcels should be restored?
      parcels_selected <- reactive({
        ## grab specific raster layer
        x = biome_stats_filtered()
        ## filter further to select only solution parcels
        x_sol <- x %>% 
          filter(category == "sol")
        ## sum and return total count of selected parcels
        sol = sum(x_sol$amount)
        return(sol)
      })
    
  
  ## Reactive caption for TMAP
  output$scenario_text <- renderText({
    paste("<b>","Figure 1:","</b>","Parcels of projected abandonment between 2020-2050 at 5km resolution. In this selected scenario, a total of", "<b>",parcels_avail(), "km",tags$sup("2"),"</b>", " (orange pixels) are projected to be abandoned by 2050. Of those,", "<b>",parcels_selected(), "km",tags$sup("2"),"</b>", " (blue pixels) were prioritized for restoration.")
  })

  
  ## Biome stats plot (Fig 2)
  output$biome_stats_plot <- renderPlot(
    ggplot(data = biome_stats_filtered(), aes(x = name_biome, y = amount)) +
      geom_col(aes(fill = category), 
               position = 'identity', 
               color = 'lightsteelblue4') +
      scale_fill_manual(values = c('lightsalmon', '#1A5EAB'),
                        labels = c('Total abandonment', 'Restoration')) +
      geom_text(aes(label = amount), vjust = -0.5, size = 4, fontface = 'bold') +
      ylab(bquote(bold('Area '(km^2)))) +
      labs(x = element_blank()) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(face = 'bold', size = 14, vjust = 5),
        axis.title.y = element_text(face = 'bold', size = 14),
        axis.text.x = element_text(face = 'bold', size = 13, vjust = 9),
        axis.text.y = element_text(size = 10),
        panel.grid.major.x = element_blank(),
        legend.title = element_blank(),
        legend.text = element_text(size = 11),
        legend.title.align = 0.5,
        legend.position = c(0.89, 0.93)
      )
  )

}


# Run the application 
shinyApp(ui = ui, server = server)
