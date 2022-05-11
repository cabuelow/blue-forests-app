library(shiny)  
#library(shinythemes)
library(dashboardthemes)
library(shinydashboard)
library(dplyr)
library(sf)
library(bslib)
library(mapdeck)
library(shinyjs)
shinyOptions(bslib = TRUE)
thematic::thematic_shiny()
rsconnect::appDependencies()
sf::sf_use_s2(FALSE)

key <- "pk.eyJ1IjoidXRqaW1teXgiLCJhIjoiY2tnMmI1OWRpMDZsdDJxb2Y4MjdnZmxpMyJ9.ImwwUvDQpod7-B0YnIUytw"  
mapdeck(token = key)

# simplify geopackages so app is faster
#library(rmapshaper)
#uni <- st_read('data/units-attributes_wgs84-L4.gpkg')
#s <- ms_simplify(uni, keep_shapes = T)
#ss <- s %>% mutate(seafarm = ifelse(seafarm_area_ha > 0, 1, 0))
#st_write(ss, 'data/units-attributes_wgs84-L4-simp.gpkg', overwrite = T, append = F)

# load data

units1 <- st_read('data/units-attributes_wgs84-simp.gpkg') %>% mutate(HYBAS_ID = ifelse(is.na(HYBAS_ID), 1, HYBAS_ID)) %>% mutate(Unit = as.character(HYBAS_ID)) %>% mutate(prop_vul_pop = lecz_pop_count_sum/pop_count_sum, mangrove_carbon_mgC_ha = mangrove_abg_mgC_ha + mangrove_soil_mgC_ha)
units2 <- st_read('data/units-attributes_wgs84-L4-simp.gpkg') %>% mutate(HYBAS_ID = ifelse(is.na(HYBAS_ID), 1, HYBAS_ID)) %>% mutate(Unit = as.character(HYBAS_ID)) %>% mutate(prop_vul_pop = lecz_pop_count_sum/pop_count_sum, mangrove_carbon_mgC_ha = mangrove_abg_mgC_ha + mangrove_soil_mgC_ha)
unitsNA <- st_read('data/units-noBF_wgs84-simp.gpkg')
wwf <- st_read('data/wwf-bf-projects.gpkg')
scores <- read.csv('data/blue-forest-scores-L4_area-standardised.csv') %>%  # choose scores to plot
  left_join(select(data.frame(st_drop_geometry(units2)), unit_ID, mangrove:seagrass))

terr <- c('Global', sort(unique(as.character(units2$TERRITORY1))))
df <- data.frame(units2) %>% 
  dplyr::select(unit_ID, mangrove, seagrass, saltmarsh, kelp, seafarm) %>% 
  tidyr::pivot_longer(-unit_ID, names_to = 'eco')
df$eco <- recode(df$eco, mangrove = 1, seagrass = 2, saltmarsh = 3, kelp = 4, seafarm = 5)

# Define UI

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem('Explore the coastline',
             radioButtons("radio", label = h6(""), 
                          choices = list('Blank coastline' = '#FFFFFF', "Country/territory" = 'TERRITORY1', 'Marine realm' = 'REALM', "Marine province" = 'PROVINC'),
                         selected = "#FFFFFF", inline = F),
            # menuItem('Choose country or territory',
                      selectInput("var", label = h5("Choose country or territory"), 
                                  choices = terr, 
                                  selected = 'Global'),
             radioButtons("radio2", label = h6(""), 
                          choices = list('Unit' = 'unit_ID', "Unit type" = 'unit_type'),
                          selected = 'unit_type', inline = F)),
    menuItem('Explore blue forest distributions',
               radioButtons("radio1", label = h6(""), 
                                  choices = list( 'None' = 0, "Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4, 'Seaweed farming' = 5,  'All' = 6, 'Number of forests' = 7),
                                  selected = 0, inline = F),
             #menuItem('Choose country or territory',
               selectInput("var1", label = h5("Choose country or territory"), 
                           choices = terr, 
                           selected = 'Global')),
    menuItem('Explore extent',
             radioButtons("radio3", label = h6(""), 
                          choices = list('None' = 0, "Mangrove area (ha)" = 1, "Seagrass area (ha)" = 2, "Saltmarsh area (ha)" = 3, 'Kelp area (ha)' = 4),
                          selected = 0, inline = F),
            # menuItem('Choose country or territory',
                      selectInput("var2", label = h5("Choose country or territory"), 
                                  choices = terr, 
                                  selected = 'Global')),
    menuItem('Explore threats',
             radioButtons("radio4", label = h6(""), 
                          choices = list('None' = 0, 'Cyclones' = 1, "Kelp rate of loss" = 2, "Proportion of mangroves lost" = 3, "Seagrass risk" = 4,
                                         'Kelp climate pressure' = 5, 'Kelp land pressure' = 6,'Kelp marine pressure' = 7, 
                                         'Mangrove climate pressure' = 8, 'Mangrove land pressure' = 9,'Mangrove marine pressure' = 10, 
                                         'Seagrass climate pressure' = 11, 'Seagrass land pressure' = 12,'Seagrass marine pressure' = 13, 
                                         'Saltmarsh climate pressure' = 14, 'Saltmarsh land pressure' = 15,'Saltmarsh marine pressure' = 16),
                          selected = 0, inline = F),
             #menuItem('Choose country or territory',
                      selectInput("var3", label = h5("Choose country or territory"), 
                                  choices = terr, 
                                  selected = 'Global')),
    menuItem('Explore carbon',
             radioButtons("radio6", label = h6(""), 
                          choices = list('None' = 0, 'Kelp carbon accumulation (gC/m2/yr)' = 1, "Mangrove carbon storage (mgC/ha)" = 2, "Seagrass storage (gC/m2)" = 3, "Saltmarsh storage (mgC/ha)" = 4),
                          selected = 0, inline = F),
            # menuItem('Choose country or territory',
                      selectInput("var5", label = h5("Choose country or territory"), 
                                  choices = terr, 
                                  selected = 'Global')),
    menuItem('Explore biodiversity',
             radioButtons("radio5", label = h6(""), 
                          choices = list('None' = 0, 'Kelp-affiliated species richness' = 1, "Mangrove-affiliated species richness" = 2, 
                                         "Seagrass-affiliated species richness" = 3, "Saltmarsh-affiliated species richness" = 4),
                          selected = 0, inline = F),
            # menuItem('Choose country or territory',
                      selectInput("var4", label = h5("Choose country or territory"), 
                                  choices = terr, 
                                  selected = 'Global')),
    menuItem('Explore co-benefit',
             radioButtons("radio7", label = h6(""), 
                          choices = list('None' = 0, 'Proportion of population vulnerable to storms/sea-level rise' = 1, "Protein consumption from fish (kg/capita/year)" = 2, 
                                        "Area suitable for seaweed farming (ha)" = 3, "Coastal protection by mangroves (people/20km)" = 4, 
                                        "Mangrove fisheries enhancement" = 5, 'Kelp-affiliated fisheries biomass (g/m2)' = 6),
                          selected = 0, inline = F),
            # menuItem('Choose country or territory',
                      selectInput("var6", label = h5("Choose country or territory"), 
                                  choices = terr, 
                                  selected = 'Global')),
    menuItem('Explore WWF Blue Forest projects',
             radioButtons("radio8", label = h5("Show WWF Blue Forest projects"), 
                          choices = list('None' = 0, 'All' = 1, "Mangrove" = 2, 
                                         "Seagrass" = 3, "Saltmarsh" = 4, "Seaweed" = 5),
                          selected = 0, inline = F)),
    menuItem('Explore hotspots',
             menuItem('Mangroves',
                      radioButtons('radio9', label = h5('Choose individual criteria'), 
                                   choices = list('None' = 0, 'Extent' = 1, "Threat" = 2, "Carbon" = 3, "Biodiversity" = 4, "Co-benefit" = 5),
                                   selected = 0, inline = F),
                      sliderInput("slider1", label = h5("Sites above percentile"), min = 0, 
                                  max = 100, value = 0)),
             menuItem('Seagrass',
                      radioButtons('radio10', label = h5('Choose individual criteria'), 
                                   choices = list('None' = 0, 'Extent' = 1, "Threat" = 2, "Carbon" = 3, "Biodiversity" = 4),# "Co-benefit" = 5),
                                   selected = 0, inline = F),
                      sliderInput("slider2", label = h5("Sites above percentile"), min = 0, 
                                  max = 100, value = 0)),
             menuItem('Saltmarsh',
                      radioButtons('radio11', label = h5('Choose individual criteria'), 
                                   choices = list('None' = 0, 'Extent' = 1, "Threat" = 2, "Carbon" = 3, "Biodiversity" = 4),# "Co-benefit" = 5),
                                   selected = 0, inline = F),
                      sliderInput("slider3", label = h5("Sites above percentile"), min = 0, 
                                  max = 100, value = 0)),
             menuItem('Kelp',
                       radioButtons('radio12', label = h5('Choose individual criteria'), 
                                    choices = list('None' = 0, 'Extent' = 1, "Threat" = 2, "Carbon" = 3, "Biodiversity" = 4, "Co-benefit" = 5),
                                    selected = 0, inline = F),
                       sliderInput("slider4", label = h5("Sites above percentile"), min = 0, 
                                   max = 100, value = 0)),
             menuItem('All forests',
             radioButtons('radio13', label = h5('Choose individual criteria'), 
                          choices = list('None' = 0, 'Extent' = 1, "Threat" = 2, "Carbon" = 3, "Biodiversity" = 4, "Co-benefit" = 5),
                          selected = 0, inline = F),
             sliderInput("slider5", label = h5("Sites above percentile"), min = 0, 
                         max = 100, value = 0)))
    ))

body <- dashboardBody(
  shinyDashboardThemes(
    theme = "blue_gradient"),
  tags$head( 
    tags$style(HTML(".main-header .logo { font-size: 30px; }")), #change the font size to 20
    tags$style(HTML(".main-sidebar { font-size: 13px; }"))
  ),
    fluidRow(box(mapdeckOutput('map', height = '800px'), width = 12)))

ui <- dashboardPage(
  dashboardHeader(title = "Blue forests"),
  sidebar,
  body
)

# Define server logic

server <- function(input, output, session) {

    output$map <- renderMapdeck({
        mapdeck(token = key, style = "mapbox://styles/mapbox/light-v10") %>% 
        add_polygon(
          data = rbind(select(units2, unit_ID, TERRITORY1, PROVINC, REALM, geom), 
                       select(unitsNA, unit_ID, TERRITORY1, PROVINC, REALM, geom)),
          fill_colour = '#FFFFFF',
          #palette = '#5f9ea0',
          fill_opacity = 0.1,
          focus_layer = F, 
          update_view = F
        )
        
    })
    
    observeEvent(
      {input$var #Choose country or territory
      input$radio #Colour the coastline
      input$radio1 #Blue forests distribution
      input$var1 #Choose country for distributions
      input$radio2 #choose unit type
      input$radio3 #Explore extent
      input$var2 #Choose country for extent
      input$radio4 #threats
      input$var3 #Choose country for threats
      input$radio5 #carbon
      input$var4 #Choose country for carbon
      input$radio6 #biodiversity
      input$var5 #Choose country for biodiversity
      input$radio7 # co-benefits
      input$var6 #Choose country for benefits
      input$radio8 #wwf blueforests
      input$radio9 # criteria hotspots
      input$slider1 #percentile
      input$radio10 # criteria hotspots
      input$slider2 #percentile
      input$radio11 # criteria hotspots
      input$slider3 #percentile
      input$radio12 # criteria hotspots
      input$slider4 #percentile
      input$radio13 # criteria hotspots
      input$slider5 #percentile
      },{
        
        # updating widgets
        
        if(input$radio != '#FFFFFF'){
         # updateSelectInput(session, "var",
               #             selected = 'Global')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio1 != 0){
        #  updateSelectInput(session, "var",
         #                   selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio3 != 0){
        #  updateSelectInput(session, "var",
         #                   selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio4 != 0){
        #  updateSelectInput(session, "var",
            #                selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio5 != 0){
        #  updateSelectInput(session, "var",
             #               selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio6 != 0){
         # updateSelectInput(session, "var",
              #              selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                            selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio7 != 0){
         # updateSelectInput(session, "var",
               #             selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio8 != 0){
         # updateSelectInput(session, "var",
                 #           selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        
        if(input$radio9 != 0){
         # updateSelectInput(session, "var",
                 #           selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio10 != 0){
          # updateSelectInput(session, "var",
          #           selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio11 != 0){
          # updateSelectInput(session, "var",
          #           selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
       
        if(input$radio12 != 0){
          # updateSelectInput(session, "var",
          #           selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio13",
                             selected = 0)
          updateSliderInput(session, "slider5",
                            value = 0)
        }
        
        if(input$radio13 != 0){
          # updateSelectInput(session, "var",
          #           selected = 'Global')
          updateRadioButtons(session, "radio",
                             selected = '#FFFFFF')
          updateRadioButtons(session, "radio1",
                             selected = 0)
          updateRadioButtons(session, "radio3",
                             selected = 0)
          updateRadioButtons(session, "radio4",
                             selected = 0)
          updateRadioButtons(session, "radio5",
                             selected = 0)
          updateRadioButtons(session, "radio6",
                             selected = 0)
          updateRadioButtons(session, "radio7",
                             selected = 0)
          updateRadioButtons(session, "radio8",
                             selected = 0)
          updateRadioButtons(session, "radio9",
                             selected = 0)
          updateSliderInput(session, "slider1",
                            value = 0)
          updateRadioButtons(session, "radio10",
                             selected = 0)
          updateSliderInput(session, "slider2",
                            value = 0)
          updateRadioButtons(session, "radio11",
                             selected = 0)
          updateSliderInput(session, "slider3",
                            value = 0)
          updateRadioButtons(session, "radio12",
                             selected = 0)
          updateSliderInput(session, "slider4",
                            value = 0)
        }
        
      if(input$radio != '#FFFFFF' & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0  & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){ 
        if(input$var == 'Global'){
            mapdeck_update(map_id = 'map') %>%
              clear_polygon(layer_id = '3') %>% 
              clear_polygon(layer_id = '1') %>% 
              clear_scatterplot(layer_id = '1') %>% 
              add_polygon(
                data = rbind(select(units2, unit_ID,TERRITORY1, PROVINC, REALM, geom), 
                             select(unitsNA, unit_ID,TERRITORY1, PROVINC, REALM, geom)),
                layer_id = '1',
                fill_colour = input$radio,
                fill_opacity = 0.7,
                focus_layer = F, 
                update_view = F
              )}else{
                
                data2 <- rbind(select(units2, unit_ID,unit_type,TERRITORY1, PROVINC, REALM, geom), 
                               select(unitsNA, unit_ID,unit_type,TERRITORY1, PROVINC, REALM, geom)) %>% 
                  filter(TERRITORY1 == input$var)
                
                if(nrow(data2) > 0){
                  
                  mapdeck_update(map_id = 'map') %>% 
                    clear_polygon(layer_id = '3') %>% 
                    clear_polygon(layer_id = '1') %>% 
                    clear_scatterplot(layer_id = '1') %>% 
                    add_polygon(
                      data = data2, 
                      update_view = F,
                      focus_layer = T, 
                      layer_id = '3',
                      fill_colour = input$radio2,
                      palette = 'spectral',
                      fill_opacity = 0.7,
                      legend = T)
              }}}else if(input$radio == '#FFFFFF' & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0 & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){ 
                if(input$var == 'Global'){
                  mapdeck_update(map_id = 'map') %>%
                    clear_polygon(layer_id = '3') %>% 
                    clear_polygon(layer_id = '1') %>% 
                    clear_scatterplot(layer_id = '1') %>% 
                    add_polygon(
                      data = rbind(select(units2, unit_ID,TERRITORY1, PROVINC, REALM, geom), 
                                   select(unitsNA, unit_ID,TERRITORY1, PROVINC, REALM, geom)),
                      layer_id = '1',
                      fill_colour = input$radio,
                      fill_opacity = 0.7,
                      focus_layer = F, 
                      update_view = F
                    )}else{
                      
                      data2 <- rbind(select(units2, unit_ID,unit_type,TERRITORY1, PROVINC, REALM, geom), 
                                     select(unitsNA, unit_ID,unit_type,TERRITORY1, PROVINC, REALM, geom)) %>% 
                        filter(TERRITORY1 == input$var)
                      
                      if(nrow(data2) > 0){
                        
                        mapdeck_update(map_id = 'map') %>% 
                          clear_polygon(layer_id = '3') %>% 
                          clear_polygon(layer_id = '1') %>% 
                          clear_scatterplot(layer_id = '1') %>% 
                          add_polygon(
                            data = data2, 
                            update_view = F,
                            focus_layer = T, 
                            layer_id = '3',
                            fill_colour = input$radio2,
                            palette = 'spectral',
                            fill_opacity = 0.7,
                            legend = T)
                
                      }}}else if(input$radio == "#FFFFFF" & input$radio1 != 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0 & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){
     
            if(input$radio1 == 1){
              if(input$var1 == 'Global'){
                
                sub <- df %>% 
                  filter(eco == 1) %>% 
                  filter(value == 1)
                
                data <- units2 %>% 
                  filter(unit_ID %in% unique(sub$unit_ID))
                
                mapdeck_update(map_id = 'map') %>%
                  clear_polygon(layer_id = '3') %>% 
                  clear_polygon(layer_id = '1') %>% 
                  clear_scatterplot(layer_id = '1') %>% 
                  add_polygon(
                    data = data,
                    layer_id = '1',
                    fill_colour = '#20b2aa',
                    fill_opacity = 0.9,
                    focus_layer = F, 
                    update_view = F)
                
              }else{
                
                sub <- df %>% 
                  filter(eco == 1) %>% 
                  filter(value == 1)
                
                data2 <- units2 %>% 
                  filter(TERRITORY1 == input$var1 & unit_ID %in% unique(sub$unit_ID))
                
                if(nrow(data2) > 0){
                  
                  mapdeck_update(map_id = 'map') %>% 
                    clear_polygon(layer_id = '3') %>% 
                    clear_polygon(layer_id = '1') %>% 
                    clear_scatterplot(layer_id = '1') %>% 
                    add_polygon(
                      data = data2, 
                      update_view = F,
                      focus_layer = T, 
                      layer_id = '3',
                      fill_colour = '#20b2aa', 
                      fill_opacity = 0.9,
                      legend = F)
                }}}else if(input$radio1 == 2){
                  if(input$var1 == 'Global'){
                    
                    sub <- df %>% 
                      filter(eco == 2) %>% 
                      filter(value == 1)
                    
                    data <- units2 %>% 
                      filter(unit_ID %in% unique(sub$unit_ID))
                    
                    mapdeck_update(map_id = 'map') %>%
                      clear_polygon(layer_id = '3') %>% 
                      clear_polygon(layer_id = '1') %>% 
                      clear_scatterplot(layer_id = '1') %>% 
                      add_polygon(
                        data = data,
                        layer_id = '1',
                        fill_colour = '#20b2aa',
                        fill_opacity = 0.9,
                        focus_layer = F, 
                        update_view = F)
                    
                  }else{
                    
                    sub <- df %>% 
                      filter(eco == 2) %>% 
                      filter(value == 1)
                    
                    data2 <- units2 %>% 
                      filter(TERRITORY1 == input$var1 & unit_ID %in% unique(sub$unit_ID))
                    
                    if(nrow(data2) > 0){
                      
                      mapdeck_update(map_id = 'map') %>% 
                        clear_polygon(layer_id = '3') %>% 
                        clear_polygon(layer_id = '1') %>% 
                        clear_scatterplot(layer_id = '1') %>% 
                        add_polygon(
                          data = data2, 
                          update_view = F,
                          focus_layer = T, 
                          layer_id = '3',
                          fill_colour = '#20b2aa', 
                          fill_opacity = 0.9,
                          legend = F)
                    }}}else if(input$radio1 == 3){
                      if(input$var1 == 'Global'){
                        
                        sub <- df %>% 
                          filter(eco == 3) %>% 
                          filter(value == 1)
                        
                        data <- units2 %>% 
                          filter(unit_ID %in% unique(sub$unit_ID))
                        
                        mapdeck_update(map_id = 'map') %>%
                          clear_polygon(layer_id = '3') %>%
                          clear_polygon(layer_id = '1') %>% 
                          clear_scatterplot(layer_id = '1') %>% 
                          add_polygon(
                            data = data,
                            layer_id = '1',
                            fill_colour = '#20b2aa',
                            fill_opacity = 0.9,
                            focus_layer = F, 
                            update_view = F)
                        
                      }else{
                        
                        sub <- df %>% 
                          filter(eco == 3) %>% 
                          filter(value == 1)
                        
                        data2 <- units2 %>% 
                          filter(TERRITORY1 == input$var1 & unit_ID %in% unique(sub$unit_ID))
                        
                        if(nrow(data2) > 0){
                          
                          mapdeck_update(map_id = 'map') %>% 
                            clear_polygon(layer_id = '3') %>% 
                            clear_polygon(layer_id = '1') %>% 
                            clear_scatterplot(layer_id = '1') %>% 
                            add_polygon(
                              data = data2, 
                              update_view = F,
                              focus_layer = T, 
                              layer_id = '3',
                              fill_colour = '#20b2aa', 
                              fill_opacity = 0.9,
                              legend = F)
                        }}}else if(input$radio1 == 4){
                          if(input$var1 == 'Global'){
                            
                            sub <- df %>% 
                              filter(eco == 4) %>% 
                              filter(value == 1)
                            
                            data <- units2 %>% 
                              filter(unit_ID %in% unique(sub$unit_ID))
                            
                            mapdeck_update(map_id = 'map') %>%
                              clear_polygon(layer_id = '3') %>% 
                              clear_polygon(layer_id = '1') %>% 
                              clear_scatterplot(layer_id = '1') %>% 
                              add_polygon(
                                data = data,
                                layer_id = '1',
                                fill_colour = '#20b2aa',
                                fill_opacity = 0.9,
                                focus_layer = F, 
                                update_view = F)
                            
                          }else{
                            
                            sub <- df %>% 
                              filter(eco == 4) %>% 
                              filter(value == 1)
                            
                            data2 <- units2 %>% 
                              filter(TERRITORY1 == input$var1 & unit_ID %in% unique(sub$unit_ID))
                            
                            if(nrow(data2) > 0){
                              
                              mapdeck_update(map_id = 'map') %>% 
                                clear_polygon(layer_id = '3') %>% 
                                clear_polygon(layer_id = '1') %>% 
                                clear_scatterplot(layer_id = '1') %>% 
                                add_polygon(
                                  data = data2, 
                                  update_view = F,
                                  focus_layer = T, 
                                  layer_id = '3',
                                  fill_colour = '#20b2aa', 
                                  fill_opacity = 0.9,
                                  legend = F)
                            }}}else if(input$radio1 == 5){
                              if(input$var1 == 'Global'){
                                
                                sub <- df %>% 
                                  filter(eco == 5) %>% 
                                  filter(value == 1)
                                
                                data <- units2 %>% 
                                  filter(unit_ID %in% unique(sub$unit_ID))
                                
                                mapdeck_update(map_id = 'map') %>%
                                  clear_polygon(layer_id = '3') %>% 
                                  clear_polygon(layer_id = '1') %>% 
                                  clear_scatterplot(layer_id = '1') %>% 
                                  add_polygon(
                                    data = data,
                                    layer_id = '1',
                                    fill_colour = '#20b2aa',
                                    fill_opacity = 0.9,
                                    focus_layer = F, 
                                    update_view = F)
                                
                              }else{
                                
                                sub <- df %>% 
                                  filter(eco == 5) %>% 
                                  filter(value == 1)
                                
                                data2 <- units2 %>% 
                                  filter(TERRITORY1 == input$var1 & unit_ID %in% unique(sub$unit_ID))
                                
                                if(nrow(data2) > 0){
                                  
                                  mapdeck_update(map_id = 'map') %>% 
                                    clear_polygon(layer_id = '3') %>% 
                                    clear_polygon(layer_id = '1') %>% 
                                    clear_scatterplot(layer_id = '1') %>% 
                                    add_polygon(
                                      data = data2, 
                                      update_view = F,
                                      focus_layer = T, 
                                      layer_id = '3',
                                      fill_colour = '#20b2aa', 
                                      fill_opacity = 0.9,
                                      legend = F)
                                }}}else if(input$radio1 == 6){
                                  if(input$var1 == 'Global'){
                                    
                                    sub <- df %>% 
                                      filter(eco %in% c(1,2,3,4,5)) %>% 
                                      filter(value == 1)
                                    
                                    data <- units2 %>% 
                                      filter(unit_ID %in% unique(sub$unit_ID))
                                    
                                    mapdeck_update(map_id = 'map') %>%
                                      clear_polygon(layer_id = '3') %>% 
                                      clear_polygon(layer_id = '1') %>% 
                                      clear_scatterplot(layer_id = '1') %>% 
                                      add_polygon(
                                        data = data,
                                        layer_id = '1',
                                        fill_colour = '#20b2aa',
                                        fill_opacity = 0.9,
                                        focus_layer = F, 
                                        update_view = F)
                                    
                                  }else{
                                    
                                    sub <- df %>% 
                                      filter(eco %in% c(1,2,3,4,5)) %>% 
                                      filter(value == 1)
                                    
                                    data2 <- units2 %>% 
                                      filter(TERRITORY1 == input$var1 & unit_ID %in% unique(sub$unit_ID))
                                    
                                    if(nrow(data2) > 0){
                                      
                                      mapdeck_update(map_id = 'map') %>% 
                                        clear_polygon(layer_id = '3') %>% 
                                        clear_polygon(layer_id = '1') %>% 
                                        clear_scatterplot(layer_id = '1') %>% 
                                        add_polygon(
                                          data = data2, 
                                          update_view = F,
                                          focus_layer = T, 
                                          layer_id = '3',
                                          fill_colour = '#20b2aa', 
                                          fill_opacity = 0.9,
                                          legend = F)
                                    }}}else if(input$radio1 == 7){
                                      if(input$var1 == 'Global'){
                                        
                                        data <- units2 %>% 
                                          mutate(total = mangrove + seagrass + saltmarsh + kelp) %>% 
                                          mutate(total = factor(total))
                                        
                                        mapdeck_update(map_id = 'map') %>%
                                          clear_polygon(layer_id = '3') %>% 
                                          clear_polygon(layer_id = '1') %>% 
                                          clear_scatterplot(layer_id = '1') %>% 
                                          add_polygon(
                                            data = data,
                                            layer_id = '1',
                                            fill_colour = 'total',
                                            fill_opacity = 0.9,
                                            palette = 'spectral',
                                            focus_layer = F, 
                                            update_view = F,
                                            legend = T)
                                        
                                      }else{
                                        
                                        data2 <- units2 %>% 
                                          mutate(total = mangrove + seagrass + saltmarsh + kelp) %>% 
                                          mutate(total = factor(total)) %>% 
                                          filter(TERRITORY1 == input$var1)
                                        
                                        if(nrow(data2) > 0){
                                          
                                          mapdeck_update(map_id = 'map') %>% 
                                            clear_polygon(layer_id = '3') %>% 
                                            clear_polygon(layer_id = '1') %>% 
                                            clear_scatterplot(layer_id = '1') %>% 
                                            add_polygon(
                                              data = data2, 
                                              update_view = F,
                                              focus_layer = T, 
                                              layer_id = '3',
                                              fill_colour = 'total', 
                                              fill_opacity = 0.9,
                                              palette = 'spectral',
                                              legend = T)
                                        }}}
                        }else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 != 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0  & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){ # extent
              
              if(input$radio3 == 1){
              if(input$var2 == 'Global'){
                
                sub <- df %>% 
                  filter(eco == input$radio3) %>% 
                  filter(value == 1)
                
                data <- units2 %>% 
                  filter(unit_ID %in% unique(sub$unit_ID))
                  
                  mapdeck_update(map_id = 'map') %>%
                    clear_polygon(layer_id = '3') %>% 
                    clear_polygon(layer_id = '1') %>% 
                    clear_scatterplot(layer_id = '1') %>% 
                    add_polygon(
                      data = data,
                      layer_id = '1',
                      fill_colour = 'mangrove_2016_area_ha',
                      fill_opacity = 0.9,
                      focus_layer = F, 
                      update_view = F,
                      palette = 'ylorrd',
                     
                      legend = T)
                  
                }else{
                  
                  sub <- df %>% 
                    filter(eco == input$radio3) %>% 
                    filter(value == 1)
                  
                  data2 <- units2 %>% 
                    filter(TERRITORY1 == input$var2 & unit_ID %in% unique(sub$unit_ID))
                  
                  if(nrow(data2) > 0){
                    
                    mapdeck_update(map_id = 'map') %>% 
                      clear_polygon(layer_id = '3') %>% 
                      clear_polygon(layer_id = '1') %>% 
                      clear_scatterplot(layer_id = '1') %>% 
                      add_polygon(
                        data = data2, 
                        update_view = F,
                        focus_layer = T, 
                        layer_id = '3',
                        fill_colour = 'mangrove_2016_area_ha', 
                        fill_opacity = 0.9,
                        palette = 'ylorrd', 
                        legend = T)
                  }}}else if(input$radio3 == 2){
                        if(input$var2 == 'Global'){
                          
                          data <- units2 %>% 
                            filter(seagrass_area_ha > 0)
                          
                          mapdeck_update(map_id = 'map') %>%
                            clear_polygon(layer_id = '3') %>% 
                            clear_polygon(layer_id = '1') %>% 
                            clear_scatterplot(layer_id = '1') %>% 
                            add_polygon(
                              data = data,
                              layer_id = '1',
                              fill_colour = 'seagrass_area_ha',
                              fill_opacity = 0.9,
                              focus_layer = F, 
                              update_view = F,
                              palette = 'ylorrd',
                              legend = T)
                          
                        }else{
                          
                          sub <- df %>% 
                            filter(eco == input$radio3) %>% 
                            filter(value == 1)
                          
                          data2 <- units2 %>% 
                            filter(TERRITORY1 == input$var2 & unit_ID %in% unique(sub$unit_ID))
                          
                          if(nrow(data2) > 0){
                            
                            mapdeck_update(map_id = 'map') %>% 
                              clear_polygon(layer_id = '3') %>% 
                              clear_polygon(layer_id = '1') %>% 
                              clear_scatterplot(layer_id = '1') %>% 
                              add_polygon(
                                data = data2, 
                                update_view = F,
                                focus_layer = T, 
                                layer_id = '3',
                                fill_colour = 'seagrass_area_ha', 
                                fill_opacity = 0.9,
                                palette = 'ylorrd', 
                                legend = T)
                          }}}else if(input$radio3 == 3){
                            if(input$var2 == 'Global'){
                              
                              sub <- df %>% 
                                filter(eco == input$radio3) %>% 
                                filter(value == 1)
                              
                              data <- units2 %>% 
                                filter(unit_ID %in% unique(sub$unit_ID))
                              
                              mapdeck_update(map_id = 'map') %>%
                                clear_polygon(layer_id = '3') %>% 
                                clear_polygon(layer_id = '1') %>% 
                                clear_scatterplot(layer_id = '1') %>% 
                                add_polygon(
                                  data = data,
                                  layer_id = '1',
                                  fill_colour = 'saltmarsh_area_ha',
                                  fill_opacity = 0.9,
                                  focus_layer = F, 
                                  update_view = F,
                                  palette = 'ylorrd',
                               
                                  legend = T)
                              
                            }else{
                              
                              sub <- df %>% 
                                filter(eco == input$radio3) %>% 
                                filter(value == 1)
                              
                              data2 <- units2 %>% 
                                filter(TERRITORY1 == input$var2 & unit_ID %in% unique(sub$unit_ID))
                              
                              if(nrow(data2) > 0){
                                
                                mapdeck_update(map_id = 'map') %>% 
                                  clear_polygon(layer_id = '3') %>% 
                                  clear_polygon(layer_id = '1') %>% 
                                  clear_scatterplot(layer_id = '1') %>% 
                                  add_polygon(
                                    data = data2, 
                                    update_view = F,
                                    focus_layer = T, 
                                    layer_id = '3',
                                    fill_colour = 'saltmarsh_area_ha', 
                                    fill_opacity = 0.9,
                                    palette = 'ylorrd', 
                                    legend = T)
                              }}}else if(input$radio3 == 4){
                                if(input$var2 == 'Global'){
                                  
                                  sub <- df %>% 
                                    filter(eco == input$radio3) %>% 
                                    filter(value == 1)
                                  
                                  data <- units2 %>% 
                                    filter(unit_ID %in% unique(sub$unit_ID))
                                  
                                  mapdeck_update(map_id = 'map') %>%
                                    clear_polygon(layer_id = '3') %>% 
                                    clear_polygon(layer_id = '1') %>% 
                                    clear_scatterplot(layer_id = '1') %>% 
                                    add_polygon(
                                      data = data,
                                      layer_id = '1',
                                      fill_colour = 'kelp_area_ha',
                                      fill_opacity = 0.9,
                                      focus_layer = F, 
                                      update_view = F,
                                      palette = 'ylorrd',
                                      
                                      legend = T)
                                  
                                }else{
                                  
                                  sub <- df %>% 
                                    filter(eco == input$radio3) %>% 
                                    filter(value == 1)
                                  
                                  data2 <- units2 %>% 
                                    filter(TERRITORY1 == input$var2 & unit_ID %in% unique(sub$unit_ID))
                                  
                                  if(nrow(data2) > 0){
                                    
                                    mapdeck_update(map_id = 'map') %>% 
                                      clear_polygon(layer_id = '3') %>% 
                                      clear_polygon(layer_id = '1') %>% 
                                      clear_scatterplot(layer_id = '1') %>% 
                                      add_polygon(
                                        data = data2, 
                                        update_view = F,
                                        focus_layer = T, 
                                        layer_id = '3',
                                        fill_colour = 'kelp_area_ha', 
                                        fill_opacity = 0.9,
                                        palette = 'ylorrd', 
                                        legend = T)
                                  }}}}else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 != 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0  & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){ # threat
              
              if(input$radio4 == 1){
                if(input$var3 == 'Global'){
                  
                  mapdeck_update(map_id = 'map') %>%
                    clear_polygon(layer_id = '3') %>% 
                    clear_polygon(layer_id = '1') %>% 
                    clear_scatterplot(layer_id = '1') %>% 
                    add_polygon(
                      data = units2,
                      layer_id = '1',
                      fill_colour = 'cyclone_tracks',
                      fill_opacity = 0.9,
                      focus_layer = F, 
                      update_view = F,
                      palette = 'ylorrd',
                      legend = T)
                  
                }else{
                  
                  data2 <- units2 %>% 
                    filter(TERRITORY1 == input$var3)
                  
                  if(nrow(data2) > 0){
                    
                    mapdeck_update(map_id = 'map') %>% 
                      clear_polygon(layer_id = '3') %>% 
                      clear_polygon(layer_id = '1') %>% 
                      clear_scatterplot(layer_id = '1') %>% 
                      add_polygon(
                        data = data2, 
                        update_view = F,
                        focus_layer = T, 
                        layer_id = '3',
                        fill_colour = 'cyclone_tracks', 
                        fill_opacity = 0.9,
                        palette = 'ylorrd', 
                        legend = T)
                  }}}else if(input$radio4 == 2){
                    if(input$var3 == 'Global'){
                      
                      sub <- df %>% 
                        filter(eco == 4) %>% 
                        filter(value == 1)
                      
                      data <- units2 %>% 
                        filter(unit_ID %in% unique(sub$unit_ID))
                      
                      data$kelp_trend <- abs(data$kelp_trend)
                      
                      mapdeck_update(map_id = 'map') %>%
                        clear_polygon(layer_id = '3') %>% 
                        clear_polygon(layer_id = '1') %>% 
                        clear_scatterplot(layer_id = '1') %>% 
                        add_polygon(
                          data = data,
                          layer_id = '1',
                          fill_colour = 'kelp_trend',
                          fill_opacity = 0.9,
                          focus_layer = F, 
                          update_view = F,
                          palette = 'ylorrd',
                          legend = T)
                      
                    }else{
                      
                      sub <- df %>% 
                        filter(eco == 4) %>% 
                        filter(value == 1)
                      
                      data2 <- units2 %>% 
                        filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                      
                      if(nrow(data2) > 0){
                        
                        mapdeck_update(map_id = 'map') %>% 
                          clear_polygon(layer_id = '3') %>% 
                          clear_polygon(layer_id = '1') %>% 
                          clear_scatterplot(layer_id = '1') %>% 
                          add_polygon(
                            data = data2, 
                            update_view = F,
                            focus_layer = T, 
                            layer_id = '3',
                            fill_colour = 'kelp_trend', 
                            fill_opacity = 0.9,
                            palette = 'ylorrd', 
                            legend = T)
                      }}}else if(input$radio4 == 3){
                        if(input$var3 == 'Global'){
                          
                          sub <- df %>% 
                            filter(eco == 1) %>% 
                            filter(value == 1)
                          
                          data <- units2 %>% 
                            filter(unit_ID %in% unique(sub$unit_ID))
                          
                          mapdeck_update(map_id = 'map') %>%
                            clear_polygon(layer_id = '3') %>% 
                            clear_polygon(layer_id = '1') %>% 
                            clear_scatterplot(layer_id = '1') %>% 
                            add_polygon(
                              data = data,
                              layer_id = '1',
                              fill_colour = 'mangrove_prop_lost_10_16',
                              fill_opacity = 0.9,
                              focus_layer = F, 
                              update_view = F,
                              palette = 'ylorrd',
                              legend = T)
                          
                        }else{
                          
                          sub <- df %>% 
                            filter(eco == 1) %>% 
                            filter(value == 1)
                          
                          data2 <- units2 %>% 
                            filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                          
                          if(nrow(data2) > 0){
                            
                            mapdeck_update(map_id = 'map') %>% 
                              clear_polygon(layer_id = '3') %>% 
                              clear_polygon(layer_id = '1') %>% 
                              clear_scatterplot(layer_id = '1') %>% 
                              add_polygon(
                                data = data2, 
                                update_view = F,
                                focus_layer = T, 
                                layer_id = '3',
                                fill_colour = 'mangrove_prop_lost_10_16', 
                                fill_opacity = 0.9,
                                palette = 'ylorrd', 
                                legend = T)
                          }}}else if(input$radio4 == 4){
                            if(input$var3 == 'Global'){
                              
                              sub <- df %>% 
                                filter(eco == 2) %>% 
                                filter(value == 1)
                              
                              data <- units2 %>% 
                                filter(unit_ID %in% unique(sub$unit_ID))
                              
                              mapdeck_update(map_id = 'map') %>%
                                clear_polygon(layer_id = '3') %>% 
                                clear_polygon(layer_id = '1') %>% 
                                clear_scatterplot(layer_id = '1') %>% 
                                add_polygon(
                                  data = data,
                                  layer_id = '1',
                                  fill_colour = 'seagrass_mean_risk',
                                  fill_opacity = 0.9,
                                  focus_layer = F, 
                                  update_view = F,
                                  palette = 'ylorrd',
                                  legend = T)
                              
                            }else{
                              
                              sub <- df %>% 
                                filter(eco == 2) %>% 
                                filter(value == 1)
                              
                              data2 <- units2 %>% 
                                filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                              
                              if(nrow(data2) > 0){
                                
                                mapdeck_update(map_id = 'map') %>% 
                                  clear_polygon(layer_id = '3') %>% 
                                  clear_polygon(layer_id = '1') %>% 
                                  clear_scatterplot(layer_id = '1') %>% 
                                  add_polygon(
                                    data = data2, 
                                    update_view = F,
                                    focus_layer = T, 
                                    layer_id = '3',
                                    fill_colour = 'seagrass_mean_risk', 
                                    fill_opacity = 0.9,
                                    palette = 'ylorrd', 
                                    legend = T)
                              }}}else if(input$radio4 == 5){
                                if(input$var3 == 'Global'){
                                  
                                  sub <- df %>% 
                                    filter(eco == 4) %>% 
                                    filter(value == 1)
                                  
                                  data <- units2 %>% 
                                    filter(unit_ID %in% unique(sub$unit_ID))
                                  
                                  mapdeck_update(map_id = 'map') %>%
                                    clear_polygon(layer_id = '3') %>% 
                                    clear_polygon(layer_id = '1') %>% 
                                    clear_scatterplot(layer_id = '1') %>% 
                                    add_polygon(
                                      data = data,
                                      layer_id = '1',
                                      fill_colour = 'kelp_climate_mean',
                                      fill_opacity = 0.9,
                                      focus_layer = F, 
                                      update_view = F,
                                      palette = 'ylorrd',
                                      legend = T)
                                  
                                }else{
                                  
                                  sub <- df %>% 
                                    filter(eco == 4) %>% 
                                    filter(value == 1)
                                  
                                  data2 <- units2 %>% 
                                    filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                  
                                  if(nrow(data2) > 0){
                                    
                                    mapdeck_update(map_id = 'map') %>% 
                                      clear_polygon(layer_id = '3') %>% 
                                      clear_polygon(layer_id = '1') %>% 
                                      clear_scatterplot(layer_id = '1') %>% 
                                      add_polygon(
                                        data = data2, 
                                        update_view = F,
                                        focus_layer = T, 
                                        layer_id = '3',
                                        fill_colour = 'kelp_climate_mean', 
                                        fill_opacity = 0.9,
                                        palette = 'ylorrd', 
                                        legend = T)
                                  }}}else if(input$radio4 == 6){
                                    if(input$var3 == 'Global'){
                                      
                                      sub <- df %>% 
                                        filter(eco == 4) %>% 
                                        filter(value == 1)
                                      
                                      data <- units2 %>% 
                                        filter(unit_ID %in% unique(sub$unit_ID))
                                      
                                      mapdeck_update(map_id = 'map') %>%
                                        clear_polygon(layer_id = '3') %>% 
                                        clear_polygon(layer_id = '1') %>% 
                                        clear_scatterplot(layer_id = '1') %>% 
                                        add_polygon(
                                          data = data,
                                          layer_id = '1',
                                          fill_colour = 'kelp_land_mean',
                                          fill_opacity = 0.9,
                                          focus_layer = F, 
                                          update_view = F,
                                          palette = 'ylorrd',
                                          legend = T)
                                      
                                    }else{
                                      
                                      sub <- df %>% 
                                        filter(eco == 4) %>% 
                                        filter(value == 1)
                                      
                                      data2 <- units2 %>% 
                                        filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                      
                                      if(nrow(data2) > 0){
                                        
                                        mapdeck_update(map_id = 'map') %>% 
                                          clear_polygon(layer_id = '3') %>% 
                                          clear_polygon(layer_id = '1') %>% 
                                          clear_scatterplot(layer_id = '1') %>% 
                                          add_polygon(
                                            data = data2, 
                                            update_view = F,
                                            focus_layer = T, 
                                            layer_id = '3',
                                            fill_colour = 'kelp_land_mean', 
                                            fill_opacity = 0.9,
                                            palette = 'ylorrd', 
                                            legend = T)
                                      }}}else if(input$radio4 == 7){
                                        if(input$var3 == 'Global'){
                                          
                                          sub <- df %>% 
                                            filter(eco == 4) %>% 
                                            filter(value == 1)
                                          
                                          data <- units2 %>% 
                                            filter(unit_ID %in% unique(sub$unit_ID))
                                          
                                          mapdeck_update(map_id = 'map') %>%
                                            clear_polygon(layer_id = '3') %>% 
                                            clear_polygon(layer_id = '1') %>% 
                                            clear_scatterplot(layer_id = '1') %>% 
                                            add_polygon(
                                              data = data,
                                              layer_id = '1',
                                              fill_colour = 'kelp_marine_mean',
                                              fill_opacity = 0.9,
                                              focus_layer = F, 
                                              update_view = F,
                                              palette = 'ylorrd',
                                              legend = T)
                                          
                                        }else{
                                          
                                          sub <- df %>% 
                                            filter(eco == 4) %>% 
                                            filter(value == 1)
                                          
                                          data2 <- units2 %>% 
                                            filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                          
                                          if(nrow(data2) > 0){
                                            
                                            mapdeck_update(map_id = 'map') %>% 
                                              clear_polygon(layer_id = '3') %>% 
                                              clear_polygon(layer_id = '1') %>% 
                                              clear_scatterplot(layer_id = '1') %>% 
                                              add_polygon(
                                                data = data2, 
                                                update_view = F,
                                                focus_layer = T, 
                                                layer_id = '3',
                                                fill_colour = 'kelp_marine_mean', 
                                                fill_opacity = 0.9,
                                                palette = 'ylorrd', 
                                                legend = T)
                                          }}}else if(input$radio4 == 8){
                                            if(input$var3 == 'Global'){
                                              
                                              sub <- df %>% 
                                                filter(eco == 1) %>% 
                                                filter(value == 1)
                                              
                                              data <- units2 %>% 
                                                filter(unit_ID %in% unique(sub$unit_ID))
                                              
                                              mapdeck_update(map_id = 'map') %>%
                                                clear_polygon(layer_id = '3') %>% 
                                                clear_polygon(layer_id = '1') %>% 
                                                clear_scatterplot(layer_id = '1') %>% 
                                                add_polygon(
                                                  data = data,
                                                  layer_id = '1',
                                                  fill_colour = 'mang_climate_mean',
                                                  fill_opacity = 0.9,
                                                  focus_layer = F, 
                                                  update_view = F,
                                                  palette = 'ylorrd',
                                                  legend = T)
                                              
                                            }else{
                                              
                                              sub <- df %>% 
                                                filter(eco == 1) %>% 
                                                filter(value == 1)
                                              
                                              data2 <- units2 %>% 
                                                filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                              
                                              if(nrow(data2) > 0){
                                                
                                                mapdeck_update(map_id = 'map') %>% 
                                                  clear_polygon(layer_id = '3') %>% 
                                                  clear_polygon(layer_id = '1') %>% 
                                                  clear_scatterplot(layer_id = '1') %>% 
                                                  add_polygon(
                                                    data = data2, 
                                                    update_view = F,
                                                    focus_layer = T, 
                                                    layer_id = '3',
                                                    fill_colour = 'mang_climate_mean', 
                                                    fill_opacity = 0.9,
                                                    palette = 'ylorrd', 
                                                    legend = T)
                                              }}}else if(input$radio4 == 9){
                                                if(input$var3 == 'Global'){
                                                  
                                                  sub <- df %>% 
                                                    filter(eco == 1) %>% 
                                                    filter(value == 1)
                                                  
                                                  data <- units2 %>% 
                                                    filter(unit_ID %in% unique(sub$unit_ID))
                                                  
                                                  mapdeck_update(map_id = 'map') %>%
                                                    clear_polygon(layer_id = '3') %>% 
                                                    clear_polygon(layer_id = '1') %>% 
                                                    clear_scatterplot(layer_id = '1') %>% 
                                                    add_polygon(
                                                      data = data,
                                                      layer_id = '1',
                                                      fill_colour = 'mang_land_mean',
                                                      fill_opacity = 0.9,
                                                      focus_layer = F, 
                                                      update_view = F,
                                                      palette = 'ylorrd',
                                                      legend = T)
                                                  
                                                }else{
                                                  
                                                  sub <- df %>% 
                                                    filter(eco == 1) %>% 
                                                    filter(value == 1)
                                                  
                                                  data2 <- units2 %>% 
                                                    filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                                  
                                                  if(nrow(data2) > 0){
                                                    
                                                    mapdeck_update(map_id = 'map') %>% 
                                                      clear_polygon(layer_id = '3') %>% 
                                                      clear_polygon(layer_id = '1') %>% 
                                                      clear_scatterplot(layer_id = '1') %>% 
                                                      add_polygon(
                                                        data = data2, 
                                                        update_view = F,
                                                        focus_layer = T, 
                                                        layer_id = '3',
                                                        fill_colour = 'mang_land_mean', 
                                                        fill_opacity = 0.9,
                                                        palette = 'ylorrd', 
                                                        legend = T)
                                                  }}}else if(input$radio4 == 10){
                                                    if(input$var3 == 'Global'){
                                                      
                                                      sub <- df %>% 
                                                        filter(eco == 1) %>% 
                                                        filter(value == 1)
                                                      
                                                      data <- units2 %>% 
                                                        filter(unit_ID %in% unique(sub$unit_ID))
                                                      
                                                      mapdeck_update(map_id = 'map') %>%
                                                        clear_polygon(layer_id = '3') %>% 
                                                        clear_polygon(layer_id = '1') %>% 
                                                        clear_scatterplot(layer_id = '1') %>% 
                                                        add_polygon(
                                                          data = data,
                                                          layer_id = '1',
                                                          fill_colour = 'mang_marine_mean',
                                                          fill_opacity = 0.9,
                                                          focus_layer = F, 
                                                          update_view = F,
                                                          palette = 'ylorrd',
                                                          legend = T)
                                                      
                                                    }else{
                                                      
                                                      sub <- df %>% 
                                                        filter(eco == 1) %>% 
                                                        filter(value == 1)
                                                      
                                                      data2 <- units2 %>% 
                                                        filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                                      
                                                      if(nrow(data2) > 0){
                                                        
                                                        mapdeck_update(map_id = 'map') %>% 
                                                          clear_polygon(layer_id = '3') %>% 
                                                          clear_polygon(layer_id = '1') %>% 
                                                          clear_scatterplot(layer_id = '1') %>% 
                                                          add_polygon(
                                                            data = data2, 
                                                            update_view = F,
                                                            focus_layer = T, 
                                                            layer_id = '3',
                                                            fill_colour = 'mang_marine_mean', 
                                                            fill_opacity = 0.9,
                                                            palette = 'ylorrd', 
                                                            legend = T)
                                                      }}}else if(input$radio4 == 11){
                                                        if(input$var3 == 'Global'){
                                                          
                                                          sub <- df %>% 
                                                            filter(eco == 2) %>% 
                                                            filter(value == 1)
                                                          
                                                          data <- units2 %>% 
                                                            filter(unit_ID %in% unique(sub$unit_ID))
                                                          
                                                          mapdeck_update(map_id = 'map') %>%
                                                            clear_polygon(layer_id = '3') %>% 
                                                            clear_polygon(layer_id = '1') %>% 
                                                            clear_scatterplot(layer_id = '1') %>% 
                                                            add_polygon(
                                                              data = data,
                                                              layer_id = '1',
                                                              fill_colour = 'seag_climate_mean',
                                                              fill_opacity = 0.9,
                                                              focus_layer = F, 
                                                              update_view = F,
                                                              palette = 'ylorrd',
                                                              legend = T)
                                                          
                                                        }else{
                                                          
                                                          sub <- df %>% 
                                                            filter(eco == 2) %>% 
                                                            filter(value == 1)
                                                          
                                                          data2 <- units2 %>% 
                                                            filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                                          
                                                          if(nrow(data2) > 0){
                                                            
                                                            mapdeck_update(map_id = 'map') %>% 
                                                              clear_polygon(layer_id = '3') %>% 
                                                              clear_polygon(layer_id = '1') %>% 
                                                              clear_scatterplot(layer_id = '1') %>% 
                                                              add_polygon(
                                                                data = data2, 
                                                                update_view = F,
                                                                focus_layer = T, 
                                                                layer_id = '3',
                                                                fill_colour = 'seag_climate_mean', 
                                                                fill_opacity = 0.9,
                                                                palette = 'ylorrd', 
                                                                legend = T)
                                                          }}}else if(input$radio4 == 12){
                                                            if(input$var3 == 'Global'){
                                                              
                                                              sub <- df %>% 
                                                                filter(eco == 2) %>% 
                                                                filter(value == 1)
                                                              
                                                              data <- units2 %>% 
                                                                filter(unit_ID %in% unique(sub$unit_ID))
                                                              
                                                              mapdeck_update(map_id = 'map') %>%
                                                                clear_polygon(layer_id = '3') %>% 
                                                                clear_polygon(layer_id = '1') %>% 
                                                                clear_scatterplot(layer_id = '1') %>% 
                                                                add_polygon(
                                                                  data = data,
                                                                  layer_id = '1',
                                                                  fill_colour = 'seag_land_mean',
                                                                  fill_opacity = 0.9,
                                                                  focus_layer = F, 
                                                                  update_view = F,
                                                                  palette = 'ylorrd',
                                                                  legend = T)
                                                              
                                                            }else{
                                                              
                                                              sub <- df %>% 
                                                                filter(eco == 2) %>% 
                                                                filter(value == 1)
                                                              
                                                              data2 <- units2 %>% 
                                                                filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                                              
                                                              if(nrow(data2) > 0){
                                                                
                                                                mapdeck_update(map_id = 'map') %>% 
                                                                  clear_polygon(layer_id = '3') %>% 
                                                                  clear_polygon(layer_id = '1') %>% 
                                                                  clear_scatterplot(layer_id = '1') %>% 
                                                                  add_polygon(
                                                                    data = data2, 
                                                                    update_view = F,
                                                                    focus_layer = T, 
                                                                    layer_id = '3',
                                                                    fill_colour = 'seag_land_mean', 
                                                                    fill_opacity = 0.9,
                                                                    palette = 'ylorrd', 
                                                                    legend = T)
                                                              }}}else if(input$radio4 == 13){
                                                                if(input$var3 == 'Global'){
                                                                  
                                                                  sub <- df %>% 
                                                                    filter(eco == 2) %>% 
                                                                    filter(value == 1)
                                                                  
                                                                  data <- units2 %>% 
                                                                    filter(unit_ID %in% unique(sub$unit_ID))
                                                                  
                                                                  mapdeck_update(map_id = 'map') %>%
                                                                    clear_polygon(layer_id = '3') %>% 
                                                                    clear_polygon(layer_id = '1') %>% 
                                                                    clear_scatterplot(layer_id = '1') %>% 
                                                                    add_polygon(
                                                                      data = data,
                                                                      layer_id = '1',
                                                                      fill_colour = 'seag_marine_mean',
                                                                      fill_opacity = 0.9,
                                                                      focus_layer = F, 
                                                                      update_view = F,
                                                                      palette = 'ylorrd',
                                                                      legend = T)
                                                                  
                                                                }else{
                                                                  
                                                                  sub <- df %>% 
                                                                    filter(eco == 2) %>% 
                                                                    filter(value == 1)
                                                                  
                                                                  data2 <- units2 %>% 
                                                                    filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                                                  
                                                                  if(nrow(data2) > 0){
                                                                    
                                                                    mapdeck_update(map_id = 'map') %>% 
                                                                      clear_polygon(layer_id = '3') %>% 
                                                                      clear_polygon(layer_id = '1') %>% 
                                                                      clear_scatterplot(layer_id = '1') %>% 
                                                                      add_polygon(
                                                                        data = data2, 
                                                                        update_view = F,
                                                                        focus_layer = T, 
                                                                        layer_id = '3',
                                                                        fill_colour = 'seag_marine_mean', 
                                                                        fill_opacity = 0.9,
                                                                        palette = 'ylorrd', 
                                                                        legend = T)
                                                                  }}}else if(input$radio4 == 14){
                                                                    if(input$var3 == 'Global'){
                                                                      
                                                                      sub <- df %>% 
                                                                        filter(eco == 3) %>% 
                                                                        filter(value == 1)
                                                                      
                                                                      data <- units2 %>% 
                                                                        filter(unit_ID %in% unique(sub$unit_ID))
                                                                      
                                                                      mapdeck_update(map_id = 'map') %>%
                                                                        clear_polygon(layer_id = '3') %>% 
                                                                        clear_polygon(layer_id = '1') %>%
                                                                        clear_scatterplot(layer_id = '1') %>% 
                                                                        add_polygon(
                                                                          data = data,
                                                                          layer_id = '1',
                                                                          fill_colour = 'salt_climate_mean',
                                                                          fill_opacity = 0.9,
                                                                          focus_layer = F, 
                                                                          update_view = F,
                                                                          palette = 'ylorrd',
                                                                          legend = T)
                                                                      
                                                                    }else{
                                                                      
                                                                      sub <- df %>% 
                                                                        filter(eco == 3) %>% 
                                                                        filter(value == 1)
                                                                      
                                                                      data2 <- units2 %>% 
                                                                        filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                                                      
                                                                      if(nrow(data2) > 0){
                                                                        
                                                                        mapdeck_update(map_id = 'map') %>% 
                                                                          clear_polygon(layer_id = '3') %>% 
                                                                          clear_polygon(layer_id = '1') %>% 
                                                                          clear_scatterplot(layer_id = '1') %>% 
                                                                          add_polygon(
                                                                            data = data2, 
                                                                            update_view = F,
                                                                            focus_layer = T, 
                                                                            layer_id = '3',
                                                                            fill_colour = 'salt_climate_mean', 
                                                                            fill_opacity = 0.9,
                                                                            palette = 'ylorrd', 
                                                                            legend = T)
                                                                      }}}else if(input$radio4 == 15){
                                                                        if(input$var3 == 'Global'){
                                                                          
                                                                          sub <- df %>% 
                                                                            filter(eco == 3) %>% 
                                                                            filter(value == 1)
                                                                          
                                                                          data <- units2 %>% 
                                                                            filter(unit_ID %in% unique(sub$unit_ID))
                                                                          
                                                                          mapdeck_update(map_id = 'map') %>%
                                                                            clear_polygon(layer_id = '3') %>% 
                                                                            clear_polygon(layer_id = '1') %>% 
                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                            add_polygon(
                                                                              data = data,
                                                                              layer_id = '1',
                                                                              fill_colour = 'salt_land_mean',
                                                                              fill_opacity = 0.9,
                                                                              focus_layer = F, 
                                                                              update_view = F,
                                                                              palette = 'ylorrd',
                                                                              legend = T)
                                                                          
                                                                        }else{
                                                                          
                                                                          sub <- df %>% 
                                                                            filter(eco == 3) %>% 
                                                                            filter(value == 1)
                                                                          
                                                                          data2 <- units2 %>% 
                                                                            filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                                                          
                                                                          if(nrow(data2) > 0){
                                                                            
                                                                            mapdeck_update(map_id = 'map') %>% 
                                                                              clear_polygon(layer_id = '3') %>% 
                                                                              clear_polygon(layer_id = '1') %>% 
                                                                              clear_scatterplot(layer_id = '1') %>% 
                                                                              add_polygon(
                                                                                data = data2, 
                                                                                update_view = F,
                                                                                focus_layer = T, 
                                                                                layer_id = '3',
                                                                                fill_colour = 'salt_land_mean', 
                                                                                fill_opacity = 0.9,
                                                                                palette = 'ylorrd', 
                                                                                legend = T)
                                                                          }}}else if(input$radio4 == 16){
                                                                            if(input$var3 == 'Global'){
                                                                              
                                                                              sub <- df %>% 
                                                                                filter(eco == 3) %>% 
                                                                                filter(value == 1)
                                                                              
                                                                              data <- units2 %>% 
                                                                                filter(unit_ID %in% unique(sub$unit_ID))
                                                                              
                                                                              mapdeck_update(map_id = 'map') %>%
                                                                                clear_polygon(layer_id = '3') %>% 
                                                                                clear_polygon(layer_id = '1') %>% 
                                                                                clear_scatterplot(layer_id = '1') %>% 
                                                                                add_polygon(
                                                                                  data = data,
                                                                                  layer_id = '1',
                                                                                  fill_colour = 'salt_marine_mean',
                                                                                  fill_opacity = 0.9,
                                                                                  focus_layer = F, 
                                                                                  update_view = F,
                                                                                  palette = 'ylorrd',
                                                                                  legend = T)
                                                                              
                                                                            }else{
                                                                              
                                                                              sub <- df %>% 
                                                                                filter(eco == 3) %>% 
                                                                                filter(value == 1)
                                                                              
                                                                              data2 <- units2 %>% 
                                                                                filter(TERRITORY1 == input$var3 & unit_ID %in% unique(sub$unit_ID))
                                                                              
                                                                              if(nrow(data2) > 0){
                                                                                
                                                                                mapdeck_update(map_id = 'map') %>% 
                                                                                  clear_polygon(layer_id = '3') %>% 
                                                                                  clear_polygon(layer_id = '1') %>% 
                                                                                  clear_scatterplot(layer_id = '1') %>% 
                                                                                  add_polygon(
                                                                                    data = data2, 
                                                                                    update_view = F,
                                                                                    focus_layer = T, 
                                                                                    layer_id = '3',
                                                                                    fill_colour = 'salt_marine_mean', 
                                                                                    fill_opacity = 0.9,
                                                                                    palette = 'ylorrd', 
                                                                                    legend = T)
                                                                              }}}}else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 != 0 & input$radio7 == 0 & input$radio8 == 0 &  input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){ # extent
                                                                                
                                                                                if(input$radio6 == 1){
                                                                                  if(input$var5 == 'Global'){
                                                                                    
                                                                                    sub <- df %>% 
                                                                                      filter(eco == 4) %>% 
                                                                                      filter(value == 1)
                                                                                    
                                                                                    data <- units2 %>% 
                                                                                      filter(unit_ID %in% unique(sub$unit_ID))
                                                                                    
                                                                                    mapdeck_update(map_id = 'map') %>%
                                                                                      clear_polygon(layer_id = '3') %>% 
                                                                                      clear_polygon(layer_id = '1') %>% 
                                                                                      clear_scatterplot(layer_id = '1') %>% 
                                                                                      add_polygon(
                                                                                        data = data,
                                                                                        layer_id = '1',
                                                                                        fill_colour = 'kelp_carbon_gC_m2_yr',
                                                                                        fill_opacity = 0.9,
                                                                                        focus_layer = F, 
                                                                                        update_view = F,
                                                                                        palette = 'ylorrd',
                                                                                        legend = T)
                                                                                    
                                                                                  }else{
                                                                                    
                                                                                    sub <- df %>% 
                                                                                      filter(eco == 4) %>% 
                                                                                      filter(value == 1)
                                                                                    
                                                                                    data2 <- units2 %>% 
                                                                                      filter(TERRITORY1 == input$var5 & unit_ID %in% unique(sub$unit_ID))
                                                                                    
                                                                                    if(nrow(data2) > 0){
                                                                                      
                                                                                      mapdeck_update(map_id = 'map') %>% 
                                                                                        clear_polygon(layer_id = '3') %>% 
                                                                                        clear_polygon(layer_id = '1') %>% 
                                                                                        clear_scatterplot(layer_id = '1') %>% 
                                                                                        add_polygon(
                                                                                          data = data2, 
                                                                                          update_view = F,
                                                                                          focus_layer = T, 
                                                                                          layer_id = '3',
                                                                                          fill_colour = 'kelp_carbon_gC_m2_yr', 
                                                                                          fill_opacity = 0.9,
                                                                                          palette = 'ylorrd', 
                                                                                          legend = T)
                                                                                    }}}else if(input$radio6 == 2){
                                                                                      if(input$var5 == 'Global'){
                                                                                        
                                                                                        sub <- df %>% 
                                                                                          filter(eco == 1) %>% 
                                                                                          filter(value == 1)
                                                                                        
                                                                                        data <- units2 %>% 
                                                                                          filter(unit_ID %in% unique(sub$unit_ID))
                                                                                        
                                                                                        mapdeck_update(map_id = 'map') %>%
                                                                                          clear_polygon(layer_id = '3') %>% 
                                                                                          clear_polygon(layer_id = '1') %>% 
                                                                                          clear_scatterplot(layer_id = '1') %>% 
                                                                                          add_polygon(
                                                                                            data = data,
                                                                                            layer_id = '1',
                                                                                            fill_colour = 'mangrove_carbon_mgC_ha',
                                                                                            fill_opacity = 0.9,
                                                                                            focus_layer = F, 
                                                                                            update_view = F,
                                                                                            palette = 'ylorrd',
                                                                                            legend = T)
                                                                                        
                                                                                      }else{
                                                                                        
                                                                                        sub <- df %>% 
                                                                                          filter(eco == 1) %>% 
                                                                                          filter(value == 1)
                                                                                        
                                                                                        data2 <- units2 %>% 
                                                                                          filter(TERRITORY1 == input$var5 & unit_ID %in% unique(sub$unit_ID))
                                                                                        
                                                                                        if(nrow(data2) > 0){
                                                                                          
                                                                                          mapdeck_update(map_id = 'map') %>% 
                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                                            add_polygon(
                                                                                              data = data2, 
                                                                                              update_view = F,
                                                                                              focus_layer = T, 
                                                                                              layer_id = '3',
                                                                                              fill_colour = 'mangrove_carbon_mgC_ha', 
                                                                                              fill_opacity = 0.9,
                                                                                              palette = 'ylorrd', 
                                                                                              legend = T)
                                                                                        }}}else if(input$radio6 == 3){
                                                                                          if(input$var5 == 'Global'){
                                                                                            
                                                                                            sub <- df %>% 
                                                                                              filter(eco == 2) %>% 
                                                                                              filter(value == 1)
                                                                                            
                                                                                            data <- units2 %>% 
                                                                                              filter(unit_ID %in% unique(sub$unit_ID))
                                                                                            
                                                                                            mapdeck_update(map_id = 'map') %>%
                                                                                              clear_polygon(layer_id = '3') %>% 
                                                                                              clear_polygon(layer_id = '1') %>% 
                                                                                              clear_scatterplot(layer_id = '1') %>% 
                                                                                              add_polygon(
                                                                                                data = data,
                                                                                                layer_id = '1',
                                                                                                fill_colour = 'seagrass_carbon_gC_m2',
                                                                                                fill_opacity = 0.9,
                                                                                                focus_layer = F, 
                                                                                                update_view = F,
                                                                                                palette = 'ylorrd',
                                                                                                legend = T)
                                                                                            
                                                                                          }else{
                                                                                            
                                                                                            sub <- df %>% 
                                                                                              filter(eco == 2) %>% 
                                                                                              filter(value == 1)
                                                                                            
                                                                                            data2 <- units2 %>% 
                                                                                              filter(TERRITORY1 == input$var5 & unit_ID %in% unique(sub$unit_ID))
                                                                                            
                                                                                            if(nrow(data2) > 0){
                                                                                              
                                                                                              mapdeck_update(map_id = 'map') %>% 
                                                                                                clear_polygon(layer_id = '3') %>% 
                                                                                                clear_polygon(layer_id = '1') %>% 
                                                                                                clear_scatterplot(layer_id = '1') %>% 
                                                                                                add_polygon(
                                                                                                  data = data2, 
                                                                                                  update_view = F,
                                                                                                  focus_layer = T, 
                                                                                                  layer_id = '3',
                                                                                                  fill_colour = 'seagrass_carbon_gC_m2', 
                                                                                                  fill_opacity = 0.9,
                                                                                                  palette = 'ylorrd', 
                                                                                                  legend = T)
                                                                                            }}}else if(input$radio6 == 4){
                                                                                              if(input$var5 == 'Global'){
                                                                                                
                                                                                                sub <- df %>% 
                                                                                                  filter(eco == 3) %>% 
                                                                                                  filter(value == 1)
                                                                                                
                                                                                                data <- units2 %>% 
                                                                                                  filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                
                                                                                                mapdeck_update(map_id = 'map') %>%
                                                                                                  clear_polygon(layer_id = '3') %>% 
                                                                                                  clear_polygon(layer_id = '1') %>% 
                                                                                                  clear_scatterplot(layer_id = '1') %>% 
                                                                                                  add_polygon(
                                                                                                    data = data,
                                                                                                    layer_id = '1',
                                                                                                    fill_colour = 'saltmarsh_carbon_mgC_ha',
                                                                                                    fill_opacity = 0.9,
                                                                                                    focus_layer = F, 
                                                                                                    update_view = F,
                                                                                                    palette = 'ylorrd',
                                                                                                    legend = T)
                                                                                                
                                                                                              }else{
                                                                                                
                                                                                                sub <- df %>% 
                                                                                                  filter(eco == 3) %>% 
                                                                                                  filter(value == 1)
                                                                                                
                                                                                                data2 <- units2 %>% 
                                                                                                  filter(TERRITORY1 == input$var5 & unit_ID %in% unique(sub$unit_ID))
                                                                                                
                                                                                                if(nrow(data2) > 0){
                                                                                                  
                                                                                                  mapdeck_update(map_id = 'map') %>% 
                                                                                                    clear_polygon(layer_id = '3') %>% 
                                                                                                    clear_polygon(layer_id = '1') %>% 
                                                                                                    clear_scatterplot(layer_id = '1') %>% 
                                                                                                    add_polygon(
                                                                                                      data = data2, 
                                                                                                      update_view = F,
                                                                                                      focus_layer = T, 
                                                                                                      layer_id = '3',
                                                                                                      fill_colour = 'saltmarsh_carbon_mgC_ha', 
                                                                                                      fill_opacity = 0.9,
                                                                                                      palette = 'ylorrd', 
                                                                                                      legend = T)
                                                                                                }}}}else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 != 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0 & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){ # extent
                                                                                                  
                                                                                                  if(input$radio5 == 1){
                                                                                                    if(input$var4 == 'Global'){
                                                                                                      
                                                                                                      sub <- df %>% 
                                                                                                        filter(eco == 4) %>% 
                                                                                                        filter(value == 1)
                                                                                                      
                                                                                                      data <- units2 %>% 
                                                                                                        filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                      
                                                                                                      mapdeck_update(map_id = 'map') %>%
                                                                                                        clear_polygon(layer_id = '3') %>% 
                                                                                                        clear_polygon(layer_id = '1') %>% 
                                                                                                        clear_scatterplot(layer_id = '1') %>% 
                                                                                                        add_polygon(
                                                                                                          data = data,
                                                                                                          layer_id = '1',
                                                                                                          fill_colour = 'kelp_spp_richness',
                                                                                                          fill_opacity = 0.9,
                                                                                                          focus_layer = F, 
                                                                                                          update_view = F,
                                                                                                          palette = 'ylorrd',
                                                                                                        
                                                                                                          legend = T)
                                                                                                      
                                                                                                    }else{
                                                                                                      
                                                                                                      sub <- df %>% 
                                                                                                        filter(eco == 4) %>% 
                                                                                                        filter(value == 1)
                                                                                                      
                                                                                                      data2 <- units2 %>% 
                                                                                                        filter(TERRITORY1 == input$var4 & unit_ID %in% unique(sub$unit_ID))
                                                                                                      
                                                                                                      if(nrow(data2) > 0){
                                                                                                        
                                                                                                        mapdeck_update(map_id = 'map') %>% 
                                                                                                          clear_polygon(layer_id = '3') %>% 
                                                                                                          clear_polygon(layer_id = '1') %>% 
                                                                                                          clear_scatterplot(layer_id = '1') %>% 
                                                                                                          add_polygon(
                                                                                                            data = data2, 
                                                                                                            update_view = F,
                                                                                                            focus_layer = T, 
                                                                                                            layer_id = '3',
                                                                                                            fill_colour = 'kelp_spp_richness', 
                                                                                                            fill_opacity = 0.9,
                                                                                                            palette = 'ylorrd', 
                                                                                                            legend = T)
                                                                                                      }}}else if(input$radio5 == 2){
                                                                                                        if(input$var4 == 'Global'){
                                                                                                          
                                                                                                          sub <- df %>% 
                                                                                                            filter(eco == 1) %>% 
                                                                                                            filter(value == 1)
                                                                                                          
                                                                                                          data <- units2 %>% 
                                                                                                            filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                          
                                                                                                          mapdeck_update(map_id = 'map') %>%
                                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                                                            add_polygon(
                                                                                                              data = data,
                                                                                                              layer_id = '1',
                                                                                                              fill_colour = 'mangrove_spp_richness',
                                                                                                              fill_opacity = 0.9,
                                                                                                              focus_layer = F, 
                                                                                                              update_view = F,
                                                                                                              palette = 'ylorrd',
                                                                                                              legend = T)
                                                                                                          
                                                                                                        }else{
                                                                                                          
                                                                                                          sub <- df %>% 
                                                                                                            filter(eco == 1) %>% 
                                                                                                            filter(value == 1)
                                                                                                          
                                                                                                          data2 <- units2 %>% 
                                                                                                            filter(TERRITORY1 == input$var4 & unit_ID %in% unique(sub$unit_ID))
                                                                                                          
                                                                                                          if(nrow(data2) > 0){
                                                                                                            
                                                                                                            mapdeck_update(map_id = 'map') %>% 
                                                                                                              clear_polygon(layer_id = '3') %>% 
                                                                                                              clear_polygon(layer_id = '1') %>% 
                                                                                                              clear_scatterplot(layer_id = '1') %>% 
                                                                                                              add_polygon(
                                                                                                                data = data2, 
                                                                                                                update_view = F,
                                                                                                                focus_layer = T, 
                                                                                                                layer_id = '3',
                                                                                                                fill_colour = 'mangrove_spp_richness', 
                                                                                                                fill_opacity = 0.9,
                                                                                                                palette = 'ylorrd', 
                                                                                                                legend = T)
                                                                                                          }}}else if(input$radio5 == 3){
                                                                                                            if(input$var4 == 'Global'){
                                                                                                              
                                                                                                              sub <- df %>% 
                                                                                                                filter(eco == 2) %>% 
                                                                                                                filter(value == 1)
                                                                                                              
                                                                                                              data <- units2 %>% 
                                                                                                                filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                              
                                                                                                              mapdeck_update(map_id = 'map') %>%
                                                                                                                clear_polygon(layer_id = '3') %>% 
                                                                                                                clear_polygon(layer_id = '1') %>% 
                                                                                                                clear_scatterplot(layer_id = '1') %>% 
                                                                                                                add_polygon(
                                                                                                                  data = data,
                                                                                                                  layer_id = '1',
                                                                                                                  fill_colour = 'seagrass_spp_richness',
                                                                                                                  fill_opacity = 0.9,
                                                                                                                  focus_layer = F, 
                                                                                                                  update_view = F,
                                                                                                                  palette = 'ylorrd',
                                                                                                                  legend = T)
                                                                                                              
                                                                                                            }else{
                                                                                                              
                                                                                                              sub <- df %>% 
                                                                                                                filter(eco == 2) %>% 
                                                                                                                filter(value == 1)
                                                                                                              
                                                                                                              data2 <- units2 %>% 
                                                                                                                filter(TERRITORY1 == input$var4 & unit_ID %in% unique(sub$unit_ID))
                                                                                                              
                                                                                                              if(nrow(data2) > 0){
                                                                                                                
                                                                                                                mapdeck_update(map_id = 'map') %>% 
                                                                                                                  clear_polygon(layer_id = '3') %>% 
                                                                                                                  clear_polygon(layer_id = '1') %>% 
                                                                                                                  clear_scatterplot(layer_id = '1') %>% 
                                                                                                                  add_polygon(
                                                                                                                    data = data2, 
                                                                                                                    update_view = F,
                                                                                                                    focus_layer = T, 
                                                                                                                    layer_id = '3',
                                                                                                                    fill_colour = 'seagrass_spp_richness', 
                                                                                                                    fill_opacity = 0.9,
                                                                                                                    palette = 'ylorrd', 
                                                                                                                    legend = T)
                                                                                                              }}}else if(input$radio5 == 4){
                                                                                                                if(input$var4 == 'Global'){
                                                                                                                  
                                                                                                                  sub <- df %>% 
                                                                                                                    filter(eco == 3) %>% 
                                                                                                                    filter(value == 1)
                                                                                                                  
                                                                                                                  data <- units2 %>% 
                                                                                                                    filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                                  
                                                                                                                  mapdeck_update(map_id = 'map') %>%
                                                                                                                    clear_polygon(layer_id = '3') %>% 
                                                                                                                    clear_polygon(layer_id = '1') %>% 
                                                                                                                    clear_scatterplot(layer_id = '1') %>% 
                                                                                                                    add_polygon(
                                                                                                                      data = data,
                                                                                                                      layer_id = '1',
                                                                                                                      fill_colour = 'saltmarsh_spp_richness',
                                                                                                                      fill_opacity = 0.9,
                                                                                                                      focus_layer = F, 
                                                                                                                      update_view = F,
                                                                                                                      palette = 'ylorrd',
                                                                                                                      legend = T)
                                                                                                                  
                                                                                                                }else{
                                                                                                                  
                                                                                                                  sub <- df %>% 
                                                                                                                    filter(eco == 3) %>% 
                                                                                                                    filter(value == 1)
                                                                                                                  
                                                                                                                  data2 <- units2 %>% 
                                                                                                                    filter(TERRITORY1 == input$var4 & unit_ID %in% unique(sub$unit_ID))
                                                                                                                  
                                                                                                                  if(nrow(data2) > 0){
                                                                                                                    
                                                                                                                    mapdeck_update(map_id = 'map') %>% 
                                                                                                                      clear_polygon(layer_id = '3') %>% 
                                                                                                                      clear_polygon(layer_id = '1') %>% 
                                                                                                                      clear_scatterplot(layer_id = '1') %>% 
                                                                                                                      add_polygon(
                                                                                                                        data = data2, 
                                                                                                                        update_view = F,
                                                                                                                        focus_layer = T, 
                                                                                                                        layer_id = '3',
                                                                                                                        fill_colour = 'saltmarsh_spp_richness', 
                                                                                                                        fill_opacity = 0.9,
                                                                                                                        palette = 'ylorrd', 
                                                                                                                        legend = T)
                                                                                                                  }}}}else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 != 0 & input$radio8 == 0 & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){ # extent
                                                                                                                    
                                                                                                                    if(input$radio7 == 1){
                                                                                                                      if(input$var6 == 'Global'){
                                                                                                                        
                                                                                                                        mapdeck_update(map_id = 'map') %>%
                                                                                                                          clear_polygon(layer_id = '3') %>% 
                                                                                                                          clear_polygon(layer_id = '1') %>% 
                                                                                                                          clear_scatterplot(layer_id = '1') %>% 
                                                                                                                          add_polygon(
                                                                                                                            data = units2,
                                                                                                                            layer_id = '1',
                                                                                                                            fill_colour = 'prop_vul_pop',
                                                                                                                            fill_opacity = 0.9,
                                                                                                                            focus_layer = F, 
                                                                                                                            update_view = F,
                                                                                                                            palette = 'ylorrd',
                                                                                                                            legend = T)
                                                                                                                        
                                                                                                                      }else{
                                                                                                                        
                                                                                                                        data2 <- units2 %>% 
                                                                                                                          filter(TERRITORY1 == input$var6)
                                                                                                                        
                                                                                                                        if(nrow(data2) > 0){
                                                                                                                          
                                                                                                                          mapdeck_update(map_id = 'map') %>% 
                                                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                                                                            add_polygon(
                                                                                                                              data = data2, 
                                                                                                                              update_view = F,
                                                                                                                              focus_layer = T, 
                                                                                                                              layer_id = '3',
                                                                                                                              fill_colour = 'prop_vul_pop', 
                                                                                                                              fill_opacity = 0.9,
                                                                                                                              palette = 'ylorrd', 
                                                                                                                              legend = T)
                                                                                                                        }}}else if(input$radio7 == 2){
                                                                                                                          if(input$var6 == 'Global'){
                                                                                                                            
                                                                                                                            mapdeck_update(map_id = 'map') %>%
                                                                                                                              clear_polygon(layer_id = '3') %>% 
                                                                                                                              clear_polygon(layer_id = '1') %>% 
                                                                                                                              clear_scatterplot(layer_id = '1') %>% 
                                                                                                                              add_polygon(
                                                                                                                                data = units2,
                                                                                                                                layer_id = '1',
                                                                                                                                fill_colour = 'protein_consumption_kg.capita.yr',
                                                                                                                                fill_opacity = 0.9,
                                                                                                                                focus_layer = F, 
                                                                                                                                update_view = F,
                                                                                                                                palette = 'ylorrd',
                                                                                                                                legend = T)
                                                                                                                            
                                                                                                                          }else{
                                                                                                                            
                                                                                                                            data2 <- units2 %>% 
                                                                                                                              filter(TERRITORY1 == input$var6)
                                                                                                                            
                                                                                                                            if(nrow(data2) > 0){
                                                                                                                              
                                                                                                                              mapdeck_update(map_id = 'map') %>% 
                                                                                                                                clear_polygon(layer_id = '3') %>% 
                                                                                                                                clear_polygon(layer_id = '1') %>% 
                                                                                                                                clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                add_polygon(
                                                                                                                                  data = data2, 
                                                                                                                                  update_view = F,
                                                                                                                                  focus_layer = T, 
                                                                                                                                  layer_id = '3',
                                                                                                                                  fill_colour = 'protein_consumption_kg.capita.yr', 
                                                                                                                                  fill_opacity = 0.9,
                                                                                                                                  palette = 'ylorrd', 
                                                                                                                                  legend = T)
                                                                                                                            }}}else if(input$radio7 == 3){
                                                                                                                              if(input$var6 == 'Global'){
                                                                                                                                
                                                                                                                                sub <- df %>% 
                                                                                                                                  filter(eco == 5) %>% 
                                                                                                                                  filter(value == 1)
                                                                                                                                
                                                                                                                                data <- units2 %>% 
                                                                                                                                  filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                                                
                                                                                                                                mapdeck_update(map_id = 'map') %>%
                                                                                                                                  clear_polygon(layer_id = '3') %>% 
                                                                                                                                  clear_polygon(layer_id = '1') %>% 
                                                                                                                                  clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                  add_polygon(
                                                                                                                                    data = data,
                                                                                                                                    layer_id = '1',
                                                                                                                                    fill_colour = 'seafarm_area_ha',
                                                                                                                                    fill_opacity = 0.9,
                                                                                                                                    focus_layer = F, 
                                                                                                                                    update_view = F,
                                                                                                                                    palette = 'ylorrd',
                                                                                                                                  
                                                                                                                                    legend = T)
                                                                                                                                
                                                                                                                              }else{
                                                                                                                                
                                                                                                                                sub <- df %>% 
                                                                                                                                  filter(eco == 5) %>% 
                                                                                                                                  filter(value == 1)
                                                                                                                                
                                                                                                                                data2 <- units2 %>% 
                                                                                                                                  filter(TERRITORY1 == input$var6 & unit_ID %in% unique(sub$unit_ID))
                                                                                                                                
                                                                                                                                if(nrow(data2) > 0){
                                                                                                                                  
                                                                                                                                  mapdeck_update(map_id = 'map') %>% 
                                                                                                                                    clear_polygon(layer_id = '3') %>% 
                                                                                                                                    clear_polygon(layer_id = '1') %>% 
                                                                                                                                    clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                    add_polygon(
                                                                                                                                      data = data2, 
                                                                                                                                      update_view = F,
                                                                                                                                      focus_layer = T, 
                                                                                                                                      layer_id = '3',
                                                                                                                                      fill_colour = 'seafarm_area_ha', 
                                                                                                                                      fill_opacity = 0.9,
                                                                                                                                      palette = 'ylorrd', 
                                                                                                                                      legend = T)
                                                                                                                                }}}else if(input$radio7 == 4){
                                                                                                                                  if(input$var6 == 'Global'){
                                                                                                                                    
                                                                                                                                    sub <- df %>% 
                                                                                                                                      filter(eco == 1) %>% 
                                                                                                                                      filter(value == 1)
                                                                                                                                    
                                                                                                                                    data <- units2 %>% 
                                                                                                                                      filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                                                    
                                                                                                                                    mapdeck_update(map_id = 'map') %>%
                                                                                                                                      clear_polygon(layer_id = '3') %>% 
                                                                                                                                      clear_polygon(layer_id = '1') %>% 
                                                                                                                                      clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                      add_polygon(
                                                                                                                                        data = data,
                                                                                                                                        layer_id = '1',
                                                                                                                                        fill_colour = 'mangrove_coastal_protection',
                                                                                                                                        fill_opacity = 0.9,
                                                                                                                                        focus_layer = F, 
                                                                                                                                        update_view = F,
                                                                                                                                        palette = 'ylorrd',
                                                                                                                                 
                                                                                                                                        legend = T)
                                                                                                                                    
                                                                                                                                  }else{
                                                                                                                                    
                                                                                                                                    sub <- df %>% 
                                                                                                                                      filter(eco == 1) %>% 
                                                                                                                                      filter(value == 1)
                                                                                                                                    
                                                                                                                                    data2 <- units2 %>% 
                                                                                                                                      filter(TERRITORY1 == input$var6 & unit_ID %in% unique(sub$unit_ID))
                                                                                                                                    
                                                                                                                                    if(nrow(data2) > 0){
                                                                                                                                      
                                                                                                                                      mapdeck_update(map_id = 'map') %>% 
                                                                                                                                        clear_polygon(layer_id = '3') %>% 
                                                                                                                                        clear_polygon(layer_id = '1') %>% 
                                                                                                                                        clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                        add_polygon(
                                                                                                                                          data = data2, 
                                                                                                                                          update_view = F,
                                                                                                                                          focus_layer = T, 
                                                                                                                                          layer_id = '3',
                                                                                                                                          fill_colour = 'mangrove_coastal_protection', 
                                                                                                                                          fill_opacity = 0.9,
                                                                                                                                          palette = 'ylorrd', 
                                                                                                                                          legend = T)
                                                                                                                                    }}}else if(input$radio7 == 5){
                                                                                                                                      if(input$var6 == 'Global'){
                                                                                                                                        
                                                                                                                                        sub <- df %>% 
                                                                                                                                          filter(eco == 1) %>% 
                                                                                                                                          filter(value == 1)
                                                                                                                                        
                                                                                                                                        data <- units2 %>% 
                                                                                                                                          filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                                                        
                                                                                                                                        mapdeck_update(map_id = 'map') %>%
                                                                                                                                          clear_polygon(layer_id = '3') %>% 
                                                                                                                                          clear_polygon(layer_id = '1') %>% 
                                                                                                                                          clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                          add_polygon(
                                                                                                                                            data = data,
                                                                                                                                            layer_id = '1',
                                                                                                                                            fill_colour = 'mangrove_fish_catch',
                                                                                                                                            fill_opacity = 0.9,
                                                                                                                                            focus_layer = F, 
                                                                                                                                            update_view = F,
                                                                                                                                            palette = 'ylorrd',
                                                                                                                                     
                                                                                                                                            legend = T)
                                                                                                                                        
                                                                                                                                      }else{
                                                                                                                                        
                                                                                                                                        sub <- df %>% 
                                                                                                                                          filter(eco == 1) %>% 
                                                                                                                                          filter(value == 1)
                                                                                                                                        
                                                                                                                                        data2 <- units2 %>% 
                                                                                                                                          filter(TERRITORY1 == input$var6 & unit_ID %in% unique(sub$unit_ID))
                                                                                                                                        
                                                                                                                                        if(nrow(data2) > 0){
                                                                                                                                          
                                                                                                                                          mapdeck_update(map_id = 'map') %>% 
                                                                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                            add_polygon(
                                                                                                                                              data = data2, 
                                                                                                                                              update_view = F,
                                                                                                                                              focus_layer = T, 
                                                                                                                                              layer_id = '3',
                                                                                                                                              fill_colour = 'mangrove_fish_catch', 
                                                                                                                                              fill_opacity = 0.9,
                                                                                                                                              palette = 'ylorrd', 
                                                                                                                                              legend = T)
                                                                                                                                        }}}else if(input$radio7 == 6){
                                                                                                                                          if(input$var6 == 'Global'){
                                                                                                                                            
                                                                                                                                            sub <- df %>% 
                                                                                                                                              filter(eco == 4) %>% 
                                                                                                                                              filter(value == 1)
                                                                                                                                            
                                                                                                                                            data <- units2 %>% 
                                                                                                                                              filter(unit_ID %in% unique(sub$unit_ID))
                                                                                                                                            
                                                                                                                                            mapdeck_update(map_id = 'map') %>%
                                                                                                                                              clear_polygon(layer_id = '3') %>% 
                                                                                                                                              clear_polygon(layer_id = '1') %>% 
                                                                                                                                              clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                              add_polygon(
                                                                                                                                                data = data,
                                                                                                                                                layer_id = '1',
                                                                                                                                                fill_colour = 'kelp_fisheries_biomass_m2',
                                                                                                                                                fill_opacity = 0.9,
                                                                                                                                                focus_layer = F, 
                                                                                                                                                update_view = F,
                                                                                                                                                palette = 'ylorrd',
                                                                                                                                                
                                                                                                                                                legend = T)
                                                                                                                                            
                                                                                                                                          }else{
                                                                                                                                            
                                                                                                                                            sub <- df %>% 
                                                                                                                                              filter(eco == 4) %>% 
                                                                                                                                              filter(value == 1)
                                                                                                                                            
                                                                                                                                            data2 <- units2 %>% 
                                                                                                                                              filter(TERRITORY1 == input$var6 & unit_ID %in% unique(sub$unit_ID))
                                                                                                                                            
                                                                                                                                            if(nrow(data2) > 0){
                                                                                                                                              
                                                                                                                                              mapdeck_update(map_id = 'map') %>% 
                                                                                                                                                clear_polygon(layer_id = '3') %>% 
                                                                                                                                                clear_polygon(layer_id = '1') %>% 
                                                                                                                                                clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                                add_polygon(
                                                                                                                                                  data = data2, 
                                                                                                                                                  update_view = F,
                                                                                                                                                  focus_layer = T, 
                                                                                                                                                  layer_id = '3',
                                                                                                                                                  fill_colour = 'kelp_fisheries_biomass_m2', 
                                                                                                                                                  fill_opacity = 0.9,
                                                                                                                                                  palette = 'ylorrd', 
                                                                                                                                                  legend = T)
                                                                                                                                            }}}
                                                                                                                    }else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 != 0  & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){
                                                                                                                                        
                                                                                                                                          if(input$radio8 == 1){
                                                                                                                                          
                                                                                                                                          mapdeck_update(map_id = 'map') %>% 
                                                                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                              
                                                                                                                                            add_scatterplot(
                                                                                                                                              data = wwf, 
                                                                                                                                              lat = st_coordinates(wwf)[,2],
                                                                                                                                              lon = st_coordinates(wwf)[,1],
                                                                                                                                              radius = 80000,
                                                                                                                                              
                                                                                                                                              layer_id = '1',
                                                                                                                                              fill_colour = 'site_type', 
                                                                                                                                              palette = 'spectral', 
                                                                                                                                              legend = T)}else if(input$radio8 == 2){
                                                                                                                                                
                                                                                                                                                d <- wwf %>% filter(mangrove == 1)
                                                                                                                                                
                                                                                                                                                mapdeck_update(map_id = 'map') %>% 
                                                                                                                                                  clear_polygon(layer_id = '3') %>% 
                                                                                                                                                  clear_polygon(layer_id = '1') %>% 
                                                                                                                                                  add_scatterplot(
                                                                                                                                                    data = d, 
                                                                                                                                                    lat = st_coordinates(d)[,2],
                                                                                                                                                    lon = st_coordinates(d)[,1],
                                                                                                                                                    radius = 80000,
                                                                                                                                                 
                                                                                                                                                    layer_id = '1',
                                                                                                                                                    fill_colour = 'site_type', 
                                                                                                                                                    palette = 'spectral', 
                                                                                                                                                    legend = T)
                                                                                                                                              }else if(input$radio8 == 3){
                                                                                                                                                
                                                                                                                                                d <- wwf %>% filter(seagrass == 1)
                                                                                                                                                
                                                                                                                                                mapdeck_update(map_id = 'map') %>% 
                                                                                                                                                  clear_polygon(layer_id = '3') %>% 
                                                                                                                                                  clear_polygon(layer_id = '1') %>% 
                                                                                                                                                  add_scatterplot(
                                                                                                                                                    data = d, 
                                                                                                                                                    lat = st_coordinates(d)[,2],
                                                                                                                                                    lon = st_coordinates(d)[,1],
                                                                                                                                                    radius = 80000,
                                                                                                                                                   
                                                                                                                                                    layer_id = '1',
                                                                                                                                                    fill_colour = 'site_type', 
                                                                                                                                                    palette = 'spectral', 
                                                                                                                                                    legend = T)
                                                                                                                                              }else if(input$radio8 == 4){
                                                                                                                                                
                                                                                                                                                d <- wwf %>% filter(saltmarsh == 1)
                                                                                                                                                
                                                                                                                                                mapdeck_update(map_id = 'map') %>% 
                                                                                                                                                  clear_polygon(layer_id = '3') %>% 
                                                                                                                                                  clear_polygon(layer_id = '1') %>% 
                                                                                                                                                  add_scatterplot(
                                                                                                                                                    data = d, 
                                                                                                                                                    lat = st_coordinates(d)[,2],
                                                                                                                                                    lon = st_coordinates(d)[,1],
                                                                                                                                                    radius = 80000,
                                                                                                                                                   
                                                                                                                                                    layer_id = '1',
                                                                                                                                                    fill_colour = 'site_type', 
                                                                                                                                                    palette = 'spectral', 
                                                                                                                                                    legend = T)
                                                                                                                                              }else if(input$radio8 == 5){
                                                                                                                                                
                                                                                                                                                d <- wwf %>% filter(seaweed == 1)
                                                                                                                                                
                                                                                                                                                mapdeck_update(map_id = 'map') %>% 
                                                                                                                                                  clear_polygon(layer_id = '3') %>% 
                                                                                                                                                  clear_polygon(layer_id = '1') %>% 
                                                                                                                                                  add_scatterplot(
                                                                                                                                                    data = d, 
                                                                                                                                                    lat = st_coordinates(d)[,2],
                                                                                                                                                    lon = st_coordinates(d)[,1],
                                                                                                                                                    radius = 80000,
                                                                                                                                                    
                                                                                                                                                    layer_id = '1',
                                                                                                                                                    fill_colour = 'site_type', 
                                                                                                                                                    palette = 'spectral', 
                                                                                                                                                    legend = T)
                                                                                                                                              }
                                                                                                                                        }else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0 & input$radio9 != 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){
                                                                                                                                        
                                                                                                                                           # find the nth percentile of sites (i.e. top nth%)
                                                                                                                                            # identify where they overlap with others
                                                                                                                                            
                                                                                                                                            i <- input$slider1/100
                                                                                                                                            
                                                                                                                                            scores.sub <- scores %>% filter(mangrove == 1)
                                                                                                                                            
                                                                                                                                            if(input$radio9 == 1){
                                                                                                                                              top <- scores.sub %>% 
                                                                                                                                                filter(mang_extent >= quantile(mang_extent, probs = i))
                                                                                                                                            }else if(input$radio9 == 2){
                                                                                                                                              top <- scores.sub %>% 
                                                                                                                                                filter(mang_threat >= quantile(mang_threat, probs = i))
                                                                                                                                            }else if(input$radio9 == 3){
                                                                                                                                              top <- scores.sub %>% 
                                                                                                                                                filter(mang_carbon >= quantile(mang_carbon, probs = i))
                                                                                                                                            }else if(input$radio9 == 4){
                                                                                                                                              top <- scores.sub %>% 
                                                                                                                                                filter(mang_biodiversity >= quantile(mang_biodiversity, probs = i))
                                                                                                                                            }else if(input$radio9 == 5){
                                                                                                                                              top <- scores.sub %>% 
                                                                                                                                                filter(mang_cobenefit >= quantile(mang_cobenefit, probs = i))
                                                                                                                                            }
                                                                                                                                       
                                                                                                                                            data <- units2 %>% 
                                                                                                                                              filter(mangrove == 1 & unit_ID %in% top$unit_ID)
                                                                                                                                            
                                                                                                                                            mapdeck_update(map_id = 'map') %>% 
                                                                                                                                              clear_polygon(layer_id = '3') %>% 
                                                                                                                                              clear_polygon(layer_id = '1') %>% 
                                                                                                                                              clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                              add_polygon(
                                                                                                                                                data = data, 
                                                                                                                                                update_view = F,
                                                                                                                                                focus_layer = T, 
                                                                                                                                                layer_id = '3',
                                                                                                                                                fill_colour = '#20b2aa', 
                                                                                                                                                fill_opacity = 0.9,
                                                                                                                                                legend = F)
                                                                                                                                            
                                                                                                                                        }else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0 & input$radio9 == 0 & input$radio10 != 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 == 0){
                                                                                                                                          
                                                                                                                                          # find the nth percentile of sites (i.e. top nth%)
                                                                                                                                          # identify where they overlap with others
                                                                                                                                          
                                                                                                                                          i <- input$slider2/100
                                                                                                                                          
                                                                                                                                          scores.sub <- scores %>% filter(seagrass == 1)
                                                                                                                                          
                                                                                                                                          if(input$radio10 == 1){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(seag_extent >= quantile(seag_extent, probs = i))
                                                                                                                                          }else if(input$radio10 == 2){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(seag_threat >= quantile(seag_threat, probs = i))
                                                                                                                                          }else if(input$radio10 == 3){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(seag_carbon >= quantile(seag_carbon, probs = i))
                                                                                                                                          }else if(input$radio10 == 4){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(seag_biodiversity >= quantile(seag_biodiversity, probs = i))
                                                                                                                                          }else if(input$radio10 == 5){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(seag_cobenefit >= quantile(seag_cobenefit, probs = i))
                                                                                                                                          }
                                                                                                                                          
                                                                                                                                          data <- units2 %>% 
                                                                                                                                            filter(seagrass == 1 & unit_ID %in% top$unit_ID)
                                                                                                                                          
                                                                                                                                          mapdeck_update(map_id = 'map') %>% 
                                                                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                            add_polygon(
                                                                                                                                              data = data, 
                                                                                                                                              update_view = F,
                                                                                                                                              focus_layer = T, 
                                                                                                                                              layer_id = '3',
                                                                                                                                              fill_colour = '#20b2aa', 
                                                                                                                                              fill_opacity = 0.9,
                                                                                                                                              legend = F)
                                                                                                                                          
                                                                                                                                        }else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0 & input$radio9 == 0 & input$radio10 == 0 & input$radio11 != 0 & input$radio12 == 0 & input$radio13 == 0){
                                                                                                                                          
                                                                                                                                          # find the nth percentile of sites (i.e. top nth%)
                                                                                                                                          # identify where they overlap with others
                                                                                                                                          
                                                                                                                                          i <- input$slider3/100
                                                                                                                                          
                                                                                                                                          scores.sub <- scores %>% filter(saltmarsh == 1)
                                                                                                                                          
                                                                                                                                          if(input$radio11 == 1){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(salt_extent >= quantile(salt_extent, probs = i))
                                                                                                                                          }else if(input$radio11 == 2){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(salt_threat >= quantile(salt_threat, probs = i))
                                                                                                                                          }else if(input$radio11 == 3){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(salt_carbon >= quantile(salt_carbon, probs = i))
                                                                                                                                          }else if(input$radio11 == 4){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(salt_biodiversity >= quantile(salt_biodiversity, probs = i))
                                                                                                                                          }else if(input$radio11 == 5){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(salt_cobenefit >= quantile(salt_cobenefit, probs = i))
                                                                                                                                          }
                                                                                                                                          
                                                                                                                                          data <- units2 %>% 
                                                                                                                                            filter(saltmarsh == 1 & unit_ID %in% top$unit_ID)
                                                                                                                                          
                                                                                                                                          mapdeck_update(map_id = 'map') %>% 
                                                                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                            add_polygon(
                                                                                                                                              data = data, 
                                                                                                                                              update_view = F,
                                                                                                                                              focus_layer = T, 
                                                                                                                                              layer_id = '3',
                                                                                                                                              fill_colour = '#20b2aa', 
                                                                                                                                              fill_opacity = 0.9,
                                                                                                                                              legend = F)
                                                                                                                                          
                                                                                                                                        }else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0 & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 != 0 & input$radio13 == 0){
                                                                                                                                          
                                                                                                                                          # find the nth percentile of sites (i.e. top nth%)
                                                                                                                                          # identify where they overlap with others
                                                                                                                                          
                                                                                                                                          i <- input$slider4/100
                                                                                                                                          
                                                                                                                                          scores.sub <- scores %>% filter(kelp == 1)
                                                                                                                                          
                                                                                                                                          if(input$radio12 == 1){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(kelp_extent >= quantile(kelp_extent, probs = i))
                                                                                                                                          }else if(input$radio12 == 2){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(kelp_threat >= quantile(kelp_threat, probs = i))
                                                                                                                                          }else if(input$radio12 == 3){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(kelp_carbon >= quantile(kelp_carbon, probs = i))
                                                                                                                                          }else if(input$radio12 == 4){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(kelp_biodiversity >= quantile(kelp_biodiversity, probs = i))
                                                                                                                                          }else if(input$radio12 == 5){
                                                                                                                                            top <- scores.sub %>% 
                                                                                                                                              filter(kelp_cobenefit >= quantile(kelp_cobenefit, probs = i))
                                                                                                                                          }
                                                                                                                                          
                                                                                                                                          data <- units2 %>% 
                                                                                                                                            filter(kelp == 1 & unit_ID %in% top$unit_ID)
                                                                                                                                          
                                                                                                                                          mapdeck_update(map_id = 'map') %>% 
                                                                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                            add_polygon(
                                                                                                                                              data = data, 
                                                                                                                                              update_view = F,
                                                                                                                                              focus_layer = T, 
                                                                                                                                              layer_id = '3',
                                                                                                                                              fill_colour = '#20b2aa', 
                                                                                                                                              fill_opacity = 0.9,
                                                                                                                                              legend = F)
                                                                                                                                          
                                                                                                                                        }else if(input$radio == "#FFFFFF" & input$radio1 == 0 & input$radio3 == 0 & input$radio4 == 0 & input$radio5 == 0 & input$radio6 == 0 & input$radio7 == 0 & input$radio8 == 0 & input$radio9 == 0 & input$radio10 == 0 & input$radio11 == 0 & input$radio12 == 0 & input$radio13 != 0){
                                                                                                                                          
                                                                                                                                          # find the nth percentile of sites (i.e. top nth%)
                                                                                                                                          # identify where they overlap with others
                                                                                                                                          
                                                                                                                                          i <- input$slider5/100
                                                                                                                                    
                                                                                                                                          if(input$radio13 == 1){
                                                                                                                                            top <- scores %>% 
                                                                                                                                              filter(extent >= quantile(extent, probs = i))
                                                                                                                                          }else if(input$radio13 == 2){
                                                                                                                                            top <- scores %>% 
                                                                                                                                              filter(threat >= quantile(threat, probs = i))
                                                                                                                                          }else if(input$radio13 == 3){
                                                                                                                                            top <- scores %>% 
                                                                                                                                              filter(carbon >= quantile(carbon, probs = i))
                                                                                                                                          }else if(input$radio13 == 4){
                                                                                                                                            top <- scores %>% 
                                                                                                                                              filter(biodiversity >= quantile(biodiversity, probs = i))
                                                                                                                                          }else if(input$radio13 == 5){
                                                                                                                                            top <- scores %>% 
                                                                                                                                              filter(cobenefit >= quantile(cobenefit, probs = i))
                                                                                                                                          }
                                                                                                                                          
                                                                                                                                          data <- units2 %>% 
                                                                                                                                            filter(unit_ID %in% top$unit_ID)
                                                                                                                                          
                                                                                                                                          mapdeck_update(map_id = 'map') %>% 
                                                                                                                                            clear_polygon(layer_id = '3') %>% 
                                                                                                                                            clear_polygon(layer_id = '1') %>% 
                                                                                                                                            clear_scatterplot(layer_id = '1') %>% 
                                                                                                                                            add_polygon(
                                                                                                                                              data = data, 
                                                                                                                                              update_view = F,
                                                                                                                                              focus_layer = T, 
                                                                                                                                              layer_id = '3',
                                                                                                                                              fill_colour = '#20b2aa', 
                                                                                                                                              fill_opacity = 0.9,
                                                                                                                                              legend = F)
                                                                                                                                          
                                                                                                                                        }
                                                                                                                                          
                                                                                                                                        
        })
    }

# Run the application 

shinyApp(ui = ui, server = server)
