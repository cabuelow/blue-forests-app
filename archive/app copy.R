library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(shinydashboard)
library(dashboardthemes)
library(leaflet)
#library(leaflet.extras2)

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

ui <- navbarPage(
  title = 'Blue Forests',
  tabPanel('Explore Blue Forest Distributions',
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")
               ),
            
               leafletOutput("map", width="100%", height="100%"),
               
               tags$style(".leaflet-control-layers-overlays{color: blue}"),
               
               absolutePanel(id = "controls", 
                             class = "panel panel-default", 
                             fixed = TRUE,
                             draggable = TRUE, 
                             top = 100, 
                             left = 30, 
                             right = "auto", 
                             bottom = "auto",
                             width = 330, 
                             height = "auto",
                             
                             tags$br(),
                             
                             tags$em("Allow a moment for layers to load."),
                 
                             selectInput("radio1", 
                                         #label=NULL,
                                         label=h4(tags$b("1. Select blue forest:")), 
                                         choices = list("None" = 0, "Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4, 'All' = 5, 'Number of forests' = 6),
                                         selected=FALSE),
                             
                             tags$br(),
                             tags$br(),
                             
                             #selectInput("var1", label = h4(tags$b("2. Choose country or territory"), 
                              #           choices = terr, 
                               #          selected = 'Global')),
               ), #end absolute panel
           ), #end div
           
           tags$div(id="cite",
                    tags$em('This map was created in support of the Blue Forests Initiative, a project supported by WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
) # end nav bar

# Define server logic

server <- function(input, output, session) {
  
  # Create basemap
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(
        data = rbind(select(units2, unit_ID, TERRITORY1, PROVINC, REALM, geom), 
                     select(unitsNA, unit_ID, TERRITORY1, PROVINC, REALM, geom)),
        color = "#FFFFFF")})

  # Change polygons based on blue forest and country/territory selection
 
  observe({
    
    forest <- input$radio1
    #cterr <- input$var1
    
    if(forest == 0){
      filtdata <- rbind(select(units2, unit_ID, TERRITORY1, PROVINC, REALM, geom), 
                        select(unitsNA, unit_ID, TERRITORY1, PROVINC, REALM, geom))
      cpal <-"#FFFFFF"
      }else if(forest == 1){
        sub <- df %>% 
          filter(eco == 1) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID))
        cpal <- '#20b2aa'
      }else if(forest == 2){
        sub <- df %>% 
          filter(eco == 2) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID))
        cpal <- '#20b2aa'
      }else if(forest == 3){
        sub <- df %>% 
          filter(eco == 3) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID))
        cpal <- '#20b2aa'
      }else if(forest == 4){
        sub <- df %>% 
          filter(eco == 4) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID))
        cpal <- '#20b2aa'
      }else if(forest == 5){
        sub <- df %>% 
          filter(eco %in% c(1,2,3,4)) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID))
        cpal <- '#20b2aa'
      }else if(forest == 6){
        filtdata <- units2 %>% 
          mutate(total = mangrove + seagrass + saltmarsh + kelp) %>% 
          mutate(total = factor(total))
        cpal <- brewer.pal(9, 'Spectral')
      }
    
    # Create reactive map
    
    leafletProxy("map") %>%
      clearControls() %>% 
      clearShapes() %>% 
      addPolygons(data = filtdata,
                  color= cpal)
  }) # end observe 
 
} #end server
  
# Run the application 

shinyApp(ui = ui, server = server)