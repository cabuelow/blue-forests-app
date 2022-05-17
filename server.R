# server

library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(leaflet)

# load and wrangle all required components for app

source("wrangling.R")

# server logic

function(input, output, session) {
  
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