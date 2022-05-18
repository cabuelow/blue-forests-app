# code adapted from: https://github.com/molly-williams/deltaSLR_map

# server

library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)

# server logic

function(input, output, session) {
  
  # Create basemap
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron)})
  
  # Change polygons based on blue forest and country/territory selection
  
  observe({
    
    forest <- input$radio1
    cterr <- input$var1
    
    if(cterr == 'Global'){
      
    if(forest == 0){
      filtdata <- rbind(select(units2, unit_ID, TERRITORY1, PROVINC, REALM, geom), 
                        select(unitsNA, unit_ID, TERRITORY1, PROVINC, REALM, geom))
      zz <- unname(st_bbox(filtdata))
      cpal <-"#FFFFFF"
    }else if(forest == 1){
      sub <- df %>% 
        filter(eco == 1) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }else if(forest == 2){
      sub <- df %>% 
        filter(eco == 2) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }else if(forest == 3){
      sub <- df %>% 
        filter(eco == 3) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }else if(forest == 4){
      sub <- df %>% 
        filter(eco == 4) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }else if(forest == 5){
      sub <- df %>% 
        filter(eco %in% c(1,2,3,4)) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }}else{
      if(forest == 0){
        filtdata <- rbind(select(units2, unit_ID, TERRITORY1, PROVINC, REALM, geom), 
                          select(unitsNA, unit_ID, TERRITORY1, PROVINC, REALM, geom)) %>% 
          filter(TERRITORY1 == input$var1)
        zz <- unname(st_bbox(filtdata))
        cpal <-"#FFFFFF"
      }else if(forest == 1){
        sub <- df %>% 
          filter(eco == 1) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$var1)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }else if(forest == 2){
        sub <- df %>% 
          filter(eco == 2) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$var1)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }else if(forest == 3){
        sub <- df %>% 
          filter(eco == 3) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$var1)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }else if(forest == 4){
        sub <- df %>% 
          filter(eco == 4) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$var1)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }else if(forest == 5){
        sub <- df %>% 
          filter(eco %in% c(1,2,3,4)) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$var1)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }
      }
    
    # Create reactive map
    
    leafletProxy("map") %>%
      clearControls() %>% 
      clearShapes() %>% 
      flyToBounds(zz[1], zz[2], zz[3], zz[4]) %>% 
      #addPolygons(
       # data = rbind(select(units2, unit_ID, TERRITORY1, PROVINC, REALM, geom), 
         #            select(unitsNA, unit_ID, TERRITORY1, PROVINC, REALM, geom)),
        #color = "#FFFFFF",
        #weight = 0.4,
        #popup = T) %>% 
      addPolygons(data = filtdata,
                  color = cpal,
                  weight = 0.4,
                  popup = T)
  }) # end observe 
  
} #end server