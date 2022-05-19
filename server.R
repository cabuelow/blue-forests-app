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

  # create basemap
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron)})
  
  observe({
    
    forest <- input$bf # forest type
    cterr <- input$ct # country/territory
    proj <- input$bfproj # wwf blue forest projects
    prof <- input$profile # enabling profile constraint layer

    if(cterr == 'Global'){ #if-else series that filters for polygons to map depending on if global & BF type
    if(forest == 0){
      filtdata <- unitsall
      zz <- unname(st_bbox(filtdata))
      cpal <-"#FFFFFF"
    }else{
      input_filter(df, forest)
    }}else{ # if not global
      if(forest == 0){
        filtdata <- unitsall %>% 
          filter(TERRITORY1 == input$ct)
        zz <- unname(st_bbox(filtdata))
        cpal <-"#FFFFFF"
      }else{
        input_filter2(df, forest, input$ct)
      }}
    
    # create reactive map
    
    if(prof == FALSE){ # if-else to define whether enabling constraints are on or off
    if(proj == FALSE){ # if-else to define whether wwf projects are on or off
    leafletProxy("map") %>%
      clearControls() %>% 
      clearShapes() %>% 
      clearMarkers() %>% 
      flyToBounds(zz[1], zz[2], zz[3], zz[4]) %>% 
      addPolygons(
        data = unitsall,
        color = "#FFFFFF",
        weight = 0.4,
        popup = T) %>% 
      addPolygons(data = filtdata,
                  color = cpal,
                  weight = 0.4,
                  popup = T)
    }else{
      leafletProxy("map") %>%
        clearControls() %>% 
        clearShapes() %>% 
        flyToBounds(zz[1], zz[2], zz[3], zz[4]) %>% 
        addPolygons(
         data = unitsall,
        color = "#FFFFFF",
        weight = 0.4,
        popup = T) %>% 
        addPolygons(data = filtdata,
                    color = cpal,
                    weight = 0.4,
                    popup = T) %>% 
        addCircleMarkers(data = wwf,
                         color = ~pal(site_type),
                         weight = 1,
                         radius = 5,
                         popup= my_popups) %>% 
        addLegend("bottomright", data = wwf,
                  pal = pal, values = ~site_type,
                  title = "Project type",
                  opacity = 1)
    }}else{
      if(proj == FALSE){ # if-else to define whether wwf projects are on or off
        leafletProxy("map") %>%
          clearControls() %>% 
          clearShapes() %>% 
          clearMarkers() %>% 
          addPolygons(
            data = profile1.sf,
            color = "#FFFFCC",
            weight = 0.4) %>% 
          flyToBounds(zz[1], zz[2], zz[3], zz[4]) %>% 
          addPolygons(
            data = unitsall,
            color = "#FFFFFF",
            weight = 0.4,
            popup = T) %>% 
          addPolygons(data = filtdata,
                      color = cpal,
                      weight = 0.4,
                      popup = T)
      }else{
        leafletProxy("map") %>%
          clearControls() %>% 
          clearShapes() %>% 
          addPolygons(
            data = profile1.sf,
            color = "#FFFFCC",
            weight = 0.4) %>% 
          flyToBounds(zz[1], zz[2], zz[3], zz[4]) %>% 
          addPolygons(
            data = unitsall,
            color = "#FFFFFF",
            weight = 0.4,
            popup = T) %>% 
          addPolygons(data = filtdata,
                      color = cpal,
                      weight = 0.4,
                      popup = T) %>% 
          addCircleMarkers(data = wwf,
                           color = ~pal(site_type),
                           weight = 1,
                           radius = 5,
                           popup= my_popups) %>% 
          addLegend("bottomright", data = wwf,
                    pal = pal, values = ~site_type,
                    title = "Project type",
                    opacity = 1)
      }}# end if-else
  }) # end observe 
  
} #end server