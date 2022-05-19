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

  ####################### Explore distributions logic ########################
  # create basemap
  
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron)})
  
  observe({
    
    req(input$nav=="Explore Blue Forest Distributions") # map for selected parameters will appear automatically when user navigates to this tab
    
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
  
  ####################### Explore hotspots logic ########################
  
  # create basemap
  
  output$map1 <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron)})
  
  # update with inputs
  
  observe({
    
    req(input$nav=="Explore Blue Forest Hotspots") # map for selected parameters will appear automatically when user navigates to this tab
    
    forest2 <- input$bfcheck # forest type
    i <- input$perc/100 # percentile
    proj2 <- input$bfproj2 # wwf blue forest projects
    prof2 <- input$profile2 # enabling profile constraint layer
    
    filt_hotspots(scores, forest2, i)
    
    # create reactive map
    
    if(prof2 == FALSE){ # if-else to define whether enabling constraints are on or off
      if(proj2 == FALSE){ # if-else to define whether wwf projects are on or off
        leafletProxy("map1") %>%
          clearControls() %>% 
          clearShapes() %>% 
          clearMarkers() %>% 
          #flyToBounds(zz[1], zz[2], zz[3], zz[4]) %>% 
          addPolygons(
            data = unitsall,
            color = "#FFFFFF",
            weight = 0.4,
            popup = T) %>% 
          addPolygons(data = p1,
                      color = '#66CC33',
                      weight = 1) %>% 
          addPolygons(data = p2,
                      color = '#CC3300',
                      weight = 0.8) %>% 
          addPolygons(data = p3,
                      color = '#9966CC',
                      weight = 0.6) %>% 
          addPolygons(data = p4,
                      color = '#FFCC00',
                      weight = 0.4) #%>% 
        addPolygons(data = p5,
                    color = '#00CCCC',
                    weight = 0.1)
      }}# end if-else
    
  }) # end observe
  
} #end server