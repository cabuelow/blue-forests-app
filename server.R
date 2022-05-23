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
      addProviderTiles(providers$CartoDB.Positron) %>% 
    addPolygons(
      group = 'basepoly',
      data = unitsall,
      color = "#FFFFFF",
      weight = 0.4)
  })
  
  # reactive expression
  
  comb_output <- reactive({
    
    if(input$ct == 'Global'){ #if-else series that filters for polygons to map depending on if global & BF type
      if(input$bf == 0){
        filtdata <- unitsall
        zz <- unname(st_bbox(filtdata))
        cpal <-"#FFFFFF"
      }else{
        input_filter(df, input$bf)
      }}else{ # if not global
        if(input$bf == 0){
          filtdata <- unitsall %>% filter(TERRITORY1 == input$ct)
          zz <- unname(st_bbox(filtdata))
          cpal <-"#FFFFFF"
        }else{
          input_filter2(df, input$bf, input$ct)
        }}
    combo <- list(filtdata = filtdata, zz = zz, cpal = cpal)
    
  }) # end reactive
  
  # observe the reactive
  
  observe({

    newdat <- comb_output() # get reactive data
    
    # create reactive map
    
    if(input$profile == FALSE){ # if-else to define whether enabling constraints are on or off
      if(input$bfproj == FALSE){ # if-else to define whether wwf projects are on or off
        leafletProxy("map") %>%
          clearControls() %>% 
          clearGroup(c('forest', 'profile')) %>% 
          clearMarkers() %>% 
          flyToBounds(newdat$zz[1], newdat$zz[2], newdat$zz[3], newdat$zz[4]) %>% 
          addPolygons(
            group = 'forest',
            data = newdat$filtdata,
            color = newdat$cpal,
            weight = 0.4,
            popup = T)
      }else{
        leafletProxy("map") %>%
          clearControls() %>% 
          clearGroup(c('forest', 'profile')) %>% 
          flyToBounds(newdat$zz[1], newdat$zz[2], newdat$zz[3], newdat$zz[4]) %>% 
          addPolygons(
            group = 'forest',
            data = newdat$filtdata,
            color = newdat$cpal,
            weight = 0.4,
            popup = T) %>% 
          addCircleMarkers(
            data = wwf,
            color = ~pal(site_type),
            weight = 1,
            radius = 5,
            popup= my_popups) %>% 
          addLegend("bottomright", data = wwf,
                    pal = pal, values = ~site_type,
                    title = "Project type",
                    opacity = 1)
      }}else{
        if(input$bfproj == FALSE){ # if-else to define whether wwf projects are on or off
          leafletProxy("map") %>%
            clearControls() %>% 
            clearGroup(c('forest', 'profile')) %>% 
            clearMarkers() %>% 
            addPolygons(
              data = profile1.sf,
              color = "#FFFFCC",
              weight = 0.4) %>% 
            flyToBounds(newdat$zz[1], newdat$zz[2], newdat$zz[3], newdat$zz[4]) %>% 
            addPolygons(
              group = 'forest',
              data = newdat$filtdata,
              color = newdat$cpal,
              weight = 0.4,
              popup = T)
        }else{
          leafletProxy("map") %>%
            clearControls() %>% 
            clearGroup(c('forest', 'profile')) %>% 
            addPolygons(
              group = 'profile',
              data = profile1.sf,
              color = "#FFFFCC",
              weight = 0.4) %>% 
            flyToBounds(newdat$zz[1], newdat$zz[2], newdat$zz[3], newdat$zz[4]) %>%
            addPolygons(
              group = 'forest',
              data = newdat$filtdata,
              color = newdat$cpal,
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
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(
        group = 'basepoly1',
        data = unitsall,
        color = "#FFFFFF",
        weight = 0.4)
  })
  
  # reactive expression
  
  comb_output1 <- reactive({
    filt_hotspots(scores, input$bfcheck, 'mang', input$perc/100)
  })
  
  # observe the reactive
  
  observe({
    
    newdat1 <- comb_output1() # get reactive data
    
 # if(input$profile2 == FALSE){ # if-else to define whether enabling constraints are on or off
  #  if(input$bfproj2 == FALSE){ # if-else to define whether wwf projects are on or off
    if(input$bfcheck != 'all'){  
      
      if(input$bfcheck == 'extent'){
        sub <- 1
        }else if(input$bfcheck == 'threat'){
          sub <- 2
        }else if(input$bfcheck == 'carbon'){
          sub <- 3
        }else if(input$bfcheck == 'cobenefit'){
          sub <- 4
        }else if(input$bfcheck == 'biodiversity'){
          sub <- 5
        }
      
    leafletProxy("map1") %>%
        clearControls() %>% 
        clearGroup(c('p1', 'p2', 'p3', 'p4', 'p5', 'p6')) %>% 
        clearMarkers() %>% 
        addPolygons(
          group = 'p1',
          data = filter(units2, unit_ID %in% newdat1$unit_ID),
          color = hot_pal[sub],
          weight = 0.5) 
    }else{
      
      a <- filter(newdat1, criteria == 'extent')
      b <- filter(newdat1, criteria == 'threat')
      c <- filter(newdat1, criteria == 'carbon')
      d <- filter(newdat1, criteria == 'cobenefit')
      e <- filter(newdat1, criteria == 'biodiversity')
      
      leafletProxy("map1") %>%
        clearControls() %>% 
        clearGroup(c('p1', 'p2', 'p3', 'p4', 'p5', 'p6')) %>% 
        clearMarkers() %>% 
        addPolygons(
          group = 'p1',
          data = filter(units2, unit_ID %in% a$unit_ID),
          color = hot_pal[1],
          weight = 1) %>% 
        addPolygons(
          group = 'p2',
          data = filter(units2, unit_ID %in% b$unit_ID),
          color = hot_pal[2],
          weight = 0.8) %>% 
        addPolygons(
          group = 'p3',
          data = filter(units2, unit_ID %in% c$unit_ID),
          color = hot_pal[3],
          weight = 0.6) %>% 
        addPolygons(
          group = 'p4',
          data = filter(units2, unit_ID %in% d$unit_ID),
          color = hot_pal[4],
          weight = 0.4) %>% 
        addPolygons(
          group = 'p5',
          data = filter(units2, unit_ID %in% e$unit_ID),
          color = hot_pal[5],
          weight = 0.1)
    }
   # }}# end if-else
  })# end observe
  
} #end server