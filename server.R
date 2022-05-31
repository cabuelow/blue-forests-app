# code adapted from: https://github.com/molly-williams/deltaSLR_map

# server

library(shiny)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(sf)
library(leaflet)
library(RColorBrewer)

# server logic

function(input, output, session) {
  
  ####################### Explore distributions logic ########################
  # create basemap
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(
        group = 'basepoly',
        data = unitsall,
        color = "#FFFFFF",
        weight = 0.4) %>%
      addCircleMarkers(group = "Blue Forest projects",
                       data = wwf,
                       color = ~pal(site_type),
                       weight = 1,
                       radius = 5,
                       popup= my_popups) %>%
      addLegend("bottomright", data = wwf,
                pal = pal, values = ~site_type,
                title = "Project type",
                opacity = 1) %>%
      addLayersControl(
        overlayGroups = c("Blue Forest projects"),
        options = layersControlOptions(collapsed = FALSE)
      )
  }) # end mymap render
  
  #
  # Filter data for ticked blue forests 
  #
  update_forest_dat <- reactive({
    x <- filter(df, eco %in% as.numeric(input$blue_forest) &
                  value ==1) 
    x <- units2[units2$unit_ID %in% x$unit_ID, ]
    return(x)
  })
  
  #
  # Update map
  #
  
  observe({
    
    req(input$nav=="Explore Blue Forest Distributions")
    
    leafletProxy("mymap") %>%
      clearGroup(c('forest')) %>%
      addPolygons(
        group = 'forest',
        data = update_forest_dat(),
        layerId=~unit_ID,
        weight = 0.4)
    #note popups block shape_click events
    
  })
  
  # ------------ 
  # Single forest pages
  # ------------ 
  forestServer("mangroves", "mangrove")
  #forestServer("seagrass", "seagrass")
  #forestServer("saltmarsh", "saltmarsh")
  #forestServer("kelp", "kelp")
  
} #end server