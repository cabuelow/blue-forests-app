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
  })
  
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
  
    leafletProxy("mymap") %>%
      # clearControls() %>%
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
  #GET WORKING THEN REPEAT AS A MODULE
  
    output$mangrove_map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(
        group = 'basepoly',
        data = unitsall,
        color = "#FFFFFF",
        weight = 0.4) %>%
        addPolygons(
          group = "mangroves",
          data = mangrove_dat,
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
  })

  update_top_sites_dat <- reactive({
    scores <- filter(scores, mang == 1)
    q <- quantile(scores$mang_extent, probs = 1-(input$perc)/100,
                  names = FALSE)
    x <- filter(scores, mang_extent > q) 
    x <- units2[units2$unit_ID %in% x$unit_ID, ]
    return(x)
  })
    
  observe({
    #can add and remove by layer ID, so should be able to speed this
    # up by just changing polygons that need to be changed. 
    leafletProxy("mangrove_map") %>%
      # # clearControls() %>%
       clearGroup(c('top_forest')) %>%
      addPolygons(
        group = 'top_forest',
        color = "red",
        data = update_top_sites_dat(),
        layerId=~unit_ID,
        weight = 0.4) 
      
  })
  
  #
  # Render dashboard
  #
  
  unit_ID_clicked <- reactive({
    input$mymap_shape_click$id
  })
  
  output$unit_ID_dashboard <- renderTable({
    if (!is.null(unit_ID_clicked())){
    x <- filter(units2, unit_ID == unit_ID_clicked())
    sf::st_geometry(x) <- NULL
    x[,c("SOVEREIGN1", "unit_area_ha", "mangrove_2016_area_ha")]
    } else {
      NULL
    }
  })
  
} #end server