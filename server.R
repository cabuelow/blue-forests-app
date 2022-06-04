# code adapted from: https://github.com/molly-williams/deltaSLR_map

# server

library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(ggplot2)

# server logic

function(input, output, session) {
  
  ####################### Explore distributions logic ########################
  
  ## use reactive values to store the id from observing the shape click
  rv <- reactiveVal()
  
  # create basemap
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(
        group = 'basepoly',
        data = unitsall,
        color = "#FFFFFF",
        weight = 0.4) %>%
      addMapPane('layer6', zIndex = 460) %>%
      addMapPane('layer7', zIndex = 470) %>%
      addCircleMarkers(group = "WWF Blue Forest projects",
                       data = wwf,
                       color = ~pal(site_type),
                       weight = 0.5,
                       radius = 5,
                       popup= my_popups,
                       options = pathOptions(pane = "layer6")) %>%
      addCircleMarkers(group = "Investment assessment",
                       data = inproj,
                       color = ~pal2(Investment_readiness_stage),
                       weight = 1,
                       radius = 2,
                       popup= my_popups2,
                       options = pathOptions(pane = "layer7")) %>%
      addLegend("bottomright", data = wwf,
                pal = pal, values = ~site_type,
                title = "WWF Blue Forest project type",
                opacity = 1, group = 'WWF Blue Forest projects') %>%
      addLegend("bottomright", data = inproj,
                pal = pal2, values = ~Investment_readiness_stage,
                title = "Investment stage",
                opacity = 1, group = 'Investment assessment') %>% 
      addLayersControl(
        overlayGroups = c("WWF Blue Forest projects", "Investment assessment"),
        options = layersControlOptions(collapsed = FALSE)) %>% 
      hideGroup('WWF Blue Forest projects') %>% 
      hideGroup("Investment assessment")
    
  }) # end render leaflet
  
  #
  # Filter data for ticked blue forests 
  #
  update_forest_dat <- reactive({
    x <- filter(df, eco %in% as.numeric(input$blue_forest) & value ==1) 
    x <- units2[units2$unit_ID %in% x$unit_ID, ]
    return(x)
  }) # end reactive
  
  #
  # Update map
  #

  observe({
    leafletProxy("mymap") %>%
      clearGroup(c('forest')) %>%
      addPolygons(
        group = 'forest',
        data = update_forest_dat(),
        color = '#008b8b',
        layerId=~unit_ID,
        weight = 0.4)
    #note popups block shape_click events
  }) # end observe
  
  observe({
    if(input$country != 'Global'){
    bounds <- unname(st_bbox(filter(units2, SOVEREIGN1 == input$country)))
    leafletProxy("mymap") %>%
      flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    }else{
      bounds <- unname(st_bbox(units2))
      leafletProxy("mymap") %>%
        flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
    }
    #note popups block shape_click events
  }) # end observe
  
  # observe shape-click event
  
  observeEvent(input$mymap_shape_click, {
    rv(input$mymap_shape_click$id)
    print(rv)
  }) # end observeEvent
  
  # new plot(s) based on shape-click
  
  output$myDf_output <- renderTable({
    if(!is.null(rv())){
      d <- st_drop_geometry(units2) %>% filter(unit_ID == rv())
      data.frame(`Mangrove area (ha)` = d$mangrove_2016_area_ha,
                 `Seagrass area (ha)` = d$seagrass_area_ha,
                 `Saltmarsh area (ha)` = d$saltmarsh_area_ha,
                 `Kelp area (ha)` = d$kelp_area_ha, check.names = F)
    }else{
      NULL
    }
  },spacing = c("xs"),
  width = "500px") # end render
  
  output$myDf_output2 <- renderTable({
    if(!is.null(rv())){
      d <- st_drop_geometry(units2) %>% filter(unit_ID == rv())
      data.frame(`% mangrove protected` = d$mang_prot,
                 `% seagrass protected` = d$seag_prot,
                 `% saltmarsh protected` = d$salt_prot,
                 `% kelp protected` = d$kelp_prot, check.names = F)
    }else{
      NULL
    }
  },spacing = c("xs"),
  width = "500px") # end render
  # ------------ 
  # Single forest pages
  # ------------ 
  forestServer("mangroves", "mangrove", criteria_mang_kelp_s)
  forestServer("seagrass", "seagrass", criteria_others_s)
  forestServer("saltmarsh", "saltmarsh", criteria_others_s)
  forestServer("kelp", "kelp", criteria_mang_kelp_s)
  
} #end server