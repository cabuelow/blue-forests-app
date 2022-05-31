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
  }) # end render leaflet
  
  #
  # Filter data for ticked blue forests 
  #
  update_forest_dat <- reactive({
    x <- filter(df, eco %in% as.numeric(input$blue_forest) &
                  value ==1) 
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
        layerId=~unit_ID,
        weight = 0.4)
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
  }) # end render
  
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
  }) # end render
  # ------------ 
  # Single forest pages
  # ------------ 
  forestServer("mangroves", "mangrove")
  forestServer("seagrass", "seagrass")
  forestServer("saltmarsh", "saltmarsh")
  forestServer("kelp", "kelp")
  
} #end server