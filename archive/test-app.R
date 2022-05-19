library(shiny)
library(dplyr)
library(sf)
library(leaflet)



moduleServer <- function(id, module) {
  callModule(module, id)
}

# Main module - UI 1 #
mod_btn_UI1 <- function(id) {
  
  ns <- NS(id)
  tagList(
    leafletOutput(ns("map")),
    mod_btn_UI2(ns("other"))
  )
}

# Main module - Server 1 #
mod_btn_server1 <- function(id){
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    # here I pass map as reactive
    passMap = reactive({input$map})
    
    coords <- quakes %>%
      sf::st_as_sf(coords = c("long","lat"), crs = 4326)
    
    
    output$map <- leaflet::renderLeaflet({
      leaflet::leaflet() %>% 
        leaflet::addTiles() %>% 
        leaflet::setView(172.972965,-35.377261, zoom = 4) %>%
        leaflet::addCircleMarkers(
          data = coords,
          stroke = FALSE,
          radius = 6)
    })
    proxymap <- reactive(leafletProxy('map'))  
    
    mod_btn_server2("other", proxymap)  
    
    
  })
}


# Other module - UI 2 #
mod_btn_UI2 <- function(id) {
  
  ns <- NS(id)
  tagList(
    actionButton(inputId = ns("btn"), label = "show points")
  )
}


# Other module - Server 2 #
mod_btn_server2 <- function(id, passMap){
  moduleServer(id, function(input, output, session) {
    
    ns <- NS(id)
    
    coords <- quakes %>%
      sf::st_as_sf(coords = c("long","lat"), crs = 4326)
    
    observeEvent(input$btn, {
      passMap() %>%
        leaflet::addCircleMarkers(
          data = coords,
          stroke = TRUE,
          color = "red",
          radius = 6)
      
    })
    
  })
}



# Final app #

ui <- fluidPage(
  
  tagList(
    mod_btn_UI1("test-btn"))
  
)

server <- function(input, output, session) {
  
  mod_btn_server1("test-btn")
  
}

shinyApp(ui = ui, server = server)