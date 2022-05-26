#modules for the single forest page 

#TODO: 
# fix 'all' criteria, not working currenlty
# what is the 'cobenefit' criteria? 
#CAB: only mangroves and kelp have cobenefit criteria, so have added secondi UI module for them
#CAB: mangrove cobenefit if coastal protection and fisheries, for kelp just fisheries
#Change this so there is a selector for forest, rather than a tab
# tabs makes it plot all of them on launch (waste of compute)
#ALso having issues with the modules...

# CAB updates:
# fixed so plotted correct forest extent in each tab
# added kelp
# added cobenefit criteria

forestUI <- function(id) {
  ns <- NS(id)
  div(class="outer",
      tags$head(
        includeCSS("styles.css")
      ),
      leafletOutput(ns("forest_map"), width="100%", height="100%"),
      tags$style(".leaflet-control-layers-overlays{color: blue}"),
      absolutePanel(id = "controls", 
                    class = "panel panel-default", 
                    fixed = TRUE,
                    draggable = TRUE, 
                    top = 100, 
                    left = 30, 
                    right = "auto", 
                    bottom = "auto",
                    width = 330, 
                    height = "auto",
                    
                    tags$br(),
                    
                    tags$em("Allow a moment for layers to load."),
                    
                    radioButtons(ns("criteria"), 
                                 #label=NULL,
                                 label=h4(tags$b("1. Select criteria:")), 
                                 choices = list("Extent" = 'extent', "Threat" = 'threat', 
                                                "Carbon" = 'carbon', "Biodiversity" = 'biodiversity'),
                                 selected = 'extent'),
                    
                    tags$br(),
                    
                    sliderInput(ns("perc"), label = h4(tags$b("2. Find management units in top percent of criteria")), 
                                min = 0, max = 100, 
                                value = 10,
                                step = 5),
                    
                    tags$br()
      )
  )
}

forestUI2 <- function(id) {
  ns <- NS(id)
  div(class="outer",
      tags$head(
        includeCSS("styles.css")
      ),
      leafletOutput(ns("forest_map"), width="100%", height="100%"),
      tags$style(".leaflet-control-layers-overlays{color: blue}"),
      absolutePanel(id = "controls", 
                    class = "panel panel-default", 
                    fixed = TRUE,
                    draggable = TRUE, 
                    top = 100, 
                    left = 30, 
                    right = "auto", 
                    bottom = "auto",
                    width = 330, 
                    height = "auto",
                    
                    tags$br(),
                    
                    tags$em("Allow a moment for layers to load."),
                    
                    radioButtons(ns("criteria"), 
                                 #label=NULL,
                                 label=h4(tags$b("1. Select criteria:")), 
                                 choices = list("Extent" = 'extent', "Threat" = 'threat', 
                                                "Carbon" = 'carbon', "Biodiversity" = 'biodiversity', 'Cobenefit' = 'cobenefit'),
                                 selected = 'extent'),
                    
                    tags$br(),
                    
                    sliderInput(ns("perc"), label = h4(tags$b("2. Find management units in top percent of criteria")), 
                                min = 0, max = 100, 
                                value = 10,
                                step = 5),
                    
                    tags$br()
      )
  )
}

forestServer <- function(id, forest_type) {
  #forest_type is the name of the columns in 
  # the scores dataframe for each forest type
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      output$forest_map <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addPolygons(
            group = 'basepoly',
            data = unitsall,
            color = "#FFFFFF",
            weight = 0.4) %>%
          addPolygons(
            group = "mangroves",
            data = units2 %>% filter_at(vars(forest_type), all_vars(. == 1)),
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
        # browser()
        scores <- scores[scores[,forest_type]==1,]
        varname <- paste0(substr(forest_type, 1,4),"_", input$criteria)
        q <- quantile(scores[,varname], probs = 1-(input$perc)/100,
                      names = FALSE)
        x <- scores$unit_ID[scores[,varname] > q] 
        x <- units2[units2$unit_ID %in% x, ]
        return(x)
      })
      
      observe({
        #can add and remove by layer ID, so should be able to speed this
        # up by just changing polygons that need to be changed. 
        
        leafletProxy(ns("forest_map")) %>%
          # # clearControls() %>%
          clearGroup(c('top_forest')) %>%
          addPolygons(
            group = 'top_forest',
            color = "red",
            data = update_top_sites_dat(),
            layerId=~unit_ID,
            weight = 0.4) 
        
      })    
    }
  )
}
