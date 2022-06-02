#modules for the single forest page 

#TODO: 
# fadd data qual descripition to pop up panel
# fix 'all' criteria, if we want to have that, not working currenlty

forestUI <- function(id, criteria_choices) {
  #criteria_choices: Named list of criteria for this forest
  # mangrove and kelp have different ones 
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
                    
                    tags$br(),
                    
                    radioButtons(ns("criteria"), 
                                 #label=NULL,
                                 label=h5(tags$b("1. Select criteria:")), 
                                 choices = criteria_choices,
                                 selected = 'extent'),
                    
                    tags$br(),
                    
                    sliderInput(ns("perc"), label = h5(tags$b("3. Find management units in top percent of criteria")), 
                                min = 0, max = 100, 
                                value = 10,
                                step = 5),
                    
                    tags$br(),
                    
                    h5(tags$b("3. Turn on enabling constraint layer")),
                    checkboxInput(ns("profile2"), label = NULL, value = FALSE)
                    
      ), # end absolute panel 1
      absolutePanel(id = "controls", 
                    class = "panel panel-default", 
                    fixed = TRUE,
                    draggable = TRUE, 
                    top = "auto", 
                    left = 30, 
                    right = "auto", 
                    bottom = 30,
                    width = 600, 
                    height = "auto",
                    
                    #tags$b("Blue forest area"),
                    tags$em("Click on a coastal management unit to find out more..."),
                    
                    tags$br(),
                    
                    tableOutput(ns('myDf_outputf')),
                    
                    tags$br(),
                    
                    #tags$b("Percent of blue forests protected"),
                    
                    tableOutput(ns('myDf_outputf2')),
                    
                    tags$br(),
                    
                    #tags$b("Percent of blue forests protected"),
                    
                    plotOutput(ns('indplot'), height = '200px', width = '500px')
                    
      ) # end absolute panel 2
  ) # end div
} # end UI


forestServer <- function(id, forest_type) {
  #forest_type is the name of the columns in 
  # the scores dataframe for each forest type
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      # create basemap 
      
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
            layerId = ~unit_ID,
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
      
      # reactive if-elses to choose the right data depending on whether enabling constraint is on or off

      dat <- reactive({
        if(input$profile2 == TRUE){
         unitdat <- units2.p 
         scoredat <- scores2
         indscoredat <- indscores.p2
         ppal <- "#FFCC99"
        }else{
         unitdat <- units2
         scoredat <- scores
         indscoredat <- indscores.p
         ppal <- '#D3D3D3'
        }
        combo <- list(unitdat = unitdat, scoredat = scoredat, indscoredat = indscoredat, ppal = ppal)
      })
      
      # update basemap based on enabling constraint
      
      observe({
      newdat <- dat() # get reactive data

        leafletProxy(ns("forest_map")) %>%
          clearGroup(c('profile', 'mangroves', 'top_forest')) %>%
          addPolygons(
            group = 'profile',
            data = profile1.sf,
            color = newdat$ppal,
            weight = 0.4) %>% 
          addPolygons(
            group = "mangroves",
            data = newdat$unitdat %>% filter_at(vars(forest_type), all_vars(. == 1)),
            layerId = ~unit_ID,
            weight = 0.4)
      }) # end observe
      
      # reactive to capture changes in top sites
      
      update_top_sites_dat <- reactive({
        newdat <- dat()
        scores <- newdat$scoredat[newdat$scoredat[,forest_type]==1,]
        varname <- paste0(substr(forest_type, 1,4),"_", input$criteria)
        q <- quantile(scores[,varname], probs = 1-(input$perc)/100,names = FALSE)
        x <- scores$unit_ID[scores[,varname] > q] 
        x <- newdat$unitdat[newdat$unitdat$unit_ID %in% x, ]
        return(x)
      }) # end reactive
      
      # update map top sites
      
      observe({
        #can add and remove by layer ID, so should be able to speed this
        # up by just changing polygons that need to be changed. 
        
        leafletProxy(ns("forest_map")) %>%
          clearGroup(c('top_forest')) %>%
          addPolygons(
            group = 'top_forest',
            color = "red",
            data = update_top_sites_dat(),
            layerId=~unit_ID,
            weight = 0.4) 
      }) # end observe
      
      # use reactive values to store the id from observing the shape click (below)
      rvf <- reactiveVal()
      
      # observe shape-click event
      
      observeEvent(input$forest_map_shape_click, {
        rvf(input$forest_map_shape_click$id)
        print(rvf)
      }) # end observeEvent
      
      # new plot(s) based on shape-click
      
      output$myDf_outputf <- renderTable({
        if(!is.null(rvf())){
          d <- st_drop_geometry(units2) %>% filter(unit_ID == rvf())
          data.frame(`Mangrove area (ha)` = d$mangrove_2016_area_ha,
                     `Seagrass area (ha)` = d$seagrass_area_ha,
                     `Saltmarsh area (ha)` = d$saltmarsh_area_ha,
                     `Kelp area (ha)` = d$kelp_area_ha, check.names = F)
        }else{
          NULL
        }
      }, spacing = c("xs"),
      width = "500px") # end render
      
      output$myDf_outputf2 <- renderTable({
        if(!is.null(rvf())){
          d <- st_drop_geometry(units2) %>% filter(unit_ID == rvf())
          data.frame(`% mangrove protected` = d$mang_prot,
                     `% seagrass protected` = d$seag_prot,
                     `% saltmarsh protected` = d$salt_prot,
                     `% kelp protected` = d$kelp_prot, check.names = F)
        }else{
          NULL
        }
      }, spacing = c("xs"),
      width = "500px") # end render
      
      observe({
      newdat <- dat()
      output$indplot <- renderPlot({
        if(!is.null(rvf())){
          d <- newdat$indscoredat %>% filter(unit_ID == rvf() & forest_name == forest_type)
          ggplot() +
            geom_violin(data = filter(newdat$indscoredat, forest_name == forest_type), aes(y = indicator_score, x = indicator_name, fill = fill), alpha = 0.5, trim = F) +
            geom_point(data = d, aes(y = indicator_score, x = indicator_name)) +
            xlab('') +
            ylab('Score') +
            ylim(c(0,100)) +
            theme_classic() +
            ggtitle(paste(unique(d$forest_name2), 'Indicator scores')) +
            scale_fill_manual(
              breaks = c('Threat', 'Extent', 'Biodiversity', 'Carbon', 'Cobenefit'),
              values = c('orangered4', 'darkolivegreen4','goldenrod3','plum4', 'cyan4')) +
            theme(
              legend.title = element_blank())
        }else{
          NULL
        }
      }) # end render
      }) # end observe
    }
  )
}
