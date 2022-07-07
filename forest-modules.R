#modules for the single forest page 

forestUI <- function(id, criteria_choices, help_tab) {
  #criteria_choices: Named list of criteria for this forest
  # mangrove and kelp have different ones 
  ns <- NS(id)
  div(class="outer",
      tags$head(
        includeCSS("styles.css")
      ),
      leafletOutput(ns("forest_map"), width="100%", height="100%"),
      tags$style(".leaflet-control-layers-overlays{color: black}"),
      absolutePanel(id = "controls", 
                    class = "panel panel-default", 
                    fixed = TRUE,
                    draggable = F, 
                    top = 100, 
                    left = 'auto', 
                    right = 200, 
                    bottom = 'auto',
                    width = 0.1, 
                    height = "auto",
                    div(id = ns('projects'))
      ),
      absolutePanel(id = "controls", 
                    class = "panel panel-default", 
                    fixed = TRUE,
                    draggable = TRUE, 
                    top = 625, 
                    left = 30, 
                    right = 'auto', 
                    bottom = 'auto',
                    width = 575, 
                    height = "auto",
                    
                    tags$style(HTML(".table>thead>tr>th {
                             border-top: 0;
                             font-size: 11px;
                             font-weight: bold;
                             font-family: 'Helvetica Neue', Helvetica;
                             padding: 8px
                             }
                            .table>tbody>tr>td {
                             border-top: 0;
                             font-size: 11px;
                             font-weight: 200;
                             font-family: 'Helvetica Neue', Helvetica;
                             }")),
                    
                    div(id = ns('dashboard'),
                    #actionButton(ns("help3"), "Dashboard", icon = icon("question")),
                    #tags$b("Blue forest area"),
                    tags$em("Coastal management unit dashboard"),
                    
                    tags$br(),
                    
                    tableOutput(ns('myDf_outputf')),
                    
                    #tags$br(),
                    
                    #tags$b("Percent of blue forests protected"),
                    
                    tableOutput(ns('myDf_outputf2')),
                    
                    #tags$br(),
                    
                    #tags$b("Percent of blue forests protected"),
                    
                    plotOutput(ns('indplot'), height = '200px', width = '500px'),
                    
                    div(id = ns('natcont'),
                    h5(tags$b("Show national context indicators:")),
                    checkboxInput(ns("natcon"), label = NULL, value = FALSE)
                    ),
                    
                    textOutput(ns('text'))
                    )
      ), # end absolute panel 2
      absolutePanel(id = "controls", 
                    class = "panel panel-default", 
                    fixed = TRUE,
                    draggable = TRUE, 
                    top = 60, 
                    left = 30, 
                    right = "auto", 
                    bottom = "auto",
                    width = 300, 
                    height = "auto",
                    
                   # tags$br(),
      
                    tags$em("Allow a moment for layers to load."),
                    
                    #tags$br(),
                    actionButton(ns("help"), "Tour page", icon = icon("star")),
                    #tags$em("Select from steps 1-3. Then click 'Map management units'."),
              
                    checkboxGroupInput(ns("criteria"), 
                                 label=h5(tags$b("1. Select criteria to map:")), 
                                 choices = criteria_choices,
                                 selected = 1,
                                 inline = TRUE),
                   #actionButton(ns("help"), "Criteria", icon = icon("question")),
                   # tags$br(),
                    
                   div(id = ns('myslider'), h5(tags$b("2. Set threshold to find management units in top percent of selected criteria:"))),
                   
                    sliderInput(ns("perc"), label = h5(tags$b("")), 
                                min = 0, max = 100, 
                                value = 100,
                                step = 5),
                    #tags$br(),
                    
                   div(id = ns('myenabling'),
                    h5(tags$b("3. Turn on enabling condition constraint layer?"))),
                   
                   div(id = ns('mymapit2'),
                    checkboxInput(ns("profile2"), label = NULL, value = FALSE)
                    ),
                   
                    actionButton(ns('mapit2'), 'Map management units'),
                    #tags$br(),
                    
                   div(id = ns('mycountry'), h5(tags$b("4. Choose country or territory:"))),
                   
                    selectInput(ns("country"), label = h5(tags$b("")), 
                                choices =  terr, 
                                selected = 'Global')
      ) # end absolute panel 1
  ) # end div
} # end UI


forestServer <- function(id, forest_type, criteria_choices, help_tab) {
  #forest_type is the name of the columns in 
  # the scores dataframe for each forest type
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      
      intro <- reactive({
        data.frame(
          step = c(1,2,3,4,5,6,7),
          element = c(paste0("#", session$ns("criteria")), 
                      paste0("#", session$ns("myslider")), 
                      paste0("#", session$ns("myenabling")), 
                      paste0("#", session$ns("mymapit2")), 
                      paste0("#", session$ns("mycountry")), 
                      paste0("#", session$ns("dashboard")),
                      #paste0("#", session$ns("natcont")),
                      paste0("#", session$ns("projects"))
                      ), 
          intro = c(paste(help_tab),"Set the threshold you want",
                    "Turn on enabling profile","Map management units", 'Choose a country', 
                    'Click on a coastal managment unit to...',
                    'Turn on local projects'), id)
      })
      
      observeEvent(input$help,
                   introjs(session, 
                           options = list(steps = intro(),
                                          "nextLabel"="Next",
                                          "prevLabel"="Back")))
      
      # create basemap 
      
      output$forest_map <- renderLeaflet({
        leaflet() %>% 
          addProviderTiles(providers$CartoDB.Positron) %>% 
          addSpinner() %>%
          startSpinner(options = list("lines" = 7, "length" = 20)) %>%
          addPolygons(
            group = 'basepoly',
            data = unitsall,
            color = "#FFFFFF",
            weight = 0.4) %>%
          addPolygons(
            group = "baseforest",
            data = units2 %>% filter_at(vars(forest_type), all_vars(. == 1)),
            color = hot_pal[1],
            layerId = ~unit_ID,
            weight = 0.4) %>% 
          stopSpinner() %>% 
          addMapPane('criteria', zIndex = 410) %>%
          addMapPane('layer6', zIndex = 460) %>%
          addMapPane('layer7', zIndex = 470) %>%
          addCircleMarkers(group = "WWF Blue Forest projects",
                           data = wwf,
                           color = ~pal(site_type),
                           weight = 1,
                           opacity = 1,
                           fillOpacity = 0.8,
                           radius = 5,
                           popup= my_popups,
                           options = pathOptions(pane = "layer6")
                           ) %>%
          addCircleMarkers(group = "Business model maturity",
                           data = inproj,
                           color = ~pal2(Investment_readiness_stage),
                           weight = 1,
                           fillOpacity = 0.6,
                           radius = 3,
                           popup= my_popups2,
                           options = pathOptions(pane = "layer7")
          ) %>%
          addLegend("bottomright",
                    colors = c(hot_pal[as.numeric(criteria_choices)], 'darkblue'), labels = c(hotdf[criteria_choices,3], 'All selected criteria'),
                    title = 'Criteria',
                    opacity = 1) %>% 
          addLegend("bottomright", data = inproj,
                    pal = pal2, values = ~Investment_readiness_stage,
                    title = "Investment stage",
                    opacity = 1, group = 'Business model maturity') %>% 
          addLegend("bottomright", data = wwf,
                    pal = pal, values = ~site_type,
                    title = "WWF Blue Forest project type",
                    opacity = 1, group = 'WWF Blue Forest projects') %>%
          addLayersControl(
            overlayGroups = c("WWF Blue Forest projects", 'Business model maturity'),
            options = layersControlOptions(collapsed = FALSE)) %>% 
          hideGroup('WWF Blue Forest projects') %>% 
          hideGroup("Business model maturity")
      }) # end render leaflet
      
      outputOptions(output, "forest_map", suspendWhenHidden = FALSE, priority = 1)
      
      
      # reactive if-elses to choose the right data depending on whether enabling constraint is on or off

      dat <- eventReactive(input$mapit2,{
        if(input$profile2 == TRUE){
         unitdat <- units2.p 
         scoredat <- scores2
         indscoredat <- indscores.p2
         natcondat <- enable2
         ppal <- "#FFCC99"
        }else{
         unitdat <- units2
         scoredat <- scores
         indscoredat <- indscores.p
         natcondat <- enable
         ppal <- "#FFFFFF"
        }
        combo <- list(unitdat = unitdat, scoredat = scoredat, indscoredat = indscoredat, natcondat = natcondat, ppal = ppal)
      }) #%>% bindCache(input$profile2)
      
      # reactive to capture changes in top sites
      
      update_top_sites_dat <- eventReactive(input$mapit2, {
        newdat <- dat()
        scores <- newdat$scoredat[newdat$scoredat[,forest_type]==1,]
        tmp <- list()
        for(i in as.numeric(input$criteria)){
        varname <- paste0(substr(forest_type, 1,4),"_", hotdf[i,2])
        q <- quantile(scores[,varname], probs = 1-(input$perc)/100,names = FALSE)
        x <- scores$unit_ID[scores[,varname] >= q] 
        x <- newdat$unitdat[newdat$unitdat$unit_ID %in% x, ]
        tmp[[i]] <- x
        }
        names(tmp) <- seq_along(tmp)
        if(length(which(sapply(tmp,is.null))) != 0){tmp <- tmp[-which(sapply(tmp, is.null))]}
        if(length(input$criteria) > 1){multunits <- Reduce(intersect, lapply(tmp, function(x){x$unit_ID}))}else{multunits <- NULL}
        combo2 <- list(newsc = tmp, multunits = multunits)
      }) #%>% bindCache(dat(), input$criteria, input$perc) # end reactive
      
      # update basemap based on enabling constraint
      
      observeEvent(input$mapit2,{
      newdat <- dat() # get reactive data
      newdat2 <- update_top_sites_dat()
      
      leafletProxy(ns("forest_map")) %>%
        clearGroup(c('profile', 'baseforest', 'extent', 'threat', 'biodiversity', 'carbon','pop_vulnerability', 'cobenefit','multi')) %>% 
        addSpinner() %>%
        startSpinner(options = list("lines" = 7, "length" = 20)) %>%
        addPolygons(
          group = 'profile',
          data = profile1.sf,
          color = newdat$ppal,
          weight = 0.4) %>% 
        stopSpinner()
      
      for(i in as.numeric(input$criteria)){
        leafletProxy(ns("forest_map")) %>%
          addSpinner() %>%
          startSpinner(options = list("lines" = 7, "length" = 20)) %>%
          addPolygons(
            group = paste(hotdf[i,2]),
            color = hot_pal[i],
            data = newdat2$newsc[[paste(i)]],
            layerId=~unit_ID,
            weight = 0.4,
            opacity = 0.5,
            options = pathOptions(pane = 'criteria')) %>% 
          stopSpinner()
      }# end for loop
      
      if(length(newdat2$multunits != 0)){
           leafletProxy(ns("forest_map")) %>% 
          addSpinner() %>%
          startSpinner(options = list("lines" = 7, "length" = 20)) %>%
          addPolygons(
            group = 'multi',
            color = 'darkblue',
            data = filter(units2, unit_ID %in% newdat2$multunits),
            layerId =~unit_ID,
            weight = 0.8,
            options = pathOptions(pane = 'criteria')) %>% 
          stopSpinner()
       }#end if
      
      }) # end observe
      
      observe({
        if(!input$country %in% c('Global', 'New Zealand', 'Fiji', 'Alaska')){
          bounds <- unname(st_bbox(filter(units2, TERRITORY1 == input$country)))
          leafletProxy(ns("forest_map")) %>%
            flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
        }else if(input$country == 'New Zealand'){
          leafletProxy(ns("forest_map")) %>%
            flyToBounds(177, -55, 179, -33)
        }else if(input$country == 'Fiji'){
          leafletProxy(ns("forest_map")) %>%
            flyToBounds(178, -15, 179.2, -19)
        }else if(input$country == 'Alaska'){
          leafletProxy(ns("forest_map")) %>%
            flyToBounds(-179, 50.5, -178, 72.8)
        }else{
          bounds <- unname(st_bbox(units2))
          leafletProxy(ns("forest_map")) %>%
            flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
        }
        #note popups block shape_click events
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
      }, spacing = c("xs"), width = "500px") %>% bindCache(rvf())# end render
      
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
      }, spacing = c("xs"), width = "500px") %>% bindCache(rvf()) # end render
      
      observe({
      newdat <- dat()
      if(input$natcon == FALSE){
      output$indplot <- renderPlot({
        if(!is.null(rvf())){
          d <- newdat$indscoredat %>% filter(unit_ID == rvf() & forest_name == forest_type)
          ggplot() +
            geom_violin(data = filter(newdat$indscoredat, forest_name == forest_type), aes(y = indicator_score, x = indicator_name, fill = fill), size = 0, scale = 'width', alpha = 0.5, trim = T) +
            geom_point(data = d, aes(y = indicator_score, x = indicator_name)) +
            xlab('') +
            ylab('Score') +
            ylim(c(0,100)) +
            theme_classic() +
            ggtitle(paste(unique(d$forest_name2), 'Indicator scores')) +
            scale_fill_manual(
              breaks = c('Extent', 'Threat',  'Carbon', 'Biodiversity','Coastal community', 'Coastal protection'),
              values = c('darkolivegreen4', 'orangered4', 'plum4', 'goldenrod3', 'cyan4', '#336666')) +
            theme(legend.title = element_blank())
        }else{
          NULL
        }
      }) %>% bindCache(rvf(), dat(), input$natcon) # end render
      }else{
        output$indplot <- renderPlot({
          if(!is.null(rvf())){
            d <- newdat$natcondat %>% filter(SOVEREIGN1 %in% unique(filter(units2, unit_ID == rvf())$SOVEREIGN1) &
                                               TERRITORY1 %in% unique(filter(units2, unit_ID == rvf())$TERRITORY1))
            ggplot() +
              geom_violin(data = newdat$natcondat, aes(y = score, x = criteria), fill = 'lightblue', size = 0,
                          alpha = 0.5, na.rm = T) +
              geom_point(data = d, aes(y = score, x = criteria, shape = SOVEREIGN1)) +
              xlab('') +
              ylab('Score') +
              ylim(c(0,100)) +
              theme_classic() +
              ggtitle('National Context (for Territories, Sovereign country scores are used)') +
              theme(legend.title = element_blank())
          }else{
            NULL
          }}) %>% bindCache(rvf(), dat(), input$natcon)  # end render
        } # end natcon if else
        }) # end observe
      
      output$text <- renderText({
        if(!is.null(rvf())){
          d <- datqual %>% filter(unit_ID == rvf() & forest == forest_type)
          if(nrow(d) >= 1){
            tmp <- c()
            for(i in 1:nrow(d)){
              d2 <- d[i,] %>% pivot_longer(cols = `marine realm average`:`global median`, values_to = 'vals', names_to = 'score') %>% filter(!is.na(vals))
              tmp[i] <- paste(unique(d2$forestname), paste(unique(d2$indicator), collapse = ' & '), 'was gap-filled with a', paste(unique(d2$score), collapse = ' & '), 'value.')
            }
            print(tmp)
          }}else{
            NULL
          }
      }) # end render
      
    }
  )
}
