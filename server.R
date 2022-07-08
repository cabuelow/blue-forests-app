# code adapted from: https://github.com/molly-williams/deltaSLR_map

# server

library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(leaflet.extras2)

# server logic

function(input, output, session) {

  observeEvent(input$help,
               introjs(session, 
                       options = list(steps = helptext,
                       "nextLabel"="Next",
                       "prevLabel"="Back")))
  
    # create basemap 
    
    output$mangrove_map <- renderLeaflet({
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
          data = units2 %>% filter(mangrove == 1),
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
                         radius = 4,
                         popup= my_popups2,
                         options = pathOptions(pane = "layer7")
        ) %>%
        addLegend("bottomright",
                  colors = c(hot_pal[c(1,2,3,4,5,6)], 'darkblue'), labels = c(hotdf[c(1,2,3,4,5,6),3], 'All selected criteria'),
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
    
    outputOptions(output, "mangrove_map", suspendWhenHidden = FALSE, priority = 2)
  
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
      scores <- newdat$scoredat[newdat$scoredat[,'mangrove']==1,]
      tmp <- list()
      for(i in as.numeric(input$criteria)){
        varname <- paste0(substr('mangrove', 1,4),"_", hotdf[i,2])
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
      
      leafletProxy("mangrove_map") %>%
        clearGroup(c('profile', 'baseforest', 'extent', 'threat', 'biodiversity', 'carbon', 'pop_vulnerability', 'cobenefit', 'multi')) %>% 
        addSpinner() %>%
        startSpinner(options = list("lines" = 7, "length" = 20)) %>%
        addPolygons(
          group = 'profile',
          data = profile1.sf,
          color = newdat$ppal,
          weight = 0.4) %>% 
        stopSpinner()
      
      for(i in as.numeric(input$criteria)){
        leafletProxy("mangrove_map") %>%
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
        leafletProxy("mangrove_map") %>% 
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
        leafletProxy("mangrove_map") %>%
          flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      }else if(input$country == 'New Zealand'){
        leafletProxy("mangrove_map") %>%
          flyToBounds(177, -55, 179, -33)
      }else if(input$country == 'Fiji'){
        leafletProxy("mangrove_map") %>%
          flyToBounds(178, -15, 179.2, -19)
      }else if(input$country == 'Alaska'){
        leafletProxy("mangrove_map") %>%
          flyToBounds(-179, 50.5, -178, 72.8)
      }else{
        bounds <- unname(st_bbox(units2))
        leafletProxy("mangrove_map") %>%
          flyToBounds(bounds[1], bounds[2], bounds[3], bounds[4])
      }
      #note popups block shape_click events
    }) # end observe
    
    # use reactive values to store the id from observing the shape click (below)
    rvf <- reactiveVal()
    
    # observe shape-click event
    
    observeEvent(input$mangrove_map_shape_click, {
      rvf(input$mangrove_map_shape_click$id)
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
            d <- newdat$indscoredat %>% filter(unit_ID == rvf() & forest_name == 'mangrove')
            ggplot() +
              geom_violin(data = filter(newdat$indscoredat, forest_name == 'mangrove'), aes(y = indicator_score, x = indicator_name, fill = fill), size = 0, scale = 'width', alpha = 0.5, trim = T) +
              geom_point(data = d, aes(y = indicator_score, x = indicator_name)) +
              xlab('') +
              ylab('Score') +
              ylim(c(0,100)) +
              theme_classic() +
              ggtitle(paste(unique(d$forest_name2), 'Indicator scores')) +
              scale_fill_manual(
                breaks = c('Extent', 'Threat',  'Carbon', 'Biodiversity','Coastal community', 'Coastal protection'),
                values = c('darkolivegreen4', 'orangered4', 'plum4', 'goldenrod3', 'cyan4', '#003333')) +
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
        d <- datqual %>% filter(unit_ID == rvf() & forest == 'mangrove')
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

  #forestServer("mangroves", "mangrove", criteria_mang_kelp_s)
  forestServer("seagrass", "seagrass", criteria_others_s, tab_seag_1_s, tab_2_s, tab_3_s, tab_4_s, tab_5_s, tab_6_s, tab_7_s)
  forestServer("saltmarsh", "saltmarsh", criteria_others_s, tab_salt_1_s,  tab_2_s, tab_3_s, tab_4_s, tab_5_s, tab_6_s, tab_7_s)
  forestServer("kelp", "kelp", criteria_kelp_s, tab_kelp_1_s,  tab_2_s, tab_3_s, tab_4_s, tab_5_s, tab_6_s, tab_7_s)
  
} #end server