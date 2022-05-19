mod_extent_server <- function(id, forest, cterr, proj){
  
  moduleServer(id, function(input, output, session) {

    # create basemap
    
    output$map <- renderLeaflet({
      leaflet() %>% 
        addProviderTiles(providers$CartoDB.Positron)})
    
    # reactive values
    forest <- reactive(input$bf) # forest type
    cterr <- reactive(input$ct) # country/territory
    proj <- reactive(input$bfproj) # wwf blue forest projects
    
  # update polygons based on blue forest and country/territory selection
  
  observe({

    if(cterr == 'Global'){ # complex if-else series that filters for polygons to map depending on if global & BF type
      
    if(forest == 0){
      filtdata <- unitsall
      zz <- unname(st_bbox(filtdata))
      cpal <-"#FFFFFF"
    }else if(forest == 1){
      sub <- df %>% 
        filter(eco == 1) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }else if(forest == 2){
      sub <- df %>% 
        filter(eco == 2) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }else if(forest == 3){
      sub <- df %>% 
        filter(eco == 3) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }else if(forest == 4){
      sub <- df %>% 
        filter(eco == 4) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }else if(forest == 5){
      sub <- df %>% 
        filter(eco %in% c(1,2,3,4)) %>% 
        filter(value == 1)
      filtdata <- units2 %>% 
        filter(unit_ID %in% unique(sub$unit_ID))
      zz <- unname(st_bbox(filtdata))
      cpal <- '#20b2aa'
    }}else{
      if(forest == 0){
        filtdata <- unitsall %>% 
          filter(TERRITORY1 == input$ct)
        zz <- unname(st_bbox(filtdata))
        cpal <-"#FFFFFF"
      }else if(forest == 1){
        sub <- df %>% 
          filter(eco == 1) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$ct)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }else if(forest == 2){
        sub <- df %>% 
          filter(eco == 2) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$ct)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }else if(forest == 3){
        sub <- df %>% 
          filter(eco == 3) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$ct)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }else if(forest == 4){
        sub <- df %>% 
          filter(eco == 4) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$ct)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }else if(forest == 5){
        sub <- df %>% 
          filter(eco %in% c(1,2,3,4)) %>% 
          filter(value == 1)
        filtdata <- units2 %>% 
          filter(unit_ID %in% unique(sub$unit_ID)) %>% 
          filter(TERRITORY1 == input$ct)
        zz <- unname(st_bbox(filtdata))
        cpal <- '#20b2aa'
      }
      }
    
    # create reactive map
    
    if(proj == FALSE){ # if-else to define whether wwf projects are on or off
    leafletProxy("map") %>%
      clearControls() %>% 
      clearShapes() %>% 
      clearMarkers() %>% 
      flyToBounds(zz[1], zz[2], zz[3], zz[4]) %>% 
      addPolygons(
        data = unitsall,
        color = "#FFFFFF",
        weight = 0.4,
        popup = T) %>% 
      addPolygons(data = filtdata,
                  color = cpal,
                  weight = 0.4,
                  popup = T)
    }else{
      leafletProxy("map") %>%
        clearControls() %>% 
        clearShapes() %>% 
        flyToBounds(zz[1], zz[2], zz[3], zz[4]) %>% 
        addPolygons(
        data = unitsall,
        color = "#FFFFFF",
        weight = 0.4,
        popup = T) %>% 
        addPolygons(data = filtdata,
                    color = cpal,
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
                  opacity = 1
        )
      }
  }) # end observe 
  
} #end server function
) # end moduleServer
} # end function