mod_extent_ui <- function(id){
  
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
              
              selectInput(NS(id, "bf"), 
                          #label=NULL,
                          label=h4(tags$b("1. Select blue forest:")), 
                          choices = list("None" = 0, "Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4, 'All' = 5),
                          selected = FALSE),
              
              tags$br(),
              
              selectInput(NS(id, "ct"), label = h4(tags$b("2. Choose country or territory")), 
                          choices =  terr, 
                          selected = 'Global'),
              
              tags$br(),
              
              h4(tags$b("3. Show WWF Blue Forest projects")),
              
              checkboxInput(NS(id, "bfproj"), label = NULL,
                            value = FALSE)
              
) #end absolute panel
} #end function