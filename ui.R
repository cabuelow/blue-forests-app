# user interface

library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(leaflet)

# navigation panel

navbarPage(
  title = 'Blue Forests',
  tabPanel('Explore Blue Forest Distributions',
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")
               ),
               
               leafletOutput("map", width="100%", height="100%"),
               
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
                             
                             selectInput("radio1", 
                                         #label=NULL,
                                         label=h4(tags$b("1. Select blue forest:")), 
                                         choices = list("None" = 0, "Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4, 'All' = 5, 'Number of forests' = 6),
                                         selected=FALSE),
                             
                             tags$br(),
                             tags$br(),
                             
                             #selectInput("var1", label = h4(tags$b("2. Choose country or territory"), 
                             #           choices = terr, 
                             #          selected = 'Global')),
               ), #end absolute panel
           ), #end div
           
           tags$div(id="cite",
                    tags$em('This map was created in support of the Blue Forests Initiative, a project supported by WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
) # end nav bar