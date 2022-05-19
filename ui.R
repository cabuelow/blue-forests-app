# code adapted from: https://github.com/molly-williams/deltaSLR_map

# user interface

library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(leaflet)

# load and wrangle all required components for app

source("wrangling.R")

# navigation panel

navbarPage(
  title = 'Blue Forests', id = 'nav',
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
                             
                             selectInput("bf", 
                                         #label=NULL,
                                         label=h4(tags$b("1. Select blue forest:")), 
                                         choices = list("None" = 0, "Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4, 'All' = 5),
                                         selected = FALSE),
                             
                             tags$br(),
                             
                             selectInput("ct", label = h4(tags$b("2. Choose country or territory")), 
                                        choices =  terr, 
                                       selected = 'Global'),
                             
                             tags$br(),
                             
                             h4(tags$b("3. Show WWF Blue Forest projects")),
                           
                             checkboxInput("bfproj", label = NULL,
                                          value = FALSE),
                             
                             tags$br(),
                             
                             h4(tags$b("4. Turn on enabling constraint layer")),
                             
                             checkboxInput("profile", label = NULL,
                                           value = FALSE)
                             
               ), #end absolute panel
           ), #end div
           
           tags$div(id="cite",
                    tags$em('This map was created in support of the Blue Forests Initiative, a project supported by WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Blue Forest Hotspots',
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")
               ),
               
               leafletOutput("map1", width="100%", height="100%"),
               
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
                             
                             checkboxGroupInput("bfcheck", 
                                         #label=NULL,
                                         label=h4(tags$b("1. Select blue forest(s):")), 
                                         choices = list("Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4),
                                         selected = 1),
                             
                             tags$br(),
                             
                             sliderInput("perc", label = h4(tags$b("2. Set hotspot threshold (percentile)")), 
                                         min = 0, max = 100, value = 0),
                             
                             tags$br(),
                             
                             h4(tags$b("3. Show WWF Blue Forest projects")),
                             
                             checkboxInput("bfproj2", label = NULL,
                                           value = FALSE),
                             
                             tags$br(),
                             
                             h4(tags$b("4. Turn on enabling constraint layer")),
                             
                             checkboxInput("profile2", label = NULL,
                                           value = FALSE)
                             
               ), #end absolute panel
           ), #end div
           
           tags$div(id="cite",
                    tags$em('This map was created in support of the Blue Forests Initiative, a project supported by WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
) # end nav bar