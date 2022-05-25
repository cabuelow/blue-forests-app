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
           textOutput("selected_var"),
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")
               ),
               
               leafletOutput("mymap", width="100%", height="100%"),
               
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
                             
                             checkboxGroupInput("blue_forest", 
                                         #label=NULL,
                                         label=h4(tags$b("1. Select blue forest:")), 
                                         choices = list("Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4),
                                         selected = NULL),
                             
                             tags$br(),
                             
                             selectInput("country", label = h4(tags$b("2. Choose country or territory")), 
                                        choices =  terr, 
                                       selected = 'Global')
               ), #end absolute panel
           ), #end div
           
           tags$div(id="cite",
                    tags$em('This map was created in support of the Blue Forests Initiative, a project supported by WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Mangroves',
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")
               ),
               
               leafletOutput("mangrove_map", width="100%", height="100%"),
               
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
                             
                             radioButtons("bfcheck", 
                                         #label=NULL,
                                         label=h4(tags$b("1. Select criteria:")), 
                                         choices = list("Extent" = 'extent', "Threat" = 'threat', "Carbon" = 'carbon', "Co-benefit" = 'cobenefit', "Biodiversity" = 'biodiversity', 'All' = 'all'),
                                         selected = 'extent'),
                             
                             tags$br(),
                             
                             sliderInput("perc", label = h4(tags$b("2. Find management units in top percent of criteria")), 
                                         min = 0, max = 100, value = 100),
                             
                             tags$br(),
                             
                             h4(tags$b("3. Turn on enabling constraint layer")),
                             
                             checkboxInput("profile2", label = NULL,
                                           value = FALSE),
                             
                             tags$br(),
                             
                             h4(tags$b("4. Show WWF Blue Forest projects")),
                             
                             checkboxInput("bfproj2", label = NULL,
                                           value = FALSE)
                             
               ), #end absolute panel
           ), #end div
           
           tags$div(id="cite",
                    tags$em('This map was created in support of the
                            Blue Forests Initiative, a project supported by 
                            WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Dashboard',
           div(class="outer",
               tags$head(
                 includeCSS("styles.css")
               ),
          tableOutput("unit_ID_dashboard")
              
           ), #end div
           
           tags$div(id="cite",
                    tags$em('This dashboard was created in support of the Blue Forests Initiative, a project supported by WWF and the Global Wetlands Project'))
           
  ) # end tabpanel
  
) # end nav bar