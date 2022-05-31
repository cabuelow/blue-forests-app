# code adapted from: https://github.com/molly-williams/deltaSLR_map

# user interface

library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(leaflet)

# load and wrangle all required components for app

source("wrangling.R")
source("forest-modules.R")
source("parametersUI.R")
  
# navigation panel

navbarPage(
  title = div("Blue Forests",
              img(src = "g-logo.png", height = "35px", style = "position: relative; top: -3px; right: -1525px;")), id = 'nav',
  
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
               ), #end absolute panel 1
               
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
                             
                             tableOutput('myDf_output'),
                             
                             tags$br(),
                             
                             #tags$b("Percent of blue forests protected"),
                             
                             tableOutput('myDf_output2')
                             
               ) # end absolute panel 2
           ), #end div
           
           tags$div(id="cite",
                    tags$em('This map was created in support of the Blue Forests Initiative, a project supported by WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Mangroves',
           forestUI("mangroves", criteria_choices = criteria_mang_kelp),
           tags$div(id="cite",
                    tags$em('This map was created in support of the
                            Blue Forests Initiative, a project supported by 
                            WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Seagrass',
           forestUI("seagrass", criteria_choices = criteria_others),
           tags$div(id="cite",
                    tags$em('This map was created in support of the
                            Blue Forests Initiative, a project supported by 
                            WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Saltmarsh',
           forestUI("saltmarsh", criteria_choices = criteria_others),
           tags$div(id="cite",
                    tags$em('This map was created in support of the
                            Blue Forests Initiative, a project supported by 
                            WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Kelp',
           forestUI("kelp", criteria_choices = criteria_mang_kelp),
           tags$div(id="cite",
                    tags$em('This map was created in support of the
                            Blue Forests Initiative, a project supported by 
                            WWF and the Global Wetlands Project'))
           
  ) # end tabpanel
  
) # end nav bar
