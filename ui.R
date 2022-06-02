# code adapted from: https://github.com/molly-williams/deltaSLR_map

# user interface

library(shiny)
library(shinyjs)
library(shinydashboard)
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
  
  tabPanel('Instructions',
           tags$head(
             includeCSS("styles.css")
           ),
           
           box(title="Welcome to the Blue Forests Web App",
               status="primary",
               solidHeader=TRUE,
               width = 12,

               tags$div("This tool allows end-users to identify places across the globe that have high conservation potential.")),
           
           box(title = 'Intended use',
               width = 12,
               collapsible = TRUE,
               collapsed = TRUE,
              
               tags$div('To provide an indication of the opportunities for blue forest conversation globally, and demonstrate how data can inform strategic decisions (blue forests = mangroves, seagrass, saltmarsh and kelp). Identifying priority locations for project development will require specific objectives to be set, and validation with local data.')),
           
           box(title = 'Data certainty and confidence',
               width = 12,
               collapsible = TRUE,
               collapsed = TRUE,
               
               tags$div('With the exception of mangroves, the extent of blue forests are not mapped globally at a high resolution. Therefore, there is large uncertainty in estimates of extent for seagrass, saltmarsh and kelp. 
'),
               tags$br(),
               
               tags$div('Indicators for criteria (described below) that were not globally comprehensive or were spatially incongruent with baseline distributions were gap-filled with regional or global averages, introducing additional uncertainty. 
')),
           
           box(title = 'Coastal and offshore management units',
               width = 12,
               collapsible = TRUE,
               collapsed = TRUE,
               
               tags$div('Coastal management units are based on watershed boundaries and their associated coastal zone (up to 50km offshore), and are nested hierarchically from coarse to fine-scale. Global blue forest opportunities were identified at a coarse resolution, and variability in criteria and indicators (listed below) were mapped in fine-scale units. Offshore management units delineate a boundary (50km radius) for blue forests that occur outside of watershed and coastal zone boundaries.
')),
           
           box(title = 'Criteria and indicators to identify Blue Forest opportunities',
               width = 12,
               collapsible = TRUE,
               collapsed = TRUE,
               
              tags$div('1. Extent: Total area of mangroves, seagrass, saltmarsh and kelp (standardised by management unit size).
'),
              tags$div('2. Threat: Composed of several indicators including rates of loss, cyclone risk, and cumulative climate, land and marine-based impacts to each forest.
'),
              tags$div('3. Carbon: Average carbon storage (mangroves, seagrass, saltmarsh) or sequestration (kelp).
'),
              tags$div('4. Biodiversity: Richness of species affiliated with each forest type. 
'),
              tags$div('5. Co-benefit: Fisheries (mangrove commercial fisheries enhancement or average kelp fisheries biomass) and coastal protection (average number of people protected by mangroves per 20km of coastline).
')),
          
           box(title = 'Opportunities constrained by enabling conditions',
               width = 12,
               collapsible = TRUE,
               collapsed = TRUE,
               
               tags$div('To identify locations where NGOs and other organisations can have the most impact, countries and their territories that have high socio-economic and political capacity to enable conservation were excluded.
'))
               
  ), # end instructions tabpanel
                 
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
                                                label=h5(tags$b("1. Select blue forest:")), 
                                                choices = list("Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4),
                                                selected = NULL),
                             
                             #tags$br(),
                             
                             #selectInput("country", label = h5(tags$b("2. Choose country or territory")), 
                              #           choices =  terr, 
                               #          selected = 'Global')
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
                            
                             #tags$b("Blue forest area"),
                             tags$em("Click on a coastal management unit to find out more..."),
                             
                             tags$br(),
                             
                             tableOutput('myDf_output'),
                             
                             #tags$br(),
                             
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
