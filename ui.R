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
source("parametersServer.R")
  
# navigation panel

navbarPage(
  title = div("Blue Forests",
              img(src = "g-logo.png", height = "35px", style = "position: relative; top: -3px; left: 10px;")), id = 'nav',
  
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
                             top = 300, 
                             left = 30, 
                             right = 'auto', 
                             bottom = 'auto',
                             width = 550, 
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
                             
                             #tags$br(),
                             
                             tags$em("Allow a moment for layers to load."),
                             
                             checkboxGroupInput("blue_forest", 
                                                #label=NULL,
                                                label=h5(tags$b("1. Select blue forest:")), 
                                                choices = list("Mangrove" = 1, "Seagrass" = 2, "Saltmarsh" = 3, "Kelp" = 4),
                                                selected = NULL, inline = TRUE),
                             
                             #tags$br(),
                             
                             selectInput("country", label = h5(tags$b("2. Choose country or territory:")), 
                                         choices =  terr, 
                                         selected = 'Global')
               ) #end absolute panel 1
              
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
           
  ), # end tabpanel
  tabPanel('Data sources',
           tags$head(
             includeCSS("styles.css")
           ),
           
           box(title="Data sources",
               status="primary",
               solidHeader=TRUE,
               width = 12,
               
               tags$div("Alongi, D. M. (2018). Blue Carbon: Coastal Sequestration for Climate Change Mitigation. Springer Nat. Available at: https://www.researchgate.net/publication/343486022.
"),tags$br(),
               tags$div("Bunting, P., Rosenqvist, A., Lucas, R. M., Rebelo, L. M., Hilarides, L., Thomas, N., et al. (2018). The global mangrove watch - A new 2010 global baseline of mangrove extent. Remote Sens. 10, 1669. doi:10.3390/rs10101669.
"),tags$br(),
               tags$div("CIESIN (2018). Documentation for the Gridded Population of the World, Version 4 (GPWv4), Revision 11 Data Sets. Palisades NY NASA Socioecon. Data Appl. Cent. Accessed [05 01 2022].
"),tags$br(),
               tags$div("CIESIN (2021). Low Elevation Coastal Zone (LECZ) Urban-Rural Population and Land Area Estimates, Version 3. Palisades, NY: NASA Socioeconomic Data and Applications Center (SEDAC). https://doi.org/10.7927/d1x1-d702. Accessed 12 01 2022.
"),tags$br(),
               tags$div("Eger, A. M., Marzinelli, E. M., Rodrigo, B., Blain, C., Blamey, L., Carnell, P. E., et al. (2021). The economic value of fisheries, blue carbon, and nutrient cycling in global marine forests. EcoEvoRxiv. doi:10.32942/osf.io/n7kjs.
"),tags$br(),
               tags$div("FAO (2020). Fish, seafood – food supply quantity (kg/capita/yr). https://www.fao.org/faostat/en/#data/FBS.
"),tags$br(),
               tags$div("Fourqurean, J. W., Duarte, C. M., Kennedy, H., Marbà, N., Holmer, M., Mateo, M. A., et al. (2012). Seagrass ecosystems as a globally significant carbon stock. Nat. Geosci. 5, 505–509. doi:10.1038/ngeo1477.
"),tags$br(),
               tags$div("Halpern, B. S., Frazier, M., Afflerbach, J., Lowndes, J. S., Micheli, F., O’Hara, C., et al. (2019). Recent pace of change in human impact on the world’s ocean. Sci. Rep. 9, 11609. doi:10.1038/s41598-019-47201-9.
"),tags$br(),
               tags$div("IUCN (2021). The IUCN Red List of Threatened Species. Version 2021-3. https://www.iucnredlist.org. Accessed on [01 December 2021].
"),tags$br(),
               tags$div("Jayathilake, D. R. M., and Costello, M. J. (2020). A modelled global distribution of the kelp biome. Biol. Conserv. 252. doi:10.1016/j.biocon.2020.108815.
"),tags$br(),
               tags$div("Kaufmann, D., Kraay, A., The, M. M., and Bank, W. (2010). The Worldwide Governance Indicators Methodology and Analytical Issues. Available at: www.govindicators.org.
"),tags$br(),
               tags$div("Knapp, K. R., Kruk, M. C., Levinson, D. H., Diamond, H. J., and Neumann, C. J. (2010). The international best track archive for climate stewardship (IBTrACS). Bull. Am. Meteorol. Soc. 91, 363–376. doi:10.1175/2009BAMS2755.1.
"),tags$br(),
               tags$div('Krumhansl, K. A., Okamoto, D. K., Rassweiler, A., Novak, M., Bolton, J. J., Cavanaugh, K. C., et al. (2016). Global patterns of kelp forest change over the past half-century. Proc. Natl. Acad. Sci. U. S. A. 113, 13785–13790. doi:10.1073/pnas.1606102113.
'),tags$br(),
               tags$div('Linke, S., Lehner, B., Ouellet Dallaire, C., Ariwi, J., Grill, G., Anand, M., et al. (2019). Global hydro-environmental sub-basin and river reach characteristics at high spatial resolution. Sci. data 6, 283. doi:10.1038/s41597-019-0300-6. This product [Blue Forests] incorporates data from the HydroSHEDS database which is © World Wildlife Fund, Inc. (2006-2013) and has been used herein under license. WWF has not evaluated the data as altered and incorporated within [Blue Forests], and therefore gives no warranty regarding its accuracy, completeness, currency or suitability for any particular purpose. Portions of the HydroSHEDS database incorporate data which are the intellectual property rights of © USGS (2006-2008), NASA (2000-2005), ESRI (1992-1998), CIAT (2004-2006), UNEP-WCMC (1993), WWF (2004), Commonwealth of Australia (2007), and Her Royal Majesty and the British Crown and are used under license. The HydroSHEDS database and more information are available at http://www.hydrosheds.org.
'),tags$br(),
               tags$div('Mcowen, C., Weatherdon, L., Van Bochove, J., Sullivan, E., Blyth, S., Zockler, C., et al. (2017). A global map of saltmarshes. Biodivers. Data J. doi:https://doi.org/10.3897/BDJ.5.e11764.
'),tags$br(),
               tags$div('Menéndez, P., Losada, I. J., Torres-Ortega, S., Narayan, S., and Beck, M. W. (2020). The Global Flood Protection Benefits of Mangroves. Sci. Rep. 10. doi:10.1038/s41598-020-61136-6.
'),tags$br(),
               tags$div('Sanderman, J., Hengl, T., Fiske, G., Solvik, K., Adame, M. F., Benson, L., et al. (2018). A global map of mangrove forest soil carbon at 30 m spatial resolution. Environ. Res. Lett. 13. doi:10.1088/1748-9326/aabe1c.
'),tags$br(),
               tags$div('Simard, M., Fatoyinbo, L., Smetanka, C., Rivera-Monroy, V. H., Castañeda-Moya, E., Thomas, N., et al. (2019). Mangrove canopy height globally related to precipitation, temperature and cyclone frequency. Nat. Geosci. 12, 40–45. doi:10.1038/s41561-018-0279-1.
'),tags$br(),
               tags$div('Spillias, S. in prep. Mapping the Global Potential for Seaweed Farming. University of Queensland.
'),tags$br(),
               tags$div('The Nature Conservancy (2022). Modelled mangrove commercial finfish enhancement; Mapping ocean wealth portal. https://maps.oceanwealth.org/.
'),tags$br(),
               tags$div('Turschwell, M. P., Connolly, R. M., Dunic, J. C., Sievers, M., Buelow, C. A., Pearson, R. M., et al. (2021). Anthropogenic pressures and life history predict trajectories of seagrass meadow extent at a global scale. Proc. Natl. Acad. Sci. 118, e2110802118. doi:10.1073/pnas.2110802118/-/DCSupplemental.
'),tags$br(),
               tags$div('UNDP (2020). Human Development Index. Hum. Dev. Rep..
'),tags$br(),
               tags$div('UNEP-WCMC and IUCN (2021), Protected Planet: The World Database on Other Effective Area-based Conservation Measures (WD-OECM) [On-line], [December 2021], Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.
'),tags$br(),
               tags$div('UNEP-WCMC, Short FT (2021). Global distribution of seagrasses (version 7.1). Seventh update to the data layer used in Green and Short (2003). Cambridge (UK): UN Environment Programme World Conservation Monitoring Centre. Data DOI: https://doi.org/10.34892/x6r3-d211.
'),tags$br(),
               tags$div('UNESCO Institute for Statistics (2021). Literacy rate, adult total (% of people ages 15 and above). https://data.worldbank.org.
')
               ),
  ), # end data sources panel
  
) # end nav bar
