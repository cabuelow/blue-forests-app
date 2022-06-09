# code adapted from: https://github.com/molly-williams/deltaSLR_map

# user interface

library(shiny)
library(shinyjs)
library(shinydashboard)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(leaflet.extras2)

# load and wrangle all required components for app

source("load-data.R")
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
           
           box(title="Welcome to the Blue Forest Data Explorer",
               status="primary",
               solidHeader=TRUE,
               width = 12,
               
               #tags$br(),

               tags$div("This tool can help identify opportunities for conserving",
                        tags$b('mangroves,'), 
                        tags$b('seagrass,'), 
                        tags$b('saltmarsh,'),'and', 
                        tags$b('kelp forests.')),
               
               #tags$br(),
               tags$div("The app needs time to",
                        tags$b('warm-up.')),
               tags$div("Please allow 30 seconds to one minute before you",
                        tags$b('click'), "on the",
               tags$b('explorer'), "pages above to tour the best available global data on blue forest",
                        tags$b("extent,"),
                        tags$b("threats,"),
                        tags$b("carbon,"),
                        tags$b("biodiversity,"),
                        tags$b("fisheries,"), 'and',
                        tags$b("coastal protection."),
                        )
               ), # end box
           
           box(title = h5('Coastal management units', style = 'font-size:18px;'),
               width = 12,
               #tags$br(),
               
               tags$div('Blue forest data is provided in',
                        tags$b('coastal management units'), 
                        'that represent hydrologically connected',
                        tags$b('land-seascapes'), 'based on shared river networks.')),
           
           box(title = h5('Criteria and indicators to identify opportunities', style = 'font-size:18px;'),
               width = 12,
              # tags$br(),
               
              tags$div(tags$b('1. Extent:'), 'Total area of mangroves, seagrass, saltmarsh and kelp (standardised by management unit size).
'),
              tags$div(tags$b('2. Threat:'), 'Composed of several indicators including rates of loss, cyclone risk, and cumulative climate, land and marine-based impacts to each forest.
'),
              tags$div(tags$b('3. Carbon:'), 'Average carbon storage (mangroves, seagrass, saltmarsh) or sequestration (kelp).
'),
              tags$div(tags$b('4. Biodiversity:'), 'Richness of species affiliated with each forest type. 
'),
              tags$div(tags$b('5. Co-benefit:'), 'Fisheries (mangrove commercial fisheries enhancement or average kelp fisheries biomass) and coastal protection (average number of people protected by mangroves per 20km of coastline).
')),
          
           box(title = h5('Identify opportunities for impact', style = 'font-size:18px;'),
               width = 12,
               #tags$br(),
               
               tags$div('Countries and their territories that have high socio-economic and political capacity to enable blue forest conservation can be excluded using an',
                        tags$b('enabling condition constraint layer.'))),
           
           box(title = h5('See where Blue Forest projects are happening', style = 'font-size:18px;'),
               width = 12,
               #tags$br(),
               
               tags$div('See where the World Wildife Fund and other organisations',
                        tags$b('are taking action'), 'to conserve Blue Forests, and find out about',
                        tags$b('business model maturity:')),
               tags$div(tags$b('1. Technical proving:'), 'proving environmental outcomes and feasibility.'),
               tags$div(tags$b('2. Monetisation:'), 'modelling and identifying cash flows and demand for services.'),
               tags$div(tags$b('3. Investment readiness:'), 'demonstrated cash flows and ready to raise investment.'),
               tags$div(tags$b('4. Pathfinder funding:'), 'patient and concessionary capital to demonstrate sustainable revenue at scale.')),

           box(title = h5('Data certainty and confidence', style = 'font-size:18px;'),
               width = 12,
               #tags$br(),
               
               tags$div('With the exception of mangroves, the extent and ecosystem service value of blue forests are not mapped globally at a high resolution. Therefore, more work is needed to better map the extent and ecosystem services for seagrass, saltmarsh and kelp.'
),
               #tags$br(),
               
               tags$div('Indicators for criteria (described above) that were not globally comprehensive or spatially congruent with baseline distributions were gap-filled with regional or global averages. These gap-filled estimates could be improved in the future as more data from local scales become available to be integrated into global maps.
')),
           box(title = h5('Other Blue Forest tools', style = 'font-size:18px;'),
               width = 12,
               #tags$br(),
               
               tags$div(tags$a('Mangrove Carbon App', href="https://mangrove-carbon.wetlands.app/", target="_blank")),
               tags$div(tags$a('Coastal Wetlands Index App', href="https://glowdex.wetlands.app/", target="_blank"))),
           
           box(title = h5('Issues or questions?', style = 'font-size:18px;'),
               width = 12,
               #tags$br(),
               
               tags$div('Please contact Christina Buelow at',
                        tags$b('c.buelow@griffith.edu.au,'),
                        'or Chris Brown at', tags$b('chris.brown@griffith.edu.au.')))
               
  ), # end instructions tabpanel
                 
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
                             top = 600, 
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
                             
                             #tags$b("Blue forest area"),
                             tags$em("Click on a coastal management unit to find out more & drag this box to fit your screen)"),
                             
                             tags$br(),
                             
                             tableOutput('myDf_outputf'),
                             
                             #tags$br(),
                             
                             #tags$b("Percent of blue forests protected"),
                             
                             tableOutput('myDf_outputf2'),
                             
                             #tags$br(),
                             
                             #tags$b("Percent of blue forests protected"),
                             
                             plotOutput('indplot', height = '200px', width = '500px'),
                             
                             h5(tags$b("Show national context indicators:")),
                             checkboxInput("natcon", label = NULL, value = FALSE),
                             
                             textOutput('text')
                             
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
                             
                             tags$br(),
                             
                             tags$em("Select from steps 1-3. Then click 'Map criteria hotspots'."),
                             
                             checkboxGroupInput("criteria", 
                                                label=h5(tags$b("1. Select criteria to map:")), 
                                                choices = list("Extent" = 1, "Threat" = 2, 
                                                               "Carbon" = 3, "Biodiversity" = 4, 'Cobenefit' = 5),
                                                selected = 1,
                                                inline = TRUE),
                             # tags$br(),
                             
                             sliderInput("perc", label = h5(tags$b("2. Set threshold to find management units in top percent of selected criteria:")), 
                                         min = 0, max = 100, 
                                         value = 100,
                                         step = 5),
                             
                             #tags$br(),
                             
                             h5(tags$b("3. Turn on enabling condition constraint layer?")),
                             checkboxInput("profile2", label = NULL, value = FALSE),
                             actionButton('mapit2', 'Map criteria hotspots'),
                             #tags$br(),
                             
                             selectInput("country", label = h5(tags$b("4. Choose country or territory:")), 
                                         choices =  terr, 
                                         selected = 'Global')
                             
               ) # end absolute panel 1
           ), # end div

           tags$div(id="cite",
                    tags$em('This map was created for the
                            Blue Forests Initiative, a project supported by 
                            WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Seagrass',
           forestUI("seagrass", criteria_choices = criteria_others),
           tags$div(id="cite",
                    tags$em('This map was created for the
                            Blue Forests Initiative, a project supported by 
                            WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Saltmarsh',
           forestUI("saltmarsh", criteria_choices = criteria_others),
           tags$div(id="cite",
                    tags$em('This map was created for the
                            Blue Forests Initiative, a project supported by 
                            WWF and the Global Wetlands Project'))
           
  ), # end tabpanel
  tabPanel('Explore Kelp',
           forestUI("kelp", criteria_choices = criteria_mang_kelp),
           tags$div(id="cite",
                    tags$em('This map was created for the
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
