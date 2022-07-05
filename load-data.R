library(sf)
library(dplyr)
library(tidyr)
library(data.table)

# load data 

units2 <- st_read('data/units-attributes_wgs84-L2-simp2.gpkg')
unitsall <- st_read('data/units-all_wgs84-simp.gpkg')
wwf <- st_read('data/wwf-bf-projects2.gpkg')
inproj <- st_read('data/invest-proj.gpkg')
profile1 <- read.csv('data/enabling-profiles.csv')
profile1.sf <- st_read('data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries.shp') %>% filter(Country %in% profile1$name)
units2.p <- units2 %>% filter(!SOVEREIGN1 %in% profile1$sovereignt & !SOVEREIGN1 == 'United States')
df <- fread('data/scores/df.csv')
datqual <- fread('data/scores/dat-qual-L2-2.csv')
scores <- data.frame(fread('data/scores/blue-forest-scores-L2_area-standardised2.csv'))
scores2 <- data.frame(fread('data/scores/blue-forest-scores-L2_area-standardised_enabling-constrained2.csv'))
indscores.p <- fread('data/scores/rescaled-ind-scoring_area-stand-L2-2.csv')
indscores.p$indicator_name <- factor(indscores.p$indicator_name, levels = c("Extent", "Climate\nimpacts" ,
                                                                            "Land\nimpacts", "Marine\nimpacts",
                                                                            "Rate of\nLoss","Cyclone\nrisk", "Probability\nof\ndecline", 
                                                                            "Carbon", "Bio-\ndiversity" ,  "Coastal\ncommunity", "Coastal\nprotection"))
indscores.p2 <- fread('data/scores/rescaled-ind-scoring_area-stand-L2_enabling-constrained2.csv')
indscores.p2$indicator_name <- factor(indscores.p2$indicator_name, levels = c("Extent", "Climate\nimpacts" ,
                                                                            "Land\nimpacts", "Marine\nimpacts",
                                                                            "Rate of\nLoss","Cyclone\nrisk", "Probability\nof\ndecline", 
                                                                            "Carbon", "Bio-\ndiversity" , "Coastal\ncommunity", "Coastal\nprotection"))
enable <- fread('data/scores/enable.csv')
enable2 <- fread('data/scores/enable2.csv')
terr <- c('Global', sort(unique(as.character(units2$TERRITORY1))))
hot_pal <- c('#66CC33','#CC3300', '#9966CC', '#FFCC00', '#00CCCC', '#003333')
hotdf <- data.frame(input = c(1,2,3,4,5,6), criteria = c('extent', 'threat', 'carbon', 'biodiversity', 'pop_vulnerability', 'cobenefit'), criteraname = c('Extent', 'Threat', 'Carbon', 'Biodiversity', 'Coastal community', 'Coastal protection'))
my_popups <- fread('data/scores/mypopups.csv') %>% pull(popup)
my_popups2 <- fread('data/scores/mypopups2.csv') %>% pull(popup)

# colour palette

pal <- colorFactor( # colour palette for blue forest projects
  palette = "Dark2",
  domain = wwf$site_type)

pal2 <- colorFactor( # colour palette for blue forest projects
  palette = "Spectral",
  domain = inproj$Investment_readiness_stage)
