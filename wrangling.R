# code adapted from: https://github.com/molly-williams/deltaSLR_map

library(sf)
library(dplyr)
library(tidyr)
library(stringr)

# simplify geopackages so app is faster
#library(rmapshaper)
#uni <- st_read('data/units-attributes_wgs84-L2.gpkg')
#s <- ms_simplify(uni, keep_shapes = T)
#ss <- s %>% mutate(seafarm = ifelse(seafarm_area_ha > 0, 1, 0))
#st_write(ss, 'data/units-attributes_wgs84-L2-simp.gpkg', overwrite = T, append = F)

# load data 

units2 <- st_read('data/units-attributes_wgs84-L2-simp.gpkg') %>% mutate(HYBAS_ID = ifelse(is.na(HYBAS_ID), 1, HYBAS_ID)) %>% mutate(Unit = as.character(HYBAS_ID)) %>% mutate(prop_vul_pop = lecz_pop_count_sum/pop_count_sum, mangrove_carbon_mgC_ha = mangrove_abg_mgC_ha + mangrove_soil_mgC_ha) %>% mutate(mangrove_2016_area_ha = round(mangrove_2016_area_ha), seagrass_area_ha = round(seagrass_area_ha), saltmarsh_area_ha = round(saltmarsh_area_ha), kelp_area_ha = round(kelp_area_ha)) %>% mutate(mang_prot = round(((mangrove_2016_wdpa_ha+mangrove_2016_oecm_ha)/mangrove_2016_area_ha)*100), seag_prot = round(((seagrass_wdpa_ha+seagrass_oecm_ha)/seagrass_area_ha)*100), salt_prot = round(((saltmarsh_wdpa_ha+saltmarsh_oecm_ha)/saltmarsh_area_ha)*100),kelp_prot = round(((kelp_wdpa_ha+kelp_oecm_ha)/kelp_area_ha)*100))  %>% mutate(seagrass_area_ha = ifelse(seagrass_points >= 1 & seagrass_area_ha == 0, 'Present (unknown area)', seagrass_area_ha), mang_prot = ifelse(is.na(mang_prot), 0, mang_prot), seag_prot = ifelse(is.na(seag_prot), 0, seag_prot), salt_prot = ifelse(is.na(salt_prot), 0, salt_prot), kelp_prot = ifelse(is.na(kelp_prot), 0, kelp_prot)) 
unitsall <- st_read('data/units-all_wgs84-simp.gpkg')
wwf <- st_read('data/wwf-bf-projects.gpkg')
profile1 <- st_read('data/enabling-profiles.gpkg') %>% st_drop_geometry() %>%filter(Enabling.profile == 1)
profile1.sf <- st_read('data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries.shp') %>% filter(Country %in% profile1$name)
scores <- read.csv('data/scores/blue-forest-scores-L2_area-standardised.csv') %>%  # choose scores to plot
  left_join(select(data.frame(st_drop_geometry(units2)), unit_ID, mangrove:seagrass))
scores2 <- read.csv('data/scores/blue-forest-scores-L2_area-standardised_enabling-constrained.csv') %>%  # choose scores to plot
  left_join(select(data.frame(st_drop_geometry(units2)), unit_ID, mangrove:seagrass))
indscores <- read.csv('data/scores/rescaled-ind-scoring_area-stand-L2.csv')
indscores2 <- read.csv('data/scores/rescaled-ind-scoring_area-stand-L2_enabling-constrained.csv')
units2.p <- units2 %>% filter(!SOVEREIGN1 %in% profile1$sovereignt & !SOVEREIGN1 == 'United States')
datqual <- read.csv('data/scores/dat-qual-L2.csv') %>% select(unit_ID, mangrove_agb, mangrove_soil, mangrove.coastal.protection, tnc_fish_catch,
                                                              seagrass_carbon, saltmarsh_carbon_storage, kelp_carbon, kelp.fisheries.biomass) %>% 
  pivot_longer(cols =-unit_ID, names_to = 'indicator', values_to = 'score') %>% 
  filter(!is.na(score) & score != 1)
datqual$forest <- recode(datqual$indicator,  'mangrove_agb' = 'mangrove', 'mangrove_soil' = 'mangrove',
                          'mangrove.coastal.protection'= 'mangrove',  'tnc_fish_catch' ='mangrove',
                         'seagrass_carbon' = 'seagrass', 'saltmarsh_carbon_storage' = 'saltmarsh',
                         'kelp_carbon' = 'kelp', 'kelp.fisheries.biomass' = 'kelp')
datqual$forestname <- recode(datqual$indicator,  'mangrove_agb' = 'Mangrove', 'mangrove_soil' = 'Mangrove',
                         'mangrove.coastal.protection'= 'Mangrove',  'tnc_fish_catch' ='Mangrove',
                         'seagrass_carbon' = 'Seagrass', 'saltmarsh_carbon_storage' = 'Saltmarsh',
                         'kelp_carbon' = 'Kelp', 'kelp.fisheries.biomass' = 'Kelp')
datqual$indicator <- recode(datqual$indicator,  'mangrove_agb' = 'above ground biomass carbon storage', 'mangrove_soil' = 'soil carbon storage',
                         'mangrove.coastal.protection'= 'coastal protection',  'tnc_fish_catch' ='fisheries enhancement',
                         'seagrass_carbon' = 'carbon storage', 'saltmarsh_carbon_storage' = 'carbon storage',
                         'kelp_carbon' = 'carbon storage', 'kelp.fisheries.biomass' = 'fisheries biomass')
datqual$score <- recode(datqual$score, '2' = 'marine provincial average', '3' = 'marine realm average', '4' = 'global average')

indscores.p <- indscores %>% 
  pivot_longer(cols = c(kelp_climate_mean:kelp_carbon_gC_m2_yr, saltmarsh_carbon_mgC_ha,
                        seagrass_carbon_gC_m2:kelp_fisheries_biomass_m2,
                        kelp_spp_richness:seagrass_spp_richness, mangrove_carbon_mgC_ha,
                        mangrove_fish_catch, mangrove_2016_area_ha, saltmarsh_area_ha,
                        seagrass_area_ha, kelp_area_ha),
               names_to = 'indicators', values_to = 'indicator_score') %>% 
  mutate(forest = substr(indicators, 1,4),
         indicator = substr(indicators, 6, 50)) %>% 
  mutate(indicator_score = ifelse(is.na(indicator_score), 0, indicator_score))
indscores.p$forest_name <- recode(indscores.p$forest, 'mang' = 'mangrove', 'seag' = 'seagrass', 'salt' = 'saltmarsh', 'kelp' = 'kelp')
indscores.p$forest_name2 <- recode(indscores.p$forest, 'mang' = 'Mangrove', 'seag' = 'Seagrass', 'salt' = 'Saltmarsh', 'kelp' = 'Kelp')

indscores.p$indicator_name <- recode(indscores.p$indicator, 
                                     'climate_mean' = 'Climate impacts',
                                     'land_mean' = 'Land impacts',
                                     'marine_mean' = 'Marine impacts',
                                     'trend' = 'Rate of Loss',
                                     'ove_prop_lost_10_16' = 'Rate of Loss',
                                     'ass_mean_risk' = 'Probability of decline',
                                     'carbon_gC_m2_yr' = 'Carbon',
                                     'arsh_carbon_mgC_ha' = 'Carbon',
                                     "ass_carbon_gC_m2" = 'Carbon', 
                                     "ove_coastal_protection" = 'Coastal protection', 
                                     'fisheries_biomass_m2' = 'Fisheries', 
                                     "spp_richness"= 'Biodiversity',
                                     "ove_spp_richness"= 'Biodiversity',
                                     "arsh_spp_richness" = 'Biodiversity',
                                     "ass_spp_richness" = 'Biodiversity',
                                     "ove_carbon_mgC_ha" = 'Carbon',
                                     "ove_fish_catch" = 'Fisheries',
                                     'ove_2016_area_ha' = 'Extent',
                                     'arsh_area_ha' = 'Extent',
                                     'ass_area_ha' = 'Extent',
                                     "area_ha"  = 'Extent')
indscores.p$fill <- recode(indscores.p$indicator_name,
                           'Rate of Loss' = 'Threat',
                           'Marine impacts' = 'Threat',
                           'Land impacts' = 'Threat', 
                           'Cyclone risk' = 'Threat',
                           'Probability of decline' = 'Threat',
                           'Climate impacts' = 'Threat',
                           'Fisheries' = 'Cobenefit',
                           'Coastal protection' = 'Cobenefit')
indscores.p$indicator_name <- str_wrap(indscores.p$indicator_name, width = 9)

indscores.p2 <- indscores2 %>% 
  pivot_longer(cols = c(kelp_climate_mean:kelp_carbon_gC_m2_yr, saltmarsh_carbon_mgC_ha,
                        seagrass_carbon_gC_m2:kelp_fisheries_biomass_m2,
                        kelp_spp_richness:seagrass_spp_richness, mangrove_carbon_mgC_ha,
                        mangrove_fish_catch, mangrove_2016_area_ha, saltmarsh_area_ha,
                        seagrass_area_ha, kelp_area_ha),
               names_to = 'indicators', values_to = 'indicator_score') %>% 
  mutate(forest = substr(indicators, 1,4),
         indicator = substr(indicators, 6, 50)) %>% 
  mutate(indicator_score = ifelse(is.na(indicator_score), 0, indicator_score))
indscores.p2$forest_name <- recode(indscores.p2$forest, 'mang' = 'mangrove', 'seag' = 'seagrass', 'salt' = 'saltmarsh', 'kelp' = 'kelp')
indscores.p2$forest_name2 <- recode(indscores.p2$forest, 'mang' = 'Mangrove', 'seag' = 'Seagrass', 'salt' = 'Saltmarsh', 'kelp' = 'Kelp')

indscores.p2$indicator_name <- recode(indscores.p2$indicator, 
                                     'climate_mean' = 'Climate impacts',
                                     'land_mean' = 'Land impacts',
                                     'marine_mean' = 'Marine impacts',
                                     'trend' = 'Rate of Loss',
                                     'ove_prop_lost_10_16' = 'Rate of Loss',
                                     'ass_mean_risk' = 'Probability of decline',
                                     'carbon_gC_m2_yr' = 'Carbon',
                                     'arsh_carbon_mgC_ha' = 'Carbon',
                                     "ass_carbon_gC_m2" = 'Carbon', 
                                     "ove_coastal_protection" = 'Coastal protection', 
                                     'fisheries_biomass_m2' = 'Fisheries', 
                                     "spp_richness"= 'Biodiversity',
                                     "ove_spp_richness"= 'Biodiversity',
                                     "arsh_spp_richness" = 'Biodiversity',
                                     "ass_spp_richness" = 'Biodiversity',
                                     "ove_carbon_mgC_ha" = 'Carbon',
                                     "ove_fish_catch" = 'Fisheries',
                                     'ove_2016_area_ha' = 'Extent',
                                     'arsh_area_ha' = 'Extent',
                                     'ass_area_ha' = 'Extent',
                                     "area_ha"  = 'Extent')
indscores.p2$fill <- recode(indscores.p2$indicator_name,
                           'Rate of Loss' = 'Threat',
                           'Marine impacts' = 'Threat',
                           'Land impacts' = 'Threat', 
                           'Cyclone risk' = 'Threat',
                           'Probability of decline' = 'Threat',
                           'Climate impacts' = 'Threat',
                           'Fisheries' = 'Cobenefit',
                           'Coastal protection' = 'Cobenefit')
indscores.p2$indicator_name <- str_wrap(indscores.p2$indicator_name, width = 9)

terr <- c('Global', sort(unique(as.character(units2$TERRITORY1))))
df <- data.frame(units2) %>% 
  dplyr::select(unit_ID, mangrove, seagrass, saltmarsh, kelp, seafarm) %>% 
  tidyr::pivot_longer(-unit_ID, names_to = 'eco')
df$eco <- recode(df$eco, mangrove = 1, seagrass = 2, saltmarsh = 3, kelp = 4, seafarm = 5)
hot_pal <- c('#66CC33','#CC3300', '#9966CC', '#FFCC00', '#00CCCC')
hotdf <- data.frame(input = c(1,2,3,4,5), criteria = c('extent', 'threat', 'carbon', 'biodiversity', 'cobenefit'), criteraname = c('Extent', 'Threat', 'Carbon', 'Biodiversity', 'Cobenefit'))
# pop-ups

my_popups <- st_drop_geometry(wwf) %>% 
  #pivot_longer(cols = mangrove:seaweed, names_to = 'blueforest', values_to = 'val') %>% 
  #filter(val == 1) %>% 
  mutate(mangrove = ifelse(mangrove == 1, 'Mangroves', NA),
         seagrass = ifelse(seagrass == 1, 'Seagrass', NA), 
         saltmarsh = ifelse(saltmarsh == 1, 'Saltmarsh', NA), 
         seaweed = ifelse(seaweed == 1, 'Seaweed', NA)) %>% 
  unite('blueforest', mangrove:seaweed, na.rm = T, sep = ' & ') %>% 
  mutate(popup = paste0("<span style='font-size: 120%'><strong>",wwf_office,"</strong></span><br/>",
                        "<strong>", "Site: ", "</strong>", site, 
                        "<br/>", 
                        "<strong>", "Blue Forest focus: ", "</strong>", blueforest)) %>% 
  pull(popup)

# colour palette

pal <- colorFactor( # colour palette for blue forest projects
  palette = "Spectral",
  domain = wwf$site_type)

