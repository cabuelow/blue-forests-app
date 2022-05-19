# code adapted from: https://github.com/molly-williams/deltaSLR_map

library(sf)
library(dplyr)
library(tidyr)

# simplify geopackages so app is faster
#library(rmapshaper)
#uni <- st_read('data/units-attributes_wgs84-L2.gpkg')
#s <- ms_simplify(uni, keep_shapes = T)
#ss <- s %>% mutate(seafarm = ifelse(seafarm_area_ha > 0, 1, 0))
#st_write(ss, 'data/units-attributes_wgs84-L2-simp.gpkg', overwrite = T, append = F)

# load data 

units2 <- st_read('data/units-attributes_wgs84-L2-simp.gpkg') %>% mutate(HYBAS_ID = ifelse(is.na(HYBAS_ID), 1, HYBAS_ID)) %>% mutate(Unit = as.character(HYBAS_ID)) %>% mutate(prop_vul_pop = lecz_pop_count_sum/pop_count_sum, mangrove_carbon_mgC_ha = mangrove_abg_mgC_ha + mangrove_soil_mgC_ha)
unitsall <- st_read('data/units-all_wgs84-simp.gpkg')
wwf <- st_read('data/wwf-bf-projects.gpkg')
profile1 <- st_read('data/enabling-profiles.gpkg') %>% st_drop_geometry() %>%filter(Enabling.profile == 1)
profile1.sf <- st_read('data/UIA_World_Countries_Boundaries/UIA_World_Countries_Boundaries.shp') %>% filter(Country %in% profile1$name)
#scores <- read.csv('data/blue-forest-scores-L4_area-standardised.csv') %>%  # choose scores to plot
 # left_join(select(data.frame(st_drop_geometry(units2)), unit_ID, mangrove:seagrass))

terr <- c('Global', sort(unique(as.character(units2$TERRITORY1))))
df <- data.frame(units2) %>% 
  dplyr::select(unit_ID, mangrove, seagrass, saltmarsh, kelp, seafarm) %>% 
  tidyr::pivot_longer(-unit_ID, names_to = 'eco')
df$eco <- recode(df$eco, mangrove = 1, seagrass = 2, saltmarsh = 3, kelp = 4, seafarm = 5)

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

# input filtering function

input_filter <- function(x, bf){
  sub <- x %>% 
    filter(eco == bf) %>% 
    filter(value == 1)
  filtdata <<- units2 %>% 
    filter(unit_ID %in% unique(sub$unit_ID))
  zz <<- unname(st_bbox(filtdata))
  cpal <<- '#20b2aa'
}

input_filter2 <- function(x, bf, country){
  sub <- x %>% 
    filter(eco == bf) %>% 
    filter(value == 1)
  filtdata <<- units2 %>% 
    filter(unit_ID %in% unique(sub$unit_ID)) %>% 
    filter(TERRITORY1 == country)
  zz <<- unname(st_bbox(filtdata))
  cpal <<- '#20b2aa'
}
