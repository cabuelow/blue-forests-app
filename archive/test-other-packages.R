library(rdeck)
library(RcppSimdJson)
library(tidyverse)
library(viridis)
library(sf)
library(leaflet)


leaflet(units2) %>% addProviderTiles(providers$CartoDB.Positron) %>% addPolygons()
options(rdeck.mapbox_access_token = "pk.eyJ1IjoidXRqaW1teXgiLCJhIjoiY2tnMmI1OWRpMDZsdDJxb2Y4MjdnZmxpMyJ9.ImwwUvDQpod7-B0YnIUytw")
MAPBOX_ACCESS_TOKEN = "pk.eyJ1IjoidXRqaW1teXgiLCJhIjoiY2tnMmI1OWRpMDZsdDJxb2Y4MjdnZmxpMyJ9.ImwwUvDQpod7-B0YnIUytw"
rdeck(map_style = mapbox_dark(),
      theme = "kepler")

m <- leaflet() %>%
  addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png", 
           attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>')
m
url <- file.path(
  "https://raw.githubusercontent.com/visgl/deck.gl-data/master",
  "examples/scatterplot/manhattan.json",
  fsep = "/"
)
manhattan_data <- fload(url) %>%
  as_tibble(.name_repair = ~ c("lon", "lat", "species")) %>%
  mutate(
    position = sfc_point(lon, lat),
    species = as.factor(species),
    species_name = if_else(species == 1, "dog", "cat")
  )

rdeck(
  map_style = mapbox_dark(),
  # set the bounds of the map to include all of the manhattan data
  initial_bounds = st_bbox(manhattan_data$position),
  # add a 2 pixel buffer to each point, making it easier to hover
  picking_radius = 2
) %>%
  add_scatterplot_layer(
    name = "manhattan_animals",
    data = manhattan_data,
    # the coloumn in manhattan_data which contains the location of each point
    get_position = position,
    # a categorical colour scale, using the species column and a cividis colour palette
    get_fill_color = scale_color_category(
      col = species,
      palette = cividis(2)
    ),
    # the radius of each point (default 1 metre) is scaled by 30
    radius_scale = 30,
    radius_min_pixels = 0.5,
    # highlight dot density
    blending_mode = "additive",
    # interactivity
    pickable = TRUE,
    auto_highlight = TRUE,
    # per-species highlight colour
    highlight_color = scale_color_category(
      col = species,
      palette = c("#0060e6", "#fff399"),
      legend = FALSE
    ),
    tooltip = c(species, species_name)
  )
