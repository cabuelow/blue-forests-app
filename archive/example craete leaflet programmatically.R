# example - creating leaflet map programmatically

source("wrangling.R")
#
# Leaflet loop example
#
library(leaflet)
x <- leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(
    group = 'basepoly1',
    data = unitsall,
    color = "#FFFFFF",
    weight = 0.4)

#In teh case of leafletProxy, this should work by 
#just adding the proxy in the loop as well
#(but check that...)
for (i in c('red', 'black')){
  x <- x %>%
    addPolygons(
      group = 'basepoly1',
      data = unitsall,
      color = i,
      weight = 0.4)
}
x