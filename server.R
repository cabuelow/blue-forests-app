# code adapted from: https://github.com/molly-williams/deltaSLR_map

# server

library(shiny)
library(shinyjs)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(leaflet.extras2)
library(reactlog)

# server logic

function(input, output, session) {
  
  # ------------ 
  # Single forest pages
  # ------------ 
  forestServer("mangroves", "mangrove", criteria_mang_kelp_s)
  forestServer("seagrass", "seagrass", criteria_others_s)
  forestServer("saltmarsh", "saltmarsh", criteria_others_s)
  forestServer("kelp", "kelp", criteria_mang_kelp_s)
  
} #end server