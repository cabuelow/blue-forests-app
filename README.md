### Code for the Blue Forest Data Explorer

This repository contains code for the Blue Forest Data Explorer, a web-application designed to help identify potential opportunities for blue forest conservation. Explore the web app [here](https://global-wetlands.shinyapps.io/blue-forests-app/).

[Chris Brown](https://github.com/cbrown5) and [Kai Ching Cheong](https://github.com/kitecheong) are contributors to this code-base.
Aesthetics were inspired by code provided [here](https://github.com/molly-williams/deltaSLR_map).

#### Upcoming data updates 

- [ ] Modelled estimates of macroalgal extent
- [ ] Remote-sensing of mangrove extent for the year 2020

#### Code script description

- ##### ui.R
  Sets up the user-interface. 
  
- ##### server.R
  Uses inputs to the user-interface to create and display maps and other plots in the app.
  
- ##### helpers
  Scripts in the 'helpers' folder are as follows:
  1. pre-wrangling.R: prepare data for displaying in the web-application.
  2. load-data.R: load wrangled data.
  3. paramatersUI.R: module parameters relevant to the user-interface.
  4. parametersServer.R: module parameters relevant to the app server.
  5. forest-modules.R: modules for replicating tabpanels for each blue forest.
  6. summarise-use.R: download shinyapp.io useage stats, summarise, and plot.



