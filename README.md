### Blue Forest App

Web application to inform blue forest conservation.

#### TODO for first end-user consultation

1. - [X] Fix glitches
      - [ ] Timing out issue – increase server ‘instance’ period, greater than 15 minutes

2. - [ ] Re-write app in leaflet (lose some speed compared to mapdeck, trade-off for features)
  - **In progress - follow mangrove tab template and repeat for rest of blue forests, probs best to modularise**

3. - [ ] Incorporate data gap-filling information as layers to switch on/off

4. - [ ] Fix Fiji zoom problem

5. - [ ] Write an instructions tab

6. - [ ] Remove area suitable for seaweed farming, TNC fisheries layer, and Eger data for now.

7. - [ ] Make data wrangling a pre-process step, so the app loads final datasets, will save
compute time

#### Decisions decisions

- [ ] Map at fine scale or coarse-scale?

- [ ] Include all individual indicator layers? e.g. land vs. climate threats, spp. richness, etc.? Could show both prototypes at consultation and see if its necessary?

#### Wish List

- [ ] Pick your own threats indicators (e.g., land vs climate, prob of decline, etc)

#### Data/Analysis updates 

- [ ] Use Duarte 2022 seaweed distribution (will involve reprocessing all seaweed layers to this distribution, including threats :( )
- [ ] Get updated TNC mang fisheries enhancement layer when ready
- [ ] Get final seaweed farming layer when published
- [ ] Get final Eger kelp data when published
- [ ] Cyclone tracks - buffer by gale force wind radius
