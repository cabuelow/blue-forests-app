### Blue Forest App

Web application to inform blue forest conservation.

#### TODO for first end-user consultation

1. - [X] Fix glitches
      - [X]	Map projection
              - Try with different polygon layer, same issues?
              -* Update: seems to be a problem with the HydroAtlas database specifically. Other polygon layers work fine. Perhaps can't solve for mapdeck
              - Works okay in leaflet - try writing app in leaflet
      - [ ] Timing out issue – increase server ‘instance’ period, greater than 15 minutes
      - [X] Double clicking on radio buttons

2. - [ ] Re-write app in leaflet

3. - [ ] On-click pop-ups

4. - [ ] Allow to switch on or off enabling constraint layer

5. - [ ] Incorporate data gap-filling information as layers to switch on/off

6. - [ ] Have WWF projects and investment feasibility as a switch on or off in every tab

7. - [ ] Remove area suitable for seaweed farming, TNC fisheries layer, and Eger data for now.

8. - [ ] Ideally, select different combinations of ecosystems and criteria, and get top locations

9. - [ ] Update final master dataframe from remote server

#### Decisions decisions

- [ ] Map individual layers at fine-scale resolution, but hotspots at coarse scale? 

#### Data/Analysis updates 

- [ ] Use Duarte 2022 seaweed distribution (will involve reprocessing all seaweed layers to this distribution, including threats :( )
- [ ] Get updated TNC mang fisheries enhancement layer when ready
- [ ] Get final seaweed farming layer when published
- [ ] Get final Eger kelp data when published
- [ ] Cyclone tracks - buffer by gale force wind radius

