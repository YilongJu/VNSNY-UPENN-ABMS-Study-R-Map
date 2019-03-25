# A Map of New York City visualizing Geographic Data by Census Tract

The map is built with R and shiny package, used for visualizing the geographic data used in the research project conducted by two groups from the University of Pennsylvania and the Visiting Nurse Service of New York. The title of the project is Addressing Disparities in Healthcare Access and Outcomes among Chronically Ill Older Adults: Assessing the Feasibility of an Agent-Based Modeling Approach.

## Getting Started

To run this shiny app on your local machine, clone it and install required packages listed at the beginning of ```app.R```.

Alternatively, just go to https://vnsnyupenn.shinyapps.io/vnsny-upenn-abms-study-r-map/, a latest version is already deployed to shinyapps.io.

## How to use the map

### Main Map

* Hover on a census tract (CT) to view the values of corresponding data.

* The hover info will also be shown after you click a CT. A box will pop up and stay there, until you click another CT or click the X at the upper right corner.

### Left Panel

* Some map options are shown at the left panel.

* At the top of the left panel, you can select what basic layer of the map to use. The basic and dark versions are simplistic, while the Google map version will show the street and building info as well, just like what we typically see in the Google Map.

* At the middle of the left panel, you can select which type of buildings to be shown on the map, such as stores, libraries, Churches, etc, with different icons. If the zoom level is small, a numbered circle with a polygon will replace the icons that are too close to each other. The number is just the number of buildings clustered together and the polygon is the convex hull of the buildings.

* At the bottom of the left panel, you can select which Healthcare provider(s) to be shown (by organization). After they appear on the map, you can click the blue pin icon to view detailed information of the provider.

### Right Panel

* There are also some map options available in the right panel. This panel is draggable.

* At the top of the right panel, you can select which one variable to be shown on the tiles (census tract). Tracts with darker colors have larger values of the selected variable.

* At the middle of the right panel, you can select which variable(s) to be shown as circles. The circles will be centered at the center of each census tract, whose radius is positively proportional to the value of the selected variable. The legend of selected variables is shown in the plot window at the bottom of the panel.

* At the middle of the right panel, you can check the definition of selected variable(s).

* At the bottom of the right panel, you can see a barplot. Add census tracts by clicking them on the main map. The values of variable shown on the tile is automatically added to the plot as a bar for each CT. If there are variables shown as circles, there will be some other bars showing their values as well. Click twice to remove that CT area. There is also a button to clear all selected points.

* By clicking the “View plot in new window” button, a new window will pop up, showing a larger version of the barplot. You can also save it to your computer.

## Authors

* **Yilong Ju** - *Initial work* -


## Acknowledgments

* I would like to thank Prof. Silverman and Dr. Ryvicker for their kind advice on this project, and Brooke Waldt for his inspiration on the shapefile, Nathan Weyer for his help on technical issues when deploying it to local server.
