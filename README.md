# DSBA 5122 Final Project

## Authors
* Nicholas Occhipinti
* Karyn Cook
* Ziyin Liu

## Final Project Website
https://nocchipi.shinyapps.io/dsba_5122_final_project/

## Running the project

* All the code and data are included in this repository, you can download the code as a ZIP file or run the following command in Git Bash "git clone https://github.com/nick-occ/dsba_5122_final_project.git".

* Open the project in R Studio.

* The following packages will have to be installed:
  * shiny
  * DT
  * ggplot2
  * wordcloud2
  * sf
  * plotly
  * reshape2
  * shinythemes
  * memoise
  * scales

* The Shiny app is in __app.R__ file and to run it in R Studio simply click the Run App button.
* The logic to fetch the data is in the __drugs.R__ file which is used by __app.R__ to generate the data.
* Some styling is generated through a CSS file located in the __www__ folder called __app.css__.
* Geographic data shapefiles that were used are located in the __shp__ folder.
* Tabular data that was loaded into our application is stored in the __data__ folder.
