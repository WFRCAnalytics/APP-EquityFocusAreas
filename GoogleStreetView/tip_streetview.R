#' Add a column displaying a basic Google maps street-view html link to the TIP
#' projects lines and points datasets
#'
#'@param tip_project_lines A shapefile containing the information for the TIP 
#' project lines, including its geographic details
#' 
#'@param tip_project_points A shapefile containgin the information for the TIP
#' project points, including its geogrpahic coordinates
#'
#'@return Two new shapefiles of the tip_project_lines and tip_project_points 
#' with an additional column displaying the basic street view link
#'
#'@details For the TIP 2023-2028 map dataset located at //server1/volumeg/Data/Observatory/Transportation/TIP/TIP2023_2028/TIPProjects_map.gdb
#' an additional column is needed that includes a streetview link showing a 
#' the location of the project. This link will be needed on the WFRC online 
#' map webpage to display a streetview image of the project location in a small
#' window on the side of the screen.
#'
#'@author Chris Day
#-------------------------------------------------------------------------------------------------------------------------------------------------------------#

#' needed libraries
library(tidyverse)
library(sf)
library(mapview)
library(stringi)


#' read in original shapefile
tip_project_lines <- st_read("//server1/volumeg/Data/Observatory/Transportation/TIP/TIP2023_2028/TIPProjects_map.gdb", layer = "TIP_Projects_lines")
tip_project_points <- st_read("//server1/volumeg/Data/Observatory/Transportation/TIP/TIP2023_2028/TIPProjects_map.gdb", layer = "TIP_Projects_points")

## LINES -----------------------------------------------------------------------------------------------------------------------------------------------------#
#' generate centroid coordinates for each line
#' use centroid coordinates and basic google street-view link to concatenate an
#'   html string of the street-view location of the centroid
tip_lines_centroids <- tip_project_lines %>%
  mutate(Shape = st_transform(Shape,st_crs(4326))) %>%
  mutate(midlinep = st_point_on_surface(Shape)) %>% #instead of st_centroid use st_point_on_surface
  mutate(midlinep_txt = st_as_text(midlinep)) %>%
  mutate(midlinep_str = substring(midlinep_txt,8)) %>%
  mutate(longm = gsub( " .*$", "", midlinep_str),
         latm = gsub('.)$', '',sub(".*? ", "",midlinep_str))) %>%
  mutate(basic_streetview = paste("http://maps.google.com/maps?q=&layer=c&cbll=",latm,",",longm,"&cbp=11,0,0,0,0",sep="")) %>%
  select(-midlinep_txt,-midlinep_str,-longm,-latm)
tip_lines_centroids2 <- tip_lines_centroids[,c(1,2,3,4,5,6,7,8,9,10,11,13,12,14)]

#' write new shapefile for lines data
st_write(tip_lines_centroids2, dsn = "results/TIP_Projects_Stretview.gpkg", layer = "basiclinks", append=TRUE) 


## POINTS ----------------------------------------------------------------------------------------------------------------------------------------------------#
#' use point coordinates to concatenate an html string of the street-view location
tip_nodes <- tip_project_points %>%
  mutate(Shape = st_transform(Shape,st_crs(4326))) %>%
  mutate(centroid_txt = st_as_text(Shape)) %>%
  mutate(centroid_str = substring(centroid_txt,8)) %>%
  mutate(long = gsub( " .*$", "", centroid_str),
         lat = gsub('.)$', '',sub(".*? ", "",centroid_str))) %>%
  mutate(basic_streetview = paste("http://maps.google.com/maps?q=&layer=c&cbll=",lat,",",long,"&cbp=11,0,0,0,0",sep="")) %>%
  select(-centroid_txt,-centroid_str,-long,-lat)

#' write new shapefile for points data
st_write(tip_nodes, dsn = "outputs/results/TIP_Projects_Stretview.gpkg", layer = "basicnodes", append=TRUE) 

