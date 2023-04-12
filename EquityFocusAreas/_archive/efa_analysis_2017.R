#' Redetermine Equity Focus Areas (EFAs) for the WFRC/MAG model region using updated
#' data (2020). EFAs are determined using ACS data relating to Income (Table C17002),
#' Minority/Ethnicity (Table B03002) and Household Vehicles (Table B25044). These tables
#' were downloaded and analyzed at the block level and downloaded for the following counties:
#' Box Elder, Davis, Weber, Salt Lake, Utah
#'
#'@param efashp A shapefile containing the previous EFAs, I beleive form 2017 data
#'@param wfrcboundary A shapefile containing the region of the WFRC/MAG model
#'@param wfrc_blockgroups A spatial object containing the 2017 Utah Block group geographies
#'@param minority Table B03002 from ACS data, organized by block groups in the region
#'@param vehicles Table B25044 from ACS data, organized by block groups in the region
#'@param income Table C17002 from ACS data, organized by block groups in region
#'@param groupQuarter Table P5 from ACS data, organized by block groups in region
#'
#'@return Geopackages containing the updated EFAs calculated from 2017 data. 
#'One shapefile is created using standard devations from the mean and the other is 
#'created from default threseholds
#'
#'@details The regions for the EFAs can be calculated in two ways. The first way uses
#'default thresholds. The second way calculates the mean and standard deviation of the 
#'variable for the region, and then determines new thresholds based on that. The thresholds
#'are shown in a table later on.
#'
#'@author Chris Day
#-------------------------------------------------------------------------------------------------------------------------------------------------------------#

#' needed libraries
library(tidyverse)
library(sf)
library(knitr)
library(kableExtra)
library(magrittr)
library(leaflet)
library(tidycensus)
library(tigris)
source("R/efamap/efa_analysis_scripts.R")
census_api_key("0196454888e2441971be7360589dd0399e036978")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Read in the data needed to redetermine the EFAs
wfrc_counties <- c("Utah","Davis","Salt Lake","Weber","Box Elder")

efashp2017_original <- st_read("data/Equity_Focus_Areas/EquityFocusAreas.shp") %>% mutate(group = "2017 Old") %>%  st_transform(4326)

wfrcboundary <- st_read("data/WFRCBoundary2018/WFRCBoundary2018.shp") %>% summarize(geometry = st_union(geometry)) %>%
  st_transform(4326)
wfrc_blockgroups_2017 <- block_groups("UT",county = wfrc_counties,cb=TRUE, year = 2017) %>%
  st_transform(4326)

minVars <- paste0("B03002_00", c(1,3))
minority17 <- get_acs_wfrc("block group", minVars, "UT", wfrc_counties,2017) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

vehVars <- c(paste0("B25044_00",c(1,3)),"B25044_010")
vehicles17 <- get_acs_wfrc("block group", vehVars, "UT", wfrc_counties, 2017) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

incVars <- paste0("C17002_00", c(1:3))
income17 <- get_acs_wfrc("block group",incVars,"UT",wfrc_counties,2017) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

# Basic Table Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' manipulate all three tables used to determine EFAs. For each table, calculate the percentage
#' for each block group. Additionally, calculate whether or not each block meets the default
#' threshold set a few years ago
MinorityTable2017 <- minority_percent(minority17)
PovertyTable2017 <- poverty_percent(income17)
VehicleTable2017 <- vehicle_percent(vehicles17)

#' join together all four tables
efa2017 <- initial_join17(MinorityTable2017, VehicleTable2017,PovertyTable2017,GroupQuarter2020)


# Join Tables and Region Geography -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' join together the joint table with the block group shapefile to assign a geometry
#' to each of the block group locations. Additionally, filter out all block groups that
#' are not within the WFRC/MAG model region. Also, determine whether or not each 
#' region's percentage is within the calculated thresehold (whether or not it is 
#' at least one standard devation from the region's mean)
efa2017shpb4 <- geometry_calculate(efa2017,wfrc_blockgroups_2017)
# select the columns needed for further analysis
efa2017shp <- efa2017shpb4 %>%
  select(OBJECTID,SHAPE,Geography,Population,Poverty,PercPovert,SD_Pov,Perc_Pov25,Perc_Pov20,Minority,PercMinori,SD_Minorit,Perc_Minorit,ZeroCar,PercZeroCa,SD_ZeroCar,Perc_ZeroCar,HighestStDev,HighestPerc25wCar,HighestPerc20wCar,HighestPerc25woCar,HighestPerc20woCar)

# Create Distribution Histograms-------------------------------------------------------------------------------------------------------------------------------------------------------------#
efaHisto17 <- efa2017shpb4 %>%
  pivot_longer(c(Poverty,Minority,ZeroCar), names_to = "Variable", values_to = "Total") %>%
  mutate(Percentage = ifelse(grepl("Pov",Variable), PercPovert, ifelse(grepl("Min",Variable), PercMinori,PercZeroCa)),
         Assumed = ifelse(grepl("Pov",Variable),0.25,ifelse(grepl("Min",Variable),0.4,0.1)),
         Year = 2017) %>%
  select(OBJECTID,Geography,Year,Population,Variable,Total,Percentage,Assumed)

createHistogram17(efaHisto17,"Minority", 2017)


#Create EFA GeoPackage for Original Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------#
efashp2017_calc1 <- efa2017shp %>% filter(HighestPerc25wCar > 0) %>% 
  mutate(group = "2017 New") %>% delete_low_pop_dens()
efashp2017_calc2 <- efa2017shp %>% filter(HighestStDev > 0) %>% 
  mutate(group = "2017 New") %>% delete_low_pop_dens()

leaflet() %>%
  addPolygons(data = efashp2017_calc1$SHAPE, group = efashp2017_calc1$group, color = "red") %>%
  addPolygons(data = efashp2017_original$geometry, group = efashp2017_original$group) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addLayersControl(
    overlayGroups = c("2017 New", "2017 Old"),
    options = layersControlOptions(collapsed=FALSE)
  )

#st_write(efaPerc2020shp, dsn = "outputs/results/EFAs_2020.gpkg",layer = "EquityFocusAreasPerc2020",append=TRUE)
#st_write(efaSD2020shp, dsn = "outputs/results/EFAs_2020.gpkg", layer = "EquityFocusAreasSD2020", append=TRUE) 


