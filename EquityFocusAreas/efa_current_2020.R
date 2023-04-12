#' Redetermine Equity Focus Areas (EFAs) for the WFRC/MAG model region using updated
#' data (2020). EFAs are determined using ACS data relating to Income (Table C17002),
#' Minority/Ethnicity (Table B03002) and Household Vehicles (Table B25044). These tables
#' were downloaded and analyzed at the block level and downloaded for the following counties:
#' Box Elder, Davis, Weber, Salt Lake, Utah
#'
#'@param efashp A shapefile containing the previous EFAs, I beleive form 2017 data
#'@param wfrcboundary A shapefile containing the region of the WFRC/MAG model
#'@param wfrc_blockgroups A spatial object containing the 2020 Utah Block group geographies
#'@param minority Table B03002 from ACS data, organized by block groups in the region
#'@param vehicles Table B25044 from ACS data, organized by block groups in the region
#'@param income Table C17002 from ACS data, organized by block groups in region
#'@param groupQuarter Table P5 from ACS data, organized by block groups in region
#'
#' Old EFAs: https://data.wfrc.org/datasets/equity-focus-areas/explore?location=40.772488%2C-111.854934%2C9.71&showTable=true
#' Group Quarter Table: https://data.census.gov/cedsci/table?q=Group%20Quarter&g=0500000US49003%241500000,49011%241500000,49035%241500000,49049%241500000,49057%241500000&y=2020&tid=DECENNIALPL2020.P5
#'
#'@return Geopackages containing the updated EFAs calculated from 2020 data. 
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
library(geojsonio)
library(geojsonlint)
source("EquityFocusAreas/efa_analysis_scripts.R")
census_api_key("0196454888e2441971be7360589dd0399e036978")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Read in the data needed to redetermine the EFAs
wfrc_counties <- c("Utah","Davis","Salt Lake","Weber","Box Elder")
wfrcboundary <- st_read("EquityFocusAreas/data/WFRCBoundary2018/WFRCBoundary2018.shp") %>% summarize(geometry = st_union(geometry)) %>%
  st_transform(4326)
wfrc_blockgroups <- block_groups("UT",county = wfrc_counties,cb=TRUE) %>%
  st_transform(4326)

minVars <- paste0("B03002_00", c(1,3))
minority20 <- get_acs_wfrc("block group", minVars, "UT", wfrc_counties,2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))
  
vehVars <- c(paste0("B25044_00",c(1,3)),"B25044_010")
vehicles20 <- get_acs_wfrc("block group", vehVars, "UT", wfrc_counties, 2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

incVars <- paste0("C17002_00", c(1:3))
income20 <- get_acs_wfrc("block group",incVars,"UT",wfrc_counties,2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

groupQuarter <- read_csv("EquityFocusAreas/data/DECENNIALPL2020.P5/DECENNIALPL2020.P5_data_with_overlays_2022-06-23T135404.csv")


# Basic Table Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' manipulate all three tables used to determine EFAs. For each table, calculate the percentage for each block group. Additionally, calculate whether or not each block meets the default threshold set a few years ago
MinorityTable2020 <- minority_percent(minority20)
PovertyTable2020 <- poverty_percent(income20)
VehicleTable2020 <- vehicle_percent(vehicles20)

# Extra data to display on the final map
GroupQuarter2020 <- groupQuarter [-1,] %>%
  rename("IP" = P5_002N, "IP_Correctional" = P5_003N,"IP_Juvenile" = P5_004N, "IP_Nursing" = P5_005N, "IP_Other" = P5_006N,
         "NIP" = P5_007N, "NIP_College"=P5_008N,"NIP_Military"=P5_009N,"NIP_Other"=P5_010N) %>%
  mutate(IP = as.numeric(IP),IP_Correctional=as.numeric(IP_Correctional),IP_Juvenile=as.numeric(IP_Juvenile),IP_Nursing=as.numeric(IP_Nursing),IP_Other=as.numeric(IP_Other),
         NIP = as.numeric(NIP),NIP_College=as.numeric(NIP_College),NIP_Military=as.numeric(NIP_Military),NIP_Other=as.numeric(NIP_Other),
         GEOID = substring(GEO_ID,10)) %>% select(-GEO_ID)

#' join together all four tables
efa2020 <- initial_join20(MinorityTable2020,VehicleTable2020,PovertyTable2020,GroupQuarter2020)


# Join Tables and Region Geography -------------------------------------------------------------------------------------------------------------------------------------------------------------#
efa2020shpb4 <- geometry_calculate(efa2020,wfrc_blockgroups)

# select the columns needed for further analysis
efa2020shp <- efa2020shpb4 %>%
  select(OBJECTID,SHAPE,Geography,Population,TotalHH,Poverty,PercPovert,SD_Pov,Perc_Pov25,Perc_Pov20,Minority,PercMinori,SD_Minorit,Perc_Minorit,ZeroCar,PercZeroCa,SD_ZeroCar,Perc_ZeroCar,HighestStDev,HighestPerc25wCar,HighestPerc20wCar,HighestPerc25woCar,HighestPerc20woCar,IP,IP_Correctional,IP_Juvenile,IP_Nursing,IP_Other,NIP,NIP_College,NIP_Military,NIP_Other)


# FINAL SELECTED EFA Zones for 2020-------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' create more spatial options where we adjust slightly the percentage calculation by using a poverty threshold of 20% instead of 25%
efaPerc2020Pov20NoCarshp<- efa2020shp %>% filter(HighestPerc20woCar > 0) %>% 
  delete_low_pop_dens(0) %>% mutate(group = "2020 Pov-20% Min-40%",color = "red")
#efaPerc2020Pov20NoCarshp<- efa2020shp %>% filter(HighestPerc20woCar > 0) %>% 
#  delete_low_pop_dens(500) %>% mutate(group = "2020 Pov-20% Min-40%",color = "red")

# FINAL SELECTED EFA Zones for 2020
efa2020FinalZones <- efaPerc2020Pov20NoCarshp %>%
  select(Geography,Population,TotalHH,Poverty,PercPovert,SD_Pov,Perc_Pov20,Minority,PercMinori,SD_Minorit,Perc_Minorit,HighestPerc20woCar,Area_Meters,Area_Miles,PopDens,SHAPE) %>%
  rename("Perc_Minority40" = Perc_Minorit, "HighestPerc" = HighestPerc20woCar, "PercPoverty" = PercPovert, "Perc_Poverty20" = Perc_Pov20, "PercMinority" = PercMinori) %>%
  mutate(HighestStDev = pmax(SD_Pov,SD_Minorit))

efa2020FinalZones_rename = efa2020FinalZones %>%
  rename("Pov" = Poverty,
         "PctPov" = PercPoverty,
         "Pct_Pov20" = Perc_Poverty20,
         "Min" = Minority,
         "PctMin" = PercMinority,
         "SD_Min" = SD_Minorit,
         "Pct_Min40" = Perc_Minority40,
         "HighPct" = HighestPerc,
         "HighSD" = HighestStDev,
         "Area_Mets" = Area_Meters,
         "Area_Mils" = Area_Miles)

st_write(efa2020FinalZones_rename, dsn = "EquityFocusAreas/results/shps/EquityFocusAreas2020_AllPopDens/EquityFocusAreas2020_AllPopDens.shp", layer = "EFA2020Pov20Min40NoCar",append=TRUE)

