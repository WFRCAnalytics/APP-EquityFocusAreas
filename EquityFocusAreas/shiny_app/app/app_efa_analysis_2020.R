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
source("app_efa_analysis_scripts.R")
census_api_key("0196454888e2441971be7360589dd0399e036978")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Read in the data needed to redetermine the EFAs
wfrc_counties <- c("Utah","Davis","Salt Lake","Weber","Box Elder")

#' TODO: Can shapefiles be read directly from internet? (efashp2017, health and schools, and maybe wfrc boundary)
#' Also, how can I read in the block group data without downloading it?

efashp2017 <- st_read("data/Equity_Focus_Areas/EquityFocusAreas.shp") %>%  st_transform(4326)
wfrcboundary <- st_read("data/WFRCBoundary2018/WFRCBoundary2018.shp") %>% summarize(geometry = st_union(geometry)) %>%
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

groupQuarter <- read_csv("data/DECENNIALPL2020.P5/DECENNIALPL2020.P5_data_with_overlays_2022-06-23T135404.csv")


# Basic Table Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' manipulate all three tables used to determine EFAs. For each table, calculate the percentage
#' for each block group. Additionally, calculate whether or not each block meets the default
#' threshold set a few years ago
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
#' join together the joint table with the block group shapefile to assign a geometry
#' to each of the block group locations. Additionally, filter out all block groups that
#' are not within the WFRC/MAG model region. Also, determine whether or not each 
#' region's percentage is within the calculated thresehold (whether or not it is 
#' at least one standard devation from the region's mean)
efa2020shpb4 <- geometry_calculate(efa2020,wfrc_blockgroups)

# select the columns needed for further analysis
efa2020shp <- efa2020shpb4 %>%
  select(OBJECTID,SHAPE,Geography,Population,TotalHH,Poverty,PercPovert,SD_Pov,Perc_Pov25,Perc_Pov20,Minority,PercMinori,SD_Minorit,Perc_Minorit,ZeroCar,PercZeroCa,SD_ZeroCar,Perc_ZeroCar,HighestStDev,HighestPerc25wCar,HighestPerc20wCar,HighestPerc25woCar,HighestPerc20woCar,IP,IP_Correctional,IP_Juvenile,IP_Nursing,IP_Other,NIP,NIP_College,NIP_Military,NIP_Other)


#Create EFA GeoPackage for Original Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------#
efaPerc2020shp <- efa2020shp %>% filter(HighestPerc25wCar > 0) %>% 
  delete_low_pop_dens(500)
efaSD2020shp <- efa2020shp %>% filter(HighestStDev > 0) %>% 
  delete_low_pop_dens(500)

#st_write(efaPerc2020shp, dsn = "outputs/results/EFAs_2020.gpkg",layer = "EquityFocusAreasPerc2020",append=TRUE)
#st_write(efaSD2020shp, dsn = "outputs/results/EFAs_2020.gpkg", layer = "EquityFocusAreasSD2020", append=TRUE) 


#Create EFA GeoPackage for Secondary Analysis-------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' create more spatial options where we adjust slightly the percentage calculation by using a poverty threshold of 20% instead of 25%
efaPerc2020Pov25NoCarshp <- efa2020shp %>% filter(HighestPerc25woCar > 0) %>% 
  delete_low_pop_dens(500) %>% mutate(group = "2020 Pov-25% Min-40%",color = "blue") 

efaPerc2020Pov20NoCarshp<- efa2020shp %>% filter(HighestPerc20woCar > 0) %>% 
  delete_low_pop_dens(500) %>% mutate(group = "2020 Pov-20% Min-40%",color = "red")

efaPerc2020Pov20Carshp <- efa2020shp %>% filter(HighestPerc20wCar > 0) %>%
  delete_low_pop_dens(500) %>% mutate(group = "2020 Pov-20% Min 40% Veh-10%",color = "yellow")

efaPerc2017shp <- efashp2017 %>%  mutate(Perc_Pov25 = ifelse(PercPovert > 0.25,1,0), Perc_Pov20 = ifelse(PercPovert > 0.2,1,0)) %>%
  mutate(SD_Pov = as.numeric(SD_Pov),SD_Minorit = as.numeric(SD_Minorit),SD_ZeroCar = as.numeric(SD_ZeroCar),HighestStD = as.numeric(HighestStD)) %>%
  mutate(HighestPerc25woCar = pmax(Perc_Pov25,SD_Minorit), HighestPerc20woCar = pmax(Perc_Pov20,SD_Minorit),SHAPE = geometry)
efaPerc2017Pov25NoCarshp <- efaPerc2017shp %>% filter(HighestPerc25woCar > 0) %>% mutate(group = "2017 Pov-25% Min-40%",color = "green")
efaPerc2017Pov20NoCarshp<- efaPerc2017shp %>% filter(HighestPerc20woCar > 0) %>% mutate(group = "2017 Pov-20% Min-40%",color = "purple")
efaPerc2017Pov25Carshp <- efaPerc2017shp %>% filter(HighestStD > 0) %>% mutate(group = "2017 Pov-25% Min-40% Veh-10%", color = "gray")

efaAnalysis <- bind_rows(list(efaPerc2020Pov25NoCarshp,efaPerc2020Pov20NoCarshp,efaPerc2020Pov20Carshp,efaPerc2017Pov25NoCarshp,efaPerc2017Pov20NoCarshp,efaPerc2017Pov25Carshp)) %>%
  mutate(group = as.factor(group))

# TODO: Would geojson be better than geopackage? Look into this.
#st_write(efaPerc2020Pov25NoCarshp, dsn = "outputs/results/EFAs_2020_2.gpkg", layer = "EFAs2020Pov25NoCar",append=TRUE)
#st_write(efaPerc2020Pov20NoCarshp, dsn = "outputs/results/EquityFocusAreas2020v2.gpkg", layer = "EFAs2020Pov20Min40NoCar",append=TRUE)
#st_write(efaPerc2020Pov20Carshp, dsn = "outputs/results/EFAs_2020_2.gpkg", layer = "EFAs2020Pov20Car",append=TRUE)
#st_write(efaPerc2017Pov25NoCarshp, dsn = "outputs/results/EFAs_2017_2.gpkg", layer = "EFA2017Pov25NoCar",append=TRUE)
#st_write(efaPerc2017Pov20NoCarshp, dsn = "outputs/results/EFAs_2017_2.gpkg", layer = "EFA2017Pov20NoCar",append=TRUE)

# FINAL SELECTED EFA Zones for 2020
efa2020FinalZones <- efaPerc2020Pov20NoCarshp %>%
  select(Geography,Population,TotalHH,Poverty,PercPovert,SD_Pov,Perc_Pov20,Minority,PercMinori,SD_Minorit,Perc_Minorit,HighestPerc20woCar,Area_Meters,Area_Miles,PopDens,SHAPE) %>%
  rename("Perc_Minority40" = Perc_Minorit, "HighestPerc" = HighestPerc20woCar, "PercPoverty" = PercPovert, "Perc_Poverty20" = Perc_Pov20, "PercMinority" = PercMinori) %>%
  mutate(HighestStDev = pmax(SD_Pov,SD_Minorit))
#st_write(efa2020FinalZones, dsn = "outputs/results/shps/EquityFocusAreas2020_AllPopDens/EquityFocusAreas2020_AllPopDens.shp", layer = "EFA2020Pov20Min40NoCar",append=TRUE)

# To save as GeoJson (ArcGIS Pro doesn't read GeoJson without paying lots of $$)
#efa2020final_json <- geojson_json(efa2020FinalZones, lat = 'longitude',lon = 'latitude' ,geometry = "polygon")
#geojsonio::geojson_write(efa2020final_json,file = "outputs/results/efa2020.geojson")


# MAP ANALYSIS --------------------------------------------------------------------------------------------------------------------------------------------------------#
#' @param utahHealth A shapefile showing all the health care facilities in Utah
#' @param utahSchools A shapefile showing all the higher education facilities in Utah
#'
#' Health Care Facilities: https://opendata.gis.utah.gov/datasets/f5e5d7c717c946fe9cd6f2c8d0bf8d86_0/explore
#' Higher Education Facilities: https://opendata.gis.utah.gov/datasets/utah-schools-higher-education/explore

utahHealth <- st_read("data/Utah_Health_Care_Facilities/HealthCareFacilities.shp") %>%  st_transform(4326) %>%
  mutate(intersection = as.integer(st_intersects(geometry, wfrcboundary))) %>% filter(is.na(intersection) == FALSE) %>%
  mutate(group = "Health Care Facilities")
utahSchools <- st_read("data/Utah_Schools_Higher_Education/Schools_HigherEducation.shp") %>%  st_transform(4326) %>%
  mutate(intersection = as.integer(st_intersects(geometry, wfrcboundary))) %>% filter(is.na(intersection) == FALSE) %>%
  mutate(group = "Higher Education Facilities")


plot_comparison_map <- function(){
  leaflet() %>%
  addPolygons(data = efaAnalysis$SHAPE, 
              fillOpacity = .5, 
              fillColor = efaAnalysis$color, 
              color = efaAnalysis$color, 
              group = efaAnalysis$group, 
              weight = .8,
              popup = paste("Population:", efaAnalysis$Population, "<br>",
                            "Population Density:", efaAnalysis$PopDens, "<br>",
                            "Poverty %:", efaAnalysis$PercPovert, "<br>",
                            "Minority %:", efaAnalysis$PercMinori, "<br>",
                            "ZeroCar %:", efaAnalysis$PercZeroCa, "<br>",
                            "Institutionalized Population:", efaAnalysis$IP, "<br>",
                            "Nursing/Skilled-Nursing Facilities:", efaAnalysis$IP_Nursing, "<br>",
                            "Noninstitutionalized Population:", efaAnalysis$NIP, "<br>",
                            "College/University Student Housing:", efaAnalysis$NIP_College))%>%
  addProviderTiles(providers$CartoDB.Positron) %>%  # providers$Esri.WorldStreetMap
  addCircleMarkers(data = utahHealth, 
                   radius = 2, 
                   color = "orange",
                   group = utahHealth$group,
                   popup = paste("Name:",utahHealth$NAME, "<br>",
                                 "Type:",utahHealth$TYPE, "<br>",
                                 "City:",utahHealth$CITY)) %>%
  addCircleMarkers(data = utahSchools, 
                   radius = 2, 
                   color = "black", 
                   group = utahSchools$group,
                   popup = paste("Name:", utahSchools$SchoolName, "<br>",
                                 "Type:", utahSchools$SchoolType, "<br>",
                                 "Campus:", utahSchools$CampusType, "<br>",
                                 "City:", utahSchools$City)) %>%
  addLayersControl(
    overlayGroups = c("2020 Pov-25% Min-40%","2020 Pov-20% Min-40%","2020 Pov-20% Min 40% Veh-10%","2017 Pov-25% Min-40%","2017 Pov-20% Min-40%","2017 Pov-25% Min-40% Veh-10%",utahHealth$group,utahSchools$group),
    options = layersControlOptions(collapsed=FALSE)
  )
}


# TABLE ANALYSIS -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' create a table comparing the default thresholds and the calculated thresholds
#' based on this 2020 data
efatestd<- percentage_table(efa2020shpb4)

efatestd %>% 
  kbl(caption = "Equity Focus Area Thresholds") %>%
  kable_classic(full_width = F, html_font = "Cambria")


# HISTOGRAM ANALYSIS -------------------------------------------------------------------------------------------------------------------------------------------------------------#
efaHisto20 <- efa2020shpb4 %>%
  mutate(calcPov = round(sdPov+meanPov,3), assumePov = 0.25,
         calcMin = round(sdMin+meanMin,3), assumeMin = 0.40,
         calcVeh = round(sdVeh+meanVeh,3), assumeVeh = 0.10) %>%
  select(OBJECTID,Geography,Population,Poverty,PercPovert,meanPov,sdPov,calcPov,assumePov,Minority,PercMinori,meanMin,sdMin,calcMin,assumeMin,ZeroCar,PercZeroCa,meanVeh,sdVeh,calcVeh,assumeVeh,SHAPE) %>%
  pivot_longer(c(Poverty,Minority,ZeroCar), names_to = "Variable", values_to = "Total") %>%
  mutate(Percentage = ifelse(grepl("Pov",Variable), PercPovert, ifelse(grepl("Min",Variable), PercMinori,PercZeroCa)),
         Calculated = ifelse(grepl("Pov",Variable),calcPov,ifelse(grepl("Min",Variable),calcMin,calcVeh)),
         Assumed = ifelse(grepl("Pov",Variable),assumePov,ifelse(grepl("Min",Variable),assumeMin,assumeVeh)),
         Year = 2020) %>%
  select(OBJECTID,Geography,Year,Population,Variable,Total,Percentage,Calculated,Assumed)

#createHistogram(efaHisto20,"ZeroCar", 2020)
#createHistogram(efaHisto20, "Minority", 2020)
#createHistogram(efaHisto20, "Poverty", 2020)

