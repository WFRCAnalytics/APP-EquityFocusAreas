#' Redetermine Equity Focus Areas (EFAs) for the WFRC/MAG model region using updated
#' data (2020). EFAs are determined using ACS data relating to Income (Table C17002),
#' Minority/Ethnicity (Table B03002) and Household Vehicles (Table B25044). These tables
#' were downloaded and analyzed at the block level and downloaded for the following counties:
#' Box Elder, Davis, Weber, Salt Lake, Utah
#'
#'@param efashp A shapefile containing the previous EFAs, I beleive form 2017 data
#'@param wfrcboundary A shapefile containing the region of the WFRC/MAG model
#'@param utahblockshp A shapefile containing the 2020 Utah Block group geographies
#'@param minority Table B03002 from ACS data, organized by block groups in the region
#'@param vehicles Table B25044 from ACS data, organized by block groups in the region
#'@param income Table C17002 from ACS data, organized by block groups in region
#'
#' Old EFAs: https://data.wfrc.org/datasets/equity-focus-areas/explore?location=40.772488%2C-111.854934%2C9.71&showTable=true
#' Utah Block Groups 2020: https://opendata.gis.utah.gov/datasets/utah-census-block-groups-2020/explore?location=39.472883%2C-111.547240%2C-1.00
#' Minority Table: https://data.census.gov/cedsci/table?q=B03002&g=0500000US49003%241500000,49011%241500000,49035%241500000,49049%241500000,49057%241500000&y=2020
#' Vehicle Table: https://data.census.gov/cedsci/table?q=B25044&g=0500000US49003%241500000,49011%241500000,49035%241500000,49049%241500000,49057%241500000&y=2020
#' Income Table: https://data.census.gov/cedsci/table?q=C17002&g=0500000US49003%241500000,49011%241500000,49035%241500000,49049%241500000,49057%241500000&y=2020
#'
#'@return Two new shapefiles containing the updated EFAs calculated from 2020 data. 
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

# Read in the data needed to redetermine the EFAs
efashp <- st_read("outputs/results/Equity_Focus_Areas/EquityFocusAreas.shp") 
wfrcboundary <- st_read("data/WFRCBoundary2018/WFRCBoundary2018.shp") %>% summarize(geometry = st_union(geometry)) %>%
  st_transform(4326)
utahblocksshp <- st_read("data/Utah_Census_Block_Groups_2020/CensusBlockGroups2020.shp") %>% 
  select(GEOID20,SHAPE_Leng,SHAPE_Area) %>%
  st_transform(4326)
minority <- read_csv("data/ACSDT5Y2020.B03002/ACSDT5Y2020.B03002_data_with_overlays_2022-06-07T113506.csv")
vehicles <- read_csv("data/ACSDT5Y2020.B25044/ACSDT5Y2020.B25044_data_with_overlays_2022-06-07T123124.csv")
income <- read_csv("data/ACSDT5Y2020.C17002/ACSDT5Y2020.C17002_data_with_overlays_2022-06-07T123423.csv")


# Basic Table Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' manipulate all three tables used to determine EFAs. For each table, calculate the percentage
#' for each block group. Additionally, calculate whether or not each block meets the default
#' threshold set a few years ago
MinorityTable2020 <- minority[-1,] %>%
  rename("Population"=B03002_001E, "White_E"=B03002_003E, "White_M" = B03002_003M) %>%
  mutate(Population = as.numeric(Population), White_E = as.numeric(White_E), White_M = as.numeric(White_M)) %>%
  mutate(Minority = Population - White_E) %>%
  mutate("PercMinori" = round(Minority/Population,3)) %>%
  select(NAME,GEO_ID,Population,Minority,PercMinori) %>%
  mutate(Perc_Minorit = ifelse(PercMinori > 0.4, 1,0))
VehicleTable2020 <- vehicles[-1,] %>%
  rename("TotalHH"=B25044_001E, "OwnerZeroVehs_E"=B25044_003E,"OwnerZeroVehs_M"=B25044_003M, "RenterZeroVehs_E" = B25044_010E,"RenterZeroVehs_M" = B25044_010M) %>%
  mutate(TotalHH = as.numeric(TotalHH), OwnerZeroVehs_E = as.numeric(OwnerZeroVehs_E), OwnerZeroVehs_M = as.numeric(OwnerZeroVehs_M), RenterZeroVehs_E = as.numeric(RenterZeroVehs_E), RenterZeroVehs_M = as.numeric(RenterZeroVehs_M)) %>%
  mutate(ZeroCar = OwnerZeroVehs_E + RenterZeroVehs_E) %>%
  mutate(PercZeroCa = round(ZeroCar/TotalHH,3)) %>%
  select(NAME,GEO_ID,ZeroCar,PercZeroCa) %>%
  mutate(Perc_ZeroCar = ifelse(PercZeroCa > 0.1,1,0))
PovertyTable2020 <- income[-1,] %>%
  rename("pop" = C17002_001E, "superpoor_e" = C17002_002E, "superpoor_m" = C17002_002M, "poor_e" = C17002_003E, "poor_m" = C17002_003M) %>%
  mutate(pop = as.numeric(pop), superpoor_e = as.numeric(superpoor_e), superpoor_m = as.numeric(superpoor_m), poor_e = as.numeric(poor_e), poor_m = as.numeric(poor_m)) %>%
  mutate(Poverty = superpoor_e + poor_e) %>%
  mutate(PercPovert = round(Poverty/pop,3)) %>%
  select(NAME,GEO_ID,Poverty,PercPovert) %>%
  mutate(Perc_Pov25 = ifelse(PercPovert > 0.25,1,0),
         Perc_Pov20 = ifelse(PercPovert > 0.2,1,0))

#' join together all three tables
efa2020 <- left_join(MinorityTable2020,VehicleTable2020,by= c("NAME","GEO_ID")) %>%
  left_join(PovertyTable2020,by=c("NAME","GEO_ID")) %>%
  mutate(HighestPerc25wCar = pmax(Perc_Pov25,Perc_ZeroCar,Perc_Minorit),
         HighestPerc20wCar = pmax(Perc_Pov20,Perc_ZeroCar,Perc_Minorit),
         HighestPerc25woCar = pmax(Perc_Pov25,Perc_Minorit),
         HighestPerc20woCar = pmax(Perc_Pov20,Perc_Minorit),) %>%
  mutate(GEOID20 = substring(GEO_ID,10))


# Join Tables and Region Geography -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' join together the joint table with the block group shapefile to assign a geometry
#' to each of the block group locations. Additionally, filter out all block groups that
#' are not within the WFRC/MAG model region. Also, determine whether or not each 
#' region's percentage is within the calculated thresehold (whether or not it is 
#' at least one standard devation from the region's mean)
efa2020shpb4 <- left_join(efa2020,utahblocksshp,by="GEOID20") %>%
  rename("Geography" = NAME) %>%
  mutate(within = as.character(st_within(geometry,wfrcboundary$geometry))) %>%
  filter(within == "1",is.na(PercZeroCa)==FALSE) %>%
  mutate(meanMin = mean(PercMinori),
         sdMin = sd(PercMinori),
         meanVeh = mean(PercZeroCa),
         sdVeh = sd(PercZeroCa),
         meanPov = mean(PercPovert),
         sdPov = sd(PercPovert)) %>%
  mutate(povsd = (PercPovert - meanPov)/sdPov,
         minsd = (PercMinori - meanMin)/sdMin,
         vehsd = (PercZeroCa - meanVeh)/sdVeh) %>%
  mutate(SD_Pov = ifelse(povsd >= 2, 2, ifelse(povsd >= 1, 1, 0)),
         SD_ZeroCar = ifelse(vehsd >= 2, 2, ifelse(vehsd >= 1, 1, 0)),
         SD_Minorit = ifelse(minsd >= 2, 2, ifelse(minsd >= 1, 1, 0))) %>%
  mutate(HighestStDev = pmax(SD_Pov,SD_ZeroCar,SD_Minorit)) %>%
  mutate(OBJECTID = row_number(), SHAPE=geometry)
# select the columns needed for further analysis
efa2020shp <- efa2020shpb4 %>%
  select(OBJECTID,SHAPE,Geography,Population,Poverty,PercPovert,SD_Pov,Perc_Pov25,Perc_Pov20,Minority,PercMinori,SD_Minorit,Perc_Minorit,ZeroCar,PercZeroCa,SD_ZeroCar,Perc_ZeroCar,HighestStDev,HighestPerc25wCar,HighestPerc20wCar,HighestPerc25woCar,HighestPerc20woCar,SHAPE_Leng,SHAPE_Area)

# Create Threshold Comparison Table -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' create a table comparing the default thresholds and the calculated thresholds
#' based on this 2020 data
efatestd<- efa2020shpb4 %>%
  summarize(calculatedPovPerc = round(sdPov+meanPov,3),
            assumedPovPerc = 0.250,
            calculatedMinPerc = round(sdMin+meanMin,3),
            assumedMinPerc = 0.400,
            calculatedVehPerc = round(sdVeh+meanVeh,3),
            assumedVehPerc = 0.100) %>%
  unique() %>%
  pivot_longer(everything(),values_to = "values", names_to = "headers") %>%
  mutate(Type = ifelse(grepl("calc",headers),"Calculated","Assumed"),
         Category = ifelse(grepl("Pov",headers),"Poverty",ifelse(grepl("Min",headers),"Minority","ZeroCar"))) %>%
  select(-headers)%>%
  pivot_wider(names_from=Category,values_from = c(values))

efatestd %>% 
  kbl(caption = "Equity Focus Area Thresholds") %>%
  kable_classic(full_width = F, html_font = "Cambria")


#Create New EFA Shapefiles for Original Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' create a geopackage of the original efa analysis. 
efaPerc2020shp <- efa2020shp %>% filter(HighestPerc25wCar > 0) %>% 
  delete_low_pop_dens()
efaSD2020shp <- efa2020shp %>% filter(HighestStDev > 0) %>% 
  delete_low_pop_dens()

st_write(efaPerc2020shp, dsn = "outputs/results/Equity_Focus_Areas.gpkg",layer = "EquityFocusAreasPerc2020",append=TRUE)
st_write(efaSD2020shp, dsn = "outputs/results/Equity_Focus_Areas.gpkg", layer = "EquityFocusAreasSD2020",append=TRUE) 


#Create New EFA Shapefiles for Secondary Analysis-------------------------------------------------------------------------------------------------------------------------------------------------------------#
#' create another shapefile where we adjust slightly the percentage calculation by using a poverty threshold of 20% instead of 25%
efaPerc2020Pov25NoCarshp <- efa2020shp %>% filter(HighestPerc25woCar > 0) %>% 
  delete_low_pop_dens()

efaPerc2020Pov20NoCarshp<- efa2020shp %>% filter(HighestPerc20woCar > 0) %>% 
  delete_low_pop_dens()

efaPerc2020Pov20Carshp <- efa2020shp %>% filter(HighestPerc20wCar > 0) %>%
  delete_low_pop_dens()



# View in Maps-------------------------------------------------------------------------------------------------------------------------------------------------------------#
leaflet() %>%
  addPolygons(data = efaPerc2020Pov20NoCarshp$SHAPE, fillColor = "red",fillOpacity = 1, color = "red") %>%
  addPolygons(data = efaPerc2020Pov25NoCarshp$SHAPE, fillColor = "black",fillOpacity = 1, color = "black") %>%
  addProviderTiles(providers$Esri.WorldStreetMap)




#functions -------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
delete_low_pop_dens <- function(table){
  table %>% 
    mutate(Area_Meters = as.numeric(st_area(SHAPE))) %>%
    mutate(Area_Miles = Area_Meters / 2589988.11) %>%
    mutate(PopDens = Population / Area_Miles) %>%
    filter(PopDens > 2000)
}
