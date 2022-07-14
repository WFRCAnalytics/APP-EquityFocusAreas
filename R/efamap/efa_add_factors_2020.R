#' 
#'
#'@param  
#'

#'@return 
#'
#'@details 
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
source("efa_analysis_scripts.R")
#source("R/efamap/efa_analysis_scripts.R")
census_api_key("0196454888e2441971be7360589dd0399e036978")


#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
# Read in the data needed to redetermine the EFAs
wfrc_counties <- c("Utah","Davis","Salt Lake","Weber","Box Elder")

wfrcboundary <- st_read("data/WFRCBoundary2018/WFRCBoundary2018.shp") %>% summarize(geometry = st_union(geometry)) %>%
  st_transform(4326)
wfrc_blockgroups <- block_groups("UT",county = wfrc_counties,cb=TRUE) %>%
  st_transform(4326)

# Minority Individuals
minVars <- paste0("B03002_00", c(1,3))
minority20 <- get_acs_wfrc("block group", minVars, "UT", wfrc_counties,2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

# Poverty Individuals
incVars <- paste0("C17002_00", c(1:3))
income20 <- get_acs_wfrc("block group",incVars,"UT",wfrc_counties,2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

# Zero Car Households
vehVars <- c(paste0("B25044_00",c(1,3)),"B25044_010")
vehicles20 <- get_acs_wfrc("block group", vehVars, "UT", wfrc_counties, 2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

# All Ages
ageVars <- c(paste0("B01001_00", c(1:9)),paste0("B01001_0",c(10:49)))
age20 <- get_acs_wfrc("block group", ageVars, "UT", wfrc_counties, 2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

# Disability by Tract
disVars <- c(paste0("B18101_00",c(1:9)),paste0("B18101_0",c(10:39)))
dis20 <- get_acs_wfrc("tract", disVars, "UT", wfrc_counties, 2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))

# Limited English Households
engVars <- c(paste0("C16002_00",c(1:9)),paste0("C16002_0",c(10:14)))
eng20 <- get_acs_wfrc("block group", engVars, "UT", wfrc_counties, 2020) %>% select(-moe) %>%
  pivot_wider(names_from = variable,values_from = c(estimate))



# Basic Table Analysis -------------------------------------------------------------------------------------------------------------------------------------------------------------#
minorityTable <- minority20 %>%
  rename("Total"=B03002_001, "White_E"=B03002_003) %>%
  mutate(FactorTotal = Total - White_E) %>%
  mutate(Percent = FactorTotal/Total,
         Factor = "minority") %>%
  select(NAME,GEOID,Factor,Percent,Total,FactorTotal)

povertyTable <- income20 %>%
  rename("Total" = C17002_001, "superpoor_e" = C17002_002, "poor_e" = C17002_003) %>%
  mutate(FactorTotal = superpoor_e + poor_e) %>%
  mutate(Percent = FactorTotal/Total,
         Factor = "poverty") %>%
  select(NAME,GEOID,Factor,Percent,Total,FactorTotal)

zeroCarTable <-   vehicles20 %>%
  rename("Total"=B25044_001, "OwnerZeroVehs_E"=B25044_003, "RenterZeroVehs_E" = B25044_010) %>%
  mutate(FactorTotal = OwnerZeroVehs_E + RenterZeroVehs_E) %>%
  mutate(Percent = FactorTotal/Total,
         Factor = "zeroCar") %>%
  select(NAME,GEOID,Factor,Percent,Total,FactorTotal)

ageTable <- age20 %>%
  rename("Total" = B01001_001) %>%
  mutate(PopUnder18 = B01001_003 + B01001_004 + B01001_005 + B01001_006 + B01001_027 + B01001_028 + B01001_029 + B01001_030,
         Pop65P = B01001_020 + B01001_021 + B01001_022 + B01001_023 + B01001_025 + B01001_025 + B01001_044 + B01001_045 + B01001_046 + B01001_047 + B01001_048 + B01001_049) %>%
  select(NAME,GEOID,Total,PopUnder18,Pop65P)
ageUnder18 <- ageTable %>%
  rename("FactorTotal" = PopUnder18) %>%
  mutate(Factor = "ageUnder18", Percent = FactorTotal/Total) %>%
  select(NAME,GEOID,Factor,Percent,Total,FactorTotal)
age65P <- ageTable %>%
  rename("FactorTotal" = Pop65P) %>%
  mutate(Factor = "age65P", Percent = FactorTotal/Total) %>%
  select(NAME,GEOID,Factor,Percent,Total,FactorTotal)

disTable <- dis20 %>%
  rename("Total" = B18101_001) %>%
  mutate(FactorTotal = B18101_004 + B18101_007 + B18101_010 + B18101_013 + B18101_016 + B18101_019 + B18101_023 + B18101_026 + B18101_029 + B18101_032 + B18101_035 + B18101_038) %>%
  mutate(Factor = "diability", Percent = FactorTotal/Total) %>%
  select(NAME,GEOID,Factor,Percent)

engTable <- eng20 %>%
  rename("Total" = C16002_001) %>%
  mutate(FactorTotal = C16002_004 + C16002_007 + C16002_010 + C16002_013) %>%
  mutate(Percent = FactorTotal/Total,
         Factor = "limitedEnglish") %>%
  select(NAME,GEOID,Factor,Percent,Total,FactorTotal)


# Join Tables and Filter to WFRC Region
additional_factors <- bind_rows(minorityTable,povertyTable,zeroCarTable,ageUnder18,age65P,engTable) %>%
  left_join(wfrc_blockgroups,by="GEOID") %>%
  mutate("Geography"=NAME.x) %>%
  mutate(within = as.character(st_within(geometry,wfrcboundary$geometry))) %>%
  filter(within == "1",is.na(Percent)==FALSE) %>%
  select(Geography,GEOID,Factor,Percent,Total,FactorTotal,geometry)

# Determine thresholds that are 1sd above the mean
add_factors_long <- additional_factors %>%
  group_by(Factor) %>%
  mutate(mean = mean(Percent), sd = sd(Percent)) %>%
  mutate(threshold = mean+sd) %>%
  ungroup()

add_factors_long_clean <- add_factors_long %>%
  #delete low pop density
  mutate(SDMEAN = (Percent - mean)/sd) %>%
  #filter(SDMEAN > 0) %>%
  select(-mean,-sd)


add_factors_wide <- add_factors_long %>%
  pivot_wider(names_from = "Factor",values_from = c("Percent","Total","FactorTotal","mean","sd","threshold"))
  #left join population and tothh
  #filter out low pop


# MAP ANALYSIS --------------------------------------------------------------------------------------------------------------------------------------------------------#

plot_add_factors_map <- function(selected_factors,slider1,slider2,slider3,slider4,slider5,slider6,weight1,weight2,weight3,weight4,weight5,weight6){

    filtered_factors <- add_factors_long_clean %>%
      mutate(weight = case_when(
        Factor == "minority" ~ as.numeric(weight1),
        Factor == "poverty" ~ as.numeric(weight2),
        Factor == "zeroCar" ~ as.numeric(weight3),
        Factor == "ageUnder18" ~ as.numeric(weight4),
        Factor == "age65P" ~ as.numeric(weight5),
        Factor == "limitedEnglish" ~ as.numeric(weight6)
      )) %>%
      mutate(score = weight*SDMEAN) %>%
      mutate(wantedThreshold = case_when(
        Factor == "minority" ~ slider1,
        Factor == "poverty" ~ slider2,
        Factor == "zeroCar" ~ slider3,
        Factor == "ageUnder18" ~ slider4,
        Factor == "age65P" ~ slider5,
        Factor == "limitedEnglish" ~ slider6
      )) %>%
      filter(Percent > wantedThreshold/100) %>%
      filter(Factor %in% selected_factors) %>%
      group_by(Geography) %>%
      summarize(sum = sum(score)) %>%
      left_join(add_factors_wide, by = c("Geography")) %>%
      rename("score" = sum)
    
  pal <- colorQuantile(palette = "Blues", domain =filtered_factors$score, na.color = "black", n = 4)
  filtered_factors <- sf::st_as_sf(filtered_factors)
  
  leaflet(data = filtered_factors) %>%
    addPolygons(
              fillOpacity = 1,
              fillColor = ~pal(filtered_factors$score),
              color = "white",
              weight = .8
    ) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%  # providers$Esri.WorldStreetMap
    addLegend(
      "bottomright",
      pal = pal,
      values = filtered_factors$score,
      title = "Legend",
      opacity = 1
    )
}




