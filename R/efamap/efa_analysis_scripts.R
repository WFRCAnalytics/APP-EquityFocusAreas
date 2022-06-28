
#Table Manipulation Functions -------------------------------------------------------------------------------------------------------------------------------------------------------------------------#
get_acs_wfrc <- function(geo,vars,state,counties,year){
  get_acs(geography = geo,
          variables = vars,
          state = state,
          county = counties,
          year = year
  )
}

minority_percent <- function(minority){
  minority %>%
    rename("Population"=B03002_001, "White_E"=B03002_003) %>%
    mutate(Minority = Population - White_E) %>%
    mutate("PercMinori" = round(Minority/Population,3)) %>%
    select(NAME,GEOID,Population,Minority,PercMinori) %>%
    mutate(Perc_Minorit = ifelse(PercMinori > 0.4, 1,0))
}

poverty_percent <- function(income){
  income %>%
    rename("pop" = C17002_001, "superpoor_e" = C17002_002, "poor_e" = C17002_003) %>%
    mutate(Poverty = superpoor_e + poor_e) %>%
    mutate(PercPovert = round(Poverty/pop,3)) %>%
    select(NAME,GEOID,Poverty,PercPovert) %>%
    mutate(Perc_Pov25 = ifelse(PercPovert > 0.25,1,0),
           Perc_Pov20 = ifelse(PercPovert > 0.2,1,0))
}

vehicle_percent <- function(vehicles){
  vehicles %>%
    rename("TotalHH"=B25044_001, "OwnerZeroVehs_E"=B25044_003, "RenterZeroVehs_E" = B25044_010) %>%
    mutate(ZeroCar = OwnerZeroVehs_E + RenterZeroVehs_E) %>%
    mutate(PercZeroCa = round(ZeroCar/TotalHH,3)) %>%
    select(NAME,GEOID,ZeroCar,PercZeroCa) %>%
    mutate(Perc_ZeroCar = ifelse(PercZeroCa > 0.1,1,0))
}

initial_join20 <- function(MinorityTable,VehicleTable,PovertyTable,GroupQuarter){
  left_join(MinorityTable,VehicleTable,by= c("NAME","GEOID")) %>%
    left_join(PovertyTable,by=c("NAME","GEOID")) %>%
    left_join(GroupQuarter, by = c("NAME","GEOID")) %>%
    mutate(HighestPerc25wCar = pmax(Perc_Pov25,Perc_ZeroCar,Perc_Minorit),
           HighestPerc20wCar = pmax(Perc_Pov20,Perc_ZeroCar,Perc_Minorit),
           HighestPerc25woCar = pmax(Perc_Pov25,Perc_Minorit),
           HighestPerc20woCar = pmax(Perc_Pov20,Perc_Minorit))
}
initial_join17 <- function(MinorityTable,VehicleTable,PovertyTable,GroupQuarter){
  left_join(MinorityTable,VehicleTable,by= c("NAME","GEOID")) %>%
    left_join(PovertyTable,by=c("NAME","GEOID")) %>%
    mutate(HighestPerc25wCar = pmax(Perc_Pov25,Perc_ZeroCar,Perc_Minorit),
           HighestPerc20wCar = pmax(Perc_Pov20,Perc_ZeroCar,Perc_Minorit),
           HighestPerc25woCar = pmax(Perc_Pov25,Perc_Minorit),
           HighestPerc20woCar = pmax(Perc_Pov20,Perc_Minorit))
}

geometry_calculate <- function(efaInitial,wfrc_blockgroups){
  left_join(efaInitial,wfrc_blockgroups,by="GEOID") %>%
    mutate("Geography"=NAME.x) %>%
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
}

delete_low_pop_dens <- function(table){
  table %>% 
    mutate(Area_Meters = as.numeric(st_area(SHAPE))) %>%
    mutate(Area_Miles = Area_Meters / 2589988.11) %>%
    mutate(PopDens = round(Population / Area_Miles),5) %>%
    filter(PopDens > 2000)
}


# Visual Analysis Functions ---------------------------------------------------------------------------------------------------------------------------------------------------------#
percentage_table <- function(efa2020shpb4){
  efa2020shpb4 %>%
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
}

createHistogram <- function(histoTable, variable, year){
  varTable <- histoTable %>% filter(Variable == variable, Year == year)
  
  ggplot(varTable, aes(x = Percentage)) +
    geom_histogram(binwidth = 0.025) + 
    geom_vline(data = varTable, aes(xintercept = Calculated, color = "Calculated Threshold"), size = 1) +
    geom_vline(data = varTable, aes(xintercept = Assumed, color = "Previous Threshold"), size = 1) +
    theme_bw() 
} 

createHistogram17 <- function(histoTable, variable, year){
  varTable <- histoTable %>% filter(Variable == variable, Year == year)
  
  ggplot(varTable, aes(x = Percentage)) +
    geom_histogram(binwidth = 0.025) + 
    geom_vline(data = varTable, aes(xintercept = Assumed, color = "Previous Threshold"), size = 1) +
    theme_bw() 
} 



