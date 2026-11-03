#Climate NA Download Snapshot Wisconsin

library(dplyr)
library(tidyr)
library(ggplot2)
library(ClimateNAr)
library(data.table)
library(elevatr)
library(sf)

#Camera locations
cam_locations <- readRDS("data/Kwasi_Effort_Nested_2026-10-06.rds")

cam_locations <- cam_locations %>%
  select(camera_location_seq_no, lat, lon) %>%
  rename(ID1 = camera_location_seq_no,
         long = lon)

write.csv(cam_locations, file = "data/cam_locations.csv")


#Dowload climate data for all camera locations
climate_years <- c("Year_2018.ann",
                   "Year_2019.ann", 
                   "Year_2020.ann", 
                   "Year_2021.ann", 
                   "Year_2022.ann",
                   "Year_2023.ann",
                   "Year_2024.ann",
                   "Year_2025.ann")


climate_list <- lapply(climate_years, function(yr){
  ClimateNA_API2(
    ClimateBC_NA = "NA",
    inputFile = "data/cam_locations.csv",
    period = yr,
    MSY = "MSY"
  )
})

#Throttled loop that automatically pauses after fifth call
climate_list <- vector("list", length(climate_years))

for(i in seq_along(climate_years)) {
  
  yr <- climate_years[i]
  
  message("Running: ", yr)
  
  climate_list[[i]] <- ClimateNA_API2(
    ClimateBC_NA = "NA",
    inputFile = "data/cam_locations.csv",
    period = yr,
    MSY = "MSY"
  )
  
  if(i == 5) {
    message("API limit reached. Waiting 1 hour...")
    Sys.sleep(60 * 60)
  }
  
}

remaining_years <- climate_years[6:8]

climate_list2 <- lapply(remaining_years, function(yr){
  
  message("Starting: ", yr, " at ", Sys.time())
  
  ClimateNA_API2(
    ClimateBC_NA = "NA",
    inputFile = "data/cam_locations.csv",
    period = yr,
    MSY = "MSY"
  )
  
})

#Get Elevation

# dataframe with lon and lat columns
pts <- st_as_sf(
  cam_locations,
  coords = c("long", "lat"),
  crs = 4326
)

elev <- get_elev_point(
  pts,
  src = "aws"
)

cam_locations$elevation <- elev$elevation

write.csv(cam_locations, file = "climatena_input.csv")
