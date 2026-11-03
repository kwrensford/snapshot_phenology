#Climate Data Exploration
library(tidyverse)

detections <- vroom("data/Kwasi_Detections_2026-10-06.csv")
cam_locations <- vroom("data/cam_locations.csv")

#Terra climate data download

library(terra)

cam_locations$lat <- as.numeric(cam_locations$lat)
cam_locations$lon <- as.numeric(cam_locations$lon)

pts <- vect(
  cam_locations,
  geom = c("lon", "lat"),
  crs = "EPSG:4326"
)

remotes::install_github("julianselke/TerraclimateR")

library(TerraclimateR)

clim <- get_terraclim(
  df = cam_locations,
  id_var = camera_ID,
  lat_var = lat,
  lon_var = lon,
  clim_vars = c("ppt", "tmin", "tmax"),
  conserve = TRUE
)

temp <- rast("annual_temp_2018.tif")

vals <- extract(temp, pts)

