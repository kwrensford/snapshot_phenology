#Download Vegetation Data

library(tidyverse)
library(vroom)
library(MODISTools)

cam_locations <- vroom("data/climatena_input.csv")

cam_locations <- cam_locations %>%
  select(ID1, lat, long) %>%
  rename(site_name = ID1, lat = lat, lon = long)

mt_bands(product = "MYD13Q1")

evi_data <- mt_batch_subset(
  df = cam_locations,
  product = "MYD13Q1",
  band = "250m_16_days_EVI",
  start = "2018-01-01",
  end = "2025-12-31"
)
