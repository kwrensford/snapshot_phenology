#Download Vegetation Data

library(tidyverse)
library(vroom)
library(MODISTools)

cam_locations <- vroom("data/climatena_input.csv")



evi_data <- mt_batch_subset(
  df = cam_locations
  product = "MYD13Q1",
  band = "30",
  start = "2018-01-01",
  end = "2025-12-31"
)