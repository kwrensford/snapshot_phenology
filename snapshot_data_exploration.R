#Snapshot data exploration

library(dplyr)
library(vroom)
library(ggplot2)
library(camtrapR)
library(lubridate)
library(hms)
library(unmarked)
library(viridis)
library(forcats)

#Read in detection data
detections <- vroom("data/Kwasi_Detections_2026-10-06.csv")

#Create column for year
detections$year <- year(detections$date)

#Segement detections by year
detections_per_year <- detections %>%
  group_split(year)

names(detections_per_year) <- detections %>%
  distinct(year) %>%
  pull(year)

#Format date_time
detections$time <- as_hms
detections$date_time <- 

#Clean up species columns
species_names <- unique(detections$metadata_group_code)
  
new species names <- c("raccoon", "white tailed deer", "Coyote", "Cottontail", "Squirrel", "Bird_other",
                       "turkey")

#Visualize number of detections
ggplot(detections, aes(x = forcats::fct_infreq(metadata_group_code))) +
  geom_bar() +
  theme_bw() +
  coord_flip()

#visualize number of cameras a species was detected
detections %>%
  dplyr::select(metadata_group_code, camera_location_seq_no) %>%
  unique() %>%
  dplyr::count(metadata_group_code) %>%
  dplyr::mutate(percent = n/length(unique(detections$camera_location_seq_no))) %>%
  ggplot(aes(x = fct_reorder(metadata_group_code, percent), y = percent)) +
  geom_col() +
  theme_bw() +
  coord_flip() +
  xlab("Percent of Locations Detected")

#Detection Density Plot


#Read in effort data
