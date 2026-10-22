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

#Convert time from secs to HMS
detections$time <- as_hms(detections$time)

#Create combined date_time
detections$date_time <- detections$date + seconds(detections$time)

#Segment detections by year
detections_per_year <- detections %>%
  group_split(year)

names(detections_per_year) <- detections %>%
  distinct(year) %>%
  pull(year)


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

#CamTrapR Diel Activity plots
activityDensity(recordTable = detections_per_year$'2018',
                speciesCol = "metadata_group_code",
                species = "COYOTE",
                recordDateTimeCol = "date_time",
                recordDateTimeFormat = "ymd HMS",
                PlotR = TRUE,
                writePNG = FALSE)


#Read in effort data

#Biocube Covariates
ggplot(BIOCUBE, aes(x = WI_Climate_CHELSA_scd_v2))+
  geom_density()
