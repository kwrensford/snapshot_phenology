#Snapshot data exploration and formatting

library(tidyverse)
library(hms)
library(unmarked)
library(viridis)

#Read in detection data
detections <- vroom("data/Kwasi_Detections_2026-10-06.csv")

#Read in effort data
snapshot_effort <- readRDS("data/Kwasi_Effort_Nested_2026-10-06.rds")

#Map sites
plot(snapshot_effort$lon, snapshot_effort$lat)

#Create column for year, month, and week
detections$year <- year(detections$date)
detections$month <- month(detections$date)
detections$week <- week(detections$date)

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
detections %>%
  filter(metadata_group_code == "DEER") %>%
  ggplot(aes(x = week, color = as.factor(year))) + 
  geom_density(linewidth = 1)+
  theme_classic()


#CamTrapR Diel Activity plots
activityDensity(recordTable = detections_per_year$'2018',
                speciesCol = "metadata_group_code",
                species = "COYOTE",
                recordDateTimeCol = "date_time",
                recordDateTimeFormat = "ymd HMS",
                PlotR = TRUE,
                writePNG = FALSE)

#Generate delta time (intervals between detection)
detection_intervals <- detections %>%
  arrange(metadata_group_code, camera_location_seq_no, date_time) %>%
  group_by(metadata_group_code, camera_location_seq_no,) %>%
  mutate(delta_time = as.numeric(difftime(date_time, lag(date_time), units = "mins"))) %>%
  ungroup()

detection_intervals %>%
  filter(
    metadata_group_code == "DEER",
    delta_time < 24 * 60
  ) %>%
  ggplot(aes(x = delta_time)) +
  geom_histogram(binwidth = 5)

#Log time interval
detection_intervals %>%
  filter(
    metadata_group_code == "DEER",
    delta_time > 0
  ) %>%
  ggplot(aes(x = delta_time)) +
  geom_histogram(bins = 100) +
  scale_x_log10()

detection_intervals %>%
  filter(
    metadata_group_code == "DEER",
    !is.na(delta_time)
  ) %>%
  ggplot(aes(x = delta_time)) +
  stat_ecdf()

#Visualize detection intervals
detection_intervals %>%
  filter(metadata_group_code == "DEER") %>%
  ggplot(aes(x = delta_time)) + 
  geom_histogram(binwidth = 5)

#Camera locations CSV
cam_locations <- site_covars %>%
  select(camera_ID, lat, lon)

write.csv(cam_locations, file = "data/cam_locations.csv")

#Biocube Covariates
ggplot(BIOCUBE, aes(x = WI_Climate_CHELSA_scd_v2))+
  geom_density()

snapshot_effort <- snapshot_effort %>%
  rename(camera_ID = camera_location_seq_no)

site_covars <- BIOCUBE %>%
  left_join(snapshot_effort, join_by(camera_ID))

site_covars$lat <- as.numeric(site_covars$lat)
site_covars$lon <- as.numeric(site_covars$lon)

ggplot(site_covars, aes(x = lon, y = lat, colour = WI_Phenology_USGS_MAXT_v2)) + 
  geom_point()+
  scale_colour_viridis_c()

#Explore patterns of detections with BIOCUBE covariates
