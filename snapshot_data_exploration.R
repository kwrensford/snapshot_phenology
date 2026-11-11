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

#Visualize detections per year
detections %>%
  ggplot(aes(x = year)) + 
  geom_bar()

#Camera locations CSV
cam_locations <- site_covars %>%
  select(camera_ID, lat, lon)

write.csv(cam_locations, file = "data/cam_locations.csv")

#Generate deployment summary for all camera locations

summarize_camera <- function(x){
  
  dates <- sort(unique(x$date))
  gaps <- diff(dates)
  
  tibble(
    start_date = min(dates),
    end_date = max(dates),
    active_days = length(dates),
    deployment_days = as.numeric(max(dates) - min(dates)) + 1,
    n_gaps = sum(gaps > 1),
    total_gap_days = sum(as.numeric(gaps[gaps > 1]) - 1),
    longest_gap = ifelse(
      any(gaps > 1),
      max(as.numeric(gaps[gaps > 1]) - 1),
      0
    )
  )
}

deployment_summary <- snapshot_effort %>%
  mutate(summary = map(data, summarize_camera)) %>%
  unnest(summary)
  
#Visualize deployment
deployment_years <- deployment_summary %>%
  mutate(
    start_year = year(start_date),
    end_year = year(end_date)
  ) %>%
  rowwise() %>%
  mutate(year = list(seq(start_year, end_year))) %>%
  tidyr::unnest(year) %>%
  ungroup() %>%
  group_by(camera_location_seq_no, year) %>%
  summarise(
    deployment_days = sum(deployment_days),
    .groups = "drop"
  )

ggplot(deployment_years,
       aes(x = year,
           y = reorder(camera_location_seq_no,
                       deployment_days,
                       max),
           fill = deployment_days)) +
  geom_tile() +
  scale_fill_viridis_c(
    name = "Deployment\ndays"
  ) +
  labs(
    x = "Year",
    y = "Camera location"
  ) +
  theme_bw()

#Deployment effort mapped
deployment_map <- deployment_summary %>%
  group_by(camera_location_seq_no, lat, lon) %>%
  summarise(
    total_days = sum(deployment_days),
    n_years = n_distinct(lubridate::year(start_date)),
    .groups = "drop"
  )

ggplot(deployment_map,
       aes(lon, lat,
           size = total_days,
           color = n_years)) +
  geom_point(alpha = 0.8) +
  scale_color_viridis_c()


deployment_quality <- deployment_summary %>%
  mutate(
    coverage =
      deployment_days /
      (deployment_days + total_gap_days)
  )

ggplot(deployment_quality,
       aes(year(start_date),
           camera_location_seq_no,
           fill = coverage)) +
  geom_tile()

#Effort Deployment Visualization
site_effort <- deployment_summary %>%
  rowwise() %>%
  mutate(
    year = list(seq(year(start_date),
                    year(end_date)))
  ) %>%
  unnest(year) %>%
  group_by(camera_location_seq_no, lat, lon) %>%
  summarise(
    total_active_days = sum(active_days),
    n_years = n_distinct(year),
    .groups = "drop"
  )


ggplot(site_effort,
       aes(lon, lat)) +
  geom_point(
    aes(
      size = total_active_days,
      color = n_years
    ),
    alpha = 0.7
  ) +
  scale_color_viridis_c(
    name = "Years sampled"
  ) +
  scale_size_area(
    name = "Active days",
    max_size = 12
  ) +
  coord_equal() +
  theme_bw()

site_year <- deployment_summary %>%
  rowwise() %>%
  mutate(
    year = list(seq(year(start_date), year(end_date)))
  ) %>%
  unnest(year) %>%
  group_by(camera_location_seq_no, lat, lon, year) %>%
  summarise(
    active_days = sum(active_days),
    .groups = "drop"
  )

ggplot(
  site_year,
  aes(lon, lat)
) +
  geom_point(
    aes(size = active_days),
    alpha = 0.6
  ) +
  facet_wrap(~year) +
  coord_equal() +
  theme_bw()

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
