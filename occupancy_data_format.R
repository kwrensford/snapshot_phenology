#Format detections for Occupancy models

library(dplyr)
library(tidyr)
library(vroom)
library(ggplot2)
library(camtrapR)
library(lubridate)
library(hms)
library(unmarked)
library(viridis)
library(forcats)
library(spOccupancy)
library(purrr)

#Load in detection data
detections <- vroom("data/Kwasi_Detections_2026-10-06.csv")

#Load in effort data
snapshot_effort <- readRDS("data/Kwasi_Effort_Nested_2026-10-06.rds")

#Format for spOccupancy

##Extract all unique dates
all_dates <- seq(
  from = min(dplyr::bind_rows(snapshot_effort$data)$date),
  to   = max(dplyr::bind_rows(snapshot_effort$data)$date),
  by   = "day"
)

##Assign week number to each date
week <- floor(
  as.numeric(all_dates - min(all_dates)) / 7
) + 1

##Count number of active days per week
table(week)

##Define weeks within year
effort_long <- snapshot_effort %>%
  select(camera_location_seq_no, data) %>%
  mutate(
    effort = purrr::map(data, ~{
      .x %>%
        mutate(
          year = lubridate::year(date),
          week = ceiling(lubridate::yday(date) / 7)
        ) %>%
        count(year, week, name = "effort_days")
    })
  ) %>%
  select(camera_location_seq_no, effort) %>%
  tidyr::unnest(effort)

##Detections by site-year-species
det_long <- detections %>%
  mutate(
    year = lubridate::year(date),
    week = lubridate::isoweek(date)
  ) %>%
  distinct(
    metadata_group_code,
    camera_location_seq_no,
    year,
    week
  ) %>%
  mutate(detected = 1)

##Join detections to effort 
species_list <- sort(unique(det_long$metadata_group_code))

survey_frame <- tidyr::expand_grid(
  species = species_list,
  effort_long
) %>%
  rename(
    camera_location_seq_no = camera_location_seq_no
  ) %>%
  left_join(
    det_long,
    by = c(
      "species" = "metadata_group_code",
      "camera_location_seq_no",
      "year",
      "week"
    )
  ) %>%
  mutate(
    detected = tidyr::replace_na(detected, 0)
  )

##Define dimensions of array
species_levels <- sort(unique(detections$metadata_group_code))
site_levels <- sort(unique(snapshot_effort$camera_location_seq_no))
year_levels <- sort(unique(detections$year))
week_levels <- 1:53

##Initialize y array
y_array <- array(
  NA,
  dim = c(
    length(species_levels),
    length(site_levels),
    length(year_levels),
    length(week_levels)
  ),
  dimnames = list(
    species = species_levels,
    site = site_levels,
    year = year_levels,
    week = week_levels
  )
)

##Fill Y Array
for(i in seq_len(nrow(survey_frame))) {
  
  row <- survey_frame[i, ]
  
  y_array[
    match(row$species, species_levels),
    match(row$camera_location_seq_no, site_levels),
    match(row$year, year_levels),
    match(row$week, week_levels)
  ] <- row$detected
}

saveRDS(y_array, file = "spoccupancy_array.rds")

#Initialize effort array
effort_array <- array(
  NA_real_,
  dim = c(
    length(site_levels),
    length(year_levels),
    length(week_levels)
  ),
  dimnames = list(
    site = site_levels,
    year = year_levels,
    week = week_levels
  )
)

##Fill effort array
for(i in seq_len(nrow(effort_long))) {
  
  effort_array[
    match(effort_long$camera_location_seq_no[i], site_levels),
    match(effort_long$year[i], year_levels),
    match(effort_long$week[i], week_levels)
  ] <- effort_long$effort_days[i]
  
}

saveRDS(effort_array, file = "effort_array.rds")

#Extract lat-long from effort and convert to matrix
coords <- snapshot_effort %>%
  distinct(
    camera_location_seq_no,
    lat,
    lon
  ) %>%
  arrange(
    match(camera_location_seq_no, site_levels)
  )

coords <- as.matrix(
  coords[, c("lon", "lat")]
)

rownames(coords) <- site_levels

saveRDS(coords, file = "coords.rds")
