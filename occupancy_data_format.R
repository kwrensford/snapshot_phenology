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

#Extract first and last day of camera operation
library(dplyr)
library(purrr)
library(tidyr)

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

extract_gaps <- function(x){
  
  dates <- sort(unique(x$date))
  
  gap_idx <- which(diff(dates) > 1)
  
  if(length(gap_idx) == 0){
    return(NULL)
  }
  
  tibble(
    gap_start = dates[gap_idx] + 1,
    gap_end = dates[gap_idx + 1] - 1,
    gap_days = as.numeric(gap_end - gap_start) + 1
  )
}

gap_table <- snapshot_effort %>%
  select(camera_location_seq_no, data) %>%
  mutate(gaps = map(data, extract_gaps)) %>%
  unnest(gaps)

deployment_summary <- snapshot_effort %>%
  mutate(summary = map(data, summarize_camera)) %>%
  unnest(summary)

#Identify gaps in camera operation
extract_runs <- function(x){
  
  dates <- sort(unique(x$date))
  
  run_id <- cumsum(c(TRUE, diff(dates) > 1))
  
  tibble(
    date = dates,
    run_id = run_id
  ) %>%
    group_by(run_id) %>%
    summarise(
      start_date = min(date),
      end_date = max(date),
      active_days = n(),
      .groups = "drop"
    )
}

deployment_periods <- snapshot_effort %>%
  select(camera_location_seq_no, data) %>%
  mutate(periods = map(data, extract_runs)) %>%
  unnest(periods)

#Build Camera Operation Matrix

all_dates <- seq(
  from = min(dplyr::bind_rows(snapshot_effort$data)$date),
  to   = max(dplyr::bind_rows(snapshot_effort$data)$date),
  by   = "day"
)

camOp_daily <- sapply(snapshot_effort$data, function(x) {
  
  out <- rep(NA, length(all_dates))
  
  out[all_dates %in% x$date] <- 1
  
  out
  
})



