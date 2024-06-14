library(tidyverse)

cdec_stations <- read_csv("data-raw/cdec_exploration/station_table.csv") |>
  select(-freq_avail, -geometry, -pct_complete) |>
  glimpse()

usgs_stations <- read_csv("data-raw/usgs_exploration/USGS_station_lookup.csv") |>
  rename(channel = Tributary, station_id = Gage, start_date = `Min Date`, end_date = `Max Date`, section = Section) |>
  mutate(start_date = as.Date(start_date, format = "%m/%d/%Y"),
         end_date = as.Date(end_date, format = "%m/%d/%Y"),
         station_id = as.character(station_id)) |>
  glimpse()

station_lookup <- bind_rows(cdec_stations, usgs_stations) |>
  mutate(station_name = str_to_title(station_name)) |>
  glimpse()

View(station_lookup)

usethis::use_data(station_lookup)
