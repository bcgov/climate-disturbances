# Copyright 2020 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



library(targets)
library(tarchetypes)
# Read the tar_script() help file for details.

## Source all functions
source("R/setup.R")

## Load packages
tar_option_set(packages = .packages())



# Load --------------------------------------------------------------------
# time variables
time_vars <- list(
  tar_target(start_date, as.Date('1981-01-01')),
  tar_target(end_date, Sys.Date())
)


#  climate data
climate_targets <- list(
  # tar_target(area_of_interest, health_lha() %>% filter(grepl("Vancouver", LOCAL_HLTH_AREA_NAME, ignore.case = TRUE) |
  #                                                      LOCAL_HLTH_AREA_NAME %in% c("Greater Nanaimo", "Kamloops", "Smithers", "Nelson", "Central Okanagan", "Greater Victoria"))),
  # #tar_target(pm25_data, pm25(area_of_interest, start_date = start_date, end_date = end_date)), ##pm data has some issues with arrow col specs
  # tar_target(weather_data, weather(area_of_interest, start_date = start_date, end_date = end_date, normals = FALSE, ask = FALSE)),
  # tar_target(area_burned_over_time, calc_area_burned_over_time(area_of_interest)),
  # tar_target(flood_example, hy_daily_flows("08NN002", start_date = start_date, end_date = end_date)),
  tar_target(ahccd_zipfile, download_ahccd_data(which = "daily_max_temp", data_dir = "data"), format = "file"),
  tar_target(daily_tmax_path, write_ahccd_data(ahccd_zipfile), format = "file"),
  tar_target(climate_stations, get_ahccd_stations()),
  tar_target(target_stations, get_target_stations(climate_stations, buffer = 200))
)

# health sites
health_facilities <- list(
  tar_target(hospitals, bcdc_query_geodata("bc-health-care-facilities-hospital") %>%
               filter(INTERSECTS(area_of_interest)) %>%
               collect())
)


# tidy --------------------------------------------------------------------

processing_targets <- list(
  # tar_target(pm25_24h, pm25_data %>%
  #              rename(date_time = date_pst) %>%
  #              distinct() %>%
  #              pm_24h_caaqs(val = "raw_value", by = c("station_name", "ems_id", "instrument", "local_hlth_area_name", "hlth_service_dlvr_area_name"))),
  tar_target(heatwaves_raw, detect_heatwave(weather_data, pctile = 95, minDuration = 3)),
  tar_target(heatwaves, bind_heatwave_data(heatwaves_raw))
)

# Output ------------------------------------------------------------------


## Pipeline

list(
  # time_vars,
  climate_targets,
  # processing_targets,
  # health_facilities,
  #tar_render(clim_overview, "out/climate-disturbance-overview.Rmd"),
  # tar_render(flood_examples, "out/flood-examples/flood-examples.Rmd"),
  # tar_render(heatwave_overview, "out/heatwave-overview.Rmd"),
  #tar_render(air_quality_examples, "out/air-quality-examples/air-quality-examples.Rmd")
  NULL
  )


