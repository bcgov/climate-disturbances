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



##  targets
climate_targets <- list(
  tar_target(stations_lha, stations_geo() %>%
               filter(prov == "BC", normals, end >= 2019) %>%
               st_join(health_lha())),
  tar_target(get_climate_data, get_climate_data(stations_lha$station_id)),
  tar_target(pm25_data, get_pm25_data()),
  tar_target(heatwave_days_over_time, calc_heatwave_days_over_time() %>%
               group_by(year) %>%
               summarise(heatwave_days_per_station = n()/n_distinct(station_id))),
  tar_target(heatwave_days_over_time_by_lha, calc_heatwave_days_over_time(geography_to_add = health_lha()) %>%
               group_by(year, LOCAL_HLTH_AREA_NAME, HLTH_AUTHORITY_NAME) %>%
               summarise(heatwave_days_per_station = n()/n_distinct(station_id))),
  tar_target(area_burned_over_time, calc_area_burned_over_time())
)



# lha_demographics <- list(
#   tar_target(lha_popn, get_lha_popn(2019)),
#   tar_target(lha_age, get_lha_age(2019))
# )

list(
  climate_targets,
  tar_render(clim_overview, "out/climate-disturbance-overview.Rmd")
  #lha_demographics,
  #tar_render(lha_assessment, "out/lha_assessment.Rmd")
  )


