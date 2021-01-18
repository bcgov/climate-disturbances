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
#tar_option_set(packages = .packages())



# Load --------------------------------------------------------------------
# time variables
time_vars <- list(
  tar_target(start_date, as.Date('1981-01-01')),
  tar_target(end_date, as.Date('2020-12-31'))
)


#  targets
climate_targets <- list(
  tar_target(lha_of_interest, health_lha() %>%
               st_filter(census_tract())),
  tar_target(pm25_data, pm25(lha_of_interest, start_date = start_date, end_date = end_date)),
  tar_target(weather_data, weather(lha_of_interest, start_date = start_date, end_date = end_date, normals = TRUE, ask = FALSE))
)


# processing

processing_targets <- list(
  tar_target(pm25_24h, pm25_data %>%
               rename(date_time = date_pst) %>%
               distinct() %>%
               pm_24h_caaqs(val = "raw_value", by = c("station_name", "ems_id", "instrument", "local_hlth_area_name", "hlth_service_dlvr_area_name"))),
  tar_target(heatwaves_raw, detect_heatwave(weather_data, pctile = 90)),
  tar_target(heatwaves, bind_heatwave_data(heatwaves_raw))
)



lha_demographics <- list(
  tar_target(lha_popn, get_lha_popn(2019) %>% st_filter(lha_of_interest)),
  tar_target(lha_age, get_lha_age(2019) %>% filter(Region %in% lha_of_interest$LOCAL_HLTH_AREA_CODE))
)


list(
  time_vars,
  climate_targets,
  processing_targets,
  lha_demographics,
  #tar_render(clim_overview, "out/climate-disturbance-overview.Rmd")
  tar_render(lha_assessment, "out/lha_assessment.Rmd")
  )


