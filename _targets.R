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



#  targets
climate_targets <- list(
  tar_target(lha_in_census_tract, health_lha() %>%
               st_filter(census_tract())),
  tar_target(pm25_data, pm25(lha_in_census_tract, start_date = as.Date('2017-01-01'), end_date = as.Date("2018-12-31"))),
  tar_target(weather_data, weather(lha_in_census_tract, start_date = as.Date('2017-01-01'), end_date = as.Date("2018-12-31"), ask = FALSE))
)



# lha_demographics <- list(
#   tar_target(lha_popn, get_lha_popn(2019)),
#   tar_target(lha_age, get_lha_age(2019))
# )

list(
  climate_targets#,
  #lha_demographics,
  #tar_render(clim_overview, "out/climate-disturbance-overview.Rmd")
  #tar_render(lha_assessment, "out/lha_assessment.Rmd")
  )


