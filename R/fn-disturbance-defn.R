# Copyright 2021 Province of British Columbia
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



# Heatwave ----------------------------------------------------------------


# heatwave_weather, open_dataset(here::here("data/weather/")) %>%
#   filter(year > 1985, station_id %in% stations_lha$station_id) %>%
#   select(station_id, station_name, date, year, month, max_temp, mean_temp) %>%
#   group_by(station_id, station_name) %>%
#   collect() %>%
#   add_heatwave_flag()


calc_heatwave_days_over_time <- function(..., geography_to_add = NULL) {
  h <- open_dataset(here::here("data/weather/")) %>%
    select(station_id, station_name, date, year, month, max_temp, mean_temp) %>%
    group_by(station_id, station_name, year) %>%
    collect()

  h <- add_heatwave_flag(h, ...)

  if (!is.null(geography_to_add)) {
    h <- h %>%
      left_join(weather_stations_geo() %>%
                  st_join(geography_to_add) %>%
                  st_drop_geometry())
  }


  h %>%
    filter(heatwave_flag == "heatwave")
}

calc_area_burned_over_time <- function(geography_to_add = NULL) {
  fires <- bcdc_query_geodata('22c7cb44-1463-48f7-8e47-88857f207702') %>%
    select(FIRE_YEAR, FIRE_SIZE_HECTARES) %>%
    collect()


  if (!is.null(geography_to_add)) {
    fires <- fires %>%
      st_intersection(geography_to_add) %>%
      st_drop_geometry() %>%
      group_by(FIRE_YEAR, LOCAL_HLTH_AREA_NAME, HLTH_AUTHORITY_NAME) %>%
      summarise(FIRE_SIZE_HECTARES = sum(FIRE_SIZE_HECTARES))
    return(fires)
  }

  fires %>%
    st_drop_geometry() %>%
    group_by(FIRE_YEAR) %>%
    summarise(FIRE_SIZE_HECTARES = sum(FIRE_SIZE_HECTARES))
}


add_heatwave_flag <- function(.data, temperature_difference = 5.0, heat_duration = 5) {
  #browser()
  ## taken from the WMO definition
  if (!all(c("max_temp", "mean_temp", "date", "month") %in% names(.data))) {
    stop("need 'max_temp', 'mean_temp', 'month', and 'date' columns", call. = FALSE)
  }

  ## this really could be sped up with data.table
  .data %>%
    mutate(diff_temp = max_temp - mean_temp) %>%
    mutate(month_flag = ifelse(month %in% c("07", "08"), 'summer', 'not summer')) %>%
    group_by(grp = data.table::rleid(diff_temp >= 5.0), .add = TRUE) %>%
    mutate(num_grp = length(grp)) %>%
    mutate(heatwave_flag = case_when(
      diff_temp >= 5.0 & num_grp >= 5 & month_flag == "summer" ~ "heatwave",
      is.na(diff_temp) ~ NA_character_,
      TRUE ~ "no-heatwave"
    )) %>%
    ungroup(grp) %>%
    select(-grp, -num_grp, -month_flag)
}
