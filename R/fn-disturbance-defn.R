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

detect_heatwave <- function(.data, climatologyPeriod = c("1981-01-01", "2010-12-31"), pctile = 95, minDuration = 2) {

  ## make sure the numbers data is there for the climatogy period
  station_id_covered <- .data %>%
    group_by(station_id) %>%
    summarise(min = min(date),
              max = max(date)) %>%
    filter(min <= as.Date(climatologyPeriod[1])) %>%
    filter(max >= as.Date(climatologyPeriod[2])) %>%
    pull(station_id)

  message("Only ", n_distinct(station_id_covered), " out of ", dplyr::n_distinct(.data$station_id), " have sufficient data")

  ## only that data that is appropriate for the climate period
  .data <- select(.data, station_id, t = date, temp = mean_temp) %>%
    filter(station_id %in% station_id_covered)

  ## iterate through the climate data to detect heatwaves
  heatwaves <- purrr::map(unique(.data$station_id), ~{
    .data %>%
      filter(station_id == .x) %>%
      heatwaveR::ts2clm(climatologyPeriod = climatologyPeriod, pctile = pctile) %>%
      heatwaveR::detect_event(minDuration = minDuration)
  }) %>%
    set_names(unique(.data$station_id))

}


bind_heatwave_data <- function(heatwaves) {
  ## Extract and combine the two dataframes
  list(
    climatology = purrr::map_df(names(heatwaves) %>% set_names(), ~heatwaves[[.x]]$climatology, .id = "station_id"),
    event = purrr::map_df(names(heatwaves) %>% set_names(), ~heatwaves[[.x]]$event, .id = "station_id")
  )
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









