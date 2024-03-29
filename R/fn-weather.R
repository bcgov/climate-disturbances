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


# Weather -----------------------------------------------------------------


get_climate_data <- function(ids, data_dir = "data/weather", interval = "day", ask = TRUE) {
  if(!dir.exists(data_dir)) dir.create(data_dir)

  potential_paths <- file.path(data_dir, ids)
  needed_stations <- ids[!dir.exists(potential_paths)]

  msg <- message(paste0(length(needed_stations),
                        " of ",
                        length(potential_paths),
                        " stations need to be downloaded."))

  if (ask) ans <- ask(msg) else ans <- TRUE

  if(ans) {
    purrr::walk(needed_stations, ~{
      d <- weathercan::weather_dl(.x, interval = interval)
      if (!dir.exists(file.path(data_dir, .x))) dir.create(file.path(data_dir, .x))
      arrow::write_parquet(d, sink = file.path(data_dir, .x, "data.parquet"))
      rm(d)
      gc()
    })
  } else {
    message("Have a nice day!")
  }

  invisible(TRUE)
}

weather_stations_geo <- function(interval_var = 'day') {

  stations <- dplyr::filter(stations(), prov == "BC", interval == interval_var)
  stations <- sf::st_as_sf(stations, coords = c("lon", "lat"), crs = "+proj=longlat")
  bcmaps::transform_bc_albers(stations)
}

normals_stations_geo <- function() {
  dplyr::filter(stations, normals, prov == "BC", interval == "day")
}

weather <- function(aoi, add_aoi_attributes = TRUE, start_date = NULL, end_date = NULL, interval_var = 'day', normals, ask = TRUE) {

  search_int <- lubridate::interval(start_date, end_date)

  aoi <- bcmaps::transform_bc_albers(aoi)

  if (normals) {
    stations_in_aoi <- sf::st_filter(weather_stations_geo(interval_var = interval_var), aoi) %>%
      filter(normals)
  } else {
    stations_in_aoi <- sf::st_filter(weather_stations_geo(interval_var = interval_var), aoi)
  }

  stations_in_aoi$station_int <- lubridate::interval(
    as.Date(paste0(stations_in_aoi$start,"-01-01")),
    as.Date(paste0(stations_in_aoi$end, "-12-31")))

  stations_in_aoi <- filter(stations_in_aoi, int_overlaps(station_int, search_int))

  if (!get_climate_data(stations_in_aoi$station_id, ask = ask)) stop("Problems with downloading", call. = FALSE)

  d <- arrow::open_dataset(here::here("data/weather/"), partitioning = "station_idtemp")

  if (!is.null(aoi)) d <- filter(d, station_idtemp %in% stations_in_aoi$station_id)

  d <- d %>%
    select(-station_idtemp) %>%
    collect()

  ## Date filtering not working with arrow right now. MVP.
  if (!is.null(end_date)) d <- filter(d, date <= end_date)
  if (!is.null(start_date)) d <- filter(d, date >= start_date)

  if(add_aoi_attributes) {
    geo_attr <- st_join(select(stations_in_aoi, station_id), aoi)
    geo_attr <- st_drop_geometry(geo_attr)

    d <- d %>% left_join(geo_attr)
  }

  janitor::clean_names(d)

}
