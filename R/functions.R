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



# Air Quality -------------------------------------------------------------


get_pm25_data <- function(...) {
  pm25_link <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2009-LatestVerified/PM25.csv"

  stored_path <- file.path("data/air_quality", basename(pm25_link))

  if (!file.exists(stored_path)) {
    dir.create(dirname(stored_path), showWarnings = FALSE )
    download.file(pm25_link, destfile = stored_path,quiet = TRUE)
  }

  arrow::open_dataset(dirname(stored_path), format = "csv", schema = arrow::schema(
    DATE_PST = arrow::timestamp(),
    STATION_NAME = arrow::string(),
    EMS_ID = arrow::string(),
    PARAMETER = arrow::string(),
    INSTRUMENT = arrow::string(),
    RAW_VALUE = arrow::float64(),
    UNIT = arrow::string(),
    ROUNDED_VALUE = arrow::float64()
  ), ...)
}


air_quality_stations_geo <- function() {
  aq_stations_link <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/Air_Monitoring_Stations/bc_air_monitoring_stations.csv"

  aq_stations <- readr::read_csv(aq_stations_link, col_types = c("cccddcdddccccDD"))
  aq_stations <- dplyr::filter(aq_stations, !is.na(LONG), !is.na(LAT))
  aq_stations <- sf::st_as_sf(aq_stations, coords = c("LONG", "LAT"), crs = "+proj=longlat")
  bcmaps::transform_bc_albers(aq_stations)

}

pm25 <- function(aoi=NULL, start_date = NULL, end_date = NULL) {
  aoi <- bcmaps::transform_bc_albers(aoi)
  stations_in_aoi <- sf::st_filter(air_quality_stations_geo(), aoi)
  d <- get_pm25_data()

  if (!is.null(aoi)) d <- filter(d, EMS_ID %in% stations_in_aoi$EMS_ID)
  if (!is.null(start_date)) d <- filter(d, DATE_PST >= start_date)
  if (!is.null(end_date)) d <- filter(d, DATE_PST <= end_date)

  d %>%
    collect()
}


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

get_normals_data <- function(climate_ids, normals_years = "1981-2010", data_dir = "data/normals") {

  parquet_dir <- file.path(data_dir, "normals.parquet")
  if (!dir.exists(data_dir)) dir.create(data_dir)

  if (file.exists(parquet_dir)) {
    existing_normals <- arrow::read_parquet(parquet_dir)
    not_present_ids <- setdiff(climate_ids, unique(existing_normals$climate_id))

  }

  if (purrr::is_empty(not_present_ids)) return(invisible(TRUE))

  n <- normals_dl(climate_ids = not_present_ids, normals_years = normals_years) %>%
    dplyr::select(-frost) %>%
    tidyr::unnest(cols = c(normals))

  n <- dplyr::bind_rows(existing_normals, n)
  arrow::write_parquet(n, sink = parquet_dir)
}


weather_stations_geo <- function(interval_var = 'day') {

  stations <- dplyr::filter(stations, prov == "BC", interval == interval_var)
  stations <- sf::st_as_sf(stations, coords = c("lon", "lat"), crs = "+proj=longlat")
  bcmaps::transform_bc_albers(stations)
}


weather <- function(aoi, start_date = NULL, end_date = NULL, interval_var = 'day', ask = TRUE) {

  search_int <- lubridate::interval(start_date, end_date)

  aoi <- bcmaps::transform_bc_albers(aoi)

  stations_in_aoi <- sf::st_filter(weather_stations_geo(interval_var = interval_var), aoi)

  stations_in_aoi$station_int <- lubridate::interval(
    as.Date(paste0(stations_in_aoi$start,"-01-01")),
    as.Date(paste0(stations_in_aoi$end, "-12-31")))

  stations_in_aoi <- filter(stations_in_aoi, int_overlaps(station_int, search_int))

  if (!get_climate_data(stations_in_aoi$station_id, ask = ask)) stop("Problems with downloading", call. = FALSE)

  d <- arrow::open_dataset(here::here("data/weather/"), partitioning = "station_idtemp")

  if (!is.null(start_date)) d <- filter(d, date >= as.Date(start_date))
  if (!is.null(end_date)) d <- filter(d, date <= as.Date(end_date))
  if (!is.null(aoi)) d <- filter(d, station_idtemp %in% stations_in_aoi$station_id)

  d %>%
    select(-station_idtemp) %>%
    collect()
}



# Health data -------------------------------------------------------------

##  Return LHA population by year

get_lha_popn <- function(year) {
  health_lha() %>%
    left_join(read_csv("data/demographics/lha_2010-2019-all-ages.csv") %>%
                filter(Year %in% year) %>%
                mutate(Region = as.character(Region)) %>%
                select(Region:Total),
              by = c("LOCAL_HLTH_AREA_CODE" = "Region", "LOCAL_HLTH_AREA_NAME" = "Local Health Area"))
}


get_lha_age <- function(year) {
  read_csv("data/demographics/lha_2010-2019-all-ages.csv") %>%
    filter(Year %in% year) %>%
    pivot_longer(cols = c(`0`:`90+`), names_to = 'age', values_to = 'n')
}






