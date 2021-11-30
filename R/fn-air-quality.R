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
    download.file(pm25_link, destfile = stored_path, quiet = TRUE, method = "curl")
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

  aq_stations <- readr::read_csv(aq_stations_link, col_types = c("cccddcdddccccDD"), na = c("", "N/A"))
  aq_stations <- dplyr::filter(aq_stations, !is.na(LONG), !is.na(LAT))
  aq_stations <- sf::st_as_sf(aq_stations, coords = c("LONG", "LAT"), crs = "+proj=longlat")
  bcmaps::transform_bc_albers(aq_stations)

}

pm25 <- function(aoi=NULL, add_aoi_attributes = TRUE, start_date = NULL, end_date = NULL) {
  aoi <- bcmaps::transform_bc_albers(aoi)
  stations_in_aoi <- sf::st_filter(air_quality_stations_geo(), aoi)
  d <- get_pm25_data()

  if (!is.null(aoi)) d <- filter(d, EMS_ID %in% unique(stations_in_aoi$EMS_ID))
  if (!is.null(start_date)) d <- filter(d, DATE_PST >= start_date)
  if (!is.null(end_date)) d <- filter(d, DATE_PST <= end_date)

  d <- d %>%
    collect()


  if(add_aoi_attributes) {
    geo_attr <- st_join(select(stations_in_aoi, EMS_ID), aoi)
    geo_attr <- st_drop_geometry(geo_attr)

    d <- d %>% left_join(geo_attr)
  }

  janitor::clean_names(d)
}
