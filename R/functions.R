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

########### Homogenized daily temps

download_homog_data <- function(save_csv = FALSE, datadir = "data") {
  url <- "https://crd-data-donnees-rdc.ec.gc.ca/CDAS/products/EC_data/AHCCD_daily/Homog_daily_max_temp_v2020_Gen3.zip"
  destfile <- tempfile(fileext = ".zip")
  download.file(url, destfile = destfile)
  exdir <- ifelse(save_csv, datadir, dirname(destfile))
  if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE)
  unzip(destfile, exdir = exdir)
}

read_homog_data <- function(datafile) {

  txt <- readLines(datafile)

  stn_meta <- unlist(strsplit(txt[1], split = "\\s*,\\s*"))
  names(stn_meta) <- c("stn_id", "stn_name", "province",
                   "stn_joined", "element", "unit",
                   "stn_last_updated")

  header <- gsub("^\\s+|\\s+$", "", txt[3])
  header <- strsplit(gsub("\\s+((Day)\\s0?)?", ",\\2", header), ",")[[1]]

  data <- txt[5:length(txt)]
  data <- gsub("-9999.9[a-zA-Z]?", " NA ", data)
  data <- gsub("^\\s+|\\s+$", "", data)
  data <- gsub("\\s+", ",", data)
  data <- paste(data, collapse = "\n")
  data <- readr::read_csv(I(data), col_names = header)

  for (i in seq_along(stn_meta)) {
    data[[names(stn_meta)[i]]] <- stn_meta[i]
  }

  data <- tidyr::pivot_longer(data, cols = starts_with("Day"),
                       names_to = "DoM", values_to = "temp")
  data <- dplyr::mutate(
    data,
    date = as.Date(
      paste(Year, Mo, gsub("Day", "", DoM), sep = "-")
    ))

  data <- dplyr::mutate(
    data,
    flag = dplyr::case_when(
      grepl("a", temp) ~ "adjusted",
      grepl("E", temp) ~ "estimated",
      TRUE ~ NA_character_
    ),
    temp = as.numeric(gsub("[a-zA-Z]", "", temp))
  )

  data %>%
    dplyr::select(date, temp, dplyr::everything(), -Year, -Mo, -DoM) %>%
    dplyr::filter(!is.na(date))
}

