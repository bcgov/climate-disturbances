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

download_ahccd_data <- function(data_dir = "data") {
  base_url <- "https://crd-data-donnees-rdc.ec.gc.ca/CDAS/products/EC_data/AHCCD_daily"
  files_df <- rvest::html_table(rvest::read_html(base_url))[[1]]
  which_file <- grepl("daily_max_temp|daily_mean_temp|daily_min_temp", files_df$Name) & grepl("2020", files_df$Name) & tools::file_ext(files_df$Name) == "zip"
  fname <- files_df$Name[which_file]

  vapply(fname, function(n) {
    url <- paste0(base_url, "/", n)
    destfile <- file.path(data_dir, n)
    download.file(url, destfile = destfile)
    destfile
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)
}

extract_ahccd_data <- function(zipfile, save_raw_txt = FALSE, data_dir) {
  exdir <- ifelse(save_raw_txt, file.path(data_dir, "raw_txt"), tempdir())
  if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE)
  unzip(zipfile, exdir = exdir)
}

read_ahccd_data_single <- function(datafile) {

  txt <- readLines(datafile, n = 3)

  stn_meta <- unlist(strsplit(txt[1], split = "\\s*,\\s*"))
  meta_names <- c("stn_id", "stn_name", "province",
                  "stn_joined", "element", "unit",
                  "stn_last_updated")
  if (length(stn_meta) != length(meta_names)) {
    stop("Unexpected format of metadata in file ", datafile,
         call. = FALSE)
  }
  names(stn_meta) <- meta_names

  header <- gsub("^\\s+|\\s+$", "", txt[3])
  header <- strsplit(gsub("\\s+((Day)\\s0?)?", ",\\2", header), ",")[[1]]

  if (!length(header) == 33) {
    stop("Unexpected data format in ", datafile, call. = FALSE)
  }

  data <- readr::read_fwf(
    datafile,
    col_positions = readr::fwf_widths(c(6, 3, rep(8, 31)), col_names = header),
    na = c("-9999.9", "-9999.9M"), skip = 4,
    col_types = paste0("ii", paste0(rep("c", 31), collapse = ""))
  )

  for (i in seq_along(stn_meta)) {
    data[[names(stn_meta)[i]]] <- stn_meta[i]
  }

  data <- tidyr::pivot_longer(data, cols = starts_with("Day"),
                              names_to = "DoM", values_to = "temp")

  # Check for undocumented flags:
  if (any(grepl("[b-df-zB-DF-Z]", data$temp))) {
    stop("Unknown data quality flags in ", datafile, call. = FALSE)
  }

  data <- dplyr::mutate(
    data,
    date = as.Date(
      paste(Year, Mo, gsub("Day", "", DoM), sep = "-")
    ))

  data <- dplyr::mutate(
    data,
    flag = dplyr::case_when(
      grepl("[aA]", temp) ~ "adjusted",
      grepl("[eE]", temp) ~ "estimated",
      TRUE ~ NA_character_
    ),
    measure = dplyr::case_when(
      element == "Homogenized daily maximum temperature" ~ "daily_max",
      element == "Homogenized daily minimum temperature" ~ "daily_min",
      element == "Homogenized daily mean temperature" ~ "daily_mean",
      TRUE ~ NA_character_
    ),
    temp = as.numeric(gsub("[a-zA-Z]", "", temp))
  )

  data %>%
    dplyr::select(stn_id, stn_name, measure, date, year = Year,
                  month = Mo, temp, dplyr::everything(),
                  -DoM) %>%
    dplyr::filter(!is.na(date)) # Remove invalid dates
}

write_ahccd_data <- function(zipfiles, save_raw_txt = FALSE,
                           data_dir = "data/AHCCD_data") {

  stopifnot(length(zipfiles) == 3L)
  stopifnot(all(file.exists(zipfiles)))

  fpaths <- lapply(zipfiles, function(n) {
    files <- extract_ahccd_data(n, save_raw_txt = save_raw_txt, data_dir = data_dir)
    files
  })

  message("Writing parquet files for ", length(fpaths[[1]]),
          " stations, partitioning by stn_id, measure, and year")

  parquet_path <- file.path(data_dir, "parquet")

  purrr::pwalk(fpaths, function(x, y, z) {
    d1 <- read_ahccd_data_single(x)
    d2 <- read_ahccd_data_single(y)
    d3 <- read_ahccd_data_single(z)

    d <- dplyr::bind_rows(d1, d2, d3)
    d <- dplyr::group_by(d, stn_id, measure, year)
    arrow::write_dataset(d, parquet_path, format = "parquet")
  })

  parquet_path
}

get_ahccd_stations <- function() {
  stations_url <- "https://crd-data-donnees-rdc.ec.gc.ca/CDAS/products/EC_data/AHCCD_daily/Homog_Temperature_Stations_Gen3.xls"

  stations_file <- basename(stations_url)

  download.file(stations_url, destfile = stations_file)

  colnames <- names(readxl::read_excel(stations_file, skip = 2, n_max = 2))

  stations <- readxl::read_excel(stations_file, skip = 4, col_names = colnames)

  stations <- stations %>%
    janitor::clean_names() %>%
    dplyr::mutate(from = paste(from, x6, sep = "-"),
           to = paste(to, x8, sep = "-")) %>%
    dplyr::select(-x6, -x8)

  sf::st_as_sf(stations, coords = c("long_deg", "lat_deg"), crs = 4326)
}

get_target_stations <- function(stations, buffer) {
  bc_buff <- sf::st_buffer(st_union(bcmaps::bc_bound()), dist = buffer * 1000)
  stations <- sf::st_transform(stations, sf::st_crs(bc_buff))
  stations <- sf::st_intersection(stations, bc_buff, sparse = FALSE)
  stations
}
