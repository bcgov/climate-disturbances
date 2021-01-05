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


# Get Data ----------------------------------------------------------------

get_climate_data <- function(ids, data_dir = "data/weather", interval = "day") {
  if(!dir.exists(data_dir)) dir.create(data_dir)

  potential_paths <- file.path(data_dir, ids)
  needed_stations <- ids[!dir.exists(potential_paths)]

  message(paste0(length(needed_stations),
                 " of ",
                 length(potential_paths),
                 " stations need to be downloaded"))

  purrr::walk(needed_stations, ~{
    d <- weather_dl(.x, interval = interval)
    if (!dir.exists(file.path(data_dir, .x))) dir.create(file.path(data_dir, .x))
    write_parquet(d, sink = file.path(data_dir, .x, "data.parquet"))
    rm(d)
    gc()
  })

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

get_pm25_data <- function(...) {
  databc_pm25 <- "ftp://ftp.env.gov.bc.ca/pub/outgoing/AIR/AnnualSummary/2009-LatestVerified/PM25.csv"
  read_csv(databc_pm25,
           col_types = cols_only(DATE_PST = col_datetime(),
                                 EMS_ID = col_character(),
                                 STATION_NAME = col_character(),
                                 INSTRUMENT = col_character(),
                                 RAW_VALUE = col_double()))
}

# Spatial -----------------------------------------------------------------


stations_geo <- function(interval_var = 'day') {
  stations %>%
    filter(prov == "BC", interval == interval_var) %>%
    st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat") %>%
    transform_bc_albers()
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






