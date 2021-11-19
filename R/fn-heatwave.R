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

########### Create temperature surfaces

download_ahccd_data <- function(data_dir = "data") {
  base_url <- "https://crd-data-donnees-rdc.ec.gc.ca/CDAS/products/EC_data/AHCCD_daily"
  files_df <- rvest::html_table(rvest::read_html(base_url))[[1]]
  which_file <- grepl("daily_max_temp|daily_mean_temp|daily_min_temp", files_df$Name) & grepl("2020", files_df$Name) & tools::file_ext(files_df$Name) == "zip"
  fname <- files_df$Name[which_file]

  data_dir <- normalizePath(file.path(data_dir, "ahccd_sources"), mustWork = FALSE)
  dir.create(data_dir, showWarnings = FALSE)

  ret <- vapply(fname, function(n) {
    url <- paste0(base_url, "/", n)
    destfile <- file.path(data_dir, n)
    download.file(url, destfile = destfile)
    destfile
  }, FUN.VALUE = character(1), USE.NAMES = FALSE)

  attr(ret, "timestamp") <- as.character(Sys.time())
  ret
}

extract_ahccd_data <- function(zipfile, save_raw_txt = FALSE, data_dir) {
  exdir <- ifelse(save_raw_txt, file.path(data_dir, "raw_txt"), tempdir())
  if (!dir.exists(exdir)) dir.create(exdir, recursive = TRUE)
  ret <- unzip(zipfile, exdir = exdir)
  attr(ret, "timestamp") <- as.character(Sys.time())
  ret
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
    data[[names(stn_meta)[i]]] <- unname(stn_meta[i])
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
    temp = as.numeric(gsub("[a-zA-Z]", "", temp)),
    temp = ifelse(temp < -9000, NA_real_, temp)
  )

  data %>%
    dplyr::select(stn_id, stn_name, measure, date, year = Year,
                  month = Mo, temp, dplyr::everything(),
                  -DoM) %>%
    dplyr::filter(!is.na(date)) # Remove invalid dates
}

write_ahccd_data <- function(zipfiles, save_raw_txt = FALSE,
                             data_dir = "data") {

  stopifnot(length(zipfiles) == 3L)
  stopifnot(all(file.exists(zipfiles)))

  fpaths <- future.apply::future_lapply(zipfiles, function(n) {
    files <- extract_ahccd_data(n, save_raw_txt = save_raw_txt, data_dir = data_dir)
    files
  })

  data_dir <- normalizePath(data_dir, mustWork = TRUE)

  parquet_path <- file.path(data_dir, "AHCCD_data", "parquet")

  if (dir.exists(parquet_path)) {
    del <- unlink(parquet_path, recursive = TRUE, force = TRUE)
    if (!del) stop("unable to delete existing directory: ", parquet_path)
  }

  dir.create(parquet_path, recursive = TRUE, showWarnings = FALSE)

  message("Writing parquet files in ", parquet_path, "for ", length(fpaths[[1]]),
          " stations, partitioning by stn_id, year, and measure")


  # this could be done in parallel
  furrr::future_pwalk(fpaths, function(x, y, z) {
    d1 <- read_ahccd_data_single(x)
    d2 <- read_ahccd_data_single(y)
    d3 <- read_ahccd_data_single(z)

    d <- dplyr::bind_rows(d1, d2, d3)
    d <- dplyr::group_by(d, stn_id, year, measure)
    arrow::write_dataset(d, parquet_path, format = "parquet")
  })

  attr(parquet_path, "timestamp") <- as.character(Sys.time())
  parquet_path
}

get_ahccd_stations <- function() {
  stations_url <- "https://crd-data-donnees-rdc.ec.gc.ca/CDAS/products/EC_data/AHCCD_daily/Homog_Temperature_Stations_Gen3.xls"

  stations_file <- normalizePath(file.path("data", "ahccd_sources", basename(stations_url)),
                                 mustWork = FALSE)
  dir.create(dirname(stations_file), showWarnings = FALSE)

  download.file(stations_url, destfile = stations_file, method = "curl")

  colnames <- names(readxl::read_excel(stations_file, skip = 2, n_max = 2))

  stations <- readxl::read_excel(stations_file, skip = 4, col_names = colnames)

  stations <- stations %>%
    janitor::clean_names() %>%
    dplyr::mutate(from = paste(from, x6, sep = "-"),
                  to = paste(to, x8, sep = "-")) %>%
    dplyr::select(-x6, -x8)

  sf::st_as_sf(stations, coords = c("long_deg", "lat_deg"), crs = 4326)
}

get_bc_target_stations <- function(stations, buffer, crs) {
  bc_buff <- sf::st_buffer(st_union(bcmaps::bc_bound()), dist = buffer * 1000)
  stations <- sf::st_transform(stations, sf::st_crs(bc_buff))
  stations <- sf::st_intersection(stations, bc_buff, sparse = FALSE)
  sf::st_transform(stations, crs)
}

get_dem <- function(aoi = NULL, res) {
  if (is.null(aoi)) {
    aoi <- bcmaps::bc_bound() %>%
      sf::st_buffer(50000) %>%
      sf::st_transform(4326)
  }

  vrt <- bcmaps::cded(aoi, check_tiles = FALSE, ask = FALSE)
  dem_stars <- stars::read_stars(vrt)
  dem_stars_out <- stars::st_warp(
    dem_stars,
    stars::st_as_stars(sf::st_bbox(aoi), dx = res),
    method = "bilinear", use_gdal = TRUE
  )
  fname <- paste0("data/dem_stars_", as.character(res), ".tif")
  stars::write_stars(dem_stars_out, fname)
  stars::read_stars(fname, proxy = FALSE)
}

model_temps_xyz <- function(temp_data, stations, months) {
  temp_data <- temp_data %>%
    dplyr::filter(month %in% months)

  stations <- cbind(sf::st_drop_geometry(stations),
                    sf::st_coordinates(stations)) %>%
    dplyr::select(stn_id, x = X, y = Y, elevation = elev_m)

  days <- unique(temp_data$date)

  out <- future.apply::future_lapply(days, function(d) {
    data <- temp_data[temp_data$date == d, , drop = FALSE]
    df <- dplyr::left_join(stations, data, by = "stn_id") %>%
      dplyr::select(x, y, elevation, temp)
    temp_tps(df)
  }, future.seed = 13L)

  stats::setNames(out, as.character(days))
}

temp_tps <- function(df) {
  df <- na.omit(df)
  indep <- df[, c("x", "y", "elevation")]

  fields::Tps(x = indep, Y = df$temp, miles = FALSE)
}

interpolate_daily_temps <- function(model_list, dem, variable) {
  stars_list <- future.apply::future_lapply(model_list, function(mod) {
    predict(dem, mod)
  }, future.packages = c("fields", "stars"))

  dates <- as.Date(names(model_list))

  stars_cube <- do.call("c", stars_list)
  stars_cube <- stars::st_redimension(stars_cube, along = list(time = dates))
  names(stars_cube) <- variable
  stars_cube
}

write_ncdf <- function(cube, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  out_rast <- stars:::st_as_raster(cube, "Raster")

  # Write as netcdf
  raster::writeRaster(out_rast, path, format = "CDF",
              xname = "x", yname = "y",
              varname = "temp", varunit = "degC",
              zname = "time", zunit = "days",
              overwrite = TRUE)
  path
}

##### Heatwave detection

#' Generate climatologies for the period of record for all pixels
#'
#' @param stars_cube stars cube of tmax for timeseries of interest
#' @param start_date date
#' @param end_date date
#'
#' @return list of climatologies (outputs of heatwaveR::ts2clm);
#' one element for each pixel
generate_pixel_climatologies <- function(stars_cube, start_date, end_date) {
  not_na_pixels <- !is.na(stars_cube$tmax)
  num_pixels_per_slice <- apply(not_na_pixels, 3, sum)
  targets::tar_assert_identical(min(num_pixels_per_slice), max(num_pixels_per_slice))
  num_pixels <- min(num_pixels_per_slice)

  times <- stars::st_get_dimension_values(stars_cube, "time")

  targets::tar_assert_inherits(start_date, "Date")
  targets::tar_assert_inherits(end_date, "Date")

  num_times <- length(times)

  # Long data frame of tmax for every pixel for every date
  all_temps <- as.data.frame(stars_cube) |>
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::rename(t = time, temp = tmax) |>
    dplyr::filter(!is.na(temp)) |> # This assumes that each pixel either has a complete time series, or all temps are NA. I think this is reasonable
    dplyr::mutate(pixel_id = paste(x, y, sep = ";"))

  all_temps_split <- split(all_temps, all_temps$pixel_id)
  targets::tar_assert_identical(length(all_temps_split), num_pixels)

  lapply(all_temps_split, \(x) {
    targets::tar_assert_identical(nrow(x), num_times)
  })

  # Calculate climatologies for each pixel
  future.apply::future_lapply(all_temps_split, heatwaveR::ts2clm,
                climatologyPeriod = c(start_date, end_date),
                future.seed = 13L)
}

#' Create lookup table of pixels:LHAs
#'
#' @param stars_cube stars cube of tmax for timeseries of interest
#' @param area_of_interest sf polygons
#'
#' @return tibble with x, y, pixel_id (concatenation of x & y),
#' LOCAL_HLTH_AREA_CODE, LOCAL_HLTH_AREA_NAME
pixel_lha_lookup <- function(stars_cube, area_of_interest) {
  rast <- dplyr::slice(stars_cube, "time", 1)
  as.data.frame(rast) |>
    dplyr::select(-tmax) |>
    dplyr::mutate(pixel_id = paste(x, y, sep = ";")) |>
    sf::st_as_sf(coords = c("x", "y"), crs = st_crs(stars_cube)) |>
    sf::st_join(
      dplyr::select(area_of_interest,
                    LOCAL_HLTH_AREA_CODE, LOCAL_HLTH_AREA_NAME)
      ) |>
    dplyr::filter(!is.na(LOCAL_HLTH_AREA_CODE))
}

#' Extract the long day-by-day identification of heatwaves at each pixel
#'
#' @param clims_list output of generate_pixel_climatologies()
#'
#' @return tibble of events (one row for each day*pixel)
events_clim_daily <- function(clims_list) {
  # Detect events at each pixel based on climatology
  events_list <- future_lapply(clims_list, \(x) {
    heatwaveR::detect_event(x, minDuration = 2, S = FALSE)
  }, future.seed = 13L)

  # extract daily event stats and combine to table
  future_lapply(names(events_list), \(x) {
    event <- events_list[[x]]$climatology
    event$pixel_id <- x
    event[c("pixel_id", setdiff(names(event), "pixel_id"))] |>
      tidyr::separate(pixel_id, into = c("x", "y"), ";",
                      remove = FALSE, convert = TRUE)
  }) |>
    dplyr::bind_rows()
}

#' Summarize climatologies of pixels by LHA
#'
#' @param event_clims output of events_clims_daily()
#' @param lha_pixel_lookup output of pixel_lha_lookup()
#'
#' @return data.frame of LHA climatology summary statistics
summarize_lha_clims <- function(event_clims, lha_pixel_lookup) {

  # summarize climatology stats by LHA by date.
  lha_event_clims <- event_clims |>
    dplyr::left_join(sf::st_drop_geometry(lha_pixel_lookup), by = "pixel_id") |>
    dplyr::group_by(LOCAL_HLTH_AREA_CODE, LOCAL_HLTH_AREA_NAME, t, doy) |>
    dplyr::summarise(
      dplyr::across(.cols = c(temp, seas, thresh),
                    .fns = list(mean = mean, median = median, max = max, sd = sd),
                    na.rm = TRUE),
      dplyr::across(temp,
                    .fns = setNames(lapply(c(0.1, 0.25, 0.75, 0.9), \(p) {
                      function(x) quantile(x, p, na.rm = TRUE, names = FALSE)
                    }),
                    c(0.1, 0.25, 0.75, 0.9))),
      n = dplyr::n(),
      dplyr::across(.cols = c(event, threshCriterion, durationCriterion),
                    .fns = ~ sum(as.numeric(.x), na.rm = TRUE) / n,
                    .names = "{.col}_percent"),
      .groups = "drop")
}

#' Detect events aggregated by LHA.
#'
#' Compares LHA mean temperature to LHA mean threshold for each day,
#' and an additional threshold of 75th percentile tmax >=30C
#'
#' @param event_clims output of summarize_lha_clims()
#'
#' @return list of event objects (from heatwaveR::detect_event());
#' one for each LHA
detect_lha_events <- function(lha_clim_summary) {
  # Use LHA summary to identify events, min 2 days
  # Using 75 percentile of pixels in LHA
  lha_clim_summary |>
    dplyr::mutate(thresh2 = temp_0.75 >= 30) |>
    dplyr::select(LOCAL_HLTH_AREA_CODE,
                  t, temp = temp_mean, seas = seas_mean,
                  thresh = thresh_mean,
                  thresh2) |>
    (\(x) split(x, x$LOCAL_HLTH_AREA_CODE))() |>
    lapply(\(x) {
      heatwaveR::detect_event(x, threshClim2 = x$thresh2, minDuration = 2,
                              categories = TRUE, climatology = TRUE, S = FALSE)
    })
}

