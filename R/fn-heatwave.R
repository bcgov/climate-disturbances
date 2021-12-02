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

read_ahccd_data_single <- function(datafile, n_max = Inf) {

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
    col_types = paste0("ii", paste0(rep("c", 31), collapse = "")),
    n_max = n_max
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

  data <- data |>
    dplyr::select(stn_id, stn_name, measure, date, year = Year,
                  month = Mo, temp, dplyr::everything(),
                  -DoM) |>
    dplyr::filter(!is.na(date)) # Remove invalid dates

  if (!is.infinite(n_max)) {
    return(head(data, n = n_max))
  }

  data
}

write_ahccd_data <- function(zipfiles, save_raw_txt = FALSE,
                             data_dir = "data", tbl_name = "ahccd_data") {

  stopifnot(length(zipfiles) == 3L)
  stopifnot(all(file.exists(zipfiles)))

  fpaths <- future.apply::future_lapply(zipfiles, function(n) {
    files <- extract_ahccd_data(n, save_raw_txt = save_raw_txt, data_dir = data_dir)
    files
  })

  data_dir <- normalizePath(data_dir, mustWork = TRUE)

  duckdb_path <- file.path(data_dir, "AHCCD_data", "duckdb", "ahccd.duckdb")

  if (file.exists(duckdb_path)) {
    del <- unlink(duckdb_path, recursive = TRUE, force = TRUE)
    if (!del) stop("unable to delete existing database: ", duckdb_path)
  }

  dir.create(dirname(duckdb_path), recursive = TRUE, showWarnings = FALSE)

  con <- duckdb_connect(duckdb_path, read_only = FALSE)

  message("Writing data to table '", tbl_name, "' in '", duckdb_path,
          "' duckdb database for ", length(fpaths[[1]]), " stations")

  DBI::dbCreateTable(con, tbl_name,
                     # get field names and types from the data we are about to read
                     fields = read_ahccd_data_single(fpaths[[1]][1], n_max = 1))

  purrr::pwalk(fpaths, function(x, y, z) {
    d1 <- read_ahccd_data_single(x)
    d2 <- read_ahccd_data_single(y)
    d3 <- read_ahccd_data_single(z)

    d <- dplyr::bind_rows(d1, d2, d3)
    DBI::dbAppendTable(conn = con, name = tbl_name, value = d)
  })

  purrr::walk(c("stn_id", "measure", "date", "year", "month"), \(x) {
    add_sql_index(con, colname = x)
  })

  attr(duckdb_path, "timestamp") <- as.character(Sys.time())
  duckdb_path
}

get_ahccd_stations <- function() {
  stations_url <- "https://crd-data-donnees-rdc.ec.gc.ca/CDAS/products/EC_data/AHCCD_daily/Homog_Temperature_Stations_Gen3.xls"

  stations_file <- normalizePath(file.path("data", "ahccd_sources", basename(stations_url)),
                                 mustWork = FALSE)
  dir.create(dirname(stations_file), showWarnings = FALSE)

  download.file(stations_url, destfile = stations_file, method = "curl")

  colnames <- names(readxl::read_excel(stations_file, skip = 2, n_max = 2))

  stations <- readxl::read_excel(stations_file, skip = 4, col_names = colnames)

  stations <- stations |>
    janitor::clean_names() |>
    dplyr::mutate(from = paste(from, x6, sep = "-"),
                  to = paste(to, x8, sep = "-")) |>
    dplyr::select(-x6, -x8)

  sf::st_as_sf(stations, coords = c("long_deg", "lat_deg"), crs = 4326)
}

#' Get stations in and around BC based on a buffer around BC
#'
#' @param stations data.frame of stations
#' @param buffer buffer distance in KMs
#' @param crs output crs
#'
#' @return sf object containing stations of interest
#' @export
get_bc_target_stations <- function(stations, buffer, crs) {
  bc_buff <- sf::st_buffer(st_union(bcmaps::bc_bound()), dist = buffer * 1000)
  stations <- sf::st_transform(stations, sf::st_crs(bc_buff))
  stations <- sf::st_intersection(stations, bc_buff, sparse = FALSE)
  sf::st_transform(stations, crs)
}

get_dem <- function(aoi = NULL, res) {
  if (is.null(aoi)) {
    aoi <- bcmaps::bc_bound() |>
      sf::st_buffer(50000) |>
      sf::st_transform(4326)
  }

  vrt <- bcmaps::cded(aoi, check_tiles = FALSE, ask = FALSE)
  dem_stars <- stars::read_stars(vrt)
  dem_stars_out <- stars::st_warp(
    dem_stars,
    stars::st_as_stars(sf::st_bbox(aoi), dx = res),
    method = "bilinear", use_gdal = TRUE,
    no_data_value = NA_real_
  )
  fname <- paste0("data/dem_stars_", as.character(res), ".tif")
  stars::write_stars(dem_stars_out, fname)
  stars::read_stars(fname, proxy = FALSE)
}

model_temps_xyz <- function(temp_data, stations, months, ...) {
  temp_data <- temp_data |>
    dplyr::filter(month %in% months)

  stations <- cbind(sf::st_drop_geometry(stations),
                    sf::st_coordinates(stations)) |>
    dplyr::select(stn_id, x = X, y = Y, elevation = elev_m)

  days <- unique(temp_data$date)

  out <- future.apply::future_lapply(days, function(d) {
    data <- temp_data[temp_data$date == d, , drop = FALSE]
    df <- dplyr::left_join(stations, data, by = "stn_id") |>
      dplyr::select(x, y, elevation, temp)
    temp_tps(df)
  }, ...)

  stats::setNames(out, as.character(days))
}

temp_tps <- function(df) {
  df <- na.omit(df)
  indep <- df[, c("x", "y", "elevation")]

  fields::Tps(x = indep, Y = df$temp, miles = FALSE)
}

interpolate_daily_temps <- function(model_list, dem, variable, path) {

  dir <- file.path(path, variable)
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  out_files <- future.apply::future_lapply(names(model_list), function(x) {
    mod <- model_list[[x]]
    pred <- predict(dem, mod)
    fname <- file.path(dir, paste0(x, ".tif"))
    stars::write_stars(pred, fname)
    fname
  }, future.packages = c("fields", "stars"))

  unlist(out_files)
}

make_stars_cube <- function(files, variable) {

  stars_proxy_obj <- stars::read_stars(files,
                          proxy = TRUE,
                          # cannot supply a list of dimensions to 'along' for proxy objects,
                          # so must do it after the fact
                          # along = list(time = as.Date(names(model_list)))
                          along = "time"
  )

  names(stars_proxy_obj) <- variable

  stars_fnames <- names(stars_proxy_obj[[variable]])
  dates <- as.Date(gsub(".*(\\d{4}-\\d{2}-\\d{2})\\.tif", "\\1", stars_fnames))

  stars_proxy_obj <- stars::st_set_dimensions(stars_proxy_obj, "time", dates)
  stars_proxy_obj
}

# write_ncdf <- function(cube, path) {
#   dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
#
#   out_rast <- stars:::st_as_raster(cube, "Raster")
#
#   # Write as netcdf
#   raster::writeRaster(out_rast, path, format = "CDF",
#               xname = "x", yname = "y",
#               varname = "temp", varunit = "degC",
#               zname = "time", zunit = "days",
#               overwrite = TRUE)
#   path
# }

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
  # Check each time period has the same number of pixels (3 is the time dimension)
  not_na <- stars::st_apply(stars_cube, "time", \(x) sum(!is.na(x)))

  num_pixels_per_slice <- stars::st_as_stars(not_na)$tmax
  targets::tar_assert_identical(min(num_pixels_per_slice), max(num_pixels_per_slice))
  num_pixels <- min(num_pixels_per_slice)

  times <- stars::st_get_dimension_values(stars_cube, "time")

  targets::tar_assert_inherits(start_date, "Date")
  targets::tar_assert_inherits(end_date, "Date")

  num_times <- length(times)

  # Long data frame of tmax for every pixel for every date
  all_temps <- as.data.frame(stars_cube) |>
    dplyr::filter(!is.na(tmax)) |> # This assumes that each pixel either has a complete time series, or all temps are NA. I think this is reasonable
    dplyr::mutate(time = as.Date(time)) |>
    dplyr::rename(t = time, temp = tmax) |>
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

#' Create lookup table of pixels:AOIs
#'
#' @param stars_cube stars cube of tmax for timeseries of interest
#' @param area_of_interest sf polygons
#'
#' @return tibble with x, y, pixel_id (concatenation of x & y),
#' and variables supplied in `group_vars`
pixel_aoi_lookup <- function(stars_cube, area_of_interest, group_vars) {
  rast <- dplyr::slice(stars_cube[area_of_interest], "time", 1)
  as.data.frame(rast) |>
    dplyr::select(-tmax) |>
    dplyr::mutate(pixel_id = paste(x, y, sep = ";")) |>
    sf::st_as_sf(coords = c("x", "y"), crs = sf::st_crs(stars_cube)) |>
    sf::st_join(
      dplyr::select(area_of_interest, {{group_vars}})
      ) |>
    dplyr::filter(dplyr::across({{group_vars}}, ~ !is.na(.)))
}

#' Extract the long day-by-day identification of heatwaves at each pixel
#'
#' @param clims_list output of `generate_pixel_climatologies()`
#' @param minDuration passed on `heatwaveR::detect_event()`
#' @param ... parameters passed on to `future_lapply`
#'
#' @return tibble of events (one row for each day*pixel)
events_clim_daily <- function(clims_list, minDuration = 2, ...) {
  # Detect events at each pixel based on climatology
  events_list <- future.apply::future_lapply(clims_list, \(x) {
    heatwaveR::detect_event(x, minDuration = minDuration, S = FALSE)
  }, ...)

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

#' Summarize climatologies of pixels by AOI
#'
#' @param event_clims output of events_clims_daily()
#' @param aoi_pixel_lookup output of pixel_aoi_lookup()
#'
#' @return data.frame of aoi climatology summary statistics
summarize_aoi_clims <- function(event_clims, aoi_pixel_lookup, group_vars) {

  # summarize climatology stats by AOI by date.
  event_clims |>
    dplyr::left_join(sf::st_drop_geometry(aoi_pixel_lookup), by = "pixel_id") |>
    dplyr::group_by(across({{group_vars}})) |>
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

#' Detect events aggregated by AOI.
#'
#' Compares AOI mean temperature to AOI mean threshold for each day,
#' and an additional threshold of 75th percentile tmax >=30C
#'
#' @param event_clims output of summarize_aoi_clims()
#' @param minDuration passed on `heatwaveR::detect_event()`
#'
#' @return list of event objects (from heatwaveR::detect_event());
#' one for each aoi
detect_aoi_events <- function(aoi_clim_summary, aoi_field, minDuration = 2) {
  # Use aoi summary to identify events, min 2 days
  # Using 75 percentile of pixels in aoi
  aoi_clim_summary |>
    dplyr::mutate(thresh2 = temp_0.75 >= 30) |>
    dplyr::select(dplyr::all_of(aoi_field),
                  t, temp = temp_mean, seas = seas_mean,
                  thresh = thresh_mean,
                  thresh2) |>
    (\(x) split(x, x[[aoi_field]]))() |>
    lapply(\(x) {
      heatwaveR::detect_event(x, threshClim2 = x$thresh2, minDuration = minDuration,
                              categories = TRUE, climatology = TRUE, S = FALSE)
    })
}

duckdb_connect <- function(db_path, read_only = TRUE) {
  DBI::dbConnect(duckdb::duckdb(), db_path, read_only = read_only)
}

add_sql_index <- function(con, tbl = "ahccd_data", colname,
                          idxname = paste0(tolower(colname), "_idx")) {
  sql_str <- sprintf("CREATE INDEX %s ON %s(%s)", idxname, tbl, colname)
  DBI::dbExecute(con, sql_str)
  invisible(NULL)
}

ahccd_tbl <- function(db_path, tbl = "ahccd_data") {
  con <- duckdb_connect(db_path, read_only = TRUE)
  # on.exit(DBI::dbDisconnect(con, shutdown = TRUE))
  dplyr::tbl(con, tbl)
}
