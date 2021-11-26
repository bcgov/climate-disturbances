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



library(targets)
library(tarchetypes)
# Read the documentation for tar_script() for help: Run ?tar_script to see the help file

tar_option_set(packages = desc::desc_get_deps()$package)
## Folders
dir.create("data", showWarnings = FALSE)


## To debug a target set the target:
#      tar_option_set(debug = "lha_events_by_date")
## and run:
#      tar_make(callr_function = NULL)
##  You can do:
#      tar_make(names = "lha_events_by_date", shortcut = TRUE, callr_function = NULL)
##  to only run the target of interest and skip checking upstream targets

## Source functions
r_files <- list.files("R", pattern = "*.R", full.names = TRUE)
dump <- lapply(r_files, source, echo = FALSE, verbose = FALSE)
if (.Platform$OS.type == "windows") options("arrow.use_threads" = FALSE)
future::plan(future::multisession(workers = 6))

## Create output directories:
hw_output_dir <- "out/heatwave_summaries"
dir.create(hw_output_dir, showWarnings = FALSE, recursive = TRUE)

# Load --------------------------------------------------------------------
# time variables
static_vars <- list(
  tar_target(start_date, as.Date('1990-04-01')),
  tar_target(end_date, as.Date("2020-09-30")),
  tar_target(raster_res, 0.05), # 0.05 degrees ~ 5km
  # For a list of possible LHAs, see: bcmaps::health_lha()$LOCAL_HLTH_AREA_NAME
  tar_target(LHAs, c("Greater Nanaimo",
                     "Kamloops",
                     "Central Okanagan",
                     "Greater Victoria",
                     "Langley",
                     "Surrey",
                     "South Surrey/White Rock",
                     "New Westminster",
                     "Delta",
                     "Burnaby",
                     "Richmond",
                     "Vancouver - City Centre",
                     "Vancouver - South",
                     "Vancouver - Westside",
                     "Vancouver - Midtown",
                     "Vancouver - Centre North",
                     "Vancouver - Northeast",
                     "West Vancouver/Bowen Island",
                     "North Vancouver"))
)


#  climate data
climate_targets <- list(
  tar_target(area_of_interest,
             bcmaps::health_lha() %>%
               dplyr::filter(LOCAL_HLTH_AREA_NAME %in% LHAs) %>%
               sf::st_transform(sf::st_crs(dem))),
  # #tar_target(pm25_data, pm25(area_of_interest, start_date = start_date, end_date = end_date)), ##pm data has some issues with arrow col specs
  # tar_target(weather_data, weather(area_of_interest, start_date = start_date, end_date = end_date, normals = FALSE, ask = FALSE)),
  # tar_target(area_burned_over_time, calc_area_burned_over_time(area_of_interest)),
  # tar_target(flood_example, hy_daily_flows("08NN002", start_date = start_date, end_date = end_date)),
  tar_target(
    ahccd_zipfiles,
    download_ahccd_data(data_dir = "data"),
    format = "file"
  ),
  tar_target(
    ahccd_parquet_path,
    write_ahccd_data(ahccd_zipfiles),
    format = "file"
  ),
  tar_target(climate_stations, get_ahccd_stations()),
  tar_target(target_stations,
             get_bc_target_stations(climate_stations,
                                    buffer = 200, # Buffer in Km
                                    crs = sf::st_crs(dem))),
  tar_target(dem, get_dem(res = raster_res)),
  tar_target(analysis_temps, arrow::open_dataset(ahccd_parquet_path) %>%
               dplyr::filter(date >= start_date, date <= end_date,
                      stn_id %in% target_stations$stn_id,
                      measure %in% c("daily_max", "daily_min")) %>%
               dplyr::collect()),
  tar_target(daily_tmax_models, model_temps_xyz(temp_data = dplyr::filter(analysis_temps, measure == "daily_max"),
                                           stations = target_stations,
                                           months = 1:12, future.seed = 13L)),
  tar_target(daily_temps_stars_cube,
             interpolate_daily_temps(daily_tmax_models,
                                     dem[area_of_interest], "tmax")),
  tar_target(out_ncdf, write_ncdf(daily_temps_stars_cube,
                                  path = paste0("out/data/daily_temps_",
                                                start_date, "-", end_date, ".nc")),
                                  format = "file")
)

heatwave_targets <- list(
  tar_target(pixel_clims, generate_pixel_climatologies(daily_temps_stars_cube,
                                                       start_date = start_date,
                                                       end_date = end_date)),
  tar_target(pixel_lha, pixel_lha_lookup(daily_temps_stars_cube, area_of_interest)),
  tar_target(pixel_events_clims, events_clim_daily(pixel_clims, future.seed = 13L)),
  tar_target(lha_clim_summary, summarize_lha_clims(pixel_events_clims, pixel_lha)),
  tar_target(lha_events, detect_lha_events(lha_clim_summary)),
  tar_target(lha_events_by_date, lapply(lha_events, `[[`, "climatology") |>
               dplyr::bind_rows(.id = "LOCAL_HLTH_AREA_CODE") |>
               dplyr::rename(date = t)),
  tar_target(lha_events_summary, lapply(lha_events, `[[`, "event") |>
               dplyr::bind_rows(.id = "LOCAL_HLTH_AREA_CODE"))
)

output_targets <- list(
  tar_target(
    lha_events_by_date_csv,
    write_csv_output(lha_events_by_date,
                     file.path(hw_output_dir, "lha_events_by_date.csv")),
    format = "file"
  ),
  tar_target(
    lha_events_summary_csv,
    write_csv_output(lha_events_summary,
                     file.path(hw_output_dir, "lha_events_summary.csv")),
    format = "file"
  )
)

# health sites
health_facilities <- list(
  tar_target(hospitals, bcdc_query_geodata("bc-health-care-facilities-hospital") %>%
               filter(INTERSECTS(area_of_interest)) %>%
               collect())
)


  # tidy --------------------------------------------------------------------

  processing_targets <- list(
    # tar_target(pm25_24h, pm25_data %>%
    #              rename(date_time = date_pst) %>%
    #              distinct() %>%
    #              pm_24h_caaqs(val = "raw_value", by = c("station_name", "ems_id", "instrument", "local_hlth_area_name", "hlth_service_dlvr_area_name"))),
    tar_target(heatwaves_raw, detect_heatwave(weather_data, pctile = 95, minDuration = 3)),
    tar_target(heatwaves, bind_heatwave_data(heatwaves_raw))
  )

  # Output ------------------------------------------------------------------


  ## Pipeline

  list(
    static_vars,
    climate_targets,
    heatwave_targets,
    output_targets,
    # processing_targets,
    # health_facilities,
    #tar_render(clim_overview, "out/climate-disturbance-overview.Rmd"),
    # tar_render(flood_examples, "out/flood-examples/flood-examples.Rmd"),
    # tar_render(heatwave_overview, "out/heatwave-overview.Rmd"),
    #tar_render(air_quality_examples, "out/air-quality-examples/air-quality-examples.Rmd")
    NULL
  )


