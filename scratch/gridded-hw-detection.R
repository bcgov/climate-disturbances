library(targets)
library(dplyr)
library(stars)
library(heatwaveR)
library(tidyr)
library(sf)
library(future)
library(future.apply)
library(viridisLite)

plan(multisession(workers = availableCores() - 1))

tar_load(daily_temps_stars_cube)
tar_load(area_of_interest)

# check how many valid pixels are in each time slice:
not_na_pixels <- !is.na(daily_temps_stars_cube$tmax)
num_pixels_per_slice <- apply(not_na_pixels, 3, sum)
tar_assert_identical(min(num_pixels_per_slice), max(num_pixels_per_slice))
num_pixels <- min(num_pixels_per_slice)

num_times <- length(st_get_dimension_values(daily_temps_stars_cube, "time"))

# Long data frame of tmax for every pixel for every date
all_temps <- as.data.frame(daily_temps_stars_cube) |>
  mutate(time = as.Date(time)) |>
  rename(t = time, temp = tmax) |>
  filter(!is.na(temp)) |> # This assumes that each pixel either has a complete time series, or all temps are NA. I think this is reasonable
  mutate(pixel_id = paste(x, y, sep = ";"))

# Assign each pixel to an LHA
lha_pixel_points <- all_temps |>
  dplyr::select(pixel_id, x, y) |>
  distinct() |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  st_join(st_transform(bcmaps::health_lha(), 4326) |>
            dplyr::select(LOCAL_HLTH_AREA_CODE, LOCAL_HLTH_AREA_NAME))

# Lookup table of pixels and LHAs
lha_pixels <- st_drop_geometry(lha_pixel_points) |>
  group_by(LOCAL_HLTH_AREA_CODE) |>
  summarise(n_lha_pixels = n())

all_temps_split <- split(all_temps, all_temps$pixel_id)
tar_assert_identical(length(all_temps_split), num_pixels)

lapply(all_temps_split, \(x) {
  tar_assert_identical(nrow(x), num_times)
})

# Calculate climatologies for each pixel
pixel_clims_list <- future_lapply(all_temps_split, ts2clm,
                       climatologyPeriod = c("1990-04-01", "2020-01-01"),
                       future.seed = 13L)

pixel_clims <- bind_rows(pixel_clims_list, .id = "pixel_id")

# Detect events at each pixel based on climatology
events <- future_lapply(pixel_clims_list, \(x) {
  detect_event(x, minDuration = 2, S = FALSE)
}, future.seed = 13L)

events_summary_by_pixel <- lapply(names(events), \(x) {
  event <- events[[x]]$event
  event$pixel_id <- x
  event[c("pixel_id", setdiff(names(event), "pixel_id"))]
}) |>
  bind_rows() |>
  tidyr::separate(pixel_id, into = c("x", "y"), ";",
                  remove = FALSE, convert = TRUE) |>
  left_join(st_drop_geometry(lha_pixel_points), by = "pixel_id")

# Extract the long day-by-day identification of heatwaves at each pixel
events_clim_daily <- future_lapply(names(events), \(x) {
  event <- events[[x]]$climatology
  event$pixel_id <- x
  event[c("pixel_id", setdiff(names(event), "pixel_id"))] |>
    tidyr::separate(pixel_id, into = c("x", "y"), ";",
                    remove = FALSE, convert = TRUE)
}) |>
  bind_rows()

# calculate climatology stats by LHA by date.
daily_lha_clim_summary <- events_clim_daily  |>
  left_join(st_drop_geometry(lha_pixel_points), by = "pixel_id") |>
  group_by(LOCAL_HLTH_AREA_CODE, LOCAL_HLTH_AREA_NAME, t, doy) |>
  summarise(across(.cols = c(temp, seas, thresh),
                   .fns = list(mean = mean, median = median, max = max, sd = sd),
                   na.rm = TRUE),
            across(temp,
                   .fns = setNames(lapply(c(0.1, 0.25, 0.75, 0.9), \(p) {
                     function(x) quantile(x, p, na.rm = TRUE, names = FALSE)
                   }),
                   c(0.1, 0.25, 0.75, 0.9))),
            n = n(),
            across(.cols = c(event, threshCriterion, durationCriterion),
                   .fns = ~ sum(as.numeric(.x), na.rm = TRUE) / n,
            .names = "{.col}_percent")) |>
  ungroup()

# Use LHA summary to identify events > 30 degrees, min 2 days
# Using 75 percentile of pixels in LHA
lha_events <- daily_lha_clim_summary |>
  mutate(thresh2 = temp_0.75 >= 30) |>
  select(LOCAL_HLTH_AREA_CODE,
         t, temp = temp_mean, seas = seas_mean,
         thresh = thresh_mean,
         thresh2) |>
  (\(x) split(x, x$LOCAL_HLTH_AREA_CODE))() |>
  lapply(\(x) {
    detect_event(x, threshClim2 = x$thresh2, minDuration = 2,
                 categories = TRUE, climatology = TRUE, S = FALSE)
  })

# LHA heatwaves - long table by date
lha_events_by_date <- lapply(lha_events, `[[`, "climatology") |>
  bind_rows(.id = "LOCAL_HLTH_AREA_CODE")

# LHA heatwave summaries
lha_events_summary <- lapply(lha_events, `[[`, "event") |>
  bind_rows(.id = "LOCAL_HLTH_AREA_CODE")

filter(events_clim_daily, t == as.Date("2009-07-29")) |>
  dplyr::select(x, y, temp) |>
  st_as_sf(coords = c("x", "y"), crs = 4326) |>
  as("Spatial") |>
  raster::rasterize(field = "temp",
                    as(slice(daily_temps_stars_cube, "time", 1), "Raster")) |>
  st_as_stars() |>
  plot(col = inferno(12), reset = FALSE)

plot(st_geometry(st_transform(area_of_interest, 4326)), border = "red", add = TRUE)

# test <- all_temps_split[[6883]]
#
# test_out_clim <- ts2clm(test, climatologyPeriod = c("1990-04-01", "2020-01-01"))
#
# test_out_events <- detect_event(test_out_clim)
#
# event_line(test_out_events, metric = "intensity_max",
#            start_date = "2000-04-01", end_date = "2020-09-30")

event_line(lha_events[[3]])
lolli_plot(lha_events[[3]])

