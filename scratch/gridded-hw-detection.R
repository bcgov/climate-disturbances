library(targets)
library(dplyr)
library(stars)
library(heatwaveR)
library(tidyr)
library(sf)
library(future)
library(future.apply)

plan(multisession(workers = availableCores() - 1))

tar_load(daily_temps_stars_cube)
tar_load(area_of_interest)

# check how many valid pixels are in each time slice:
not_na_pixels <- !is.na(daily_temps_stars_cube$tmax)
num_pixels_per_slice <- apply(not_na_pixels, 3, sum)
tar_assert_identical(min(num_pixels_per_slice), max(num_pixels_per_slice))
num_pixels <- min(num_pixels_per_slice)

num_times <- length(st_get_dimension_values(daily_temps_stars_cube, "time"))

all_temps <- as.data.frame(daily_temps_stars_cube) %>%
  mutate(time = as.Date(time)) %>%
  rename(t = time, temp = tmax) %>%
  filter(!is.na(temp)) %>% # This assumes that each pixel either has a complete time series, or all temps are NA. I think this is reasonable
  mutate(pixel_id = paste(x, y, sep = ";"))

# Assign each pixel to an LHA
lha_pixel_points <- all_temps %>%
  dplyr::select(pixel_id, x, y) %>%
  distinct() %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  st_join(st_transform(bcmaps::health_lha(), 4326) %>%
            dplyr::select(LOCAL_HLTH_AREA_CODE, LOCAL_HLTH_AREA_NAME))

lha_pixels <- st_drop_geometry(lha_pixel_points) %>%
  group_by(LOCAL_HLTH_AREA_CODE) %>%
  summarise(n_lha_pixels = n())

all_temps_split <- split(all_temps, all_temps$pixel_id)
tar_assert_identical(length(all_temps_split), num_pixels)

lapply(all_temps_split, \(x) {
  tar_assert_identical(nrow(x), num_times)
})

# Calculate climatologies
clims <- future_lapply(all_temps_split, ts2clm,
                       climatologyPeriod = c("1990-04-01", "2020-01-01"),
                       future.seed = 13L)

# Add 30C minimum threshold as new column (logical)
clims <- lapply(clims, \(x) {x$thresh2 <- x$temp >= 30; x})

# Detect events based on climatology
events <- future_lapply(clims, \(x) {
  detect_event(x, minDuration = 2, threshClim2 = x$thresh2)
}, future.seed = 13L)

events_daily <- lapply(names(events), \(x) {
  event <- events[[x]]$climatology
  event$pixel_id <- x
  event[c("pixel_id", setdiff(names(event), "pixel_id"))]
}) %>%
  bind_rows() %>%
  tidyr::separate(pixel_id, into = c("x", "y"), ";",
                  remove = FALSE, convert = TRUE) %>%
  filter(event == TRUE)

# calcualte HW stats by LHA by date.
# TODO - min/max/sd of temps
# TODO - rename appropriate columns and input into detect_event for LHA-level event summary
daily_lha_event_summary <- events_daily %>%
  left_join(st_drop_geometry(lha_pixel_points), by = "pixel_id") %>%
  group_by(LOCAL_HLTH_AREA_CODE, LOCAL_HLTH_AREA_NAME, t, doy) %>%
  summarise(mean_temp = mean(temp, na.rm = TRUE),
            mean_seas = mean(seas, na.rm = TRUE),
            mean_thresh = mean(thresh, na.rm = TRUE),
            n = n()) %>%
  left_join(lha_pixels) %>%
  mutate(percent_pixels = (n / n_lha_pixels) * 100)

events_summary_by_pixel <- lapply(names(events), \(x) {
  event <- events[[x]]$event
  event$pixel_id <- x
  event[c("pixel_id", setdiff(names(event), "pixel_id"))]
}) %>%
  bind_rows() %>%
  tidyr::separate(pixel_id, into = c("x", "y"), ";",
                  remove = FALSE, convert = TRUE) %>%
  left_join(st_drop_geometry(lha_pixel_points), by = "pixel_id")

filter(events_daily, t == as.Date("1995-06-28")) %>%
  dplyr::select(x, y, temp) %>%
  st_as_sf(coords = c("x", "y"), crs = 4326) %>%
  as("Spatial") %>%
  raster::rasterize(field = "temp",
                    as(slice(daily_temps_stars_cube, "time", 1), "Raster")) %>%
  st_as_stars() %>%
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
