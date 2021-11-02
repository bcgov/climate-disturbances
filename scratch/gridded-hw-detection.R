library(targets)
library(dplyr)
library(stars)
library(heatwaveR)

tar_load(daily_temps_stars_cube)

# check how many valid pixels are in each time slice:
not_na_pixels <- !is.na(daily_temps_stars_cube$tmax)
num_pixels_per_slice <- apply(not_na_pixels, 3, sum)
tar_assert_identical(min(num_pixels_per_slice), max(num_pixels_per_slice))
num_pixels <- min(num_pixels_per_slice)

num_times <- length(st_get_dimension_values(daily_temps_stars_cube, "time"))

# event_only <- function(df){
#   # First calculate the climatologies
#   clim <- ts2clm(data = df, climatologyPeriod = c("1990-04-01", "2020-01-01"))
#   # Then the events
#   event <- detect_event(data = clim)
#   # Return only the event metric dataframe of results
#   return(event$event)
# }

all_temps <- as.data.frame(daily_temps_stars_cube) %>%
  mutate(time = as.Date(time)) %>%
  rename(t = time, temp = tmax) %>%
  filter(!is.na(temp)) %>% # This assumes that each pixel either has a complete time series, or all temps are NA. I think this is reasonable
  mutate(pixel_id = paste(x, y, sep = ";"))
# system.time(
#   events <- all_temps %>%
#     group_by(x, y) %>%
#     group_modify(~event_only(.x))
# )

all_temps_split <- split(all_temps, all_temps$pixel_id)
tar_assert_identical(length(all_temps_split), num_pixels)

lapply(all_temps_split, \(x) {
  tar_assert_identical(nrow(x), num_times)
})

clims <- lapply(all_temps_split[1:20], ts2clm, climatologyPeriod = c("1990-04-01", "2020-01-01"))
events <- lapply(clims, detect_event, minDuration = 3)

# test <- all_temps_split[[6883]]
#
# test_out_clim <- ts2clm(test, climatologyPeriod = c("1990-04-01", "2020-01-01"))
#
# test_out_events <- detect_event(test_out_clim)
#
# event_line(test_out_events, metric = "intensity_max",
#            start_date = "2000-04-01", end_date = "2020-09-30")
