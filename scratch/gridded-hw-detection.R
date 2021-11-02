library(targets)
library(dplyr)
library(stars)
library(heatwaveR)

tar_load(daily_temps_stars_cube)

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
  filter(!is.na(temp)) # This assumes that each pixel either has a complete time series, or all temps are NA. I think this is reasonable

# system.time(
#   events <- all_temps %>%
#     group_by(x, y) %>%
#     group_modify(~event_only(.x))
# )

all_temps_split <- split(all_temps, list(all_temps$x, all_temps$y))

i <- sapply(all_temps_split, \(x) !all(is.na(x$temp)))
# which(i)
# choose 7728:7732 to test
i[7728:7732]
names(i[7728:7732])
dput(names(i[7728:7732]))
#
# c("-119.633204534209,51.4758720589969", "-119.583204534209,51.4758720589969",
#   "-119.533204534209,51.4758720589969", "-119.483204534209,51.4758720589969",
#   "-119.433204534209,51.4758720589969")

clims <- lapply(all_temps_split[7728:7732], ts2clm, climatologyPeriod = c("1990-04-01", "2020-01-01"))
events <- lapply(clims, detect_event, minDuration = 3)

# test <- all_temps_split[[6883]]
#
# test_out_clim <- ts2clm(test, climatologyPeriod = c("1990-04-01", "2020-01-01"))
#
# test_out_events <- detect_event(test_out_clim)
#
# event_line(test_out_events, metric = "intensity_max",
#            start_date = "2000-04-01", end_date = "2020-09-30")
