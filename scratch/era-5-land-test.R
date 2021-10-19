# devtools::install_github("bevingtona/era5landDownloadTools")
library(era5landDownloadTools)
library(sf)
library(stars)
library(bcmaps)
library(dplyr)

# aoi <- bc_cities(ask = FALSE) %>%
#   filter(NAME == "Kamloops") %>%
#   st_buffer(dist = 200*1000) %>%
#   st_transform(4326)

aoi <- bc_bound() %>%
  st_buffer(50000) %>%
  st_transform(4326)

years <- 2020
months <- 7:8
days <- 1:31
hours <- 0:23

testera <- "ERA5-land-hourly_test_2020-2020y_07-08m_01-31d_00:00-23:00h_1vars.nc"

if (!file.exists(testera)) {
  testera <- era5land_download_hourly(aoi = aoi,
                                      aoi_name = "test",
                                      years = years,
                                      months = months,
                                      days = days,
                                      hours = hours,
                                      variables = "2m_temperature",
                                      user = "105160")
}

d <- stars::read_stars(testera)
st_crs(d) <- 4326

# daily max
agg_dates <- st_get_dimension_values(d, "time") %>%
  as.Date() %>%
  unique() %>%
  lubridate::as_datetime()

d_agg <- aggregate(d, by = agg_dates, FUN = max)
d_agg_test <- slice(d_agg, "time", 1) - 273.15

# plot(slice(d_agg, along = "time", 1:3))
