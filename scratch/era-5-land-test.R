# devtools::install_github("bevingtona/era5landDownloadTools")
library(era5landDownloadTools)
library(sf)
library(stars)
library(bcmaps)
library(dplyr)

aoi <- bc_cities(ask = FALSE) %>%
  filter(NAME == "Kamloops") %>%
  st_buffer(dist = 200*1000) %>%
  st_transform(4326)

years <- 2020
months <- 7:8
days <- 1:31
hours <- 0:23

testera <- era5land_download_hourly(aoi = aoi,
                                    aoi_name = "test",
                                    years = years,
                                    months = months,
                                    days = days,
                                    hours = hours,
                                    variables = "2m_temperature",
                                    user = "105160")

d <- stars::read_stars(testera)
