library(bcmaps)
library(stars)
library(sf)
library(targets)
library(dplyr)

bbox <- bc_cities(ask = FALSE) %>%
  filter(NAME == "Kamloops") %>%
  st_buffer(dist = 200*1000) %>%
  st_transform(4326) %>%
  st_bbox()

dem_stars <- cded_stars(bbox, check = FALSE)
dem_rast <- cded_raster(bbox, check = FALSE)

tar_load(climate_stations)
tar_load(ahccd_parquet_path)

min_date <- as.Date("2020-07-01")
max_date <- as.Date("2020-09-01")
all_data <- arrow::open_dataset(ahccd_parquet_path) %>%
  filter(year == 2020, date >= min_date, date < max_date,
         stn_id %in% stations$stn_id,
         measure == "daily_max") %>%
  select(stn_id, date, temp) %>%
  collect()

stations <- st_filter(climate_stations, st_as_sfc(bbox)) %>%
  st_transform(st_crs(dem_stars))

stations$elevation <- st_extract(dem_stars, stations)
# extract elevations - store as z or column?

stns_elev <- select(stations, stn_id, elev_m)

dates <- as.Date("2020-08-01")

for (day in dates) {
  temp_data <- filter(all_data, date == day)
  stns_temp <- left_join(stns_elev, temp_data, by = c("stn_id" = "stn_id"))
  #interpolate using stn_temp and DEM
  #write surface
}
