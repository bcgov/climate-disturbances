library(bcmaps)
library(stars)
library(raster)
library(fields)
library(sf)
library(targets)
library(dplyr)

tar_load(climate_stations)
tar_load(ahccd_parquet_path)

bbox <- bc_cities(ask = FALSE) %>%
  filter(NAME == "Kamloops") %>%
  st_buffer(dist = 200*1000) %>%
  st_transform(4326) %>%
  st_bbox()

vrtfile <- "data/test.vrt"

if (!file.exists(vrtfile)) {
  # dem_stars <- cded_stars(bbox, check_tiles = FALSE, dest_vrt = vrtfile)
  dem_rast <- cded_raster(bbox, check_tiles = FALSE)
} else {
  # dem_stars <- stars::read_stars(vrtfile, proxy = FALSE)
  dem_rast <- raster(vrtfile)
}

stations <- st_filter(climate_stations, st_as_sfc(bbox)) %>%
  st_transform(st_crs(dem_stars))

# stations$elevation <- st_extract(dem_stars, stations)
stations$elevation <- raster::extract(dem_rast, as(stations, "Spatial"))

min_date <- as.Date("2020-07-01")
max_date <- as.Date("2020-09-01")
all_data <- arrow::open_dataset(ahccd_parquet_path) %>%
  filter(year == 2020, date >= min_date, date < max_date,
         stn_id %in% stations$stn_id,
         measure == "daily_max") %>%
  select(stn_id, date, temp) %>%
  collect()


# extract elevations - store as z or column?

stns_elev <- select(stations, stn_id, elev_m)

dates <- as.Date("2020-08-01")

for (day in dates) {
  temp_data <- filter(all_data, date == day)
  stns_temp <- left_join(stns_elev, temp_data, by = c("stn_id" = "stn_id"))
  #interpolate using stn_temp and DEM
  #write surface
}
