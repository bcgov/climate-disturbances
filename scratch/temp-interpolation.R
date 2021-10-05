library(bcmaps)
library(stars)
library(raster)
library(fields)
library(sf)
library(targets)
library(dplyr)
library(terra) # install.packages('terra', repos='https://rspatial.r-universe.dev')

tar_load(climate_stations)
tar_load(ahccd_parquet_path)

bbox <- bc_cities(ask = FALSE) %>%
  filter(NAME == "Kamloops") %>%
  st_buffer(dist = 100*1000) %>%
  st_transform(4326) %>%
  st_bbox()

vrtfile <- "data/test.vrt"

if (!file.exists(vrtfile)) {
  vrtfile <- cded(bbox, check_tiles = FALSE, dest_vrt = vrtfile)
}

dem_terra <- terra::rast(vrtfile)
dem_rast <- raster::raster(vrtfile)

# dem_rast_agg <- raster::aggregate(dem_rast, fact = 4)
# dem_terra_agg <- terra::aggregate(dem_terra, fact = 4)

stations <- st_filter(climate_stations, st_as_sfc(bbox)) %>%
  st_transform(st_crs(dem_terra))

system.time(
  stations$elevation <- raster::extract(dem_rast, as(stations, "Spatial"))
)

system.time(
  stations$elevation <- terra::extract(dem_terra, sf::st_coordinates(stations))
)

min_date <- as.Date("2020-07-01")
max_date <- as.Date("2020-09-01")
all_data <- arrow::open_dataset(ahccd_parquet_path) %>%
  filter(year == 2020, date >= min_date, date < max_date,
         stn_id %in% stations$stn_id,
         measure == "daily_max") %>%
  select(stn_id, date, temp) %>%
  collect()

stns_elev <- select(stations, stn_id, elevation)

day <- dates <- as.Date("2020-08-01")

temp_tps <- function(df) {
  coords <- sf::st_coordinates(df)
  df <- sf::st_drop_geometry(df)
  indep <- cbind(coords, df$elevation)

  fields::Tps(x = indep, Y = df$temp, lon.lat = TRUE, miles = FALSE)
}

# for (day in dates) {
  temp_data <- filter(all_data, date == day)
  stns_temp <- left_join(stns_elev, temp_data, by = "stn_id")
  mod <- temp_tps(stns_temp)

  system.time(
    # out_rast <- raster::interpolate(dem_rast, mod, xyOnly = FALSE)
  )

  system.time(
    out_terra <- terra::interpolate(dem_terra, mod, xyOnly = FALSE)
  )

  pred_temp_rast <- raster::extract(out_rast, stns_elev)
  pred_temp_terra <- raster::extract(out_terra, stns_elev)
  #interpolate using stn_temp and DEM
  #write surface
# }
