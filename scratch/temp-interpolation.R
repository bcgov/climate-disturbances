library(bcmaps)
library(stars)
library(raster)
library(fields)
library(sf)
library(targets)
library(dplyr)
# library(terra) # install.packages('terra', repos='https://rspatial.r-universe.dev')
library(tictoc)
library(ncdf4)
library(ggplot2)
library(tidyr)

source("scratch/era-5-land-test.R")

tar_load(climate_stations)
tar_load(ahccd_parquet_path)

# bbox <- bc_cities(ask = FALSE) %>%
#   filter(NAME == "Kamloops") %>%
#   st_buffer(dist = 200*1000) %>%
#   st_transform(4326) %>%
#   st_bbox()
bbox <- aoi

vrtfile <- "data/test_allbc.vrt"

if (!file.exists(vrtfile)) {
  vrtfile <- cded(bbox, check_tiles = FALSE, dest_vrt = vrtfile)
}

# dem_terra <- terra::rast(vrtfile)
# dem_rast <- raster::raster(vrtfile)
dem_stars <- stars::read_stars(vrtfile)

tic()
dem_stars_1km <- st_warp(dem_stars, st_as_stars(st_bbox(bbox), dx = 0.01),
                         method = "bilinear", use_gdal = TRUE)
toc()

dem_rast <- as(dem_stars, "Raster")

# dem_rast <- projectRaster(dem_rast, as(d_agg_test, "Raster"), method = "bilinear")
# dem_rast <- projectRaster(dem_rast, res = 1000, crs = "+init=epsg:3005", method = "bilinear")
# dem_stars <- st_as_stars(dem_rast)

# Warp to match resolution of ERA5-Land
tic("warp")
dem_stars_era5_res <- st_warp(dem_stars, d_agg_test, use_gdal = TRUE, method = "bilinear")
toc()

# dem_rast_agg <- raster::aggregate(dem_rast, fact = 4)
# dem_terra_agg <- terra::aggregate(dem_terra, fact = 4)

stations <- st_filter(climate_stations, st_buffer(aoi, 100000)) %>%
  st_transform(st_crs(dem_stars_1km))

# stations$elevation_rast_hres <- raster::extract(dem_rast, as(stations, "Spatial"))
# dem_rast_l <-as(dem_stars, "Raster")
# stations$elevation_rast_lres <- raster::extract(dem_rast_l, as(stations, "Spatial"))

min_date <- as.Date("2020-07-01")
max_date <- as.Date("2020-09-01")
all_data <- arrow::open_dataset(ahccd_parquet_path) %>%
  filter(year == 2020, date >= min_date, date <= max_date,
         stn_id %in% stations$stn_id,
         measure == "daily_max") %>%
  select(stn_id, date, temp) %>%
  collect() %>%
  mutate(temp = ifelse(temp < -9000, NA_real_, temp))

stns_temp <- left_join(stations, all_data, by = "stn_id")

temp_tps <- function(df, elev_col_name = "elev_m") {
  df <- na.omit(df)
  coords <- sf::st_coordinates(df)
  df <- sf::st_drop_geometry(df)
  indep <- cbind(coords, df[[elev_col_name]])
  colnames(indep) <- c("x", "y", "elevation")

  fields::Tps(x = indep, Y = df$temp, miles = FALSE)
}

dates <- seq(min_date, max_date, by = "1 day")

tic("stars era5 res")
stars_list_era5_res <- lapply(dates, function(day) {
  temp_data <- filter(stns_temp, date == day)
  mod <- temp_tps(temp_data)

  predict(dem_stars_era5_res, mod)
})

stars_cube_era5_res <- do.call("c", stars_list_era5_res)
stars_cube_era5_res <- st_redimension(stars_cube_era5_res, along = list(time = dates))
names(stars_cube_era5_res) <- "temp"
toc()

tic("stars 1km")
stars_list_1km <- lapply(dates, function(day) {
  temp_data <- filter(stns_temp, date == day)
  mod <- temp_tps(temp_data)

  predict(dem_stars_1km, mod)
})

stars_cube_1km <- do.call("c", stars_list_1km)
stars_cube_1km <- st_redimension(stars_cube_1km, along = list(time = dates))
names(stars_cube_1km) <- "temp"
toc()


## Test plot
ggplot(mapping = aes(fill = temp, col = temp)) +
  geom_stars(data = filter(stars_cube_1km, time == as.Date("2020-07-01"))) +
  geom_sf(data = filter(stns_temp, date == as.Date("2020-07-01"))) +
  scale_color_viridis_c(option = ) +
  scale_fill_viridis_c(option = "B")

# tic("raster")
# rast_list <- lapply(dates, function(day) {
#   temp_data <- filter(all_data, date == day)
#   stns_temp <- left_join(stns_elev, temp_data, by = "stn_id")
#   mod <- temp_tps(stns_temp)
#
#   raster::interpolate(dem_rast, mod, xyOnly = FALSE)
# })
# out_rast <- brick(rast_list)
#
# out_rast <- setZ(out_rast, dates, name = "time")
# names(out_rast) <- as.character(dates)
# toc()

stars_test_1km <- slice(stars_cube_1km, "time", 1)
stars_test_era5_res <- slice(stars_cube_era5_res, "time", 1)

d_agg_test <- st_crop(d_agg_test, st_union(st_as_sf(stars_test_era5_res)))

plot(stars_test_1km)
plot(stars_test_era5_res)
plot(d_agg_test)
plot(d_agg_test - stars_test_era5_res)
plot(abs(d_agg_test - stars_test_era5_res))

stations <- stations %>%
  left_join(all_data %>%
              filter(date == dates[1]) %>%
              select(stn_id, temp),
            by = "stn_id")

stations$temp_pred_1km <- st_extract(stars_test_1km, stations)[["temp"]]
stations$temp_pred_era5_res <- st_extract(stars_test_era5_res, stations)[["temp"]]
stations$temp_era5 <- st_extract(d_agg_test, stations)[["X00h_1vars.nc"]]

stations <- st_filter(stations, bbox, .predicate = st_within)

stations %>%
  st_drop_geometry() %>%
  pivot_longer(c("temp", "temp_pred_1km", "temp_pred_era5_res", "temp_era5"), names_to = "temp_type",
               values_to = "temp") %>%
  ggplot(aes(x = elev_m, y = temp, colour = temp_type)) +
  geom_point() +
  geom_smooth()

stations %>%
  st_drop_geometry() %>%
  mutate(diff_1km = temp_pred_1km - temp,
         diff_era5_res = temp_pred_era5_res - temp,
         diff_era5 = temp_era5 - temp) %>%
  pivot_longer(c("diff_1km", "diff_era5_res", "diff_era5"), names_to = "temp_type",
               values_to = "temp_diff") %>%
  ggplot(aes(x = elev_m, y = temp_diff, colour = temp_type)) +
  geom_point()
  # geom_smooth()

############### Testing reading

out_rast <- as(stars_cube_era5_res, "Raster")
out_rast_1km <- as(stars_cube_1km, "Raster")

# Write as netcdf
writeRaster(out_rast_1km, "rast-test_1km.nc", format = "CDF",
            xname = "x", yname = "y",
            varname = "temp", varunit = "degC",
            zname = "time", zunit = "days",
            overwrite = TRUE)

# Read in NetCDF as stars object
stars_in <- read_stars("rast-test_1km.nc", proxy = FALSE)
stars_in <- st_set_dimensions(stars_in, "time",
                              as.Date(st_get_dimension_values(stars_in, "time")))

nc <- nc_open("rast-test_1km.nc")
nc_crs <- ncatt_get(nc, "crs")
st_crs(stars_in) <- nc_crs$proj4
names(stars_in) <- "tmax"

p <- ggplot(mapping = aes(fill = tmax)) +
  geom_stars(data = stars_in, downsample = c(20, 20, 1)) +
  facet_wrap(vars(time)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text = element_blank())

p

ggsave("test.png", p)

################ Rayshader

day1 <- as(filter(stars_in, time == as.Date("2020-07-01")), "Raster")
matrix <- raster_to_matrix(day1)

matrix %>%
  height_shade(texture = viridisLite::viridis(256)) %>%
  plot_3d(matrix)

# Check PNWAMet

dat <- read_stars("data/PNWNAmet_tasmax_1990-2012.nc.nc")
st_crs(dat) <- 4326

bc_box <- bcmaps::bc_bbox(crs = 4326)

dat <- dat[bc_box]

dt <- as.POSIXct("1990-06-03", tz = "UTC")

filter(dat, time == dt) %>%
  plot(downsample = c(5,5,1))
