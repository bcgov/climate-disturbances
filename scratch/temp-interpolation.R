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

source("scratch/era-5-land-test.R")

tar_load(climate_stations)
tar_load(ahccd_parquet_path)

# bbox <- bc_cities(ask = FALSE) %>%
#   filter(NAME == "Kamloops") %>%
#   st_buffer(dist = 200*1000) %>%
#   st_transform(4326) %>%
#   st_bbox()
bbox <- aoi

vrtfile <- "data/test.vrt"

if (!file.exists(vrtfile)) {
  vrtfile <- cded(bbox, check_tiles = FALSE, dest_vrt = vrtfile)
}

# dem_terra <- terra::rast(vrtfile)
# dem_rast <- raster::raster(vrtfile)
dem_stars <- stars::read_stars(vrtfile)

## Aggregate to .01 degree (about 900m)
# dem_stars <- st_warp(dem_stars, crs = 3005, cellsize = 0.01, method = "cubic", use_gdal = TRUE)
# dem_rast <- as(dem_stars, "Raster")

# dem_rast <- projectRaster(dem_rast, as(d_agg_test, "Raster"), method = "bilinear")
# dem_rast <- projectRaster(dem_rast, res = 1000, crs = "+init=epsg:3005", method = "bilinear")
# dem_stars <- st_as_stars(dem_rast)

# Warp to match resolution of ERA5-Land
dem_stars <- st_warp(dem_stars, d_agg_test, use_gdal = TRUE, method = "bilinear")

# dem_rast_agg <- raster::aggregate(dem_rast, fact = 4)
# dem_terra_agg <- terra::aggregate(dem_terra, fact = 4)

stations <- st_filter(climate_stations, st_buffer(st_as_sfc(bbox), 100000)) %>%
  st_transform(st_crs(dem_stars))

stations$elevation_rast <- raster::extract(dem_rast, as(stations, "Spatial"))

min_date <- as.Date("2020-07-01")
max_date <- as.Date("2020-09-01")
all_data <- arrow::open_dataset(ahccd_parquet_path) %>%
  filter(year == 2020, date >= min_date, date <= max_date,
         stn_id %in% stations$stn_id,
         measure == "daily_max") %>%
  select(stn_id, date, temp) %>%
  collect() %>%
  mutate(temp = ifelse(temp < -9000, NA_real_, temp))

stns_elev <- select(stations, stn_id, elev_m)

temp_tps <- function(df, elev_col_name = "elev_m") {
  df <- na.omit(df)
  coords <- sf::st_coordinates(df)
  df <- sf::st_drop_geometry(df)
  indep <- cbind(coords, df[[elev_col_name]])
  colnames(indep) <- c("x", "y", "elevation")

  fields::Tps(x = indep, Y = df$temp, miles = FALSE)
}

dates <- seq(min_date, max_date, by = "1 day")

tic("stars")
stars_list <- lapply(dates, function(day) {
  temp_data <- filter(all_data, date == day)
  stns_temp <- left_join(stns_elev, temp_data, by = "stn_id")
  mod <- temp_tps(stns_temp)

  predict(dem_stars, mod)
})

stars_cube <- do.call("c", stars_list)
stars_cube <- st_redimension(stars_cube, along = list(time = dates))
names(stars_cube) <- "temp"
toc()


## Test plot
ggplot(mapping = aes(fill = temp, col = temp)) +
  geom_stars(data = filter(stars_cube, time == as.Date("2020-07-01"))) +
  geom_sf(data = filter(stns_temp, date == as.Date("2020-07-01"))) +
  scale_color_viridis_c()

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

# Write as netcdf
writeRaster(out_rast, "rast-test.nc", format = "CDF",
            xname = "x", yname = "y",
            varname = "temp", varunit = "degC",
            zname = "time", zunit = "days",
            overwrite = TRUE)


stars_test <- slice(stars_cube, "time", 1)

stations <- stations %>%
  left_join(all_data %>%
              filter(date == dates[1]) %>%
              select(stn_id, temp),
            by = "stn_id")

stations$temp_pred <- st_extract(stars_test, stations)[["temp"]]
stations$temp_pred_era5 <- st_extract(d_agg_test, stations)[["X00h_1vars.nc"]]


############### Testing reading

# Read in NetCDF as stars object
stars_in <- read_stars("rast-test.nc", )
stars_in <- st_set_dimensions(stars_in, "time",
                              as.Date(st_get_dimension_values(stars_in, "time")))

nc <- nc_open("rast-test.nc")
nc_crs <- ncatt_get(nc, "crs")
st_crs(stars_in) <- nc_crs$proj4
names(stars_in) <- "temp"

p <- ggplot(mapping = aes(fill = temp)) +
  geom_stars(data = stars_in) +
  facet_wrap(vars(time)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  theme(axis.text = element_blank())

ggsave("test.png", p)

################ Rayshader

day1 <- as(filter(stars_in, time == as.Date("2020-07-01")), "Raster")
matrix <- raster_to_matrix(day1)

matrix %>%
  height_shade(texture = viridisLite::viridis(256)) %>%
  plot_3d(matrix)
