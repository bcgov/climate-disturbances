# Copyright 2021 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.



library(tidyhydat)
library(bcdata)
library(ggplot2)
library(bcmaps)
library(lubridate)

## Return a watershed based on a search string. This check the name of a city, watershed or river gauge station
## and then return whatever is needed
munipality_watersheds <- function(search_string) {
   m <- municipalities() %>%
    filter(grepl(search_string, ADMIN_AREA_NAME, ignore.case = TRUE))

   cm <- wsc_drainages() %>%
     st_filter(m)

   cw <- wsc_drainages() %>%
     filter(grepl(search_string, SUB_SUB_DRAINAGE_AREA_NAME, ignore.case = TRUE))

   cs <- wsc_drainages() %>%
     st_filter(
       hy_stations(prov_terr_state_loc = "BC") %>%
         filter(grepl(search_string, STATION_NAME, ignore.case = TRUE)) %>%
         st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
         transform_bc_albers()
     )

   bind_rows(cm, cw) %>%
     bind_rows(cs) %>%
     select(contains(c("DRAINAGE")))
}

munipality_river_data <- function(name_of_region) {
  stns <- hy_stations(prov_terr_state_loc = "BC") %>%
    st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
    transform_bc_albers()

  stns %>%
    st_join(munipality_watersheds(name_of_region), left = FALSE) %>%
    pull(STATION_NUMBER) %>%
    hy_daily_flows(start_date = '2000-01-01') %>%
    mutate(date_within_year = ymd(paste0('2016-',month(Date),"-", day(Date)))) %>%
    mutate(year = factor(year(Date))) %>%
    left_join(hy_stations()) %>%
    mutate(STATION = paste0(STATION_NAME, " (", STATION_NUMBER, ")")) %>%
    mutate(name_of_region = name_of_region)

}

plot_munipality_river_data <- function(.data, year_of_interest) {
  name_of_region <- unique(.data$name_of_region)

  ggplot(.data, aes(x = date_within_year, y = Value, colour = year, group = year)) +
    geom_line(alpha = 0.5) +
    geom_line(data = .data[.data$year == year_of_interest,], size = 2) +
    scale_x_date(date_labels = "%b") +
    scale_colour_viridis_d(name = 'Year') +
    labs(title = glue::glue("River Gauging Stations in Watersheds that Intersect '{name_of_region}'"),
         subtitle = glue::glue("Thicker line represents the year of flood ({year_of_interest})"),
         x = "Date", y = "River Flow (m^3/s)") +
    facet_wrap(vars(STATION), scales = "free_y") +
    theme_minimal() +
    theme(plot.title.position = 'plot')
}




# Pemberton 2003 ----------------------------------------------------------

munipality_river_data('Pemberton') %>%
  plot_munipality_river_data(2003)


# Kingcome 2010 -----------------------------------------------------------

munipality_river_data('Kingcome') %>%
  plot_munipality_river_data(2010)


# Grand Forks 2018 --------------------------------------------------------

munipality_river_data('Grand Forks') %>%
  plot_munipality_river_data(2018)




# Prince George 2007 ------------------------------------------------------

munipality_river_data('Prince George') %>%
  plot_munipality_river_data(2007)


# Smithers 2015 -----------------------------------------------------------

munipality_river_data('Smithers') %>%
  plot_munipality_river_data(2015)


# Bulkley 2007 ------------------------------------------------------------

munipality_river_data('Bulkley') %>%
  plot_munipality_river_data(2007)









mapview::mapview(stns) +mapview(flood_cities)

flood_cities <- bc_cities() %>%
  filter(grepl("Grand Forks|Lillooet|Sicamous|Dawson Creek|Cache Creek", NAME)) %>%
  st_buffer(30000)

flood_watersheds <- wsc_drainages() %>%
  st_join(flood_cities, left = FALSE)


flood_flows_stations <- stns %>%
  st_join(flood_watersheds, left = FALSE)

flood_flows <- flood_flows_stations %>%
  st_drop_geometry() %>%
  pull(STATION_NUMBER) %>%
  hy_daily_flows(start_date = "2000-01-01") %>%
  left_join(st_drop_geometry(flood_flows_stations))

flood_flows %>%
  filter(NAME == "Cache Creek") %>%
  ggplot(aes(x = Date, y = Value, colour = NAME)) +
  geom_line() +
  facet_wrap(vars(STATION_NAME), scales = "free")


floodplains <- bcdc_query_geodata('mapped-floodplains-in-bc-historical') %>%
  filter(INTERSECTS(grand_forks_bbox)) %>%
  collect()

floodplains_dem <- cded_stars(grand_forks_bbox)

tm_shape(floodplains_dem) +
  tm_raster(n = 60) +
  tm_shape(floodplains) +
  tm_polygons(alpha = 0.5)


bcdc_query_geodata('evacuation-orders-and-alerts')
