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




library(targets)
library(dplyr)
library(lubridate)
library(weathercan)
library(heatwaveR)

tar_load(heatwaves)
source('R/setup.R')

## Areas with the most heatwaves
heatwaves$climatology %>%
  filter(between(month(t), 5, 9)) %>%
  count(station_id, event) %>%
  group_by(station_id) %>%
  mutate(total = sum(n)) %>%
  filter(event) %>%
  mutate(per = n/total) %>%
  left_join(weather_stations_geo() %>% mutate(station_id = as.character(station_id))) %>%
  arrange(desc(per))

heatwaves$event %>%
  filter(between(month(date_start), 5, 9)) %>%
  arrange(desc(duration)) %>%
  left_join(weather_stations_geo() %>% mutate(station_id = as.character(station_id))) %>%
  select(station_name, station_id, duration, date_start, date_end) %>%
  filter(year(date_start) == 2021)


heatwaves$event %>%
  distinct(station_id) %>%
  mutate(station_id = as.numeric(station_id)) %>%
  left_join(stations()) %>%
  select(station_id, station_name) %>%
  unique()

tar_load(heatwaves_raw)

plot(lubridate::month(heatwaves$event$date_peak), heatwaves$event$intensity_max)



foo <- heatwaves_raw[["261"]]

foo$event

heatwaves_raw[["114"]] %>%
  event_line(min_duration = 2, start_date = "2019-06-01", category = TRUE)


heatwaves_raw[["192"]] %>%
  event_line(min_duration = 2)

june_hw <- heatwaves_raw[["114"]] %>%
  pluck("climatology") %>%
  filter(t >= as.Date("2019-06-25"))

june_hw_top <- june_hw %>%
  filter(t >= as.Date("2021-06-25"))

ggplot(data = june_hw, aes(x = t)) +
  geom_flame(aes(y = temp, y2 = thresh, fill = "all"), show.legend = T) +
  geom_flame(data = june_hw_top, aes(y = temp, y2 = thresh, fill = "top"),  show.legend = T) +
  geom_line(aes(y = temp, colour = "temp")) +
  geom_line(aes(y = thresh, colour = "thresh"), size = 1.0) +
  geom_line(aes(y = seas, colour = "seas"), size = 1.2) +
  scale_colour_manual(name = "Line Colour",
                      values = c("temp" = "black",
                                 "thresh" =  "forestgreen",
                                 "seas" = "grey80")) +
  scale_fill_manual(name = "Event Colour",
                    values = c("all" = "salmon",
                               "top" = "red")) +
  scale_x_date(date_labels = "%b %Y") +
  guides(colour = guide_legend(override.aes = list(fill = NA))) +
  labs(y = expression(paste("Temperature [", degree, "C]")), x = NULL)

ggplot(june_hw, aes(x = t, y = temp, y2 = thresh)) +
  geom_flame() +
  geom_text(aes(x = as.Date("2011-02-25"), y = 25.8, label = "the Destroyer\nof Kelps"))



## Let look at the raw data to see the data ranges
s_df <- stations() %>%
  filter(prov == "BC") %>%
  filter(interval == "day") %>%
  select(station_name, station_id, start, end)

d <- open_dataset("data/weather/") %>%
  select(station_id, station_name, date) %>%
  group_by(station_id, station_name) %>%
  collect() %>%
  summarise(
    min = min(date),
    max = max(date)
    ) %>%
  left_join(s_df)

open_dataset("data/weather/") %>%
  filter(station_id == 52) %>%
  collect()
