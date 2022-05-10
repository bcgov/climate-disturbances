library(dplyr)
library(ggplot2)
library(lme4)
library(glmmTMB)
library(ggeffects)
library(glue)
library(targets)
library(scales)

plus_1990_trans <- function() trans_new("plus_1990", function(x) x + 1990, function(x) x - 1990)

tar_load(c("aoi_events_summary", "station_events_summary"))

aoi_events_summary <- aoi_events_summary |>
  mutate(year = lubridate::year(date_start),
         year_n = year - min(year))

aoi_events_by_year <- aoi_events_summary |>
  group_by(LOCAL_HLTH_AREA_CODE, year, year_n) |>
  tally()

aoi_n_mod <- glmmTMB(n ~ year_n + (1 | LOCAL_HLTH_AREA_CODE),
             data = aoi_events_by_year,
             family = poisson())
summary(aoi_n_mod)

aoi_n_mod_pred <- ggpredict(aoi_n_mod, terms = "year_n")
plot(aoi_n_mod_pred, add.data = TRUE)

aoi_percent_increase_n <- ((max(aoi_n_mod_pred$predicted) - min(aoi_n_mod_pred$predicted)) /
                       max(aoi_n_mod_pred$predicted)) * 100

ggplot() +
geom_point(aes(x = year_n, y = n), alpha =  0.5, data = events_by_year,
           position = position_jitter(width = 1, height = 0),
           colour = viridisLite::viridis(5)[3]) +
  geom_line(aes(x, predicted), data = aoi_n_mod_pred, colour = viridisLite::viridis(1),
            size = 1) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.1, fill = viridisLite::viridis(1),
              data = aoi_n_mod_pred) +
  theme_minimal() +
  # scale_x_continuous(labels = c(1990, 2000, 2010, 2020)) +
  scale_x_continuous(trans = "plus_1990") +
  labs(
    title = "Increasing Number of Heatwaves in B.C.",
    x = "Year",
    y = "Number of Heatwaves",
    caption = "Each point is a single LHA in a single year"
  )


aoi_int_mod_mixed <- glmmTMB(intensity_max ~ year + (1 | LOCAL_HLTH_AREA_CODE),
                     data = aoi_events_summary)
summary(aoi_int_mod_mixed)

aoi_int_mixed_pred <- ggpredict(aoi_int_mod_mixed, terms = "year")
plot(aoi_int_mod_mixed)

percent_increase <- ((max(aoi_int_mixed_pred$predicted) - min(aoi_int_mixed_pred$predicted)) /
  max(aoi_int_mixed_pred$predicted)) * 100

ggplot() +
  geom_point(aes(x = year, y = intensity_max), alpha = 0.3, data = aoi_events_summary,
             colour = viridisLite::viridis(5)[3]) +
  geom_line(aes(x, predicted), data = int_mixed_pred,
            colour = viridisLite::viridis(1), size = 1) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.1, fill = viridisLite::viridis(1),
              data = aoi_int_mixed_pred) +
  theme_minimal() +
  labs(
    title = glue("Increasing Intensity of Heat Waves in B.C."),
    x = "Year",
    y = "Heatwave Maximim Intensity"
  )

#######################
# By Station
#######################

station_events_summary <- station_events_summary |>
  mutate(year = lubridate::year(date_start),
         year_n = year - min(year))

station_events_by_year <- station_events_summary |>
  group_by(LOCAL_HLTH_AREA_CODE, year, year_n) |>
  tally()

stn_events_mixed_mod <- glmmTMB(n ~ year_n + (1 | LOCAL_HLTH_AREA_CODE),
                 data = station_events_by_year,
                 family = poisson())
summary(stn_events_mixed_mod)

stn_events_mixed_mod_pred <- ggpredict(stn_events_mixed_mod, terms = "year_n")
plot(stn_events_mixed_mod_pred, add.data = TRUE)

percent_increase_n_events_stn <- ((max(stn_events_mixed_mod_pred$predicted) - min(stn_events_mixed_mod_pred$predicted)) /
                         max(stn_events_mixed_mod_pred$predicted)) * 100

ggplot() +
  geom_point(aes(x = year_n, y = n), alpha =  0.5, data = station_events_by_year,
             position = position_jitter(width = 1, height = 0),
             colour = viridisLite::viridis(5)[3]) +
  geom_line(aes(x, predicted), data = stn_events_mixed_mod_pred, colour = viridisLite::viridis(1),
            size = 1) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.1, fill = viridisLite::viridis(1),
              data = stn_events_mixed_mod_pred) +
  theme_minimal() +
  # scale_x_continuous(labels = c(1990, 2000, 2010, 2020)) +
  scale_x_continuous(trans = "plus_1990") +
  labs(
    title = "Increasing Number of Heatwaves in B.C.",
    x = "Year",
    y = "Number of Heatwaves",
    caption = "Each point is a single LHA in a single year"
  )


intensity_stn_mixed_mod <- glmmTMB(intensity_max ~ year + (1 | LOCAL_HLTH_AREA_CODE),
                      data = station_events_summary)
summary(intensity_stn_mixed_mod)


intensity_stn_mixed_pred <- ggpredict(intensity_stn_mixed_mod, terms = "year")
plot(intensity_stn_mixed_pred, add.data = TRUE)

percent_increase_stn_intensity <- ((max(intensity_stn_mixed_pred$predicted) - min(intensity_stn_mixed_pred$predicted)) /
                       max(intensity_stn_mixed_pred$predicted)) * 100

ggplot() +
  geom_point(aes(x = year, y = intensity_max), alpha = 0.3, data = station_events_summary,
             colour = viridisLite::viridis(5)[3]) +
  geom_line(aes(x, predicted), data = intensity_stn_mixed_pred,
            colour = viridisLite::viridis(1), size = 1) +
  geom_ribbon(aes(x = x, ymin = conf.low, ymax = conf.high),
              alpha = 0.1, fill = viridisLite::viridis(1),
              data = intensity_stn_mixed_pred) +
  theme_minimal() +
  labs(
    title = glue("Increasing Intensity of Heat Waves in B.C."),
    x = "Year",
    y = "Heatwave Maximim Intensity"
  )

#######################


ggplot(aoi_events_by_year, aes(x = year, y = n)) +
  geom_point() +
  facet_wrap(vars(LOCAL_HLTH_AREA_CODE)) +
  geom_smooth(method = "lm")

ggplot(aoi_events_summary, aes(x = year, y = duration)) +
  geom_point() +
  facet_wrap(vars(LOCAL_HLTH_AREA_CODE)) +
  geom_smooth(method = "lm")

dur_mod <- lme(duration ~ year,
               random = ~ 1 | LOCAL_HLTH_AREA_CODE, data = station_events_summary)
summary(dur_mod)
