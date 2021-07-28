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




library(dplyr)
library(bcmaps)
library(sf)
library(cancensus)
library(ggplot2)
library(scico)

low_income <- get_census('TX2018', regions=list(PR = '59'), vectors = c('families' = 'v_TX2018_607',
                                                                        'median_income' ='v_TX2018_29',
                                                                        'low_income_families' = 'v_TX2018_786'),
                         geo_format = 'sf', level=c("CT", "CSD"), quiet = TRUE) %>%
  mutate(share=`low_income_families`/families) %>%
  transform_bc_albers()


csd <- census_subdivision() %>%
  st_drop_geometry() %>%
  select(contains("CENSUS"))

low_income_csd <- low_income %>%
  left_join(csd, by = c("CSD_UID" = "CENSUS_SUBDIVISION_ID"))


low_income_csd %>%
  filter(!is.na(median_income)) %>%
  ggplot(aes(x = median_income, fill = CENSUS_DIVISION_NAME)) +
  geom_histogram(alpha = 0.5) +
  labs(title = "Distribution of Census Tract 2018 Median Incomes by Census Division",
       x = "Census Tract Median Income",
       y = "Number of Census Tracts") +
  scale_fill_scico_d() +
  facet_wrap(vars(CENSUS_DIVISION_NAME), scales = "free_y") +
  hrbrthemes::theme_ipsum(grid = "Y") +
  theme(plot.title.position = "plot")


hist(low_income_csd$share)

mapview::mapview(low_income_csd, zcol = "share")


p <- tm_shape(low_income_csd) +
  tm_fill(col = "share", palette = "viridis") +
  tm_facets(by = "CENSUS_DIVISION_NAME", nrow = 3)

tmap_save(p, filename = "low-income.png")



tm_shape(census_division()) +
  tm_borders() +
  tm_shape(low_income) +
  tm_fill(col = "share")

