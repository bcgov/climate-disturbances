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


# Health data -------------------------------------------------------------

##  Return LHA population by year

get_lha_popn <- function(year) {
  health_lha() %>%
    left_join(read_csv("data/demographics/lha_2010-2019-all-ages.csv") %>%
                filter(Year %in% year) %>%
                mutate(Region = as.character(Region)) %>%
                select(Region:Total),
              by = c("LOCAL_HLTH_AREA_CODE" = "Region", "LOCAL_HLTH_AREA_NAME" = "Local Health Area"))
}


get_lha_age <- function(year) {
  read_csv("data/demographics/lha_2010-2019-all-ages.csv") %>%
    filter(Year %in% year) %>%
    pivot_longer(cols = c(`0`:`90+`), names_to = 'age', values_to = 'n')
}



## Demographics that I want
## - Population
## - Age
## - Gender
## - Income

# library(cancensus)
# library(mapview)
# source('R/setup.R')
#
#
# lha_with_income <- health_lha() %>%
#   st_filter(
#     get_census('TX2018', regions=list(CMA="59"),
#                geo_format = 'sf', level=c("CT"), quiet = TRUE) %>%
#       transform_bc_albers()
#   ) %>%
#   select(LOCAL_HLTH_AREA_NAME, LOCAL_HLTH_AREA_CODE)
#
# lha_popn <- get_lha_popn(2019) %>%
#   st_filter(income)
#
#
# get_lha_age(2019) %>%
#   filter(Region %in% lha_popn$LOCAL_HLTH_AREA_CODE)
#
# mapview(income) +
#   mapview(lha, zcol = "Total")
