normals <- function(aoi, normals_years = "1981-2010", data_dir = "data/normals") {
  normals_stations <- weather_stations_geo() %>%
    dplyr::filter(normals) %>%
    sf::st_filter(aoi)

  parquet_dir <- file.path(data_dir, "normals.parquet")
  if (!dir.exists(data_dir)) dir.create(data_dir)

  if (file.exists(parquet_dir)) {
    existing_normals <- arrow::read_parquet(parquet_dir)
    not_present_ids <- setdiff(normals_stations$climate_id, unique(existing_normals$climate_id))
    if (purrr::is_empty(not_present_ids)) return(existing_normals)

    n <- weathercan::normals_dl(climate_ids = not_present_ids, normals_years = normals_years) %>%
      dplyr::select(-frost) %>%
      tidyr::unnest(cols = c(normals))

    n <- dplyr::bind_rows(existing_normals, n)
    n <- arrow::write_parquet(n, sink = parquet_dir)
  } else {
    n <- normals_dl(climate_ids = normals_stations$climate_id, normals_years = normals_years) %>%
      dplyr::select(-frost) %>%
      tidyr::unnest(cols = c(normals))
    n <- arrow::write_parquet(n, sink = parquet_dir)
  }

  return(n)
}


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
