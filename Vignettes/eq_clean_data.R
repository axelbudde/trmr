eq_clean_data <- function(fileName) {



eq_location_clean <- fileName %>%
  replace_na(list("MONTH" = 1, "DAY" = 1)) %>%

  dplyr::mutate(DATE = julian(MONTH, DAY, YEAR)) %>%
  dplyr::mutate(DATE = as.Date(DATE, origin = "1970-01-01")) %>%

  dplyr::mutate(LATITUDE = as.numeric(LATITUDE)) %>%
  dplyr::mutate(LONGITUDE = as.numeric(LONGITUDE)) %>%

  dplyr::mutate(TOTAL_DEATHS = as.numeric(TOTAL_DEATHS)) %>%

  dplyr::mutate(LOCATION_NAME = gsub(".*: ", "", LOCATION_NAME)) %>%
  dplyr::mutate(LOCATION_NAME = gsub(":.*", "", LOCATION_NAME)) %>%
  dplyr::mutate(LOCATION_NAME = ifelse(LOCATION_NAME == COUNTRY, "", LOCATION_NAME)) %>%
  dplyr::mutate(LOCATION_NAME = str_to_title(LOCATION_NAME))
}


