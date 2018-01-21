#' Cleaning the NOAA data for furter processing
#' 
#' @description 
#' The eq_clean data() function takes the raw NOAA data frame and returns a clean data frame.
#' 
#'The clean data frame has the following:
#' 
#' 1. A DATE column created by uniting the year, month, day and converting it to the Date class
#' 2. LATITUDE and LONGITUDE columns converted to numeric class
#' 
#' @note 
#' When creating the DATE column the following need to be accounted for:
#' a) missing values (year, month or day)
#' b) negative years (B.C.)
#' 
#' @param A data frame in NOAA Significant Earthquake Database format
#' 
#' @return A cleaned NOAA data frame
#' 
#' @examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
#' earthquakes <- eq_location_clean(data)
#' }
 
eq_clean_data <- function(fileName) {
  require(magrittr)
  require(chron)
  require(stringr)
  require(tidyverse)
  
#' Cleaning the NOAALOCATION_NAME column
#' 
#' The function eq_location_clean() cleans the LOCATION_NAME column by stripping out the country name (including the colon) and converting the names to title case (as opposed to all caps).
#'
#' @param A data frame in NOAA Significant Earthquake Database format
#'
#' @return A NOAA data frame with a cleaned LOCATION_NAME column
#' 
#' @note The function is not exported
#'
#' @examples
#' \dontrun{
#' data <- readr::read_delim("earthquakes.tsv.gz", delim = "\t")
#' earthquakes <- eq_location_clean(data)
#' }
#'
#' @importFrom dplyr %>% mutate
#' @importFrom stringr str_replace str_trim str_to_title

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


