library(tidyverse)
library(ggmap)
library(rvest)

gmapsapikey <- 'AIzaSyDlqaaSkZ-Mrk9YHQE_To-sYTB4AxqJ8eQ'
register_google(gmapsapikey)


load_data <- function() {
  url <- 'http://publichealth.lacounty.gov/media/Coronavirus/locations.htm'
  geocode_locations <- read_rds('geocoded_lacounty_cities.rds')
  dat <- read_html(url) %>%
    html_table() %>% .[[1]] %>%
    as_tibble(.) %>%
    slice(which(str_detect(Locations, 'CITY/COMMUNITY') == T)+1:nrow(.)) %>%
    janitor::clean_names() %>%
    filter(!str_detect(locations, 'Investigation')) %>%
    mutate(., locations = str_remove_all(locations, '\\*')) %>%
    mutate(total_cases = as.numeric(total_cases)) %>%
    left_join(., geocode_locations, by = c('locations' = 'city')) %>%
    mutate(
      label = glue::glue('<strong>{locations}</strong> <br> <strong>{total_cases}</strong> Confirmed Cases')
    ) %>%
    mutate(
      label = map(label, ~ htmltools::HTML(.x))
    )
  
  return(dat)
  
}

add_missing_locations_to_db <- function(current_scraped_data, current_geocode_data) {
  new_locations <- load_data() %>%
    select(
      locations, lon, lat
    ) %>%
    filter(
      is.na(lon)
    ) %>%
    mutate(
      query = glue::glue('{locations}, Los Angeles, CA')
    ) %>%
    select(
      -lon, -lat
    )
  
  new_locations <- geocode(new_locations$locations) %>%
    bind_cols(new_locations, .) %>%
    select(., -query) %>%
    rename(., 'city' = locations)
  
  updated_locations_data <- bind_rows(current_geocode_data, new_locations)
  
  return(updated_locations_data)
}


data <- load_data()

geo_data <- read_rds('geocoded_lacounty_cities.rds')

geo_data <- add_missing_locations_to_db(data, geo_data)

write_rds(geo_data, 'geocoded_lacounty_cities.rds')














