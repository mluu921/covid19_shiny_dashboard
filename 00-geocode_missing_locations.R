library(tidyverse)
library(ggmap)

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
