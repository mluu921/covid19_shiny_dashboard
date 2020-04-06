library(tidyverse)
library(rvest)
library(googlesheets4)

update_lac_dph_data <- function() {
  url <- 'http://publichealth.lacounty.gov/media/Coronavirus/locations.htm'
  
  dat <- read_html(url) %>%
    html_table() %>%
    .[[1]] %>%
    as_tibble()
  
  data <- dat %>%
    slice(which(str_detect(X1, 'CITY/COMMUNITY') == T)+1:nrow(.)) %>%
    rename( 'locations' = X1, 'count' = X2, 'rate' = X3) %>%
    mutate(
      date = Sys.Date()
    )
  
  sheets_write(data, '1lZ7QJ2jKC0fHh4f3FkyB4WC-7EUNwiBwP9wX9WjTdLw', sheet = as.character(Sys.Date()))
  
}


update_lac_dph_data_summary <- function() {
  url <- 'http://publichealth.lacounty.gov/media/Coronavirus/locations.htm'
  
  dat <- read_html(url) %>%
    html_table() %>%
    .[[1]] %>%
    as_tibble()
  
  data <- tibble(
    date = Sys.Date(),
    total_cases = as.numeric(dat[dat$X1 == "Laboratory Confirmed Cases (LCC)",]$X2),
    total_deaths = as.numeric(dat$X2[[7]])
  )
  
  # sheets_write(data, '1lZ7QJ2jKC0fHh4f3FkyB4WC-7EUNwiBwP9wX9WjTdLw', sheet = 'summary')
  sheets_append(data, '1lZ7QJ2jKC0fHh4f3FkyB4WC-7EUNwiBwP9wX9WjTdLw', sheet = 'summary')
  
}

update_lac_dph_data()
update_lac_dph_data_summary()







