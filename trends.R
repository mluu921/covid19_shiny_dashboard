library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)
library(leaflet)

download_california_data <- function() {
  
  possibly_read_csv <- possibly(read_csv, otherwise = NA)
  
  url <-
    'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
  
  
  dat <- tibble(date = seq.Date(mdy('3/22/2020') - days(2), Sys.Date(), '1 day'),
                file = paste0(format(date, '%m-%d-%Y'), '.csv')) %>%
    mutate(
      url = paste0(url, file),
      data = map(
        url,
        ~ possibly_read_csv(.x, col_types = cols(.default = 'c'))
      )
    ) %>%
    select(date, data) %>%
    filter(., !is.na(data)) %>%
    unnest(data) %>% 
    janitor::clean_names() %>%
    filter(., str_detect(combined_key, 'California')) %>%
    mutate_if(., is.character, ~ parse_guess(.x))
  
  pop <- readxl::read_excel('california_county_population.xlsx') %>%
    mutate(
      county = str_remove_all(county, '\\.')
    )
  
  dat <- dat %>%
    mutate(
      county = paste0(admin2, ' County, California')
    ) %>%
    left_join(., pop) %>%
    mutate(
      rate = confirmed / population
    ) %>%
    group_nest(date) %>%
    slice(n()) %>%
    unnest(data) %>%
    select(
      date, combined_key, confirmed, deaths, population, long, lat, rate
    ) %>%
    mutate(
      rate_10000 = rate * 10000
    )
  
}

dat <- download_california_data()

dat



url <- 'https://raw.githubusercontent.com/codeforamerica/click_that_hood/master/public/data/california-counties.geojson'


leaflet() %>%
  addTiles() %>%
  addPolygons()


make_trend_plot <- function(data, county) {
  
  plot_dat <- data %>% filter(., admin2 %in% county)
  
  label_dat <- plot_dat %>% group_by(., admin2) %>% slice(n())
  
  plot <- ggplot(plot_dat,
                 aes(x = date, y = confirmed, color = admin2)) +
    geom_point() +
    geom_line() +
    geom_text_repel(
      data = label_dat,
      aes(label = admin2),
      direction = 'y',
      hjust = 0,
      nudge_x = .1,
      size = 5
    ) +
    scale_x_date(limits = c(mdy('3/22/2020'), Sys.Date())) +
    scale_y_continuous(limits = c(0, NA)) +
    theme_minimal() +
    theme(legend.position = 'none',
          axis.title.y = element_text(size = 15),
          axis.text = element_text(size = 12)) +
    geom_smooth(method = 'lm',
                linetype = 'dotted',
                alpha = .25,
                se = F) +
    labs(x = '', y = 'Confirmed COVID-19 Cases')
  
  return(plot)
}

make_trend_plot(data = dat1, c('Los Angeles', 'San Diego'))





































