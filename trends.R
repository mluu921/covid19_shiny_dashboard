library(tidyverse)
library(lubridate)
library(skimr)
library(ggrepel)

download_covid_timeseries <- function() {
  url <-
    'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
  
  
  dat <- tibble(date = seq.Date(mdy('1/27/2020'), Sys.Date(), '1 day'),
                file = paste0(format(date, '%m-%d-%Y'), '.csv')) %>%
    mutate(
      url = paste0(url, file),
      data = map(
        url,
        ~ read_csv(.x, col_types = cols(.default = 'c')) %>% janitor::clean_names()
      )
    ) %>%
    select(date, data) %>%
    unnest(data)
}


dat <- dat %>%
  select(date, data) %>%
  unnest(data)


dat1 <- dat %>%
  filter(
    !is.na(fips)
  )

dat1 <- dat1 %>%
  mutate_if(is.character, ~ parse_guess(.x)) %>% 
  filter(., str_detect(combined_key, 'California'))


pop <- readxl::read_excel('california_county_population.xlsx') %>%
  mutate(
    county = str_remove_all(county, '\\.')
  )


plot_dat <- dat1 %>%
  mutate(
    county = paste0(admin2, ' County, California')
  ) %>%
  left_join(., pop)






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





































