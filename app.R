library(tidyverse)
library(leaflet)
library(rvest)
library(geojsonio)
library(shiny)
library(shinydashboard)
library(lubridate)

################################## loading the data
load_data <- function() {
    url <- 'http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm'
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

data <- load_data()

load_california_data <- function() {
    process_today_data <- function() {
        url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
        data <- read_csv(paste0(url, format(Sys.Date(), '%m-%d-%Y.csv'))) %>% janitor::clean_names()
        return(data)
    }
    
    process_yesterday_data <- function() {
        url <- 'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/'
        data <- read_csv(paste0(url, format(Sys.Date()-1, '%m-%d-%Y.csv'))) %>% janitor::clean_names()
        return(data)
    }
    
    try_today <- try(process_today_data())
    
    if(inherits(try_today, 'try-error') == T) {
        data <- process_yesterday_data()
    } else {
        data <- process_today_data()
    }
    
    data <- data %>%
        # filter(., country_region == 'US') %>%
        filter(., province_state == 'California') %>%
        mutate(
            combined_key = str_remove_all(combined_key, ', California, US'),
            label = glue::glue('<strong>{combined_key}</strong> <br> <strong>{confirmed}</strong> Confirmed Cases')
        ) %>%
        mutate(
            label = map(label, ~ htmltools::HTML(.x))
        ) %>%
        filter(
            confirmed != 0
        )
        
    
    return(data)
    
}

load_california_data()


################################## shiny dashboard components
header <- dashboardHeader(
    title = 'Los Angeles County COVID-19 Tracker',
    titleWidth = 400
)

sidebar <- dashboardSidebar(
    collapsed = T,
    disable = T
    
)

body <- dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(100vh - 200px) !important;}"),
    tags$style(type = "text/css", "#california_map {height: calc(100vh - 200px) !important;}"),
    tags$head(
        tags$script(
            '!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");'
        ),
        tags$link(rel = 'icon', type = 'image/png', href = 'www/logo.png')
    ),
    
    fluidRow(
        valueBoxOutput('box_last_update'),
        valueBoxOutput('box_total_cases'),
        valueBoxOutput('box_total_deaths')
        
    ),
    
    fluidRow(
        tabBox(
            width = 12,
            title = '',
            id = 'tabset1',
            tabPanel(title = 'LA County',
                     leafletOutput('map'),
                     DT::dataTableOutput('table_count')),
            tabPanel(title = 'California',
                     leafletOutput('california_map'),
                     DT::dataTableOutput('california_table')),
            tabPanel(
                title = 'Twitter',
                a(
                    class = "twitter-timeline",
                    href = "https://twitter.com/lapublichealth?ref_src=twsrc%5Etfw",
                    height = "1000"
                )
            ),
            tabPanel(
                title = 'Sources',
                h4('Los Angeles County, Department of Public Health'),
                a(
                    'http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm',
                    href = 'http://publichealth.lacounty.gov/media/Coronavirus/locations.htm'
                ),
                h4('Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE'),
                a(
                    'https://github.com/CSSEGISandData/COVID-19',
                    href = 'https://github.com/CSSEGISandData/COVID-19'
                )
                
            ),
            tabPanel(
                title = 'Contact',
                h4('Michael Luu, MPH'),
                a('Email', href = 'mailto:michael.luu@cshs.org'),
                br(),
                a('Website', href = 'http://luumichael.com')
                
            )
        )
        
    )
)
##################################

ui <- dashboardPage(
    header,
    sidebar,
    body
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$map <- renderLeaflet({
        make_map <- function() {
            legend_colors <-
                colorNumeric('YlOrRd', c(min(data$total_cases), max(data$total_cases)))
            
            map <- leaflet(data) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(
                    .,
                    lng = ~ lon,
                    lat = ~ lat,
                    radius = ~ sqrt(total_cases)*5,
                    fillColor = ~ legend_colors(total_cases),
                    fillOpacity = .75,
                    weight = 2,
                    color = 'black',
                    label = ~ label
                ) %>%
                addLegend(
                    pal = legend_colors,
                    values = ~ total_cases,
                    title = 'COVID-19 Cases'
                )
        }
        
        make_map()
    })
    
    output$california_map <- renderLeaflet({

        make_california_map <- function() {
            data <- load_california_data()
            
            legend_colors <-
                colorNumeric('YlOrRd', c(min(data$confirmed), max(data$confirmed)))
            
            leaflet(data) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(
                    .,
                    lng = ~ long,
                    lat = ~ lat,
                    radius = ~ sqrt(confirmed) * 2,
                    fillColor = ~ legend_colors(confirmed),
                    fillOpacity = .75,
                    weight = 2,
                    color = 'black',
                    label = ~ label
                ) %>%
                addLegend(pal = legend_colors,
                          values = ~ confirmed,
                          title = 'COVID-19 Cases')
            
        }
        
        make_california_map()
    })
    
    output$table_count <- DT::renderDataTable({
        table_data <- data %>% select(., locations, total_cases) %>% arrange(., desc(total_cases))
        
        DT::datatable(table_data, colnames = c('City', 'Confirmed'),
                      caption = NULL)
        
    })
    
    output$california_table <- DT::renderDataTable({
        data <- load_california_data() %>% select(., combined_key, confirmed, deaths) %>% arrange(., desc(confirmed))
        DT::datatable(data, colnames = c('Location', 'Confirmed', 'Deaths'))
    })
    
    output$box_last_update <- renderValueBox({
        
        last_update <- function() {
            url <-
                'http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm'
            
            last_update <- read_html(url) %>%
                html_node(xpath = '//*[@id="content"]/div[2]/div[1]/div[6]/p/small/text()') %>%
                html_text() %>%
                str_extract(., './.+') %>%
                paste0(., '/2020') %>%
                mdy(.) %>%
                format(., format = '%B %d, %Y')
            
            return(last_update)
        }
        
        valueBox(last_update(), subtitle = 'Last Updated', color = 'light-blue', icon = icon("feed"))
    })
    
    output$box_total_deaths <- renderValueBox({
        total_deaths <- function() {
            url <-
                'http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm'
            
            deaths <- read_html(url) %>%
                html_node(xpath = '//*[@id="content"]/div[2]/div[1]/div[4]/div/div') %>%
                html_text()
            
            return(deaths)
        }
        
        valueBox(total_deaths(), subtitle = 'Total Deaths', color = 'red', icon = icon("medkit"))
    })
    
    output$box_total_cases <- renderValueBox({
        total_cases <- function() {
            url <-
                'http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm'
            
            cases <- read_html(url) %>%
                html_node(xpath = '//*[@id="content"]/div[2]/div[1]/div[3]/div/div') %>%
                html_text()
            
            return(cases)
        }
        
        valueBox(total_cases(), subtitle = 'Total Confirmed Cases', color = 'orange', icon = icon('warning'))
        
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)
































