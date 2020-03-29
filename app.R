library(tidyverse)
library(leaflet)
library(rvest)
library(geojsonio)
library(shiny)
library(shinydashboard)
library(lubridate)
library(plotly)

################################## loading the data
load_data <- function() {
    url <- 'http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm'
    geocode_locations <- read_rds('geo_data.rds')
    dat <- read_html(url) %>%
        html_table() %>% .[[1]] %>%
        as_tibble(.) %>%
        slice(which(str_detect(Locations, 'CITY/COMMUNITY') == T)+1:nrow(.)) %>%
        janitor::clean_names() %>%
        filter(!str_detect(locations, 'Investigation') & !str_detect(locations, 'Temple')) %>%
        mutate(., locations = str_remove_all(locations, '\\*')) %>%
        mutate(total_cases = as.numeric(total_cases)) %>%
        left_join(., geocode_locations) %>%
        mutate(
            total_case_label = ifelse(is.na(total_cases), '--', total_cases),
            label = glue::glue('<strong>{locations}</strong> <br> <strong>{total_case_label}</strong> Confirmed Cases')
        ) %>%
        mutate(
            label = map(label, ~ htmltools::HTML(.x))
        ) %>%
        filter(
            total_cases != 0 & locations != 'City of Los Angeles'
        )
    
    return(dat)
}

data <- load_data()

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
        filter(., confirmed != 0 & !is.na(population)) %>%
        mutate(
            rate_10000 = rate * 10000
        ) %>%
        mutate(
            label = glue::glue('<strong>{combined_key}</strong> <br> 
                               <strong>{confirmed}</strong> Confirmed Cases <br>
                               <strong>{deaths}</strong> Deaths <br>
                               <strong>{format(round(rate_10000, 2), 2)}</strong> per 10,000 people
                               ' )
        ) %>%
        mutate(
            label = map(label, ~ htmltools::HTML(.x))
        )
    
    return(dat)
    
}

download_california_county_timerseries <- function() {
    url <-
        'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
    
    data <- read_csv(url)
    
    ca <- data %>%
        filter(., state == 'California')
    
    return(ca)
}

california_timeseries_data <- download_california_county_timerseries()




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
    tags$style(type = "text/css", "#map {height: calc(100vh - 300px) !important;}"),
    tags$style(type = "text/css", "#california_map {height: calc(100vh - 300px) !important;}"),
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
                title = 'Time Series',
                selectInput(
                    'input_county',
                    'County',
                    choices = unique(california_timeseries_data$county),
                    selected = c('Los Angeles'),
                    selectize = T,
                    multiple = T
                ),
                plotlyOutput('figure_county_timerseries'),
                DT::dataTableOutput('table_incidence_rate')
            ), 
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
                    href = 'http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm'
                ),
                h4('Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE'),
                a(
                    'https://github.com/CSSEGISandData/COVID-19',
                    href = 'https://github.com/CSSEGISandData/COVID-19'
                ),
                h4('The New York Times, COVID-19 Data'),
                a('https://github.com/nytimes/covid-19-data', href = 'https://github.com/nytimes/covid-19-data')
                
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
                colorNumeric('YlOrRd', c(min(data$total_cases, na.rm = T), max(data$total_cases, na.rm = T)))
            
            map <- leaflet(data) %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addCircleMarkers(
                    .,
                    lng = ~ lon,
                    lat = ~ lat,
                    radius = ~ sqrt(total_cases)*5,
                    fillColor = ~ legend_colors(total_cases),
                    fillOpacity = .50,
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
            data <- download_california_data()
            
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
                    fillOpacity = .50,
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
                      caption = NULL, rownames = F)
        
    })
    
    output$california_table <- DT::renderDataTable({
        data <- download_california_data() %>% 
            select(., combined_key, confirmed, deaths, population, rate_10000) %>% 
            arrange(., desc(confirmed)) %>%
            mutate(rate_10000 = format(round(rate_10000, 2), 2)) %>%
            mutate(death_10000 = format(round((deaths / population) * 10000, 2), 2)) %>%
            mutate(combined_key = str_remove_all(combined_key, ', California, US'))  %>%
            select(., -population)
        
        DT::datatable(data, 
                      colnames = c('Location', 'Confirmed', 'Deaths', 'Case per 10,000', 'Death per 10,000'),
                      rownames = F)
    })
    
    output$box_last_update <- renderValueBox({
        
        last_update <- function() {
            url <-
                'http://www.publichealth.lacounty.gov/media/Coronavirus/locations.htm'
            
            last_update <- read_html(url) %>%
                html_node(xpath = '//*[@id="content"]/div[2]/table/caption') %>%
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
                html_node(xpath = '//*[@id="content"]/div[2]/table[1]/tbody/tr[6]/td') %>%
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
                html_node(xpath = '//*[@id="content"]/div[2]/table[1]/tbody/tr[1]/td') %>%
                html_text()
            
            return(cases)
        }
        
        valueBox(total_cases(), subtitle = 'Total Confirmed Cases', color = 'orange', icon = icon('warning'))
        
    })
    
    output$figure_county_timerseries <- renderPlotly({
        
        selected_county <- reactive({input$input_county})
        
        time_series_plot <- function(data, selected_county) {
            plot_data <- data %>%
                filter(county %in% selected_county)
            
            plot_ly(
                data = plot_data,
                x = ~ date,
                y = ~ cases,
                color = ~ county,
                colors = 'Set1',
                type = 'scatter',
                mode = 'lines+markers'
            ) %>%
                layout(
                    xaxis = list(title = ''),
                    yaxis = list(title = 'Confirmed COVID-19 Cases')
                )
            
        }
        
        time_series_plot(california_timeseries_data, selected_county())
    })
    
    output$table_incidence_rate <- DT::renderDataTable({
        
        # selected_county <- reactive({input$input_county})
        
        county_incidence_rate_table <- function(data) {
            rate_table <- data %>%
                group_nest(
                    county
                ) %>%
                mutate(
                    fit = map(data, ~ glm(cases ~ date, data = .x, family = poisson())),
                    res = map(fit, ~ broom::tidy(.x, exponentiate = T, conf.int = T))
                ) %>%
                unnest(res) %>%
                filter(., !str_detect(term, '(Intercept)')) %>%
                mutate(
                    p.value = ifelse(p.value < 0.001, '<0.001', format(round(p.value, 3), 3))
                ) %>%
                mutate_at(
                    ., c('estimate', 'conf.low', 'conf.high'), ~ format(round(.x, 3), 3)
                ) %>%
                select(., county, 'irr' = estimate, conf.low, conf.high, p.value) %>%
                arrange(., desc(irr))
            
            DT::datatable(rate_table, colnames = c('Incidence Rate', 'Conf.Low', 'Conf.High', 'p'),
                          caption = NULL, rownames = F)
        }
        
        county_incidence_rate_table(california_timeseries_data)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
































