# Preparation for ui and server ----
library(shiny)       # framework to generate this web-app
library(tidyverse)   # dplyr and co.
library(plotly)      # generates interactive web-graphics
library(wpp2019)     # used to get population data
library(countrycode) # used for assigning countries to continents
library(data.table)  # used for speed up the application

# set Date-Output to english ----
Sys.setlocale("LC_TIME", "English")

# import data ----
## population data from wpp2019 ====
data(pop)

## Getting raw Data from johns hopkins (absolute values) ====
df_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                         col_types = cols(Lat = col_skip(), Long = col_skip(), `Province/State` = col_skip()))
df_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                      col_types = cols(Lat = col_skip(), Long = col_skip(), `Province/State` = col_skip()))
df_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                         col_types = cols(Lat = col_skip(), Long = col_skip(), `Province/State` = col_skip()))


# prepare data
## convert it into series (of absolute values) ====
df_confirmed <- df_confirmed %>%
  pivot_longer(cols = c(-`Country/Region`), names_to = "date") %>%
  group_by(country = `Country/Region`, date) %>%
  summarise(confirmed = sum(value))
df_deaths <- df_deaths %>%
  pivot_longer(cols = c(-`Country/Region`), names_to = "date") %>%
  group_by(country = `Country/Region`, date) %>%
  summarise(deaths = sum(value))
df_recovered <- df_recovered %>%
  pivot_longer(cols = c(-`Country/Region`), names_to = "date") %>%
  group_by(country = `Country/Region`, date) %>%
  summarise(recovered = sum(value))


## building a dataframe ====
covid <- df_confirmed %>%
  full_join(df_deaths) %>%
  full_join(df_recovered)
covid$date <- lubridate::mdy(covid$date)

## calculate daily new cases using the lag ====
covid <- covid %>%
  group_by(country, date) %>%
  summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>%
  mutate(new_confirmed = confirmed - lag(confirmed, n=1, order_by = date)) %>%
  mutate(new_deaths = deaths - lag(deaths, n=1, order_by = date)) %>%
  mutate(new_recovered = recovered - lag(recovered, n=1, order_by = date))

## calculate amount of infected people ====
covid <- covid %>%
  mutate(net_infected = confirmed - deaths - recovered)

## getting countries ISO-codes ====
covid$iso <- countrycode(sourcevar = covid$country,
                         origin = "country.name",
                         destination = "iso3n")

## getting continents from package countrycode (manual work for Kosovo) ====
covid$continent <- countrycode(sourcevar = covid$country,
                               origin = "country.name",
                               destination = "continent")

covid <- covid %>%
  mutate(continent = case_when(
    country=="Kosovo" ~"Europe",
    TRUE ~ continent
    ))

## drop tuples which are no countries (i.e. ships) ====
covid <- covid %>%
  filter(!is.na(continent))

## add population data ====
### get population data from wpp2019 ####
pop_df <- pop %>%
  select(iso = country_code, pop = "2020") %>%
  mutate(pop = pop*1000)
covid <- covid %>%
  left_join(pop_df)

### set missing population data manually ####
covid <- covid %>%
  mutate(pop = case_when(
    country=="Andorra" ~ 77006,
    country=="Dominica" ~ 71293,
    country=="Holy See" ~ 453,
    country=="Kosovo" ~ 1907592,
    country=="Liechtenstein" ~ 38650,
    country=="Monaco" ~ 37300,
    country=="Saint Kitts and Nevis" ~ 56000,
    country=="San Marino" ~ 33420,
    TRUE ~ pop
  ))

# covid as data table ----
covid <- as.data.table(covid)
      # covid %>%
      #   filter(is.na(pop)) %>%
      #   distinct(country)

# data for input panels ----
## vector enables selection of a region ====
continentslist <- covid$continent %>%  unique()
countrieslist <- covid$country %>%  unique()
regionlist <- continentslist %>% prepend(c("World", "-------CONTINENTS-------")) %>% append("-------COUNTRIES-------") %>% append(countrieslist)
## vector enables selection of case-type ====
plotlist <- c("net_infected", "confirmed", "deaths", "recovered", "new_confirmed", "new_deaths", "new_recovered")

## these values represent the boundaries of the selectable period ====
max_date <-(max(covid$date))
min_date <-(min(covid$date))

# plotting functions ---------------------------------------------------------------
plotting <- function(regionchoice, plotchoice, daterange, switch_absolut_relative){
  # Abort if there is no region choosen
  if(regionchoice == "empty"){
    return(NULL)
  }
  
   # wrangling the regionchoice
  if (regionchoice %in% c("World", "-------CONTINENTS-------", "-------COUNTRIES-------")) {
    regionchoice <- "World"
    used_data <- covid
  }
  else if (regionchoice %in% continentslist) {
    used_data <- covid[continent==regionchoice]
  }
  else {
    used_data <- covid[country==regionchoice]
  }
  
  # specific settings for absolut/relative  display 
  if (switch_absolut_relative == "Absolut"){
    divisor <- 1
    plottitle <- paste0(regionchoice, " - Number of persons")
    y_axis <- scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  }
  else {
    divisor <- used_data %>%
      distinct(country, pop) %>%
      summarise(sum(as.numeric(pop)))
    divisor <- divisor[[1]]
    plottitle <- paste0(regionchoice, " - percentage of the population")
    y_axis <- scale_y_continuous(labels = scales::percent)
  }
  
  # general plot-settings  
  plot <- used_data %>%
    filter(date >= min(daterange) & date <= max(daterange)) %>%
    group_by(date) %>%
    summarise_at(plotlist, sum, na.rm = TRUE) %>%
    mutate_at(plotlist, function(x) (x/divisor)) %>%
    ggplot() +
    y_axis +
    labs(title = plottitle) +
    theme(plot.title = element_text(size = 20)) +
    theme_minimal() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
  # construct layers  
  for (i in plotchoice){
    if (i == "confirmed"){
      plot <- plot + geom_area(aes(x = date, y = confirmed), fill = "darkred", color = "darkred", alpha = 0.5)
    }
  }   
  for (i in plotchoice){
    if (i == "recovered"){
      plot <- plot + geom_area(aes(x = date, y = recovered), fill = "darkgreen", color = "darkgreen", alpha = 0.5)
    }
  }
  for (i in plotchoice){
    if (i == "deaths"){
      plot <- plot + geom_area(aes(x = date, y = deaths), fill = "black", color = "black", alpha = 0.5)
    }
  }
  for (i in plotchoice){
    if (i == "net_infected"){
      plot <- plot + geom_line(aes(x = date, y = net_infected), color = "darkorange", size = 1)
    }
  }  
  for (i in plotchoice){
    if (i == "new_confirmed"){
      plot <- plot + geom_line(aes(x = date, y = new_confirmed), color = "red")
    }
  }   
  for (i in plotchoice){
    if (i == "new_recovered"){
      plot <- plot + geom_line(aes(x = date, y = new_recovered), color = "green")
    }
  }
  for (i in plotchoice){
    if (i == "new_deaths"){
      plot <- plot + geom_line(aes(x = date, y = new_deaths), color = "black")
    }
  }
  return(ggplotly(plot))
}

# UI-Panels ---------------------------------------------------------------
## World Input ====
getUIWorldInputPanel <- function(){
  l_World_Input <- sidebarPanel(
  selectizeInput(inputId = "regionchoice"
                 , label = "Select up to 2 regions of interest:"
                 , choices = regionlist, selected = c("Germany")
                 , options = list(maxItems = 2)
  )
  , checkboxGroupInput(inputId = "plotchoice_values"
                       , label = "Select values:"
                       , choices = plotlist[1:4]
                       , selected = c("net_infected")
                       , inline = FALSE
  )
  , checkboxGroupInput(inputId = "plotchoice_trend"
                       , label = "Select trend values:"
                       , choices = plotlist[5:7]
                       , inline = FALSE
  )
  , sliderInput(inputId = "daterange"
                , label = "What Period are you interested in?"
                , min = min_date
                , max = max_date
                , value = c(min(covid$date), max(covid$date))
                , timeFormat="%Y-%m-%d"
  )
  , radioButtons(inputId = "switch_absolut_relative"
                 , label ="Type of display:"
                 , choices = c("Absolut", "Relative")
                 , selected = "Absolut"
                 , inline = TRUE
  )
  )
  return (l_World_Input)
}
## World Output ====
getUIWorldOutputPanel <- function() {
  l_World_Output <- mainPanel(
    plotlyOutput("plot1")
    , plotlyOutput("plot2")
  )
  return(l_World_Output)
}
