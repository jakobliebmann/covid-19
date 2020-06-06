# Preparation for ui and server ----
library(shiny)       # framework to generate this web-app
library(shinydashboard) # generate a dashboard frontend
library(shiny.i18n)  # translation
library(tidyverse)   # dplyr and co.
library(plotly)      # generates interactive web-graphics
library(wpp2019)     # used to get population data
library(countrycode) # used for assigning countries to continents
library(data.table)  # used for speed up the application

# Set Date-Output to english ----
Sys.setlocale("LC_TIME", "English")

# Import data ----
## Population data from wpp2019 ====
data(pop)

## Translation for Frontend ====
  translator <- Translator$new(translation_json_path = "../data/translations/translation.json")

## Getting raw Data from johns hopkins (absolute values) ====
df_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                         col_types = cols(Lat = col_skip(), Long = col_skip(), `Province/State` = col_skip()))
df_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                      col_types = cols(Lat = col_skip(), Long = col_skip(), `Province/State` = col_skip()))
df_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv",
                         col_types = cols(Lat = col_skip(), Long = col_skip(), `Province/State` = col_skip()))

df_covid <- data.frame()

## Getting raw Data from RKI Landkreise (absolute values) ====
df_rki_destrict <- read_csv("https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.csv")

## World - Convert it into series (of absolute values) ====
get_df_covid_john_hopkins <- function(){
  if(length(df_covid) == 0){
    l_df_confirmed <- df_confirmed %>%
      pivot_longer(cols = c(-`Country/Region`), names_to = "date") %>%
      group_by(country = `Country/Region`, date) %>%
      summarise(confirmed = sum(value))
    l_df_deaths <- df_deaths %>%
      pivot_longer(cols = c(-`Country/Region`), names_to = "date") %>%
      group_by(country = `Country/Region`, date) %>%
      summarise(deaths = sum(value))
    l_df_recovered <- df_recovered %>%
      pivot_longer(cols = c(-`Country/Region`), names_to = "date") %>%
      group_by(country = `Country/Region`, date) %>%
      summarise(recovered = sum(value))

## Building a dataframe ====
    l_covid <- l_df_confirmed %>%
      full_join(l_df_deaths) %>%
      full_join(l_df_recovered)
    
    l_covid$date <- lubridate::mdy(l_covid$date)

## Calculate daily new cases using the lag ====
    l_covid <- l_covid %>%
      group_by(country, date) %>%
      summarise(confirmed = sum(confirmed), deaths = sum(deaths), recovered = sum(recovered)) %>%
      mutate(new_confirmed = confirmed - lag(confirmed, n=1, order_by = date)) %>%
      mutate(new_deaths = deaths - lag(deaths, n=1, order_by = date)) %>%
      mutate(new_recovered = recovered - lag(recovered, n=1, order_by = date))

## Calculate amount of infected people ====
    l_covid <- l_covid %>%
      mutate(net_infected = confirmed - deaths - recovered)

## Getting countries ISO-codes ====
    l_covid$iso <- countrycode(sourcevar = l_covid$country,
                           origin = "country.name",
                           destination = "iso3n")

## Getting continents from package countrycode (manual work for Kosovo) ====
    l_covid$continent <- countrycode(sourcevar = l_covid$country,
                                 origin = "country.name",
                                 destination = "continent")

    l_covid <- l_covid %>%
      mutate(continent = case_when(
        country=="Kosovo" ~"Europe",
        TRUE ~ continent
      ))

## Drop tuples which are no countries (i.e. ships) ====
    l_covid <- l_covid %>%
      filter(!is.na(continent))

## Add population data ====
### Get population data from wpp2019 ####
    df_pop <- pop %>%
      select(iso = country_code, pop = "2020") %>%
      mutate(pop = pop*1000)
  
    l_covid <- l_covid %>%
      left_join(df_pop)

### Set missing population data manually ####
    l_covid <- l_covid %>%
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
    df_covid <- as.data.frame(l_covid)
  }
  return(df_covid)
}
# World - Covid as data table ----
get_dt_covid_world <- function(p_df_covid){
  if (length(covid) == 0){
    covid <- as.data.table(p_df_covid)
  }
  return(covid)
}

# Data for input panels ----
## World - Vector enables selection of a region ====
rl_value_world <- "World"
rl_value_continents <- "-------CONTINENTS-------"
rl_value_countries <- "-------COUNTRIES-------"
regionlist <- list() %>% prepend(c(rl_value_world, rl_value_continents)) %>% append(rl_value_countries)

## World - Vector enables selection of case-type ====
plotlist <- c("net_infected"
              , "confirmed"
              , "deaths"
              , "recovered"
              , "new_confirmed"
              , "new_deaths"
              , "new_recovered"
              )

## World - These values represent the boundaries of the selectable period ====
max_date <- lubridate::ymd("9999-12-31")
min_date <- lubridate::ymd("1900-01-01")
## World - Covid table initialize ====
covid <- data.table()

## Germany - Vector enables selection of federal states ====
choices_state_t2 <- "Federal States"
choices_state_pre <- paste("--- All" , choices_state_t2 , "---")
choices_state <- unique(df_rki_destrict$BL) 
choices_state <- sort(choices_state)
choices_state <- choices_state %>% prepend(choices_state_pre)

## UI Dynamic ====
### World - Vector enables selection of two regions ####
get_regionlist <- function(p_covid){
  continentslist <- p_covid$continent %>% unique()
  countrieslist <- p_covid$country %>% unique()
  l_regionlist <- continentslist %>% prepend(c(rl_value_world, rl_value_continents)) %>% append(rl_value_countries) %>% append(countrieslist)
  return(l_regionlist)
}

# Plotting functions ---------------------------------------------------------------
## World - Plottings ====
plotting <- function(regionchoice, plotchoice, daterange, switch_absolut_relative, p_dt_covid){
  if (length(p_dt_covid) == 0){
    return(NULL)
  }
    
  # Abort if there is no region choosen
  if(regionchoice == "empty"){
    return(NULL)
  }
  
  l_continentslist <- p_dt_covid$continent %>% unique()
   # wrangling the regionchoice
  if (regionchoice %in% c(rl_value_world, rl_value_continents, rl_value_countries)) {
    regionchoice <- rl_value_world
    used_data <- p_dt_covid
  }
  else if (regionchoice %in% l_continentslist) {
    used_data <- p_dt_covid[continent==regionchoice]
  }
  else {
    used_data <- p_dt_covid[country==regionchoice]
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
## UI-IDs of menu tabs ====
df_tab_ids <- data.frame(
  list(c("World", "Germany", "Settings", "About")
       , c("tab_country", "tab_germany", "tab_settings", "tab_about")
       #https://fontawesome.com/icons?d=gallery&q=globe&m=free
       #    "globe-europe"
       , c("globe", "globe-asia","users-cog", "qrcode")
  )
)
colnames(df_tab_ids) <- c("label","id","icon")

## Header of shiny app ====
getHeaderLabel <- function() {
  l_title <- translator$t("Self service analysis: Covid-19")
  return(l_title)  
}

getHeader <- function(){
  l_app_header <- dashboardHeader(
    title = textOutput(outputId = "appTitle")
  )
  return(l_app_header)
}

## World - Panel of input ====
getUIWorldInputPanel <- function(){
  l_World_Input <- box(
    title = "Select below",
    width = 12,
    collapsible = TRUE
    # i.A. update in server
    # regionlist
    , selectizeInput(inputId = "regionchoice"
                     , choices = regionlist
                     , label = "Select up to 2 regions of interest:"
                     , selected = c("Germany")
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
                  , value = c(min_date, max_date)
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
## World - Panel of output ====
getUIWorldOutputPanel <- function() {
  l_World_Output <- box(
    title = "Select above",
    width = 12,
    collapsible = TRUE,
    plotlyOutput("plot1"),
    plotlyOutput("plot2")
  )
  return(l_World_Output)
}

## GERMANY - Tab Box - Input elements ====
getUIGermanyInputTabBox <- function() {
  l_in_ger_tb <-           box(
    title = "Select below"
    , width = 12
    , collapsible = TRUE
    , selectInput(inputId = "federalState"
                  , label = "Select a federal state of interest:"
                  , choices = choices_state
                  , selected = c("Schleswig-Holstein")
    )
    , selectInput(inputId = "destrict"
                  , label = "Select a destrict of interest:"
                  , choices = NULL
    )
  )
  return(l_in_ger_tb)
}
## GERMANY - Tab Box - Output data table ====
getUIGermanyOutputTabBoxPlots <- function() {
  l_out_ger_tb_pl <- box(
    width = 12
    , tabBox(
      id = "tabsetPlotsGermany"
      , width = "400px"
      , tabPanel(
        "Cases"
        #                    , textOutput("plotStateTitle")
        , plotlyOutput("plotCases"
                       , width = 600
        )
      )
      , tabPanel(
        "Cases per 100k"
        , textOutput("plotStateTitle")
        , plotlyOutput("plotCasesPer100K"
                       , width = 600
        )
      )
      , tabPanel(
        "Deaths"
        , plotlyOutput("plotDeaths"
                       , width = 600
        )
      )
    )
  )
  return(l_out_ger_tb_pl)
}
## GERMANY - Tab Box - Output data table ====
getUIGermanyOutputTabBoxDataTables <- function() {
  l_out_ger_tb_dt <- box(
      title = "Details to selection"
      , width = 12
      , collapsible = TRUE
      , tabBox(
        id = "tabsetDetailsGermany"
        , width = "400px"
        , tabPanel("Result of input selection"
                   , tableOutput("dataState")
        )
        ### Germany - Output details to plot cases ####
        , tabPanel("Details to plot cases"
                   , tableOutput("dataDetailsCases")
        )
        ### Germany - Output details to plot cases per 100k ####
        , tabPanel("Details to plot cases per 100k"
                   , tableOutput("dataDetailsCases100k")
        )
        ### Germany - Output details to plot cases per 100k ####
        , tabPanel("Details to plot deaths"
                   , tableOutput("dataDetailsDeaths")
        )
      )
  )
  return(l_out_ger_tb_dt)
}
## Settings - Change Language ------ 
set_language <- function(p_language_id="Deutsch") {
  switch (p_language_id,
          "Deutsch" = {
            translator$set_translation_language("de")
            getHeader()
          }
          , {
            translator$set_translation_language("en")
            getHeader()
          }
  )
}
