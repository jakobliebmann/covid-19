############################
# Import libraries         #
############################

library(shiny)       # web-app
library(tidyverse)   # dplyr and co.
library(plotly)      # web-graphics
library(gapminder)   # used for continents and population

# set Date-Output to english
Sys.setlocale("LC_TIME", "English")

############################
# Creation of a data basis #
############################

# Getting raw Data from johns hopkins (absolute values)
df_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
df_deaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
df_recovered <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")

# convert it into series (of absolute values)
df_confirmed <- df_confirmed %>%
  pivot_longer(cols = c(-`Province/State`, -`Country/Region`, -Lat, -Long), names_to = "Date") %>%
  rename(Confirmed = value)
df_deaths <- df_deaths %>%
  pivot_longer(cols = c(-`Province/State`, -`Country/Region`, -Lat, -Long), names_to = "Date")
df_recovered <- df_recovered %>%
  pivot_longer(cols = c(-`Province/State`, -`Country/Region`, -Lat, -Long), names_to = "Date")

# building a dataframe
covid <- df_confirmed %>%
  left_join(select(df_deaths, -Lat, -Long, Deaths = value))%>%
  left_join(select(df_recovered, -Lat, -Long, Recovered = value))
covid$Date <- lubridate::mdy(covid$Date)

# calculate daily new cases using the lag
covid <- covid %>%
  group_by(`Country/Region`, `Province/State`) %>%
  mutate(new_confirmed = Confirmed - lag(Confirmed, n=1, order_by = Date)) %>%
  mutate(new_deaths = Deaths - lag(Deaths, n=1, order_by = Date)) %>%
  mutate(new_recovered = Recovered - lag(Recovered, n=1, order_by = Date)) %>%
  ungroup()

# calculate amount of infected people
covid <- covid %>%
  mutate(netInfected = Confirmed - Deaths - Recovered)

############################
# getting choice values    #
############################

# using Gapminder to get the specific countries of a continent
gapm <- gapminder
gapm$country <- as.character.factor(gapminder$country)
gapm$continent <- as.character.factor(gapminder$continent)
countries_from_continent <- gapm %>% select(country, continent) %>%  distinct(country, continent)
continentslist <- countries_from_continent$continent %>%  unique
countrieslist <- covid$`Country/Region` %>%  unique()

# vector enables selection of a region
regionlist <- continentslist %>% prepend(c("World", "-------CONTINENTS-------")) %>% append("-------COUNTRIES-------") %>% append(countrieslist)

# vector enables selection of case-type
plotlist <- c("netInfected", "Confirmed", "Deaths", "Recovered", "new_confirmed", "new_deaths", "new_recovered")

# these values represent the boundaries of the selectable period 
max_date <-(max(covid$Date))
min_date <-(min(covid$Date))

############################
# getting population       #
############################

populationlist <- gapm %>%
  group_by(country) %>%
  filter(year == max(year)) %>%
  select_at(c("country", "pop"))

div.pop <- function(x, divisor){
  return(x/divisor)
}

############################
# plotting function        #
############################
# Abort if there is no region choosen
plotting <- function(regionchoice, plotchoice, daterange, switch_absolut_relative){
  if (regionchoice == "empty"){
    return(NULL)
  }
  
# wrangling the regionchoice
  if (regionchoice %in% c("World", "-------CONTINENTS-------", "-------COUNTRIES-------")) {
    countrieschoice <- countrieslist
    regionchoice <- "World"}
  else if (regionchoice %in% continentslist) {
    countrieschoice <- countries_from_continent %>% filter(continent == regionchoice) %>% select_at("country")
    countrieschoice <- countrieschoice[[1]]}
  else {
    countrieschoice <- regionchoice}
  
# specific settings for absolut/relative  display 
  if (switch_absolut_relative == "Absolut"){
    divisor <- 1
    plottitle <- paste0(regionchoice, " - Number of persons")
    y_axis <- scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE))
  }
  else {
    divisor <- populationlist %>%
      filter(country %in% countrieschoice) %>%
      ungroup() %>%
      summarise(sum(as.numeric(pop)))
    divisor <- divisor[[1]]
    plottitle <- paste0(regionchoice, " - percentage of the population")
    y_axis <- scale_y_continuous(labels = scales::percent)
  }

# general plot-settings    
  plot <- covid %>%
    filter(`Country/Region` %in% countrieschoice, Date >= min(daterange) & Date <= max(daterange)) %>%
    group_by(Date) %>%
    summarise_at(plotchoice, sum, na.rm = TRUE) %>%
    mutate_at(plotchoice, function(x) (x/divisor)) %>%
    ggplot() +
    y_axis +
    labs(title = plottitle) +
    theme(plot.title = element_text(size = 20)) +
    theme_classic() +
    theme(axis.title.x=element_blank(),
          axis.title.y=element_blank())
  
# construct layers  
  for (i in plotchoice){
    if (i == "Confirmed"){
      plot <- plot + geom_area(aes(x = Date, y = Confirmed), fill = "darkred", alpha = 0.3)
    }
  }   
  for (i in plotchoice){
    if (i == "Recovered"){
      plot <- plot + geom_area(aes(x = Date, y = Recovered), fill = "darkgreen", alpha = 0.3)
    }
  }
  for (i in plotchoice){
    if (i == "netInfected"){
      plot <- plot + geom_line(aes(x = Date, y = netInfected), color = "darkorange", size = 1)
    }
  }  
  for (i in plotchoice){
    if (i == "Deaths"){
      plot <- plot + geom_area(aes(x = Date, y = Deaths), fill = "black", alpha = 0.3)
    }
  }
  for (i in plotchoice){
    if (i == "new_confirmed"){
      plot <- plot + geom_line(aes(x = Date, y = new_confirmed), color = "red")
    }
  }   
  for (i in plotchoice){
    if (i == "new_recovered"){
      plot <- plot + geom_line(aes(x = Date, y = new_recovered), color = "green")
    }
  }
  for (i in plotchoice){
    if (i == "new_deaths"){
      plot <- plot + geom_line(aes(x = Date, y = new_deaths), color = "black")
    }
  }
  return(ggplotly(plot))
}
