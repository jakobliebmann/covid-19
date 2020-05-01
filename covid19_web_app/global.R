############################
# Import libraries         #
############################

library(shiny)
library(tidyverse)
library(plotly)

############################
# Creation of a data basis #
############################

# Getting raw Data (absolute values)
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
countrieslist <- covid$`Country/Region` %>%
  unique()
plotlist <- c("netInfected", "Confirmed", "Deaths", "Recovered", "new_confirmed", "new_deaths", "new_recovered")
max_date <-(max(covid$Date))
min_date <-(min(covid$Date))


str(as.character(dateboundaries[1,"min(Date)"]))
