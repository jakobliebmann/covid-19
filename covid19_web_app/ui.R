shinyUI(fluidPage(
  
  titlePanel("A summary of Covid-19 propagation"),
  

  sidebarLayout(
    sidebarPanel(
       selectInput(inputId = "countrieschoice", label = "Select Region of interest!", choices = countrieslist, selected = c("Germany"), multiple = FALSE),
       selectInput(inputId = "plotchoice", label = "What are you interested in?", choices = plotlist, selected = c("netInfected"), multiple = TRUE),
       sliderInput("daterange",
                   "What Period are you interested in?",
                   min = min_date,
                   max = max_date,
                   value = c(min(covid$Date), max(covid$Date)),
                   timeFormat="%Y-%m-%d")
    ),

    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("plot1")
    )
  )
))
