shinyUI(fluidPage(
  
  titlePanel("A summary of Covid-19 propagation"),
  

  sidebarLayout(
    sidebarPanel(
       selectInput(inputId = "countrieschoice", label = "Select one ore more countries of interest!", choices = countrieslist, selected = c("Germany"), multiple = TRUE),
       selectInput(inputId = "plotchoice", label = "What are you interested in?", choices = plotlist, selected = c("netInfected"), multiple = TRUE),
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("plot1")
    )
  )
))
