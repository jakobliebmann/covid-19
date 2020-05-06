shinyUI(fluidPage(
  
  titlePanel("Self service analysis: Covid-19"),
  

  sidebarLayout(
    sidebarPanel(
       selectInput(inputId = "regionchoice", 
                   label = "Select Region of interest!", 
                   choices = regionlist, selected = c("Germany"), 
                   multiple = FALSE),
       selectInput(inputId = "plotchoice", 
                   label = "What are you interested in?", 
                   choices = plotlist, 
                   selected = c("netInfected"), 
                   multiple = TRUE),
       sliderInput(inputId = "daterange",
                   label = "What Period are you interested in?",
                   min = min_date,
                   max = max_date,
                   value = c(min(covid$Date), max(covid$Date)),
                   timeFormat="%Y-%m-%d"),
       radioButtons(inputId = "switch_absolut_relative", 
                    label ="Type of display:",
                    choices = c("Absolut", "Relative"),
                    selected = "Absolut",
                    inline = TRUE)
    ),
    # Show a plot of the generated distribution
    mainPanel(
       plotlyOutput("plot1")
    )
  )
))
