shinyUI(fluidPage(
  
  titlePanel("Self service analysis: Covid-19"),
  

  sidebarLayout(
    sidebarPanel(
       selectizeInput(inputId = "regionchoice", 
                      label = "Select up to 2 regions of interest!", 
                      choices = regionlist, selected = c("Germany"), 
                      options = list(maxItems = 2)),
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
       plotlyOutput("plot1"),
       plotlyOutput("plot2")
    )
  )
))
