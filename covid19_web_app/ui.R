shinyUI(fluidPage(
  
  titlePanel("Self service analysis: Covid-19"),
  

  sidebarLayout(
    sidebarPanel(
       selectizeInput(inputId = "regionchoice", 
                      label = "Select up to 2 regions of interest:", 
                      choices = regionlist, selected = c("Germany"), 
                      options = list(maxItems = 2)),
       checkboxGroupInput(inputId = "plotchoice_values", 
                          label = "Select values:", 
                          choices = plotlist[1:4], 
                          selected = c("net_infected"),
                          inline = FALSE),
       checkboxGroupInput(inputId = "plotchoice_trend", 
                          label = "Select trend values:", 
                          choices = plotlist[5:7], 
                          inline = FALSE),       
       sliderInput(inputId = "daterange",
                   label = "What Period are you interested in?",
                   min = min_date,
                   max = max_date,
                   value = c(min(covid$date), max(covid$date)),
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
