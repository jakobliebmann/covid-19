# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$plot1 <- renderPlotly({
    regionchoice <- input$regionchoice
    plotchoice <- input$plotchoice
    daterange <- input$daterange
    if (regionchoice %in% c("World", "-------CONTINENTS-------", "-------COUNTRIES-------")) {
      countrieschoice <- countrieslist}
    else if (regionchoice %in% continentslist) {
      countrieschoice <- countries_from_continent %>% filter(continent == regionchoice) %>% select_at("country")
      countrieschoice <- countrieschoice[[1]]}
    else {
      countrieschoice <- regionchoice}
      
    plotting(countrieschoice, plotchoice, daterange)
  })
})


