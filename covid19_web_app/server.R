# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$plot1 <- renderPlotly({
    countrieschoice <- input$countrieschoice
    plotchoice <- input$plotchoice
    daterange <- input$daterange
    
    plotting(countrieschoice, plotchoice, daterange)

  })
})
