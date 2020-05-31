shinyServer(function(input, output) {
# Define server logic required to draw a histogram ----
## World output - plot 1 ====
  output$plot1 <- renderPlotly({
    plotting(ifelse(length(input$regionchoice)==0, "empty", input$regionchoice[[1]])
             , input$plotchoice_values %>% append(input$plotchoice_trend)
             , input$daterange
             , input$switch_absolut_relative
             )
  })
## World output - plot 2 ====  
  output$plot2 <- renderPlotly({
    plotting(ifelse(length(input$regionchoice)<2, "empty", input$regionchoice[[2]])
             , input$plotchoice_values %>% append(input$plotchoice_trend)
             , input$daterange
             , input$switch_absolut_relative
             )
  })
})