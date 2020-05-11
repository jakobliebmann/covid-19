# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  output$plot1 <- renderPlotly({
    plotting((if (length(input$regionchoice)==0){
                "empty"} 
              else {
                input$regionchoice[[1]]}),
             input$plotchoice_values %>% append(input$plotchoice_trend), 
             input$daterange, 
             input$switch_absolut_relative)
  })
  output$plot2 <- renderPlotly({
    plotting((if (length(input$regionchoice)<2){
                "empty"} 
              else {
                input$regionchoice[[2]]}),
             input$plotchoice_values %>% append(input$plotchoice_trend), 
             input$daterange, 
             input$switch_absolut_relative)
  })
})


