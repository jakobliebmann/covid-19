# Define server logic required to draw a histogram
shinyServer(function(input, output) {
   
  output$plot1 <- renderPlot({
    countrieschoice <- input$countrieschoice
    plotchoice <- input$plotchoice
    
    plot <- covid %>%
      group_by(`Country/Region`, Date, `Province/State`) %>%
      filter(`Country/Region` %in% countrieschoice) %>%
      summarise_at(plotchoice, mean, na.rm = TRUE) %>% 
      ggplot(aes(x = Date, y = Amount)) +
      labs(title = "Number of persons affected in the selected region") +
      theme(plot.title = element_text(size = 20))+
      theme_minimal()+
      theme(axis.title.x=element_blank(),
            axis.title.y=element_blank())

    for (i in plotchoice){
      if (i == "Confirmed"){
        plot <- plot + geom_col(aes(x = Date, y = Confirmed), fill = "black")
      }
    }   
        
    for (i in plotchoice){
      if (i == "Recovered"){
        plot <- plot + geom_col(aes(x = Date, y = Recovered), fill = "green")
      }
    }
    
    for (i in plotchoice){
      if (i == "Deaths"){
        plot <- plot + geom_col(aes(x = Date, y = Deaths), fill = "red")
      }
    }
    for (i in plotchoice){
      if (i == "new_confirmed"){
        plot <- plot + geom_col(aes(x = Date, y = new_confirmed), color = "black")
      }
    }   
    
    for (i in plotchoice){
      if (i == "new_recovered"){
        plot <- plot + geom_col(aes(x = Date, y = new_recovered), color = "green")
      }
    }
    
    for (i in plotchoice){
      if (i == "new_deaths"){
        plot <- plot + geom_col(aes(x = Date, y = new_deaths), color = "red")
      }
    } 
    plot
  })

    output$plot2 <- renderPlot({
    countrieschoice <- input$countrieschoice
    plotchoice <- input$plotchoice
  
    covid %>%
      group_by(`Country/Region`, Date, `Province/State`) %>%
      filter(`Country/Region` %in% countrieschoice) %>%
      summarise(.data[[plotchoice[1]]])%>%
      ggplot() +
      geom_col(aes(x = Date, y = .data[[plotchoice[1]]])) +
      #geom_col(aes(x = Date, y = .data[[plotchoice[2]]]), color = "red") +
      labs(title = "Region of Interest") +
      theme(plot.title = element_text(size = 20))
      
  })
  
})
