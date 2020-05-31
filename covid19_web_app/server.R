shinyServer(function(input, output, session) {
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
## Dashboad ====
### Left Menu ####
  output$left_menu <- renderMenu({
    set_language(input$switch_language)
    sidebarMenu(
      id = "tabs"
      # mit selected = TRUE - bekomme ich die Auswahl ohne Sortierung der Menü-Items
      # expandName ist wohl für SubMenüItems
      , menuItem(text = translator$t(df_tab_ids$label[[1]]), tabName = df_tab_ids$id[[1]], icon = icon(df_tab_ids$icon[[1]]))
      , menuItem(text = translator$t(df_tab_ids$label[[2]]), tabName = df_tab_ids$id[[2]], icon = icon(df_tab_ids$icon[[2]]))
      , menuItem(selected = TRUE, badgeLabel = "new", 
                 text = translator$t(df_tab_ids$label[[3]]), tabName = df_tab_ids$id[[3]], icon = icon(df_tab_ids$icon[[3]]))
    )
  })  
### Translation ####
  observeEvent(
    i18n()
    ,{})

  i18n <- reactive({
    selected <- input$switch_language
    set_language(input$switch_language)
    updateSelectInput(session,"switch_language", label = translator$t("Language"))
  })
  
  # Ausgabe-Feld in Settings unterhalb der Sprache
  output$value <- renderPrint({ 
    paste("Sprache-render",input$switch_language)
  })
  
### Menu item selection ####
  output$res <- renderText({
    selectedTab <- input$tabs
    if (length(selectedTab) > 0){
      switch (selectedTab,
              "tab_country" = {
                paste("Tab-Aktion starten für ",selectedTab)
              }
              , "tab_germany" = {
                  paste("Tab-Aktion starten für ",selectedTab)
              }
              , {
                  paste("Keine Aktion für",selectedTab)
              }
      )
    } else {
      paste("Keine Aktion, da tablength 0 ist", length(selectedTab))
    }
  })
# End of Server ----  
})