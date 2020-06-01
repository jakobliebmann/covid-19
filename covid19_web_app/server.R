shinyServer(function(input, output, session) {
  dt_covid_server <- data.table()
  s_min_date <- min_date
  s_max_date <- max_date
  
# Define server logic required to draw a histogram ----
## World output - plot 1 ====
  output$plot1 <- renderPlotly({
    plotting(ifelse(length(input$regionchoice)==0, "empty", input$regionchoice[[1]])
             , input$plotchoice_values %>% append(input$plotchoice_trend)
             , input$daterange
             , input$switch_absolut_relative
             , dt_covid_server
    )
  })
## World output - plot 2 ====  
  output$plot2 <- renderPlotly({
    plotting(ifelse(length(input$regionchoice)<2, "empty", input$regionchoice[[2]])
             , input$plotchoice_values %>% append(input$plotchoice_trend)
             , input$daterange
             , input$switch_absolut_relative
             , dt_covid_server
    )
  })
## Dashboad ====
### Left Menu ####
  output$left_menu <- renderMenu({
    set_language(input$switch_language)
    sidebarMenu(
      id = "tabs"
      # mit selected = TRUE - bekomme ich die Auswahl ohne Sortierung der Menü-Items
      , menuItem(text = translator$t(df_tab_ids$label[[1]]), tabName = df_tab_ids$id[[1]], icon = icon(df_tab_ids$icon[[1]]))
      , menuItem(text = translator$t(df_tab_ids$label[[2]]), tabName = df_tab_ids$id[[2]], icon = icon(df_tab_ids$icon[[2]]))
      , menuItem(selected = TRUE, badgeLabel = "new", 
                 text = translator$t(df_tab_ids$label[[3]]), tabName = df_tab_ids$id[[3]], icon = icon(df_tab_ids$icon[[3]]))
    )
  })  
### Translation ####
  observeEvent(
    # variante 2    
    i18n()
    ,{})

  i18n <- reactive({
    selected <- input$switch_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      set_language(input$switch_language)
    }
    updateSelectInput(session
                      , inputId = "switch_language"
                      , label = translator$t("Language")
    )
    updateSelectizeInput(session
                         , inputId = "regionchoice"
                         , label = translator$t("Select up to 2 regions of interest:")
    )
    updateSliderInput(session
                      , inputId = "daterange"
                      , label = translator$t("What Period are you interested in?")
                      # sonst schrott in der Anzeige
                      , min = s_min_date
                      , max = s_max_date
                      , value = c(s_min_date, s_max_date)
    )
    
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
                if (length(dt_covid_server) == 0) {
                progress <- shiny::Progress$new()
                progress$set(message = "Computing data", value = 0)
                # debugging
                browser()
                progress$set(value = 0.2, detail = NULL)
                temp_covid <- get_df_covid_john_hopkins()
                progress$set(value = 0.4, detail = NULL)
                tmp_covid <- get_dt_covid_world(temp_covid)
                progress$set(value = 0.5, detail = NULL)
                dt_covid_server <<- as.data.table(tmp_covid)
                progress$set(value = 0.6, detail = NULL)
                temp_regionList <- get_regionlist(tmp_covid)
                progress$set(value = 0.8, detail = NULL)
                updateSelectizeInput(session
                                     , inputId = "regionchoice"
                                     , choices = temp_regionList
#                                     , label = translator$t("Select up to 2 regions of interest:")
                                     , selected = c("Germany")
                                   )
                # these values represent the boundaries of the selectable period 
                s_min_date <<- (min(tmp_covid$date))
                s_max_date <<- (max(tmp_covid$date))
                progress$set(value = 0.9, detail = NULL)
                #
                updateSliderInput(session
                                  , inputId = "daterange"
                                  , min = s_min_date
                                  , max = s_max_date
                                  , value = c(s_min_date, s_max_date)
                                  , label = translator$t("What Period are you interested in?")
                                  )
                progress$close()
                }
                # würde auch zum Transport gehen
                #attr(session, "covid_dt") <- tmp_covid
                #print(str(session))
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