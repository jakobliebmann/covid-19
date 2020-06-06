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
      id = "tabSidebar"
      # mit selected = TRUE - bekomme ich die Auswahl ohne Sortierung der Menü-Items
      , menuItem(text = translator$t(df_tab_ids$label[[1]]), tabName = df_tab_ids$id[[1]], icon = icon(df_tab_ids$icon[[1]]))
      , menuItem(text = translator$t(df_tab_ids$label[[2]]), tabName = df_tab_ids$id[[2]], icon = icon(df_tab_ids$icon[[2]]))
      , menuItem(selected = TRUE, badgeLabel = "new", 
                 text = translator$t(df_tab_ids$label[[3]]), tabName = df_tab_ids$id[[3]], icon = icon(df_tab_ids$icon[[3]]))
      , menuItem(text = translator$t(df_tab_ids$label[[4]]), tabName = df_tab_ids$id[[4]], icon = icon(df_tab_ids$icon[[4]]))
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
  output$languageSelection <- renderPrint({ 
    paste("Sprache-render",input$switch_language)
  })
  
  output$appTitle <- renderText({
    selected <- input$switch_language
    if (length(selected) > 0 && selected %in% translator$languages) {
      set_language(input$switch_language)
    }
    getHeaderLabel()
  })
  
### Menu item selection ####
  tabPerform <- reactive({
    selectedTab <- input$tabSidebar
    if (length(selectedTab) > 0){
      switch (selectedTab,
              "tab_country" = {
                if (length(dt_covid_server) == 0) {
                  progress <- shiny::Progress$new()
                  progress$set(message = "Computing data", value = 0)
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
                                       , label = translator$t("Select up to 2 regions of interest:")
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
                # wuerde auch zum Transport gehen
                #attr(session, "covid_dt") <- tmp_covid
                #print(str(session))
                paste("Tab-Aktion starten für",selectedTab)
              }
              , "tab_germany" = {
                                updateSelectInput(session
                                  , inputId = "federalState"
                                  , label = "Select a federal state of interest:"
#                                  , choices = choices_state
#                                  , selected = c("Schleswig-Holstein")
                )
                paste("Tab-Aktion starten für",selectedTab)
              }
              , {
                paste("Keine Aktion für",selectedTab)
              }
      )
    } else {
      paste("Keine Aktion, da tablength 0 ist", length(selectedTab))
    }
  })
  
  observeEvent(tabPerform(),{})
  
  output$plotStateTitle <- renderText({
    paste("Top 10 destricts in", input$federalState ,"with cases per 100.000")
  })
  output$tabSelection <- renderText({
    selectedTab <- input$tabSidebar
    if (length(selectedTab) > 0){
      switch (selectedTab,
              "tab_country" = {
                paste("Tab-Aktion starten für",selectedTab)
              }
              , "tab_germany" = {
                paste("Tab-Aktion starten für",selectedTab)
              }
              , {
                paste("Keine Aktion für",selectedTab)
              }
      )
    } else {
      paste("Keine Aktion, da tablength 0 ist", length(selectedTab))
    }
  })

### Germany - Federal state with destricts ####
# Germany comprises sixteen federal states which are collectively referred to as Bundesländer. 
# Each state has its own state constitution, and is largely autonomous in regard to its internal organisation. 
# As of 2017 Germany is divided into 401 districts (Kreise) at a municipal level; 
# these consist of 294 rural districts and 107 urban districts.  

  s_destrict_pre <- "--- All Destricts ---"
  
### Germany - District choice list depending on federal state selection ####  
  federalState <- reactive({
    lSelect <- input$federalState
    if (length(lSelect) > 0 ){
      if(lSelect %in% df_rki_destrict$BL) {
        l_auswahl <- df_rki_destrict %>% 
          filter(BL == lSelect) %>%
          arrange(county) %>%
          select(county) 
        #l_choices <- unique(federalstate()$GEN)
        l_choices <- unique(l_auswahl$county) %>% prepend(s_destrict_pre)
        updateSelectInput(session
                          , "destrict"
                          , choices = l_choices
                          )
      } else {
        l_auswahl <- df_rki_destrict %>% 
          arrange(county) %>%
          select(county) 
        l_choices <- unique(l_auswahl$county) %>% prepend(s_destrict_pre)
        updateSelectInput(session
                          , "destrict"
                          , choices = l_choices
                          , selected = s_destrict_pre
                          )
      }
    }
  })
### Germany - Federal state actions ####
  observeEvent(federalState(), {}
  )

### Germany - Destrict actions ####
  observeEvent(destricts(), {}
  )

  destricts <- reactive({
    lSelect <- input$destrict
    if (length(lSelect) > 0 && lSelect %in% df_rki_destrict$county) {
#      print(
        paste("Teste federalState", lSelect)
#        )
    }
  })
  
### Germany - Data frame with conditions ####
  getDataState <- function(p_federalStrate, p_destrict, p_attribute){
#    print(paste("getDataState",p_federalStrate, p_destrict, p_attribute))
    if (p_federalStrate == choices_state_pre) {
      return(df_rki_destrict)
    } else
      if (p_destrict == s_destrict_pre){
        return(df_rki_destrict %>%
          filter(BL == p_federalStrate) 
        )
      } else {
        l_dataState <- df_rki_destrict %>% 
          filter(BL == p_federalStrate) 
        l_data_destrict <- l_dataState %>% filter(county == p_destrict)
        l_data <- l_dataState %>%         
          arrange(desc(.data[[p_attribute]])) 
        l_data_top <- l_data %>%         
             top_n(1, wt = .data[[p_attribute]])  
        return(union(
            l_data_top
          , l_data_destrict
        ))
      }
  }  

### Germany - Data frame for cases per 100k ####  
  getCasesPer100k <- function(p_federalStrate, p_destrict) {
    return(
      getDataState(p_federalStrate, p_destrict, "cases_per_100k") %>% 
        arrange(desc(cases_per_100k)) %>% 
        top_n(10, wt = cases_per_100k) %>% 
        select(
        #          BL , GEN
          county 
        #, EWZ,	KFL,	DEBKG_ID,	death_rate	,cases	,deaths	
        , cases_per_100k	
        #,cases_per_population, last_update 
      ) 
    )
  }
  
### Germany - Data frame for deaths ####  
  getDeaths <- function(p_federalStrate, p_destrict) {
    return(
      getDataState(p_federalStrate, p_destrict, "deaths") %>% 
        arrange(desc(deaths)) %>% 
        top_n(10, wt = deaths) %>% 
        select(
            county 
          , deaths	
        ) 
    )
  }
  
### Germany - Data frame for cases ####  
  getCases <- function(p_federalStrate, p_destrict) {
    return(
      getDataState(p_federalStrate, p_destrict, "cases") %>% 
        arrange(desc(cases)) %>% 
        top_n(10, wt = cases) %>% 
        select(
          #          BL , GEN
          county 
          #, EWZ,	KFL,	DEBKG_ID,	death_rate	
          , cases	
          #,deaths	
          #, cases_per_100k, cases_per_population, last_update 
        ) 
    )
  }
  
### Germany - Plot for cases ####    
  output$plotCases <- renderPlotly({
    req(input$tabSidebar)
    if(input$tabSidebar == df_tab_ids$id[[2]]){
#      print(paste("output$plotCases",input$tabSidebar))
      g_df <- getCases(input$federalState, input$destrict)
      #+ coord_flip()
      ggplot(data=g_df, aes(x =cases, y = county)) + geom_bar(stat = "identity") + labs(x = "Durchschnitt Fälle", y = "")
    }
  })
  
### Germany - Plot for cases per 100k ####    
  output$plotCasesPer100K <- renderPlotly({
    req(input$tabsetPlotsGermany)
    if (input$tabsetPlotsGermany == "Cases per 100k") {
#      print(paste("output$plotCasesPer100K",input$tabSidebar))
      g_df <- getCasesPer100k(input$federalState, input$destrict)
    #+ coord_flip()
      ggplot(data=g_df, aes(x =cases_per_100k, y = county)) + geom_bar(stat = "identity") + labs(x = "Durchschnitt Fälle", y = "")
    }
  })

### Germany - Plot for deaths ####    
  output$plotDeaths <- renderPlotly({
    req(input$tabsetPlotsGermany)
    if (input$tabsetPlotsGermany == "Deaths") {
      #      print(paste("output$plotCasesPer100K",input$tabSidebar))
      g_df <- getDeaths(input$federalState, input$destrict)
      #+ coord_flip()
      ggplot(data=g_df, aes(x =deaths, y = county)) + geom_bar(stat = "identity") + labs(x = "Durchschnitt Fälle", y = "")
    }
  })

### Germany - Output table for selection ####    
  output$dataState <- renderTable({
#    req(input$destrict)
    if (input$destrict == s_destrict_pre){
      l_data <- df_rki_destrict %>%
        filter(BL == input$federalState) %>%
        arrange(county)
    } else {
      l_data <- df_rki_destrict %>% 
        filter(county == input$destrict) 
    }
    l_data %>% 
      select(
        county , BL , EWZ
        # ,	KFL,	DEBKG_ID,	death_rate	
        ,cases	,deaths	,cases_per_100k	
        # ,cases_per_population , last_update 
      )
    
  })  

### Germany - Output table for cases ####    
  output$dataDetailsCases <- renderTable({
    getCases(input$federalState, input$destrict)
  })  
  
### Germany - Output table for cases per 100k ####    
  output$dataDetailsCases100k <- renderTable({
    getCasesPer100k(input$federalState, input$destrict)
  })  
  
### Germany - Output table for deaths ####    
  output$dataDetailsDeaths <- renderTable({
    getDeaths(input$federalState, input$destrict)
  })  
### Germany - Output plots ####
  tabOutGermanyPerform <- reactive({
#    print(
      paste("Tab Output",input$tabsetPlotsGermany)
#      )
  })
  
  observeEvent(tabOutGermanyPerform(),{})
  
### About - Output ####
  output$sourceGermany <- renderText({
    paste("Data source of tab germany: Robert Koch Institut,",unique(df_rki_destrict$last_update))  
  })
  
  output$sourceWorld <- renderText({
    #https://github.com/CSSEGISandData/COVID-19
    paste("Data source of tab world: Johns Hopkins University")  
  })

# End of Server ----  
})