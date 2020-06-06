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
      , menuItem(text = translator$t(df_tab_ids$label[[1]]), tabName = df_tab_ids$id[[1]], icon = icon(df_tab_ids$icon[[1]]))
      , menuItem(selected = TRUE, text = translator$t(df_tab_ids$label[[2]]), tabName = df_tab_ids$id[[2]], icon = icon(df_tab_ids$icon[[2]]))
      , menuItem(badgeLabel = "new", 
                 text = translator$t(df_tab_ids$label[[3]]), tabName = df_tab_ids$id[[3]], icon = icon(df_tab_ids$icon[[3]]))
      , menuItem(text = translator$t(df_tab_ids$label[[4]]), tabName = df_tab_ids$id[[4]], icon = icon(df_tab_ids$icon[[4]]))
    )
  })  
### Translation ####
  observeEvent(
    i18n()
    ,{}
  )
  
  updateLanguage <- function(p_selected) {
    if (length(p_selected) > 0 && p_selected %in% translator$languages) {
      set_language(p_selected)
    }
  }
  
  i18n <- reactive({
    selected <- input$switch_language
    updateLanguage(selected)
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
                      , min = s_min_date
                      , max = s_max_date
                      , value = c(s_min_date, s_max_date)
    )
  })
  
  output$languageSelection <- renderPrint({ 
    paste("Sprache-render",input$switch_language)
  })
  
  output$appTitle <- renderText({
    selected <- input$switch_language
    updateLanguage(selected)
    getHeaderLabel()
  })
  
  output$settings_below <- renderText({ 
    selected <- input$switch_language
    updateLanguage(selected)
    translator$t("Settings below")
  })
  
  output$select_below_world <- renderText({ 
    selected <- input$switch_language
    updateLanguage(selected)
    translator$t("Select below")
  })
  
  output$select_below_ger <- renderText({ 
    selected <- input$switch_language
    updateLanguage(selected)
    translator$t("Select below")
  })
  
### Menu item selection ####
  tabPerform <- reactive({
    selectedTab <- input$tabSidebar
    if (length(selectedTab) > 0){
      switch (selectedTab,
              "tab_world" = {
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
                  progress$set(value = 0.7, detail = NULL)
                  updateSelectizeInput(session
                                       , inputId = "regionchoice"
                                       , choices = temp_regionList
                                       , label = translator$t("Select up to 2 regions of interest:")
                                       , selected = c("Germany")
                  )
                  s_min_date <<- (min(tmp_covid$date))
                  s_max_date <<- (max(tmp_covid$date))
                  progress$set(value = 0.8, detail = NULL)
                  #
                  updateSliderInput(session
                                    , inputId = "daterange"
                                    , min = s_min_date
                                    , max = s_max_date
                                    , value = c(s_min_date, s_max_date)
                                    , label = translator$t("What Period are you interested in?")
                  )
                  progress$set(value = 0.9, detail = NULL)
                  progress$close()
                }
                paste("Tab-Aktion starten für",selectedTab)
              }
              , "tab_germany" = {
                                updateSelectInput(session
                                  , inputId = "federalState"
                                  , label = translator$t("Select a federal state of interest:")
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
  
  output$plotCasesPer100kTitle <- renderText({
    paste("Top 10 districts in", input$federalState, "with most affected cases/100.000")
  })
  output$plotCasesTitle <- renderText({
    paste("Top 10 districts in", input$federalState, "with most affected cases")
  })
  output$plotDeathsTitle <- renderText({
    paste("Top 10 districts in", input$federalState, "with most affected deaths")
  })
  
  output$tabSelection <- renderText({
    selectedTab <- input$tabSidebar
    if (length(selectedTab) > 0){
      switch (selectedTab,
              "tab_world" = {
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

### Germany - Federal state with districts ####
  s_district_pre <- "--- All Districts ---"
### Germany - District choice list depending on federal state selection ####  
  federalState <- reactive({
    lSelect <- input$federalState
    if (length(lSelect) > 0 ){
      if(lSelect %in% df_rki_district$BL) {
        l_auswahl <- df_rki_district %>% 
          filter(BL == lSelect) %>%
          arrange(county) %>%
          select(county) 
        #l_choices <- unique(federalstate()$GEN)
        l_choices <- unique(l_auswahl$county) %>% prepend(s_district_pre)
        updateSelectInput(session
                          , "district"
                          , choices = l_choices
                          , label = translator$t("Select a district of interest:")
                          )
      } else {
        l_auswahl <- df_rki_district %>% 
          arrange(county) %>%
          select(county) 
        l_choices <- unique(l_auswahl$county) %>% prepend(s_district_pre)
        updateSelectInput(session
                          , "district"
                          , choices = l_choices
                          , label = translator$t("Select a district of interest:")
                          , selected = s_district_pre
                          )
      }
    }
  })
### Germany - Federal state actions ####
  observeEvent(federalState(), {}
  )

### Germany - District actions ####
  observeEvent(districts(), {}
  )

  districts <- reactive({
    lSelect <- input$district
    if (length(lSelect) > 0 && lSelect %in% df_rki_district$county) {
        paste("Teste federalState", lSelect)
    }
  })
  
### Germany - Data frame with conditions ####
  getDataState <- function(p_federalStrate, p_district, p_attribute){
    if (p_federalStrate == choices_state_pre) {
      return(df_rki_district)
    } else
      if (p_district == s_district_pre){
        return(df_rki_district %>%
          filter(BL == p_federalStrate) 
        )
      } else {
        l_dataState <- df_rki_district %>% 
          filter(BL == p_federalStrate) 
        l_data_district <- l_dataState %>% filter(county == p_district)
        l_data <- l_dataState %>%         
          arrange(desc(.data[[p_attribute]])) 
        l_data_top <- l_data %>%         
             top_n(1, wt = .data[[p_attribute]])  
        return(union(
            l_data_top
          , l_data_district
        ))
      }
  }  

### Germany - Data frame for cases ####  
  getCases <- function(p_federalStrate, p_district) {
    l_df <- getDataState(p_federalStrate, p_district, "cases") %>% 
        arrange(desc(cases)) %>% 
        top_n(10, wt = cases) %>% 
        select(
          county 
          , cases	
        ) 
    colnames(l_df) <- c("District", "Cases")
    return(l_df)
  }
  
### Germany - Data frame for cases per 100k ####  
  getCasesPer100k <- function(p_federalStrate, p_district) {
    l_df <- getDataState(p_federalStrate, p_district, "cases_per_100k") %>% 
        arrange(desc(cases_per_100k)) %>% 
        top_n(10, wt = cases_per_100k) %>% 
        select(
          county 
        , cases_per_100k	
      ) 
    colnames(l_df) <- c("District", "Cases per 100.000")
    return(l_df)
  }
  
### Germany - Data frame for deaths ####  
  getDeaths <- function(p_federalStrate, p_district) {
    l_df <- getDataState(p_federalStrate, p_district, "deaths") %>% 
        arrange(desc(deaths)) %>% 
        top_n(10, wt = deaths) %>% 
        select(
            county 
          , deaths	
        ) 
    colnames(l_df) <- c("District", "Deaths")
    return(l_df)
  }
  
### Germany - Plot for cases ####    
  output$plotCases <- renderPlotly({
    req(input$tabSidebar)
    if(input$tabSidebar == df_tab_ids$id[[2]]){
      g_df <- getCases(input$federalState, input$district)
      ggplot(data=g_df, aes(x = Cases, y = District)) + geom_bar(stat = "identity") + labs(x = "Confirmed Cases", y = "")
    }
  })

### Germany - Plot for cases per 100k ####    
  output$plotCasesPer100K <- renderPlotly({
    req(input$tabsetPlotsGermany)
    if (input$tabsetPlotsGermany == "Cases per 100k") {
      g_df <- getCasesPer100k(input$federalState, input$district)
      ggplot(data=g_df, aes(x =`Cases per 100.000`, y = District)) + geom_bar(stat = "identity") + labs(x = "Confirmed cases per 100.000", y = "")
    }
  })

### Germany - Plot for deaths ####    
  output$plotDeaths <- renderPlotly({
    req(input$tabsetPlotsGermany)
    if (input$tabsetPlotsGermany == "Deaths") {
      g_df <- getDeaths(input$federalState, input$district)
      ggplot(data=g_df, aes(x =Deaths, y = District)) + geom_bar(stat = "identity") + labs(x = "Confirmed deaths", y = "")
    }
  })

### Germany - Output table for selection ####    
  output$dataState <- renderTable({
    if (input$federalState == choices_state_pre){
      l_data <- df_rki_district %>%
        arrange(county)
    } else if (input$district == s_district_pre){
      l_data <- df_rki_district %>%
        filter(BL == input$federalState) %>%
        arrange(county)
    } else if (input$district %in% df_rki_district$county) {
      l_data <- df_rki_district %>% 
        filter(county == input$district) %>%
        arrange(county)
    }
    l_ds <- l_data %>% 
      select(
        county , BL , EWZ
        ,cases	,deaths	,cases_per_100k	
      )
    colnames(l_ds) <- c("District", "Federal state", "Population", "Cases" , "Deaths" , "Cases per 100.000")
    l_ds
  })  

### Germany - Output table for cases ####    
  output$dataDetailsCases <- renderTable({
    getCases(input$federalState, input$district)
  })  
  
### Germany - Output table for cases per 100k ####    
  output$dataDetailsCases100k <- renderTable({
    getCasesPer100k(input$federalState, input$district)
  })  
  
### Germany - Output table for deaths ####    
  output$dataDetailsDeaths <- renderTable({
    getDeaths(input$federalState, input$district)
  })  
### Germany - Output Data status ####      
  output$dataStatusGermany <- renderText({
    paste(translator$t("Data status"),":",unique(df_rki_district$last_update))
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
    paste("Data source of tab germany: Robert Koch Institut")
  })
  
  output$sourceWorld <- renderText({
    #https://github.com/CSSEGISandData/COVID-19
    paste("Data source of tab world: Johns Hopkins University")  
  })

# End of Server ----  
})