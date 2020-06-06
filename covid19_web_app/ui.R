# Shiny app Frontend or User-Interface ----
shinyUI(
  dashboardPage(
## Title of the frontend or User-Interface ====
    getHeader()
## Body of the frontend ====
### Menu ####
    , dashboardSidebar(
        sidebarMenuOutput("left_menu")
    )
### Content of dashboardBody ####
, dashboardBody(
  tabItems(
    # Hier ist die Reihenfolge egal, die Reihenfolge wird über sidebarMenu gesteuert        
### Content of settings ####
    tabItem(
      tabName = df_tab_ids$id[[3]],
      fluidRow(
        box(
          title = "Settings below",
          width = 12,
          collapsible = TRUE,
          radioButtons(inputId = "switch_language", 
                       label ="Language",
                       choices = c("English", "Deutsch"),
                       selected = "Deutsch",
                       inline = TRUE
                       )
        )
      )
    )
### Content of information, e.g. about ####
    , tabItem(
        tabName = df_tab_ids$id[[4]],
        fluidRow(
          box(
              title = "Informations",
              width = 12,
              collapsible = FALSE
              , textOutput("informations")
              , textOutput("sourceWorld")
              , textOutput("sourceGermany")
          )
        )
    )
### Content of world ####
    , tabItem(
        tabName = df_tab_ids$id[[1]],
        fluidRow(
        ### Input panel of world ####
          getUIWorldInputPanel()
        ### Output panel of world ####
        # Show a plot of the generated distribution
        , getUIWorldOutputPanel()
        )
    )
### Content of germany ####
    , tabItem(
        tabName = df_tab_ids$id[[2]],
        fluidRow(
          box(
            title = "Select below"
            , width = 12
            , collapsible = TRUE
            , selectInput(inputId = "federalState"
                        , label = "Select a federal state of interest:"
                        , choices = choices_state
                        , selected = c("Schleswig-Holstein")
            )
            , selectInput(inputId = "destrict"
                        , label = "Select a destrict of interest:"
                        , choices = NULL
            )
          )
### Germany - Output plots ####
          , box(
            width = 12
            , tabBox(
    # The id lets us use input$tabset1 on the server to find the current tab
                id = "tabsetPlotsGermany"
#                , height = "350px"
                , width = "400px"
                , tabPanel(
                    "Cases"
#                    , textOutput("plotStateTitle")
                    , plotlyOutput("plotCases"
#                                 , height = 250
                                   , width = 600
                  )
                )
                , tabPanel(
                  "Cases per 100k"
                  , textOutput("plotStateTitle")
                  , plotlyOutput("plotCasesPer100K"
#                                 , height = 250
                                 , width = 600
                  )
                )
                , tabPanel(
                  "Deaths"
#                    , textOutput("plotStateTitle")
                  , plotlyOutput("plotDeaths"
                 #                                 , height = 250
                 , width = 600
                  )
                )
            )
          )
### Germany - Output details to selection ####
          , box(
            title = "Details to selection"
            , width = 12
            , collapsible = TRUE
            , tabBox(
              #    title = "First tabBox",
              # The id lets us use input$tabset1 on the server to find the current tab
              id = "tabsetDetailsGermany"
              #, height = "350px"
              , width = "400px"
              , tabPanel("Result of input selection"
                       #textOutput("plotStateTitle")
                      , tableOutput("dataState")
              )
### Germany - Output details to plot cases ####
              , tabPanel("Details to plot cases"
                         , tableOutput("dataDetailsCases")
              )
### Germany - Output details to plot cases per 100k ####
              , tabPanel("Details to plot cases per 100k"
                         , tableOutput("dataDetailsCases100k")
              )
### Germany - Output details to plot cases per 100k ####
              , tabPanel("Details to plot deaths"
                        , tableOutput("dataDetailsDeaths")
              )
            )
          )
      )
    )  
# End of Shiny App ------ 
  )
)
  )
)  
