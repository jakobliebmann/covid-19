# Shiny app Frontend or User-Interface ----
shinyUI(
  dashboardPage(
## Title of the frontend or User-Interface ====
    getHeader()
## Body of the frontend ====
### Menu ####
    , dashboardSidebar(
      sidebarMenuOutput("left_menu")
    # idee mit Hidden und dann die aktion abfangen        
    #, textOutput("tabSelection")
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
                id = "tabset1"
#                , height = "350px"
                , tabPanel(
                    "cases per 100k"
                  , textOutput("plotStateTitle")
#             "First tab content"
                  , plotlyOutput("plotFederalState"
#                                 , height = 250
                                 )
                )
                , tabPanel(
                    "Tab2"
                    , "Tab content 2"
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
              id = "tabset1"
              #, height = "350px"
              , tabPanel("cases per 100k"
                       , 
                       #textOutput("plotStateTitle")
                                    "First tab content"
                       , tableOutput("dataState")
              ),
              tabPanel("Tab2", "Tab content 2")
            )
          )
### Germany - Output details to plot ####
          , box(
            title = "Details to plot"
            , width = 12
            , collapsible = TRUE
            , tableOutput("dataDetails")
          )
      )
    )  
# End of Shiny App ------ 
  )
)
  )
)  
