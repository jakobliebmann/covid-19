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
      , textOutput("res")
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
                       inline = TRUE)              
          , verbatimTextOutput("value")
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
          title = "Select below",
          width = 12,
          collapsible = TRUE,
          selectizeInput(inputId = "regionchoice", 
                         label = "Select up to 2 regions of interest:", 
                         choices = regionlist, selected = c("Germany"), 
                         options = list(maxItems = 2))
        )
      )
    )  
# End of Shiny App ------ 
  )
)
  )
)  
