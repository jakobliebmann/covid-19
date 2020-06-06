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
          getUIWorldInputPanel()
        , getUIWorldOutputPanel()
        )
    )
### Content of germany ####
    , tabItem(
        tabName = df_tab_ids$id[[2]],
        fluidRow(
            getUIGermanyInputTabBox()
          , getUIGermanyOutputTabBoxPlots()
          , getUIGermanyOutputTabBoxDataTables()
      )
    )  
# End of Shiny App ------ 
  )
)
  )
)  
