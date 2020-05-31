# Shiny app Frontend or User-Interface ----
shinyUI(
  fluidPage(
## Title of the frontend or User-Interface ====
    titlePanel("Self service analysis: Covid-19"),
## Body of the frontend ====
    sidebarLayout(
### Input panel of world ####
      getUIWorldInputPanel()
### Output panel of world ####
    # Show a plot of the generated distribution
      , getUIWorldOutputPanel()
    )
  )
)