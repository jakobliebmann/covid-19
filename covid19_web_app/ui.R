shinyUI(
  fluidPage(
    titlePanel("Self service analysis: Covid-19"),
    sidebarLayout(
      getUIWorldInputPanel()
    # Show a plot of the generated distribution
      , getUIWorldOutputPanel()
    )
  )
)