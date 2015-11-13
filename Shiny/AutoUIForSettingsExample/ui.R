body <- dashboardBody(
  fluidPage(
    br(),
    actionButton('generateUI', "Generate UI", class = "btn btn-large btn-block"),
    uiOutput('uiFrame'),
    actionButton('saveSettings', "Save Settings", class = "btn btn-large btn-block")
  )
)

# Put them together into a dashboardPage
dashboardPage(
  dashboardHeader(title = "ETRAN Shiny App v0.1", titleWidth = 100),
  sidebar = dashboardSidebar(),
  body
)