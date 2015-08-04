tabItem(tabName = "Trials",
        fluidPage(
          fluidRow(  
            box(
              title = "Available Trials", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              DT::dataTableOutput('trialsList')
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 12, 
              fluidRow(
                column(3,actionButton('addTrial', label = "Add New", class = "btn btn-large btn-block")),
                column(3,actionButton('updateTrial', label = "Update Selected", class = "btn btn-large btn-block")),
                column(3,actionButton('delTrial', label = "Delete Selected", class = "btn btn-large btn-block"))
              )
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 12,
              selectInput("trialExpName", label = "Experiment", choices = c("")),
              textInput(inputId = 'trialName', "Trial Name", value = ""),
              tags$textarea(id = "trialDescription", rows=4, cols=54, "My trial description")
            )
          )
        )
)