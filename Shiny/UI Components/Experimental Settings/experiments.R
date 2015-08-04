tabItem(tabName = "Experiment",
        fluidPage(
          fluidRow(  
            box(
              title = "Available Experiments", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              DT::dataTableOutput('experimentsList')
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 12, 
              fluidRow(
                column(3,actionButton('addExp', label = "Add New", class = "btn btn-large btn-block")),
                column(3,actionButton('updateExp', label = "Update Selected", class = "btn btn-large btn-block")),
                column(3,actionButton('delExp', label = "Delete Selected", class = "btn btn-large btn-block"))
              )
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 12,
              textInput(inputId = 'expName', "Experiment Name", value = "My Experiment"),
              dateInput(inputId = 'expDate', "Experiment Date"),
              tags$textarea(id = "expDescription", rows=4, cols=27, "My experiment description"),
              br(),
              tags$textarea(id = "expExperimenters", rows=4, cols=27, "Experimenters")
            )
          )
        )
)