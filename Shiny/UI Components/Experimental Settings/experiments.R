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
                column(3,actionButton('viewExp', label = "View Selected", class = "btn btn-large btn-block")),
                column(3,actionButton('updateExp', label = "Update Selected", class = "btn btn-large btn-block")),
                column(3,actionButton('delExp', label = "Delete Selected", class = "btn btn-large btn-block"))
              )
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 4,
              textInput(inputId = 'expName', "Experiment Name", value = "My Experiment"),
              dateInput(inputId = 'expDate', "Experiment Date"),
              tags$textarea(id = "expDescription", rows=4, cols=27, "My experiment description"),
              br(),
              tags$textarea(id = "expExperimenters", rows=4, cols=27, "Experimenters")
            ),
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 4,
              selectInput(inputId = 'expEye', label = "Recording Mode", choices = c("Left Eye", "Right Eye", "Binocular"), multiple = F, selected = 1),
              numericInput(inputId = 'expSampleRate', label = "Sample Rate (Hz)", value = NA, min = 0, step = 1),
              selectInput(inputId = 'expTimeUnits', label = "Time Unit", choices = c("microseconds", "milliseconds", "seconds"), multiple = F, selected = 1),
              checkboxInput('pupilDataExist', label = "Pupil Data Exist?"),
              conditionalPanel(condition = "input.pupilDataExist",
                               selectInput(inputId = 'expPupSizeUnits', label = "Pupil Size Unit", choices = c("px", "mm", "cm"), multiple = F, selected = 1),
                               selectInput(inputId = 'expPupShape', label = "Pupil Shape", choices = c("circle", "ellipse"), multiple = F, selected = 1)
              )
            ),
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 4,
              numericInput('expScreenDist', "Screen Distance (cm)", value = NA, min = 1, step = 1),
              numericInput('expScreenSizeX', "Screen Size X (cm)", value = NA, min = 1, step = 1),
              numericInput('expScreenSizeY', "Screen Size Y (cm)", value = NA, min = 1, step = 1),
              numericInput('expScreenDimX', "Screen Dimension X (px)", value = NA, min = 1, step = 1),
              numericInput('expScreenDimY', "Screen Dimension Y (px)", value = NA, min = 1, step = 1)
            )
          )
        )
)