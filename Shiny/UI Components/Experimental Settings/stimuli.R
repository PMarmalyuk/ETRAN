tabItem(tabName = "Stimuli",
        fluidPage(
          fluidRow(  
            box(
              title = "Available Stimuli", status = "primary", solidHeader = TRUE,
              collapsible = TRUE, width = 12,
              DT::dataTableOutput('stimuliList')
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 12, 
              fluidRow(
                column(3,actionButton('addStimuli', label = "Add New", class = "btn btn-large btn-block")),
                column(3,actionButton('viewStimuli', label = "View Selected", class = "btn btn-large btn-block")),
                column(3,actionButton('updateStimuli', label = "Update Selected", class = "btn btn-large btn-block")),
                column(3,actionButton('delStimuli', label = "Delete Selected", class = "btn btn-large btn-block"))
              )
            )),
          fluidRow(
            box(
              status = "primary", solidHeader = TRUE,
              collapsible = F, width = 6,
              textInput(inputId = 'stimName', "Stimuli Name", value = ""),
              tags$textarea(id = "stimDescription", rows=4, cols=45, "My stimulus description"),
              selectInput("stimType", label = "Stimulus Type", choices = c("Image", "Video", "Scene"), selected = 1),
              conditionalPanel(condition = "input.stimType == 'Image' | input.stimType == 'Video'",
                               fileInput(inputId = "stimFilepath", label = "Stimulus File"),
                               numericInput("stimDimX", label = "Stimulus Size X (px)", value = NA, min = 0),
                               numericInput("stimDimY", label = "Stimulus Size Y (px)", value = NA, min = 0)
                               ),
              conditionalPanel(condition = "input.stimType == 'Video'",
                               numericInput("stimDuration", label = "Stimulus Duration (sec)", value = NA, min = 0),
                               numericInput("stimFramesCnt", label = "Number of Frames", value = NA, min = 0)
              )

            ),
            box(status = "primary", solidHeader = TRUE,
                collapsible = F, width = 6,
                imageOutput("stimImage"))
          )
        )
)