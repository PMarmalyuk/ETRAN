detectionOptionsTabPanel <- source("UI Components\\Data Analysis\\detectionOptions.R")$value
tabItem(tabName = "Analysis",
        column(4,
               column(6,
                      selectInput("expNameForAnalysis", "Experiment", choices = c(""))
               ),
               column(6,
                      selectInput("trialNameForAnalysis", "Trial", choices = c(""))
               ),
               box(title = "Filters", status = "primary", solidHeader = TRUE,
                   collapsible = T, collapsed = T, width = 12,
                   h3("Filter by selected Factor"),
                   h3("Filter by selected Statistics"),
                   h3("Filter by selected Cluster"),
                   h3("Undo"),
                   h3("Disable Filters")
               ),
               DT::dataTableOutput("dataRecordsSelectionTable")
        ),
        column(8,
               tabsetPanel(selected = 1, type = "tabs", position = "left",
                           tabPanel(inputId = "gazeData", title = "Visualizations",
                                    tabsetPanel(selected = 1, type = "tabs", position = "left",
                                                tabPanel(inputId = "xyPlot", title = "X-Y Plots",
                                                         actionButton('plotIT', label = "Plot!"),
                                                         plotOutput('XYplot')
                                                         ),
                                                tabPanel(inputId = "xtPlot", title = "Time Charts",
                                                         
                                                         plotOutput('xtplot')
                                                         ),
                                                tabPanel(inputId = "heatMap", title = "Heatmaps"),
                                                tabPanel(inputId = "heatMap", title = "Scanpaths")
                                    ),
                                    radioButtons('eyeForPlot', label = "Select eye", choices = NA, inline = T),
                                    checkboxInput('angularPlots', "Angular", value = F),
                                    selectInput('markerType', "Select Markers", choices = c("No markers", "Filter markers", "Event markers"), selected = 1),
                                    conditionalPanel("input.markerType == 'No Markers'",
                                                     numericInput('pointsColor', "Points Color", value = 1, min = 1, step = 1)
                                                     ),
                                    sliderInput("timeRange", "Time Interval To Show:",
                                                min = 1, max = 100000, value = c(200,50000), step = 0.1),
                                    selectInput('typeForPlotXt', "Select Plot Type:", choices = c("POR", "Pupil", "Speed", "Acceleration"))
                                    ),
                           tabPanel(inputId = "eventData", title = "Event Detection",
                                    tabsetPanel(selected = 1, type = "tabs", position = "left",
                                                tabPanel(inputId = "detectEvents", title = "Detect Events", 
                                                         box(title = "Detect Events", status = "primary", solidHeader = TRUE,
                                                             collapsible = F, collapsed = F, width = 12,
                                                             column(6,
                                                                    actionButton("detectEventsForSelected", label = "Detect Events (selected records)", class = "btn btn-large btn-block")
                                                             ),
                                                             column(6,
                                                                    actionButton("detectEventsForAll", label = "Detect Events (all records)", class = "btn btn-large btn-block")
                                                             )
                                                         ),
                                                         box(title = "Detection Options", status = "primary", solidHeader = TRUE,
                                                             collapsible = T, collapsed = T, width = 12,
                                                             detectionOptionsTabPanel)
#                                                          box(title = "Events Parameters Selection", status = "primary", solidHeader = TRUE,
#                                                              collapsible = T, width = 12,
#                                                              radioButtons("eventType", label = "Choose Event:", inline = T,
#                                                                           choices = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Gap", "Artifact")),
#                                                              dataTableOutput("selectParamsToEvaluate")
#                                                          )
                                                )
                                    )
                           ),
                           tabPanel(inputId = "statAnalysis", title = "Statistical Analysis",
                                    tabsetPanel(selected = 1, type = "tabs", position = "left",
                                                tabPanel(inputId = "evaluateStats", title = "Evaluate Statistics"),    
                                                tabPanel(inputId = "statsDistributions", title = "Sample Distributions"),
                                                tabPanel(inputId = "relationsAnalysis", title = "Statistical Relationships")
                                    )
                           ),
                           tabPanel(inputId = "clustering", title = "Cluster Analysis",
                                    tabsetPanel(selected = 1, type = "tabs", position = "left",
                                                tabPanel(inputId = "evaluateStats", title = "Options"),
                                                tabPanel(inputId = "statsDistributions", title = "Dendrogram")
                                    )
                           )
               )
        )
)