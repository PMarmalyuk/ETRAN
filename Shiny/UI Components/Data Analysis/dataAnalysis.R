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
               tabsetPanel(id = "dataAnalysisTabset", selected = NULL, type = "tabs", position = "right",
                           tabPanel(inputId = "gazeData", title = "Visualizations",
                                    selectInput('typeForPlotXt', "Select Plot Type:", choices = c("POR", "Pupil", "Velocity", "Acceleration")),
                                    # plotOutput('XYplot'),
                                    plotOutput('xtplot'),
                                    sliderInput("timeRange", "Time Interval To Show:",
                                                min = 1, max = 100000, value = c(200,50000), step = 0.1),
                                    radioButtons('eyeForPlot', label = "Select eye", choices = NA, inline = T),
                                    checkboxInput('angularPlots', "Angular", value = F),
                                    conditionalPanel(condition = "input.typeForPlotXt == 'Velocity'",
                                                     selectInput('velEstMethodForPlot', label = "Velocity Estimation Method", 
                                                                 choices = c("Finite Differentiation", "Polynomial Approximation"), selected = "Finite Differentiation"),
                                                     conditionalPanel(condition = "input.velEstMethodForPlot == 'Polynomial Approximation'",
                                                                      numericInput('filterLengthForPlot', "Approximation Window Width (msec)", value = 20,  min = 1, step = 1)
                                                     )
                                    ),
                                    selectInput('markerType', "Select Markers", choices = c("No markers", "Filter markers", "Event markers"), selected = 1),
                                    conditionalPanel("input.markerType == 'No Markers'",
                                                     numericInput('pointsColor', "Points Color", value = 1, min = 1, step = 1)
                                    )
                           ),
                           tabPanel(inputId = "eventData", title = "Event Detection",
                                    tabsetPanel(selected = 1, type = "tabs", position = "above",
                                                tabPanel(inputId = "detectOptions", title = "Detection",
                                                         column(6,
                                                                actionButton("detectEventsForSelected", label = "Detect Events (selected records)", class = "btn btn-large btn-block")
                                                         ),
                                                         column(6,
                                                                actionButton("detectEventsForAll", label = "Detect Events (all records)", class = "btn btn-large btn-block")
                                                         ),
                                                         detectionOptionsTabPanel
                                                ),
                                                tabPanel(inputId = "eventsParams", title = "Event Parameters Calculation", 
                                                         dataTableOutput("subFunctions"),
                                                         uiOutput("subFunctionSettings")
                                                )
                                    )
                           ),
                           tabPanel(inputId = "statAnalysis", title = "Statistical Analysis",
                                    tabsetPanel(selected = 1, type = "tabs", position = "above",
                                                tabPanel(inputId = "statisticsEstimation", title = "Statistics Estimation",
                                                         tabPanel(inputId = "eventsParams", title = "Calculate Event Parameters", 
                                                                  dataTableOutput("subFunctions2"),
                                                                  uiOutput("subFunctionSettings2"),
                                                                  actionButton("calcStats", label = "Calculate Statistics")
                                                         )
                                                ),
                                                tabPanel(inputId = "statsDistributions", title = "Distributions"),
                                                tabPanel(inputId = "relationshipsAnalysis", title = "Relationships")
                                    )
                           ),
                           tabPanel(inputId = "clustering", title = "Cluster Analysis",
                                    tabsetPanel(selected = 1, type = "tabs", position = "above",
                                                tabPanel(inputId = "clusteringOptions", title = "Options"),
                                                tabPanel(inputId = "clusteringVisualisations", title = "Dendrogram")
                                    )
                           )
               ),
               conditionalPanel(condition = "input.dataAnalysisTabset == 'Visualizations'",
                 box(title = "Data Filtering & Smoothing", status = "primary", solidHeader = TRUE,
                                    collapsible = T, width = 12,
                                    box(title = "Filtering Settings", status = "primary", solidHeader = TRUE,
                                        collapsible = F, width = 6,
                                        selectInput(inputId = 'filterType', label = "Select Filter", choices = c("No filter", "Core Filter", "External Filter"), selected = "No filter"),
                                        conditionalPanel(condition = "input.filterType == 'Core Filter'",
                                                         checkboxInput('useExpConditions', "Use Stimulus Dimensions Specified in Record's Conditions", value = F),
                                                         conditionalPanel(condition = "!input.useExpConditions", 
                                                                          numericInput('filterScrResX', label = "Max X Coordinate", value = 1280, min = 0),
                                                                          numericInput('filterScrResY', label = "Max Y Coordinate", value = 1024, min = 0)
                                                         ),
                                                         checkboxInput('interpolate', label = "Interolate Gaps?")
                                        ),
                                        conditionalPanel(condition = "input.filterType == 'External Filter'",
                                                         selectInput('extFilter', label = "Choose Ext Filter", choices = NULL)
                                        )
                                    ),
                                    box(title = "Smoothing Settings", status = "primary", solidHeader = TRUE,
                                        collapsible = F, width = 6,
                                        selectInput(inputId = 'smootherType', label = "Select Smoother", choices = c("No smoother", "Core Smoother", "External Smoother"), selected = "No smoother"),
                                        conditionalPanel(condition = "input.smootherType == 'Core Smoother'",
                                                         selectInput(inputId = 'smootherSubFun', label = "Select Method", 
                                                                     choices = c("Moving Average", "Moving Median", "Savitzky-Golay")),
                                                         numericInput(inputId = "fl", label = "Window width (number of samples)", value = 3, min = 3),
                                                         conditionalPanel(condition = "input.smootherSubFun == 'Savitzky-Golay'",
                                                                          numericInput(inputId = "forder", label = "Filter Order", value = 2, min = 2)
                                                         )
                                        ),
                                        conditionalPanel(condition = "input.smootherType == 'External Smoother'",
                                                         selectInput('extSmoother', label = "Choose External Smoother", choices = NULL)
                                        )
                                    )
               ))
        )
)
