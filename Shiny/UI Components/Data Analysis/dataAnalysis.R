detectionOptionsTabPanel <- source("UI Components\\Data Analysis\\detectionOptions.R")$value
tabItem(tabName = "Analysis",
        column(4,
               column(6,
                      selectInput("expID", "Experiment ID", choices = c(""))
               ),
               column(6,
                      selectInput("trialID", "Trial ID", choices = c(""))
               ),
               box(title = "Filters", status = "primary", solidHeader = TRUE,
                   collapsible = T, collapsed = T, width = 12,
                   h3("Filter by selected Factor"),
                   h3("Filter by selected Statistics"),
                   h3("Filter by selected Cluster"),
                   h3("Undo"),
                   h3("Disable Filters")
               ),
               DT::dataTableOutput("dataRecordsSelectionTable"),
               actionButton("yo", "Yo")
        ),
        column(8,
               tabsetPanel(selected = 1, type = "tabs", position = "left",
                           tabPanel(inputId = "gazeData", title = "Visualizations",
                                    tabsetPanel(selected = 1, type = "tabs", position = "left",
                                                tabPanel(inputId = "xyPlot", title = "X-Y Plots"),
                                                tabPanel(inputId = "xtPlot", title = "Time Charts"),
                                                tabPanel(inputId = "heatMap", title = "Heatmaps"),
                                                tabPanel(inputId = "heatMap", title = "Scanpaths")
                                    )),
                           tabPanel(inputId = "eventData", title = "Event Detection",
                                    tabsetPanel(selected = 1, type = "tabs", position = "left",
                                                tabPanel(inputId = "detectEvents", title = "Detect Events", 
                                                         box(title = "Detect Events", status = "primary", solidHeader = TRUE,
                                                             collapsible = F, collapsed = F, width = 12,
                                                             column(6,
                                                                    actionButton("detectEventsForSelected", label = "Detect Events (selected subjects)", class = "btn btn-large btn-block")
                                                             ),
                                                             column(6,
                                                                    actionButton("detectEventsForAll", label = "Detect Events (all subjects)", class = "btn btn-large btn-block")
                                                             )
                                                         ),
                                                         box(title = "Detection Options", status = "primary", solidHeader = TRUE,
                                                             collapsible = T, collapsed = T, width = 12,
                                                             detectionOptionsTabPanel)
                                                ),
                                                tabPanel(inputId = "evParEstimation", title = "Event Parameters Estimation",
                                                         radioButtons("eventType", label = "Choose Event:", inline = T,
                                                                      choices = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Gap", "Artifact")),
                                                         dataTableOutput("selectParamsToEvaluate")
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