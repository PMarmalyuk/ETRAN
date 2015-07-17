fluidPage(
  fluidRow(
    box(title = "Filtering Settings", status = "primary", solidHeader = TRUE,
        collapsible = T, width = 6,
        selectInput(inputId = 'filterForDetector', label = "Select Filter:", choices = c("No filter", "Core Filter", "External Filter")),
        conditionalPanel(condition = "input.filterForDetector == 'Core Filter'",
                         numericInput('filterScrDimX', label = "Max X Coordinate", value = 1280, min = 0),
                         numericInput('filterScrDimY', label = "Max Y Coordinate", value = 1024, min = 0),
                         actionButton('useExpConditions1', "Use Values Specified in Experiment Conditions"),
                         checkboxInput('interpolate', label = "Interolate Gaps?")
        ),
        conditionalPanel(condition = "input.filterForDetector == 'External Filter'",
                         selectInput('extFilterForDetector', label = "Choose Ext Filter", choices = NULL)
        )
    ),
    box(title = "Smoothing Settings", status = "primary", solidHeader = TRUE,
        collapsible = T, width = 6,
        selectInput(inputId = 'smootherForDetector', label = "Select Smoother:", choices = c("No smoother", "Core Smoother", "External Smoother")),
        conditionalPanel(condition = "input.smootherForDetector == 'Core Smoother'",
                         selectInput(inputId = 'smootherSubFun', label = "Select Method:", 
                                     choices = c("Moving Average", "Moving Median", "Savitzky-Golay")),
                         numericInput(inputId = "fl", label = "Window width:", value = 3, min = 3),
                         conditionalPanel(condition = "input.smootherSubFun == 'Savitzky-Golay'",
                                          numericInput(inputId = "forder", label = "Filter Order:", value = 2, min = 2),
                                          numericInput(inputId = "dorder", label = "Drivative Order:", value = 1, min = 1)
                         )
        ),
        conditionalPanel(condition = "input.smootherForDetector == 'External Smoother'",
                         selectInput('extSmootherForDetector', label = "Choose Ext Smoother", choices = NULL)
        )
    )
  ),
  fluidRow(
    box(title = "Detection Settings", status = "primary", solidHeader = TRUE,
        collapsible = T, width = 12,
        checkboxInput('useExtDetector', label = "Use External Detector?"),
        conditionalPanel(condition = "input.useExtDetector",
                         selectInput('extDetector', label = "Choose External Detector:", choices = c())
        ),
        conditionalPanel(condition = "!input.useExtDetector",
                         selectInput('detector', label = "Choose Detection Algorithm:", choices = c("IVT", "IDT", "Adaptive IDT")),
                         conditionalPanel(condition = "input.detector == 'IVT'",
                                          numericInput('vt', label = "Velocity Threshold (px/angles per second)", value = 30, min = 0, step = 1)
                         ),
                         conditionalPanel(condition = "input.detector == 'IDT'",
                                          numericInput('dt', label = "Dispersion Threshold (px/angles)", value = 12345, min = 0, step = 1)
                         ),
                         conditionalPanel(condition = "input.detector == 'Adaptive IDT'",
                                          numericInput('adadt', label = "Dispersion Threshold (px/angles)", value = 12345, min = 0, step = 1)
                         ),
                         checkboxInput('angular', label = "Use Angular Velocities?"),
                         conditionalPanel(condition = "input.angular",
                                          fluidRow(
                                            column(12,
                                                   box(title = "Settings For Angular Velocity Estimation", status = "primary", solidHeader = TRUE,
                                                       collapsible = T, width = 12,
                                                       actionButton('useExpConditions2', "Use Values Specified in Experiment Conditions"),
                                                       numericInput('detectorScreenDist', "Screen Distance (cm)", value = 70, min = 1, step = 1),
                                                       numericInput('detectorScreenSizeX', "Screen Size X (cm)", value = 33.7, min = 1, step = 1),
                                                       numericInput('detectorScreenSizeY', "Screen Size Y (cm)", value = 27, min = 1, step = 1),
                                                       numericInput('detectorScreenDimX', "Screen Dimension X (px)", value = 1280, min = 1, step = 1),
                                                       numericInput('detectorScreenDimY', "Screen Dimension Y (px)", value = 1024, min = 1, step = 1)
                                                   )                                                      
                                            )
                                          )
                                          
                         ),
                         checkboxInput('postProcess', label = "Enable Events Post-processing?"),
                         conditionalPanel(condition = "input.postProcess",
                                          conditionalPanel(condition = "input.detector == 'IVT'",
                                                           box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                               collapsible = T, width = 12,
                                                               numericInput('maxTBetFix', "Maximum Time Between Fixations (ms)", value = 75, min = 1, step = 1),
                                                               numericInput('maxDistBetFix', "Maximum Distance Between Fixations (px/angles)", value = 0.5, min = 0, step = 1),
                                                               numericInput('maxGapLen', "Maximum Gap Duration (ms)", value = 75,  min = 1, step = 1),
                                                               numericInput('maxVel', "Maximum Velocity (px/angles per second)",value = 1000,  min = 1, step = 1),
                                                               numericInput('maxAccel', "Maximum Accelleration",value = 1000000, min = 1, step = 1),
                                                               numericInput('minFixLen', "Minimum Fixation Duration (ms)",value = 50,  min = 1, step = 1),
                                                               checkboxInput('classifyGaps', label = "Classify Gaps?")
                                                           )
                                                           
                                          ),
                                          conditionalPanel(condition = "input.detector == 'IDT'",
                                                           box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                               collapsible = T, width = 12,
                                                               h2("Some params for IDT")
                                                           )
                                                           
                                          ),
                                          conditionalPanel(condition = "input.detector == 'Adaptive IDT'",
                                                           box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                               collapsible = T, width = 12,
                                                               h2("Some params for AdaIDT")
                                                           )
                                                           
                                          )
                         )
                         
        )
    )
  )
)