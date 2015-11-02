## TO DO: CHECK THIS UI PART
fluidPage(
  checkboxInput('useExternalDetector', label = "Use External Detector?", value = F),
  conditionalPanel(condition = "input.useExternalDetector",
                   selectInput('extDetector', label = "Choose External Detector", choices = c())
  ),
  conditionalPanel(condition = "!input.useExternalDetector",
                   selectInput('detector', label = "Choose Detection Algorithm", choices = c("IVT", "IDT", "Adaptive IVT")),
                   checkboxInput('angular', label = "Angular", value = F),
                   conditionalPanel(condition = "input.detector == 'IVT'",
                                    numericInput('vt', label = "Velocity Threshold (px/sec or degrees/sec)", value = 30, min = 0, step = 1)
                   ),
                   conditionalPanel(condition = "input.detector == 'IDT'",
                                    numericInput('dispT', label = "Dispersion Threshold (px/degrees)", value = 1, min = 0, step = 1),
                                    numericInput('durT', "Minimum Fixation Duration (msec)", value = 50, min = 0, step = 1)
                   ),
                   conditionalPanel(condition = "input.detector == 'Adaptive IVT'",
                                    numericInput('ANHmaxSaccadeVel', "Maximum Saccade Velocity (degrees/sec)", value = 1000, min = 1, step = 10),
                                    numericInput('ANHmaxSaccadeAcc', "Maximum Saccade Acceleration (degrees/sec^2)", value = 100000, min = 1, step = 100),
                                    numericInput('ANHminSaccadeDur', "Minimum Saccade Duration (msec)", value = 10,  min = 1, step = 1),
                                    numericInput('ANHminFixationDur', "Minimum Fixation Duration (msec)", value = 40,  min = 1, step = 1)
                   ),
                   conditionalPanel(condition = "input.detector == 'IVT' || input.detector == 'Adaptive IVT'",
                                    selectInput('velocityEstimationMethod', label = "Velocity Estimation Method", 
                                                choices = c("Finite Differentiation", "Polynomial Approximation"), selected = "Finite Differentiation"),
                                    conditionalPanel(condition = "input.velocityEstimationMethod == 'Polynomial Approximation'",
                                                     numericInput('fLength', "Approximation Window Width (msec)", value = 20,  min = 1, step = 1)
                                    )
                   ),
                   checkboxInput('postProcess', label = "Enable Events Post-processing?"),
                   conditionalPanel(condition = "input.postProcess",
                                    conditionalPanel(condition = "input.detector == 'IVT'",
                                                     box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                         collapsible = T, width = 12,
                                                         numericInput('maxTBetFix', "Maximum Time Between Fixations (msec)", value = 75, min = 1, step = 1),
                                                         numericInput('maxDistBetFix', "Maximum Distance Between Fixations (px or degrees)", value = 0.5, min = 0, step = 1),
                                                         numericInput('maxGapLen', "Maximum Gap Duration (msec)", value = 75,  min = 1, step = 1),
                                                         numericInput('maxVel', "Maximum Velocity (px/sec or degrees/sec)",value = 1000,  min = 1, step = 10),
                                                         numericInput('maxAccel', "Maximum Accelleration (px/sec^2 or degrees/sec^2",value = 100000, min = 1, step = 100),
                                                         numericInput('minFixLen', "Minimum Fixation Duration (msec)", value = 40,  min = 1, step = 1),
                                                         checkboxInput('classifyGaps', label = "Classify Gaps?")
                                                     )
                                                     
                                    ),
                                    conditionalPanel(condition = "input.detector == 'IDT'",
                                                     box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                         collapsible = T, width = 12,
                                                         h2("No post-processing for selected detection algorithm")
                                                     )
                                    ),
                                    conditionalPanel(condition = "input.detector == 'Adaptive IVT'",
                                                     box(title = "Settings For Post-Processing", status = "primary", solidHeader = TRUE,
                                                         collapsible = T, width = 12,
                                                         h2("No post-processing for selected detection algorithm")
                                                     )
                                                     
                                    )
                   )
                   
  )
)