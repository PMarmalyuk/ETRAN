operations <- c("Event Statistic Evaluation", "Object Statistic Evaluation")
events <- c("Fixation", "Saccade", "Glissade", "Smooth Pursuit", "Artifact", "Gap")
factor_types <- c("numeric", "integer", "factor", "ordFactor")
factor_owners <- c("Experiment", "Subject", "Trial", "Stimulus", "Event Group", "Data Record")
applications <- c("EyesData", "EventData", "AOISequence", "AOIMatrix", "AOIVector")

# Sub Function structure and examples:
## Examples for data smoothing
movAvgFiltering <- new(Class = "SubFunction", 
                       fun = movAvgFilt, # a function to evaluate
                       name = "Running Average Filtering", # name of a function
                       settings = list(fl = 3), # default settings to apply evaluating a function
                       description = "Smooth Trajectory using Running Average Filter", # description of a function
                       type = list(operation = "Trajectory Smoothing" # type of operation: one of c("Trajectory Smoothing", "Event Detection", "Event Statistic Evaluation", "Object Statistic Evaluation")
                       )
)

medFiltering <- new(Class = "SubFunction", 
                    fun = medianFilt, # a function to evaluate
                    name = "Median Filtering", # name of a function
                    settings = list(fl = 3), # default settings to apply evaluating a function
                    description = "Smooth Trajectory using Median Filter", # description of a function
                    type = list(operation = "Trajectory Smoothing" # type of operation: one of c("Trajectory Smoothing", "Event Detection", "Event Statistic Evaluation", "Object Statistic Evaluation")
                    )
)

savGolFiltering <- new(Class = "SubFunction", 
                       fun = savGolFiltering, # a function to evaluate
                       name = "Savitzky-Golay Filtering", # name of a function
                       settings = list(fl = 3, forder = 2, dorder = 1), # default settings to apply evaluating a function
                       description = "Smooth Trajectory using Savitzky-Golay Filter", # description of a function
                       type = list(operation = "Trajectory Smoothing" # type of operation: one of c("Trajectory Smoothing", "Event Detection", "Event Statistic Evaluation", "Object Statistic Evaluation")
                       )
)

## Examples for event detection
IVTDetection <- new(Class = "SubFunction", 
                       fun = IVT, # a function to evaluate
                       name = "IVT Event Detector", # name of a function
                       settings = list(postProcess = F,
                                       VT = 30,
                                       angular = T,
                                       screenDist = 100,
                                       screenDim = c(1280, 1024),
                                       screenSize = c(33.7, 27),
                                       MaxTBetFix = 0.075,
                                       MaxDistBetFix = 0.5,
                                       minFixLen = 0.05,
                                       maxGapLen = 0.07,
                                       maxVel = 1000,
                                       maxAccel = 1000000,
                                       classifyGaps = F), # default settings to apply evaluating a function
                       description = "Events Detection by Velocity Threshold Algorithm", # description of a function
                       type = list(operation = "Event Detection" # type of operation: one of c("Trajectory Smoothing", "Event Detection", "Event Statistic Evaluation", "Object Statistic Evaluation")
                       )
)

## Examples for event parameters evaluation
valCode <- new(Class = "SubFunction", 
               fun = getValCode, # a function to evaluate
               name = "Validity Code", # name of a function
               settings = list(), # settings to apply evaluating a function
               description = "Get validity code of event", # description of a function
               type = list(operation = "Event Statistic Evaluation", # type of operation: one of c("Trajectory Smoothing", "Event Detection", "Event Statistic Evaluation", "Object Statistic Evaluation")
                           events = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit"), # to which event groups the fun should be applied
                           output = c(new(Class = "Factor", 
                                          varName = "valCode", # name of resulting statistic
                                          description = "Validity code of event", # description of resulting statistic
                                          type = "factor", # type of resulting statistic: one of c("numeric", "integer", "factor", "ordFactor")
                                          levels = c("Invalid", "Valid"), # levels of resulting factor/ordFactor statistic
                                          owner = "Event Group"
                           )
                           )
               )
)

onOffsetDuration <- new(Class = "SubFunction", 
                        fun = getOnOffSetDuration, # a function to evaluate
                        name = "On, OffSet and Duration", # name of a function
                        settings = list(), # settings to apply evaluating a function
                        description = "Get onset, offset and duration of event", # description of a function
                        type = list(operation = "Event Statistic Evaluation", # type of operation: one of c("Trajectory Smoothing", "Event Detection", "Event Statistic Evaluation", "Object Statistic Evaluation")
                                    events = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit"), # to which event groups the fun should be applied
                                    output = c(new(Class = "Factor", 
                                                   varName = "Onset", # name of resulting statistic
                                                   description = "Onset of event", # description of resulting statistic
                                                   type = "numeric", # type of resulting statistic: one of c("numeric", "integer", "factor", "ordFactor")
                                                   levels = NA, # levels of resulting factor/ordFactor statistic
                                                   owner = "Event Group"
                                    ),
                                    new(Class = "Factor", 
                                        varName = "Offset", # name of resulting statistic
                                        description = "Offset of event", # description of resulting statistic
                                        type = "numeric", # type of resulting statistic: one of c("numeric", "integer", "factor", "ordFactor")
                                        levels = NA, # levels of resulting factor/ordFactor statistic
                                        owner = "Event Group"
                                    ),
                                    new(Class = "Factor", 
                                        varName = "Duration", # name of resulting statistic
                                        description = "Duration of event", # description of resulting statistic
                                        type = "numeric", # type of resulting statistic: one of c("numeric", "integer", "factor", "ordFactor")
                                        levels = NA, # levels of resulting factor/ordFactor statistic
                                        owner = "Event Group"
                                    )
                                    )
                        )
)

## Example for EyesData object statistic evaluation
trajDuration <- new(Class = "SubFunction", 
                    fun = trajDurationEstimator, # a function to evaluate
                    name = "Trajectory Duration", # name of a function
                    settings = list(), # settings to apply evaluating a function
                    description = "Get duration of a gaze trajectory", # description of a function
                    type = list(operation = "Object Statistic Evaluation", # type of operation: one of c("Trajectory Smoothing", "Event Detection", "Event Statistic Evaluation", "Object Statistic Evaluation")
                                # events = c("Fixation", "Saccade", "Glissade", "Smooth Pursuit"), # to which event groups the fun should be applied
                                output = c(new(Class = "Factor", 
                                               varName = "trajDuration", # name of resulting statistic
                                               description = "Trajectory Duration", # description of resulting statistic
                                               type = "numeric", # type of resulting statistic: one of c("numeric", "integer", "factor", "ordFactor")
                                               levels = NA, # levels of resulting factor/ordFactor statistic
                                               owner = "Data Record"
                                )
                                ),
                                applyTo = c("EyesData") # to which object a function should be applied to: 
                    )
)
