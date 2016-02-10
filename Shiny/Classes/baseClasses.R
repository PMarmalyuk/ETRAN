setClass("Experiment",
         representation(name = "character",
                        expDate = "character",
                        description = "character",
                        experimenters = "character"
         ),
         prototype(
         )
)

## TO DO: add a slot for ordering information
setClass("Trial",
         representation(expID = "numeric",
                        name = "character",
                        description = "character"#,
                        # order = "numeric"
         ),
         prototype(
         )
)


setClass("Subject",
         representation(code = "character",
                        fullname = "character",
                        birthdate = "character"
         ),
         prototype(
         )
)

setClass("Factor",
         representation(varName = "character",
                        description = "character",
                        type = "character", # numeric, integer, factor, ordFactor
                        levels = "character", #list of factor levels, order is important for ordFactor, NULL for numeric and integer
                        owner = "character" # "Subject", "Trial". "Stimulus", "Record", "Observation", "EventGroup", ...
         ),
         prototype(
         )
)

setClass("Stimulus",
         representation(id = "numeric", 
                        name = "character",
                        description = "character",
                        path = "character",
                        type = "character", # "Scene", "Image", "Video", "Web page", etc.
                        dim = "integer",
                        dimOnPresentation = "integer",
                        duration = "numeric", # Duration in ms
                        framesCount = "numeric"
         ),
         prototype(
         )
)

setClass("AOI",
         representation(name = "character",
                        dynamic = "logical",
                        type = "character", # "Classic", "Fuzzy"
                        shape = "character", # "Rectangle", "Circle", "Ellipse", "Polyhedron", "Gaussian" etc.
                        dispositionData = "list"
         ),
         prototype(
         )
)

setClass("RawDataRecord",
         representation(filePath = "character",
                        sep = "character",
                        headerLines = "character",
                        data = "data.frame"
         ),
         prototype(
         )
)

setClass("DataRecord",
         representation(expID = "numeric",
                        subjectID = "numeric",
                        trialID = "numeric",
                        eyesDataObject = "EyesData",
                        analysisResults = "list" #list of EventData, FrameData, AOIData, AOISequence, AOITransMatrix, AOIStatsVector objects
         ),
         prototype(
         )
)

setClass("Loader",
         representation(name = "character",
                        fun = "function",
                        settings = "list")
)

setClass("Parser",
         representation(name = "character",
                        fun = "function",
                        settings = "list")
)

setClass("Filter",
         representation(fun = "function",
                        name = "character",
                        settings = "list")
)

setClass("Smoother",
         representation(fun = "function",
                        name = "character",
                        settings = "list")
)


setClass("EventDetector",
         representation(fun = "function",
                        name = "character",
                        settings = "list")
)

setClass("EventAnalyzer",
         representation(fun = "function",
                        name = "character",
                        settings = "list")
)

setClass("SubFunction",
         representation(fun = "function",
                        name = "character", # displayed name of a sub function
                        description = "character",
                        operation = "character", # can be "Event Analysis" or "Record Analysis" or "Main Entity Analysis"
                        applyTo = "character", # Event Analysis cases: "Oculomotor Events", "AOI Events", "Frame Events",
                                               # Record Analysis cases: "Eyes Data", "Event Data", "AOI Data", "AOI Sequence", "AOI Stats Vector", "AOI Transition Matrix"
                                               # Main Entity Analysis cases: "ObservationsData", "TrialData", "StimulusData"
                        applyWhen = "character", # if applyTo == "Oculomotor Events" then event types should be specified (for which this function is appropriate)
                                              # if applyTo == "AOI Events" then AOI types should be specified
                                              # ***
                                              # if applyTo == "EventData" then event types should be specified (for which this function is appropriate)
                                              # if applyTo == "AOIData" then AOI type should be specified (for which this function is appropriate)
                                              # 
                        
                        settings = "list" # settings of a sub function
                        )
)




setClass("ParamEstimator",
         representation(fun = "function",
                        name = "character",
                        settings = "list")
)
