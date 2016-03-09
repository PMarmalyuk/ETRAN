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
         representation(name = "character",
                        description = "character",
                        valueClass = "character", # numeric, integer, factor, ordFactor
                        levels = "character", #list of factor levels, order is important for ordFactor, NULL for numeric and integer
                        owners = "list" # "Subject", "Trial". "Stimulus", "Record", "Observation", "EventGroup", ...
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
                        analysisResults = "list" # FactorsData object
         )
)

setClass("Loader",
         representation(name = "character",
                        fun = "function",
                        settings = "list")
)

setClass("Parser",
         representation(name = "character",
                        description = "character",
                        fun = "function",
                        settings = "list")
)

setClass("Smoother",
         representation(name = "character",
                        description = "character",
                        fun = "function",
                        settings = "list")
)

setClass("EventDetectors", representation(ids = "numeric", detectors = "list"))

setClass("EventMarkersDefinitions", representation(definitions = "list"))

setClass("EventMarkersDefinition",
         representation(eventClass = "character", # FilterEvent, OculomotorEvent, etc.
                        eventTypesIDs = "numeric", # a vector of IDs of event types
                        typesMarkers = "character" # a vector of markers for corresponding event type IDs
         )
)

setClass("EventDetector", representation(id = "numeric",
                                         name = "character",
                                         description = "character",
                                         fun = "function",
                                         settings = "list")
)

setClass("EventAnalyzer",
         representation(fun = "function",
                        name = "character",
                        settings = "list")
)

setClass("ParamEstimator",
         representation(fun = "function",
                        name = "character",
                        settings = "list")
)

setClass("SubFunction",
         representation(fun = "function",
                        name = "character", # displayed name of a sub function
                        description = "character",
                        classes = "list", # list(mainClass = "EyesData", subClass = "PupilData")
                                          # list(mainClass = "EventMarkers", subClass = "FilterEventMarkers")
                                          # list(mainClass = "EventData", subClass = "OculomotorEvent", eventIDs = c(1, 2, 3))
                                          # list(mainClass = "EventData", subClass = "SyncEvent", eventIDs = c(4, 5, 6))
                        settings = "list")) # settings of a sub function



