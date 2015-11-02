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
                        owner = "character" # "Subject", "Trial". "Stimulus", "Record", "EventGroup"
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
                        analysisResults = "list", #list of EventData, AOISequence, AOITransMatrix, AOIStatsVector objects for each eye
                        # representations = "list",
                        statistics = "list" # list of statistics (left and right) evaluated for eyesDataObject or an element of analysisResults list
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
                        name = "character", # dispalyed name of a sub function
                        description = "character",
                        operation = "character",
                        applyTo = "character", # can be "EyesData", "EventGroup", "EventData", "AOISequence", etc. (base eye data class)
                        events = "character", # if applyTo == "EventGroups" then event types should be specified (to estimate event groups parameters)
#                        outputs = "numeric", # numeric vector of Factor _ids_: it is necessary to have information about a value returned by sub function
                        settings = "list" # settings of a sub function
                        )
)

setClass("ParamEstimator",
         representation(fun = "function",
                        name = "character",
                        settings = "list")
)
