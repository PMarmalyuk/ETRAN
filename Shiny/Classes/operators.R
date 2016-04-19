
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

setClass("EventDetector", representation(id = "numeric",
                                         name = "character",
                                         description = "character",
                                         fun = "function",
                                         settings = "list")
)

setClass("EventDetectors", 
         representation(ids = "numeric", 
                        detectors = "list"))

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

