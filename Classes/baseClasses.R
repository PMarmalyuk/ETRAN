setClass("Experiment",
         representation(name = "character",
                        startDate = "Date",
                        endDate = "Date",
                        description = "character",
                        instruction = "character",
                        experimenters = "character",
                        conditions = "Conditions"
         ),
         prototype(
         )
)

setClass("Trial",
         representation(id = "numeric", 
                        expID = "numeric",
                        name = "character",
                        description = "character",
                        conditions = "Conditions"
         ),
         prototype(
         )
)


setClass("Subject",
         representation(code = "character",
                        fullname = "character",
                        birthdate = "Date"
         ),
         prototype(
         )
)

setClass("Factor",
         representation(varName = "character",
                        description = "character",
                        type = "character", # numeric, integer, factor, ordFactor
                        levels = "character", #list of factor levels, order is important for ordFactor, NULL for numeric and integer
                        owner = "character" # "subject", "trial" or "stimulus
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
                        duration = "numeric", 
                        durationUnits = "numeric", # e.g. 1E0 for seconds
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
                        analysisResults = "list", #list of EventData, AOISequence, AOITransMatrix, AOIStatsVector objects
                        statistics = "list" # list of statistics evaluated for eyesDataObject or an element of analysisResults list
         ),
         prototype(
         )
)