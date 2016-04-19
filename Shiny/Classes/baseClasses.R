setClass("Experiment",
         representation(name = "character",
                        expDate = "character",
                        description = "character",
                        experimenters = "character"
         ),
         prototype(
         )
)

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

