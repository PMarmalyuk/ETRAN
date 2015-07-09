setClass("Experiments",
         representation(expList = "list"
         ),
         prototype(
         )
)

setClass("Trials",
         representation(trialsList = "list"
         ),
         prototype(
         )
)

setClass("Subjects",
         representation(subjectsList = "list"
         ),
         prototype(
         )
)

setClass("Stimuli",
         representation(stimuliList = "list"
         ),
         prototype(
         )
)

setClass("AOISet", 
         representation(name = "character",
                        AOIs = "list" # list of AOI objects
         ),
         prototype()
)

setClass("AOISets",
         representation(name = "character",
                        AOISetsList = "list" # list of ids and AOIs elements
         ),
         prototype(
         )
)

setClass("TAS",
         representation(TAS = "data.frame" # df of trialID, AOISetID and stimulusID columns
         ),
         prototype(
         )
)

setClass("AvailableFactors",
         representation(availableFactors = "data.frame" #df of factor id, factor name, type, levels and owner
         ),
         prototype(
         )
)

setClass("FactorsData",
         representation(subjectsFactors = "data.frame",
                        stimuliFactors = "data.frame",
                        trialsFactors = "data.frame")
         ,
         prototype(
         )
)

setClass("RawDataRecords",
         representation(rawDataRecordsList = "list" # list of rawDataRecords
         ),
         prototype(
         )
)

setClass("DataSample",
         representation(keys = "data.frame", # df with expID, subjectID, trialID fields
                        eyesDataObjectsList = "list",
                        analysisResultsList = "list",
                        statisticsList = "list"
         ),
         prototype()
         )

setClass("SubFunctions",
         representation(SubFunctionsList = "list"),
         prototype()
)

setClass("Loaders",
         representation(loadersList = "list") # list of $ids and $loaders
         )

setClass("Parsers",
         representation(parsersList = "list"),
         prototype(parsersList = list(ids = list(1), parsers = list(new(Class = "Parser"))))
)

