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
         representation(availableFactors = "data.frame" #df of factor id, factor name, description, type, levels and owner
         ),
         prototype(
         )
)

## factors Data is used as a table for keeping various calculations results or objects' imported factor values
## Usage:
## - we can create one such table per session for keeping values of DataRecords overall statistics (e.g. duration)
## - or create multiple embedded tables for keeping values of multiple objects related to one DataRecord 
## (e.g. one EventData table per data record for an eye)
## owner_id is ID of an object linked to a factor's value (e.g. subject ID, stimulus ID, trial ID, data record ID, event group ID, etc.)
## owner is a type of an object ("Subject", "Stimulus", "Trial", "DataRecord", "EventGroup", etc.)
setClass("FactorsData",
         representation(factorsDataList = "list") # list of owner_id, owner, value
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
         representation(keys = "data.frame", # df with expID, subjectID, trialID
                        ids = "numeric", 
                        DataRecordsList = "list"
         ),
         prototype()
         )


setClass("SubFunctions",
         representation(subFunctionsList = "list"),
         prototype()
)

setClass("Loaders",
         representation(loadersList = "list") # list of $ids and $loaders
         )

setClass("Parsers",
         representation(parsersList = "list")
)

setClass("Filters",
         representation(filtersList = "list")
)

setClass("Smoothers",
         representation(smoothersList = "list")
)

setClass("Detectors",
         representation(detectorsList = "list")
)

