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

setClass("FactorsAndRepresentationsDefinitions",
         representation(ids = "numeric",
                        factorsDef = "list" #df of factor id, factor name, description, class, levels and owner
         ),
         prototype(
         )
)

## factors Data is used as a table for keeping various factors values (e.g. calculations results or main entities' imported properties)
## All factors are stored in separate object which is a data.frame with the following columns:
## factorID, eye, value, ownerID and owner
### factorID points to the AvailableExternalFactors table containing external factors definitions
### eye is needed only in case of internal factors for separating values obtained for different eyes
### value is a list with one named element with explicitly specified type/class
### ownerID is a list with several named elements containing IDs which point to corresponding main entities within the system
### owner is a label of the owner type

# ----------- Main Entities FactorsData ----------- 
#### owner: Experiment", "Subject", "Trial", "Stimulus", "Observation"
#### ownerIDs:
#### list(ExpID = 1) for owner "Experiment"
#### list(SubjectID = 1) for owner "Subject"
#### list(ExpID = 1, TrialID = 1) for owner "Trial"
#### list(StimulusID = 1) for owner "Stimulus"
#### list(ExpID = 1, TrialID = 1, SubjectID = 1) for owner "Observation"
# ----------- EyesDataObjects FactorsData ----------- 
#### owner: list("Event", "Fixation"), list("AOI", "MyAOIName"), list("Frame")
#### ownerIDs:
#### list(EventGroup = 1) for owner[[1]] == "Event"
#### list(AOIID = 1) for owner "owner[[1]] == "AOI"
#### list(FrameID = 1) for owner "owner[[1]] == "Frame"

## So, we will have one table for Main Entities FactorsData which is stored as a reactive value: one table per ETRAN session
## We will have several tables with EyesDataObjects FactorsData: one table per DataRecord

setClass("FactorsData",
         representation(factorsData = "data.frame") # data.frame of factorID, eye, value, owner_id
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

