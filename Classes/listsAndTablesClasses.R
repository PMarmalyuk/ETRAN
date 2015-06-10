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

setClass("Factors",
         representation(factorsList = "list" # list of ids and values elements
         ),
         prototype(
         )
)

setClass("FactorsData",
         representation(owners = "character",
                        ownersIDs = "numeric",
                        factorsList = "list" #list of Factors
         ),
         prototype(
         )
)

setClass("RawDataRecords",
         representation(rawDataRecordsList = "list" # list of fileNumbers and rawDataRecords
         ),
         prototype(
         )
)

setClass("DataSample",
         representation(keys = "data.frame", # df with expID, subjectID, trialID fields
                        eyesDataObjectsList = "list",
                        analysisResults = "list",
                        statistics = "list"
         ),
         prototype()
         )

