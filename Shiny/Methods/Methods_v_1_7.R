# Classes v 1.6
####################
#class_declarations#
####################
# classesFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Git\\EyeTrackingProject\\Classes"
# setwd(classesFolder)
# source("optionsAndSettingsClasses.R")
# source("baseEyeDataClasses.R")
# source("baseClasses.R")
# source("listsAndTablesClasses.R")
# functionsFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Git\\EyeTrackingProject\\Functions"
# setwd(functionsFolder)
# source("dataLoaders.R")
# source("dataParsers.R")
# source("filters.R")
# source("smoothers.R")
# source("detectors.R")
# source("eventAnalyzers.R")
# source("estimators.R")
# source("miscFunctions.R")
# source("mainFunctions.R")

##############################
#generic_methods_declarations#
##############################
setGeneric("addExperiment", function(self, expObject){standardGeneric("addExperiment")})
setGeneric("updateExperimentById", function(self, id, expObject){standardGeneric("updateExperimentById")})
setGeneric("delExperimentsById", function(self, ids){standardGeneric("delExperimentsById")})
# setGeneric("getConditionsById", function(self, id){standardGeneric("getConditionsById")})
setGeneric("getExperimentsNames", function(self){standardGeneric("getExperimentsNames")})
setGeneric("getExperimentIdByName", function(self, name){standardGeneric("getExperimentIdByName")})
setGeneric("getExperimentNameById", function(self, id){standardGeneric("getExperimentNameById")})

setGeneric("addTrial", function(self, trialObject){standardGeneric("addTrial")})
setGeneric("updateTrialById", function(self, id, trialObject){standardGeneric("updateTrialById")})
setGeneric("delTrialsById", function(self, ids){standardGeneric("delTrialsById")})
setGeneric("getTrialsNames", function(self, expID){standardGeneric("getTrialsNames")})
setGeneric("getTrialIdByName", function(self, expID, name){standardGeneric("getTrialIdByName")})
setGeneric("getTrialNameById", function(self, expID, trialID){standardGeneric("getTrialNameById")})

setGeneric("addSubject", function(self, subjObject){standardGeneric("addSubject")})
setGeneric("updateSubjectById", function(self, id, subjObject){standardGeneric("updateSubjectById")})
setGeneric("delSubjectsById", function(self, ids){standardGeneric("delSubjectsById")})
setGeneric("getSubjectCodes", function(self){standardGeneric("getSubjectCodes")})
setGeneric("getSubjectIdByCode", function(self, code){standardGeneric("getSubjectIdByCode")})
setGeneric("getSubjectCodeById", function(self, id){standardGeneric("getSubjectCodeById")})


setGeneric("addStimulus", function(self, stimulusObject){standardGeneric("addStimulus")})

setGeneric("addAOI", function(self, AOIObject){standardGeneric("addAOI")})

setGeneric("addAOISet", function(self, AOISetObject, orderIndex){standardGeneric("addAOISet")})

setGeneric("addAOISets", function(self, AOISetsObject){standardGeneric("addAOISets")})

setGeneric("addFactorDefinition", function(self, factor){standardGeneric("addFactorDefinition")})
setGeneric("factorExists", function(self, factor){standardGeneric("factorExists")})
setGeneric("deleteFactorDefinition", function(self, factorID){standardGeneric("deleteFactorDefinition")})
setGeneric("updateFactorDefinition", function(self, factorID, factor, FactorsData){standardGeneric("updateFactorDefinition")})
setGeneric("getFactorIDByName", function(self, factorName){standardGeneric("getFactorIDByName")})
setGeneric("getNameByFactorID", function(self, factorID){standardGeneric("getNameByFactorID")})
setGeneric("getTypeByFactorID", function(self, factorID){standardGeneric("getTypeByFactorID")})
setGeneric("getLevelsByFactorID", function(self, factorID){standardGeneric("getLevelsByFactorID")})

setGeneric("factorValueIsCorrect", function(self, factorID, value){standardGeneric("factorValueIsCorrect")})

setGeneric("addFactorValue", function(self, availableFactors, owner, ownerID, factorID, eye, value, replace){standardGeneric("addFactorValue")})
setGeneric("deleteFactorValue", function(self, owner, ownerID, factorID, eye){standardGeneric("deleteFactorValue")})
setGeneric("deleteAllFactorValuesForOwner", function(self, owner){standardGeneric("deleteAllFactorValuesForOwner")})
setGeneric("deleteAllFactorValuesForOwnerID", function(self, ownerID){standardGeneric("deleteAllFactorValuesForOwnerID")})
setGeneric("deleteAllFactorValuesForFactorID", function(self, factorID){standardGeneric("deleteAllFactorValuesForFactorID")})

setGeneric("asDataFrame", function(self, owner){standardGeneric("asDataFrame")})

setGeneric("addDataRecord", function(self, dataRecord){standardGeneric("addDataRecord")})
setGeneric("updateDataRecord", function(self, id, dataRecord, updateFlag){standardGeneric("updateDataRecord")})
setGeneric("filterDataSample", function(self, expIDs, subjectIDs, trialFilters, factorFilters){standardGeneric("filterDataSample")})

## data loaders

setGeneric("addLoader", function(self, loaderObject){standardGeneric("addLoader")})
setGeneric("addRawDataRecord", function(self, filepath, loader){standardGeneric("addRawDataRecord")})
setGeneric("addRawDataRecords", function(self, filesList, loader){standardGeneric("addRawDataRecords")})
setGeneric("delRawDataRecordsById", function(self, ids){standardGeneric("delRawDataRecordsById")})

## data parser
setGeneric("parseDataRecord", function(self, parser){standardGeneric("parseDataRecord")})

## EyesData extractor from DataRecord object for specified eye
setGeneric("getDataFrame", function(self, eye){standardGeneric("getDataFrame")})

## data filter
setGeneric("dataFilter", function(self, filter){standardGeneric("dataFilter")})

## data smoother
setGeneric("dataSmoother", function(self, smoother){standardGeneric("dataSmoother")})

## event detector
setGeneric("eventDetector", function(self, detector){standardGeneric("eventDetector")})

## events detection uses particular filter, smoother and detector
setGeneric("detectEvents", function(self, filter, smoother, detector){standardGeneric("detectEvents")})

## events analysis
setGeneric("eventAnalyzer", function(self, analyzer){standardGeneric("eventAnalyzer")})

## estimation of parameters of EyesData, EventData, etc.
setGeneric("estimateParams", function(self, estimator){standardGeneric("estimateParams")})

## sub functions
setGeneric("addSubFunction", function(self, subFunction){standardGeneric("addSubFunction")})
setGeneric("getSubfunctions", function(self, operation = NA, applyTo = NA, event = NA){standardGeneric("getSubfunctions")})

## Cluster analysis of DataRecords
setGeneric("findClusters", function(self, clusterAnalyzer){standardGeneric("findClusters")})

## visualisations
setGeneric("plotXY", function(self, eye, filter, smoother, period, onStimulus){standardGeneric("plotXY")})
setGeneric("plotXt", function(self, settings){standardGeneric("plotXt")})

######################
#methods_realizations#
######################

# Method adds an experiment object into Experiments list with ids and experiments sublists
## Method increments ids of experiments
## Method doesn't prevent duplicates in Experiments because their presence is not critical due to user's control
setMethod("addExperiment",  "Experiments",                                   
          function(self, expObject)
          {                         
            expCnt <- length(self@expList$ids)
            if (expCnt == 0) 
            {
              self@expList$ids <- 1
              self@expList$experiments <- list(expObject)
              return(self)
            }
            id <- self@expList$ids[[expCnt]] + 1
            self@expList$ids <- c(self@expList$ids, id)
            self@expList$experiments <- append(self@expList$experiments, list(expObject))
            return(self)
          }
)


setMethod("getExperimentsNames",  "Experiments",                                   
          function(self)
          {                         
            expNames <- sapply(self@expList$experiments, FUN = function(x) {x@name})
            return(expNames)
          }
)

setMethod("getExperimentIdByName",  "Experiments",                                   
          function(self, name)
          {                         
            expNames <- getExperimentsNames(self)
            if (any(expNames == name))
            {
              id <- self@expList$ids[[which(expNames == name)]]
              return(id)
            }
            else return(NULL)
          }
)

setMethod("getExperimentNameById", "Experiments",
          function(self, id)
          {
            self@expList$experiments[[which(self@expList$ids == id)]]@name
          }
)

setMethod("asDataFrame",  "Experiments",                                   
          function(self)
          {
            if (length(self@expList$ids) == 0)
            {
              df <- data.frame(Id = numeric(), Name = character(), Date = character(), 
                               Description = character(), Experimenters = character())
              return(df)
            }
            ids <- self@expList$ids
            names <- sapply(self@expList$experiments, FUN = function(x) {x@name})
            expDates <- sapply(self@expList$experiments, FUN = function(x) {x@expDate})
            descriptions <- sapply(self@expList$experiments, FUN = function(x) {x@description})
            experimenters <- sapply(self@expList$experiments, FUN = function(x) {x@experimenters})
            df <- data.frame(Id = ids, Name = names, Date = expDates, Description = descriptions, Experimenters = experimenters, stringsAsFactors = F)
            return(df)
          }
)

setMethod("updateExperimentById",  "Experiments",                                   
          function(self, id, expObject)
          {
            self@expList$experiments[[which(self@expList$ids == id)]] <- expObject
            return(self)
          }
)

setMethod("delExperimentsById",  "Experiments",                                   
          function(self, ids)
          {
            for (i in ids)
            {
              expToDel <- which(self@expList$ids == i)
              if (expToDel != 0)
              {
                self@expList$ids <- self@expList$ids[-expToDel]
                self@expList$experiments <- self@expList$experiments[-expToDel]
              }
            }
            return(self)
          }
)
          

# Method adds a trial object into Trials list with ids and trials sublists
setMethod("addTrial",  "Trials",                                   
          function(self, trialObject)
          {    
            trialsCnt <- length(self@trialsList$ids)
            if (trialsCnt == 0) 
            {
              self@trialsList$ids <- 1
              self@trialsList$trials <- list(trialObject)
              return(self)
            }
            id <- self@trialsList$ids[[trialsCnt]] + 1
            self@trialsList$ids <- c(self@trialsList$ids, id)
            self@trialsList$trials <- append(self@trialsList$trials, list(trialObject))
            return(self)
          }
)

setMethod("updateTrialById",  "Trials",                                   
          function(self, id, trialObject)
          {
            self@trialsList$trials[[which(self@trialsList$ids == id)]] <- trialObject
            return(self)
          }
)

setMethod("delTrialsById",  "Trials",                                   
          function(self, ids)
          {
            for (i in ids)
            {
              trialToDel <- which(self@trialsList$ids == i)
              if (trialToDel != 0)
              {
                self@trialsList$ids <- self@trialsList$ids[-trialToDel]
                self@trialsList$trials <- self@trialsList$trials[-trialToDel]
              }
            }
            return(self)
          }
)

setMethod("getTrialsNames", "Trials",
          function(self, expID)
          {
            expIDs <- sapply(self@trialsList$trials, FUN = function(x) {x@expID})
            if (any(expIDs == expID)) 
            {
              names <- sapply(self@trialsList$trials[which(expIDs == expID)], FUN = function(x) {x@name})
              return(names)
            }
            else
            {
              return(NULL)
            }
          }
)


setMethod("getTrialIdByName", "Trials",
          function(self, expID, name)
          {
            if (length(self@trialsList) == 0) return(NULL)
            names <- sapply(self@trialsList$trials, FUN = function(x) {x@name})
            expIDs <- sapply(self@trialsList$trials, FUN = function(x) {x@expID})
            if (any(names == name & expIDs == expID))
            {
              id <- self@trialsList$ids[which(names == name & expIDs == expID)]
              return(id)
            }
            else
            {
              return(NULL)
            }
          }
)

setMethod("getTrialNameById", "Trials",
          function(self, expID, trialID)
          {
            expIDs <- sapply(self@trialsList$trials, FUN = function(x) {x@expID})
            if (any(expIDs == expID & self@trialsList$ids == trialID))
            {
              self@trialsList$trials[[which(expIDs == expID & self@trialsList$ids == trialID)]]@name
            }
            else return(NULL)
          }
)


setMethod("asDataFrame",  "Trials",                                   
          function(self)
          {
            if (length(self@trialsList$ids) == 0)
            {
              df <- data.frame(Id = numeric(), ExpID = numeric(), Name = character(), Description = character())
              return(df)
            }
            ids <- self@trialsList$ids
            expIds <- sapply(self@trialsList$trials, FUN = function(x) {x@expID})
            names <- sapply(self@trialsList$trials, FUN = function(x) {x@name})
            descriptions <- sapply(self@trialsList$trials, FUN = function(x) {x@description})
            df <- data.frame(Id = ids, ExpID = expIds, Name = names, Description = descriptions, stringsAsFactors = F)
            return(df)
          }
)

# Method adds a subject object into the Subjects list with ids and subjects sublists
## Method prevents duplicating subject codes
setMethod("addSubject",  "Subjects",                                   
          function(self, subjObject)
          {                         
            subjCnt <- length(self@subjectsList$ids)
            if (subjCnt == 0) 
            {
              self@subjectsList$ids <- 1
              self@subjectsList$subjects <- list(subjObject)
              return(self)
            }
            if (subjObject@code %in% getSubjectCodes(self))
            {
              stop(paste("The subject with code", subjObject@code, "already exists!"))
            }
            id <- tail(self@subjectsList$ids, n = 1) + 1
            self@subjectsList$ids <- c(self@subjectsList$ids, id)
            self@subjectsList$subjects <- c(self@subjectsList$subjects, subjObject)
            return(self)
          }
)

setMethod("updateSubjectById",  "Subjects",                                   
          function(self, id, subjObject)
          {
            self@subjectsList$subjects[[which(self@subjectsList$ids == id)]] <- subjObject
            return(self)
          }
)

setMethod("delSubjectsById",  "Subjects",                                   
          function(self, ids)
          {
            for (i in ids)
            {
              subjToDel <- which(self@subjectsList$ids == i)
              if (subjToDel != 0)
              {
                self@subjectsList$ids <- self@subjectsList$ids[-subjToDel]
                self@subjectsList$subjects <- self@subjectsList$subjects[-subjToDel]
              }
            }
            return(self)
          }
)

# Method returns a vector of subject codes
setMethod("getSubjectCodes", "Subjects",
          function(self)
          {
            sapply(self@subjectsList$subjects, function(x) {return(x@code)})
          }
)

setMethod("getSubjectIdByCode", "Subjects",
          function(self, code)
          {
            codes <- sapply(self@subjectsList$subjects, function(x) {return(x@code)})
            if (length(which(codes == code)) != 0)
            {
              return(self@subjectsList$ids[[which(codes == code)]])
            }
            else {return(NULL)}
          }
)

setMethod("getSubjectCodeById", "Subjects",
          function(self, id)
          {
            self@subjectsList$subjects[[which(self@subjectsList$ids == id)]]@code
          }
)

setMethod("asDataFrame",  "Subjects",                                   
          function(self)
          {
            if (length(self@subjectsList$ids) == 0)
            {
              df <- data.frame(Id = numeric(), Code = character(), Fullname = character(), Birthdate = character())
              return(df)
            }
            ids <- self@subjectsList$ids
            codes <- sapply(self@subjectsList$subjects, FUN = function(x) {x@code})
            fullnames <- sapply(self@subjectsList$subjects, FUN = function(x) {x@fullname})
            birthdates <- sapply(self@subjectsList$subjects, FUN = function(x) {x@birthdate})
            df <- data.frame(Id = ids, Code = codes, Fullname = fullnames, Birthdate = birthdates, stringsAsFactors = F)
            return(df)
          }
)

# Method adds a Stimulus object into the Stimuli list with ids and stimuli sublists
## TO DO: method should prevent duplicates in Stimuli
setMethod("addStimulus",  "Stimuli",                                   
          function(self, stimulusObject)
          {                         
            stimCnt <- length(self@stimuliList$ids)
            if (stimCnt == 0) 
            {
              self@stimuliList$ids <- 1
              self@stimuliList$stimuli <- list(stimulusObject)
              return(self)
            }
            id <- tail(self@stimuliList$ids, n = 1) + 1
            self@stimuliList$ids <- c(self@stimuliList$ids, id)
            self@stimuliList$stimuli <- c(self@stimuliList$stimuli, stimulusObject)
            return(self)
          }
)

# Method adds an AOI object into the AOISet object
## TO DO: method should prevent duplicates in AOISet
setMethod("addAOI",  "AOISet",                                   
          function(self, AOIObject)
          {                         
            if (length(self@AOIs) == 0)
            {
              self@AOIs <- AOIObject
            }
            self@AOIs <- c(self@AOIs, AOIObject)
            return(self)
          }
)

# Method adds an AOISet object with into the AOISet object
## TO DO: method should prevent duplicates in AOISet
setMethod("addAOISet",  "AOISets",                                   
          function(self, AOISetObject)
          {                         
            setsCnt <- length(self@AOISetsList$ids)
            if (setsCnt == 0) 
            {
              self@AOISetsList$ids <- 1
              self@AOISetsList$AOISets <- list(AOISetObject)
              return(self)
            }
            id <- tail(self@AOISetsList$ids, n = 1) + 1
            self@AOISetsList$ids <- c(self@AOISetsList$ids, id)
            self@AOISetsList$AOISets <- c(self@AOISetsList$AOISets, AOISetObject)
            return(self)
          }
)

# Method adds a dataRecord object into the DataSample object
setMethod("addDataRecord",  "DataSample",                                   
          function(self, dataRecord)
          {                         
            expID <- ifelse(length(dataRecord@expID) == 0, as.numeric(NA), dataRecord@expID)
            subjectID <- ifelse(length(dataRecord@subjectID) == 0, as.numeric(NA), dataRecord@subjectID)
            trialID <- ifelse(length(dataRecord@trialID) == 0, as.numeric(NA), dataRecord@trialID)
            complexKey <- list(expID = expID, subjectID = subjectID, trialID = trialID)
            recCnt <- length(self@ids)
            if (recCnt == 0)
            {
              self@keys <- data.frame(complexKey)
              self@ids <- 1
              self@DataRecordsList <- append(self@DataRecordsList, list(dataRecord))
              return(self)
            }
            id <- tail(self@ids, n = 1) + 1
            self@ids <- c(self@ids, id)
            self@keys <- rbind(self@keys, complexKey)
            self@DataRecordsList <- append(self@DataRecordsList, list(dataRecord))
            return(self)
          }
)

# Method adds a dataRecord object into the DataSample object
setMethod("updateDataRecord",  "DataSample",                                   
          function(self, id, dataRecord)
          {                         
            recNum <- which(self@ids == id)
            expID <- dataRecord@expID
            subjectID <- dataRecord@subjectID
            trialID <- dataRecord@trialID
            complexKey <- list(expID = expID, subjectID = subjectID, trialID = trialID)
            self@keys[recNum,] <- complexKey
            self@DataRecordsList[[recNum]] <- dataRecord
            return(self)
          }
)

setMethod("filterDataSample", "DataSample",
          function(self, expIDs, subjectIDs, trialFilters, factorFilters)
          {
            expFilter <- function(DataSample, expIDs)
            {
              recIds <- DataSample@ids[which(DataSample@keys$expID %in% expIDs)]
              return(recIds)
            }
            if (!is.na(expIDs))
            {
              ids1 <- expFilter(self, expIDs)
            }
            return(ids1)
          }
)

# Method adds a factor definition (object of the class Factor) into the availableFactors data frame
## Method prevents adding factors with duplicate names
setMethod("factorExists", "AvailableFactors",
          function(self, factor)
          {
            if (nrow(self@availableFactors) == 0) {return(list(exists = FALSE, id = NA))}
            varName <- factor@varName
            owner <- factor@owner
            
            identical(list("a", "b"),list("a", "b"))
            
            varNames <- self@availableFactors$varName[self@availableFactors$owner == owner]
            if (any(varNames == varName)) 
            {
              factorID <- self@availableFactors$id[which(varNames == varName)]
              return(list(exists = TRUE, id = factorID))
            }
            return(list(exists = FALSE, id = NA))
          }
)

setMethod("addFactorDefinition",  "AvailableFactors",                                   
          function(self, factor)
          {             
            if (factorExists(self, factor)$exists)
            {
              warning(paste("A factor with name", factor@varName, "already exists for owner", factor@owner))
              return(self)
            } else
            {
              varName <- factor@varName
              description <- factor@description
              type <- factor@type
              owner <- factor@owner
              if (length(factor@levels) == 0) {levels = NA} else {levels <- factor@levels}
              facCnt <- nrow(self@availableFactors)
              if (facCnt == 0)
              {
                self@availableFactors <- data.frame(id = 1, varName = varName, description = description, type = type, levels = I(list(levels)), owner = I(list(owner)), stringsAsFactors = F)
                # colnames(self@availableFactors) <- c("id", "varName", "description", "type", "levels", "owner")
              }
              else
              {
                newFactorDef <- list(id = self@availableFactors[facCnt,1]+1, varName = varName, description = description, type = type, levels = I(list(levels)), owner = I(list(owner)))
                self@availableFactors <- rbind(self@availableFactors, newFactorDef)
              }
            }
            return(self)
          }
)

setMethod("getFactorIDByName",  "AvailableFactors",                                   
          function(self, factorName)
          {           
            factorRecNum <- which(self@availableFactors$varName == factorName)
            return(self@availableFactors$id[factorRecNum])
          }
)

setMethod("getNameByFactorID",  "AvailableFactors",                                   
          function(self, factorID)
          {           
            factorRecNum <- which(self@availableFactors$id == factorID)
            return(self@availableFactors$varName[factorRecNum])
          }
)

setMethod("getTypeByFactorID",  "AvailableFactors",                                   
          function(self, factorID)
          {           
            factorRecNum <- which(self@availableFactors$id == factorID)
            return(self@availableFactors$type[factorRecNum])
          }
)

setMethod("getLevelsByFactorID",  "AvailableFactors",                                   
          function(self, factorID)
          {           
            factorRecNum <- which(self@availableFactors$id == factorID)
            return(self@availableFactors$levels[factorRecNum])
          }
)

## May be we need to drop factor's values after deletion (but in app's logic, not in methods)
setMethod("deleteFactorDefinition",  "AvailableFactors",                                   
          function(self, factorID)
          {             
            self@availableFactors <- self@availableFactors[-which(self@availableFactors$id == factorID)]
            return(self)
          }
)

## TO DO: 
## Method updates data of a factor defined in AvailableFactors object by factorID
## varName and description can be updated without any consistensy checking
## if owner changed then we need to drop ownerID data in FactorsData object in app's code
## type can be changed with corresponding transformations: 
## e.g. factor levels to factor level numbers if changing type from factor/ordFactor to int/numeric
## if changing type from int/num to factor then levels and order should be specified
## levels names can be changed without any checking
## if a level is deleted then drop corresponding values in FactorsData object
## if a level is added then nothing to check
setMethod("updateFactorDefinition",  "AvailableFactors",                                   
          function(self, factorID, factor, FactorsData)
          {             
            oldFactorDef <- self@availableFactors[which(self@availableFactors$id == factorID)]
            return(self)
          }
)

# Method adds the factor id and value into Factors list
## Method prevents adding values for factors which have already been set

setMethod("factorValueIsCorrect", "AvailableFactors",
          function(self, factorID, value)
          {
            if (is.na(value) | is.null(value))
            {
              return(T)
            }
            factorDefNum <- which(self@availableFactors$id == factorID)
            factorType <- self@availableFactors$type[factorDefNum]
            if (factorType %in% c("factor", "ordFactor"))
            {
              if (!(value %in% self@availableFactors$levels[[factorDefNum]]))
              {
                warning("Cannot add a value because it is not a level of the selected factor!")
                return(F)
              }
            }
            if (factorType %in% c("integer", "numeric"))
            {
              if (!(is.numeric(value) | is.integer(value)))
              {
                warning("Cannot add a value because it is not a number!")
                return(F)
              }
            }
            return(T)
          }
)

setMethod("addFactorValue",  "FactorsData",                                   
          function(self, availableFactors, owner, ownerID, factorID, eye, value, replace)
          {                         
            if (factorValueIsCorrect(self = availableFactors, factorID = factorID, value = value))
            {
              recEqual <- F
              if (length(self@factorsDataList) > 0)
              {
                recEqual <- (self@factorsDataList$factorID == factorID) & 
                  (sapply(self@factorsDataList$ownerID, FUN = identical, ownerID)) &
                  (sapply(self@factorsDataList$owner, FUN = identical, owner)) &
                  (self@factorsDataList$eye == eye)
                
              }
              if (any(recEqual))
              {
                if (replace)
                {
                  recNum <- which(recEqual)
                  self@factorsDataList$value[recNum] <- I(value)
                  self@factorsDataList$eye[recNum] <- eye
                  return(self)
                }
                else
                {
                  warning("The factor value has been specified already!")
                  return(self)
                }
              }
              else
              {
                factorRecord <- data.frame(factorID = factorID, eye = eye, value = I(list(value)), ownerID = I(list(ownerID)), owner = I(list(owner)))
                self@factorsDataList <- rbind(self@factorsDataList, factorRecord)
                return(self)
              }
            }
            else return(self)
})


setMethod("deleteFactorValue",  "FactorsData",                                   
          function(self, owner, ownerID, factorID, eye)
          {                        
            print(any(self@factorsDataList$factorID == factorID))
            print(any(sapply(self@factorsDataList$ownerID, FUN = identical, ownerID)))
            print(any(sapply(self@factorsDataList$owner, FUN = identical, owner)))
            print(any((self@factorsDataList$eye == eye)))
            recEqual <- (self@factorsDataList$factorID == factorID) & 
              (sapply(self@factorsDataList$ownerID, FUN = identical, ownerID)) &
              (sapply(self@factorsDataList$owner, FUN = identical, owner)) &
              (self@factorsDataList$eye == eye)
            if (any(recEqual))
            {
              self@factorsDataList <- self@factorsDataList[-which(recEqual),]
            }
            else 
            {
              print("There is no such value!")
            }
            return(self)
          }
         
)

setMethod("deleteAllFactorValuesForOwner",  "FactorsData",                                   
          function(self, owner)
          {                         
            recEqual <- sapply(self@factorsDataList$owner, FUN = identical, owner)
            if (any(recEqual))
            {
              self@factorsDataList <- self@factorsDataList[-which(recEqual),]
            }
            else 
            {
              print("There is no such owner!")
            }
            return(self)
          }
          
)

setMethod("deleteAllFactorValuesForOwnerID",  "FactorsData",                                   
          function(self, ownerID)
          {                         
            recEqual <- sapply(self@factorsDataList$ownerID, FUN = identical, ownerID)
            if (any(recEqual))
            {
              self@factorsDataList <- self@factorsDataList[-which(recEqual),]
            }
            else 
            {
              print("There is no such owner ID!")
            }
            return(self)
          }
          
)

setMethod("deleteAllFactorValuesForFactorID",  "FactorsData",                                   
          function(self, factorID)
          {                         
            recEqual <- (self@factorsDataList$factorID == factorID)
            
            if (any(recEqual))
            {
              self@factorsDataList <- self@factorsDataList[-which(recEqual),]
            }
            else 
            {
              print("There is no such factor ID!")
            }
            return(self)
          }
          
)


setMethod("asDataFrame",  "FactorsData",                                   
          function(self, owner)
          {
            owners <- sapply(self@factorsDataList, FUN = function(x) {x$owner})
            ownerFactorsData <- self@factorsDataList[which(owners == owner)]
            if (length(ownerFactorsData) == 0) {warning("Factor values are not set for this owner!"); return(NULL)}
            factorsNames <- sapply(ownerFactorsData, FUN = function(x) {names(x$values)})
            dim(factorsNames) <- prod(dim(factorsNames))
            uniqueFactorsNames <- unique(factorsNames)
            df <- lapply(uniqueFactorsNames, FUN = function(x)
            {
              res <- sapply(ownerFactorsData, FUN = function(y)
              {
                val <- y$values[[which(names(y$values) == x)]]
                val
              })
              names(res) <- NULL
              return(res)
            })
            ownerIDs <- sapply(ownerFactorsData, FUN = function(x) {x$ownerID})
            df <- append(list(ownerID = ownerIDs), df)
            names(df) <- c("ownerID", uniqueFactorsNames)
            return(data.frame(df, stringsAsFactors = F))
          }
)

setMethod("addLoader",  "Loaders",                                   
          function(self, loaderObject)
          {                         
            loadersCnt <- length(self@loadersList$ids)
            if (loadersCnt == 0) 
            {
              self@loadersList$ids <- 1
              self@loadersList$loaders <- list(loaderObject)
              return(self)
            }
            id <- self@loadersList$ids[[loadersCnt]] + 1
            self@loadersList$ids <- c(self@loadersList$ids, id)
            self@loadersList$loaders <- append(self@loadersList$loaders, list(loaderObject))
            return(self)
          }
)

setMethod("asDataFrame",  "Loaders",                                   
          function(self)
          {
            if (length(self@loadersList$ids) == 0)
            {
              df <- data.frame(id = NA, name = NA, stringsAsFactors = F)[-1,]
              names(df) <- c("Id", "Name")
              return(df)
            }
            id <- self@loadersList$ids
            name <- sapply(self@loadersList$loaders, FUN = function(x) {x@name})
            df <- data.frame(id, name, stringsAsFactors = F)
            names(df) <- c("Id", "Name")
            return(df)
          }
)

## TO DO: prevent creating duplicate records
setMethod("addRawDataRecord",  "RawDataRecords",                                   
          function(self, filepath, loader)
          { 
            fun <- loader@fun
            settings <- loader@settings
            rawDataRecCnt <- length(self@rawDataRecordsList)
            newRawDataRec <- fun(filepath, settings)
            
            if (rawDataRecCnt == 0) 
            {
              self@rawDataRecordsList$ids <- 1
              self@rawDataRecordsList$rawDataRecords <- list(newRawDataRec)
              return(self)
            }
            id <- tail(self@rawDataRecordsList$ids, n = 1) + 1
            self@rawDataRecordsList$ids <- c(self@rawDataRecordsList$ids, id)
            self@rawDataRecordsList$rawDataRecords <- append(self@rawDataRecordsList$rawDataRecords, list(newRawDataRec))
            return(self)
          }
)


setMethod("addRawDataRecords",  "RawDataRecords",                                   
          function(self, filesList, loader)
          { 
            fun <- loader@fun
            settings <- loader@settings
            newRawDataRecords <- lapply(filesList, FUN = fun, settings = settings)
            rawDataRecCnt <- length(self@rawDataRecordsList$ids)
            if (rawDataRecCnt == 0) 
            {
              firstId <- 1
            }
            else
            {
              firstId <- tail(self@rawDataRecordsList$ids, n = 1) + 1
            }
            recordsCnt <- length(newRawDataRecords)
            ids <- seq(firstId, length.out = recordsCnt)
            self@rawDataRecordsList$ids <- c(self@rawDataRecordsList$ids, ids)
            self@rawDataRecordsList$rawDataRecords <- append(self@rawDataRecordsList$rawDataRecords, newRawDataRecords)
            return(self)
          }
)

setMethod("delRawDataRecordsById",  "RawDataRecords",                                   
          function(self, ids)
          {
            for (i in ids)
            {
              recToDel <- which(self@rawDataRecordsList$ids == i)
              if (recToDel != 0)
              {
                self@rawDataRecordsList$ids <- self@rawDataRecordsList$ids[-recToDel]
                self@rawDataRecordsList$rawDataRecords <- self@rawDataRecordsList$rawDataRecords[-recToDel]
              }
            }
            return(self)
          }
)


# setMethod("asDataFrame",  "RawDataRecords",                                   
#           function(self)
#           {
#             if (length(self@rawDataRecordsList) == 0)
#             {
#               df <- data.frame(Id = numeric(), Filename = character(), stringsAsFactors = F)
#               names(df) <- c("Id", "FileNames")
#               return(df)
#             }
#             ids <- self@rawDataRecordsList$ids
#             fileNames <- sapply(self@rawDataRecordsList$rawDataRecords, FUN = function(x) {x@filePath})
#             df <- data.frame(Id = ids, Filename = fileNames, stringsAsFactors = F, row.names = ids)
#             names(df) <- c("Id", "Filename")
#             return(as.data.frame(df,  stringsAsFactors = F))
#           }
# )

setMethod("asDataFrame",  "DataSample",                                   
          function(self)
          {
            if (length(self@DataRecordsList) == 0)
            {
              df <- data.frame(numeric(),numeric(),numeric(),numeric())
              names(df) <- c("Id", "ExpID", "TrialID", "SubjectID")
              return(df)
            }
            ids <- self@ids
            expIds <- self@keys$expID
            trialIds <- self@keys$trialID
            subjectIds <- self@keys$subjectID
            df <- data.frame(ids, expIds, trialIds, subjectIds, row.names = ids)
            names(df) <- c("Id", "ExpID", "TrialID", "SubjectID")
            return(df)
          }
)

# method returns a list with EyesData objects and additional info:
# filePaths, subjectCodes, trials
setMethod("parseDataRecord",  "RawDataRecord",                                   
          function(self, parser)
          {
            fun <- parser@fun
            settings <- parser@settings
            res <- fun(self, settings)
            return(res)
          }
)

# TO DO: return additional fields also
setMethod("getDataFrame", "EyesData",
          function(self, eye)
          {
            data <- list()
            if (eye == "left")
            {
             if (self@conditions@conditions$eye == "left" | self@conditions@conditions$eye == "both")
             {
               if (length(self@time@time) != 0)
               {
                 data <- append(data, list(time = self@time@time))
               }
               if (length(self@frame@frame) != 0)
               {
                 data <- append(data, list(frame = self@frame@frame))
               }
               data <- append(data, list(porx = self@leftEyeSamples@eyeData$porx))
               data <- append(data, list(pory = self@leftEyeSamples@eyeData$pory))
               if (length(self@leftPupilSamples@pupilData$pupxsize) != 0)
               {
                 data <- append(data, list(pupxsize = self@leftPupilSamples@pupilData$pupxsize))
               }
               if (length(self@leftPupilSamples@pupilData$pupysize) != 0)
               {
                 data <- append(data, list(pupysize = self@leftPupilSamples@pupilData$pupysize))
               }
               if (length(self@leftFilterMarkers@filterMarkers) != 0)
               {
                 data <- append(data, list(filterMarkers = self@leftFilterMarkers@filterMarkers))
               }
               if (length(self@leftEventMarkers@eventMarkers) != 0)
               {
                 data <- append(data, list(eventMarkers = c(self@leftEventMarkers@eventMarkers)))
                 data <- append(data, list(eventGroups = c(self@leftEventMarkers@eventGroups)))
               }
               return(as.data.frame(data))
              }
              else
              {
                warning("Cannot return data due to its absence!")
                return(NULL)
              }
            }
            if (eye == "right")
            {
              if (self@conditions@conditions$eye == "right" | self@conditions@conditions$eye == "both")
              {
                if (length(self@time@time) != 0)
                {
                  data <- append(data, list(time = self@time@time))
                }
                if (length(self@frame@frame) != 0)
                {
                  data <- append(data, list(frame = self@frame@frame))
                }
                data <- append(data, list(porx = self@rightEyeSamples@eyeData$porx))
                data <- append(data, list(pory = self@rightEyeSamples@eyeData$pory))
                if (length(self@rightPupilSamples@pupilData$pupxsize) != 0)
                {
                  data <- append(data, list(pupxsize = self@rightPupilSamples@pupilData$pupxsize))
                }
                if (length(self@rightPupilSamples@pupilData$pupysize) != 0)
                {
                  data <- append(data, list(pupysize = self@rightPupilSamples@pupilData$pupysize))
                }
                if (length(self@rightFilterMarkers@filterMarkers) != 0)
                {
                  data <- append(data, list(filterMarkers = self@rightFilterMarkers@filterMarkers))
                }
                if (length(self@rightEventMarkers@eventMarkers) != 0)
                {
                  data <- append(data, list(eventMarkers = c(self@rightEventMarkers@eventMarkers, NA)))
                  data <- append(data, list(eventGroups = c(self@rightEventMarkers@eventGroups, NA)))
                }
                return(as.data.frame(data))
              }
              else
              {
                warning("Cannot return data due to its absence!")
                return(NULL)
              }
            }
}
)

## ??? TO DO: implement a method that adds a specific DataRecord object into a DataSample object 
setMethod("dataFilter", "DataRecord",
          function(self, filter)
          {
            fun <- filter@fun
            # markersDef <- filter@markersDefinition
            filterID <- filter@id
            settings <- append(filter@settings, list(filterID = filterID))#, markersDef = markersDef))
            res <- fun(self, settings)
            print("Filtered")
            return(res)
          }
)

setMethod("dataSmoother", "DataRecord",
          function(self, smoother)
          {
            fun <- smoother@fun
            settings <- smoother@settings
            res <- fun(self, settings)
            print("Smoothed")
            return(res)
          }
)

setMethod("eventDetector", "DataRecord",
          function(self, detector)
          {
            fun <- detector@fun
            detectorID <- detector@id
            settings <- append(detector@settings, list(detectorID = detectorID))#, markersDef = markersDef))
            res <- fun(self, settings)
            print("Detected")
            return(res)
          }
)

setMethod("detectEvents",  "DataRecord",                                   
          function(self, filter, smoother, detector)
          {
            self <- dataFilter(self, filter)
            self <- dataSmoother(self, smoother)
            self <- eventDetector(self, detector)
            return(self)
          }
)

setMethod("eventAnalyzer", "DataRecord",
          function(self, analyzer)
          {
            fun <- analyzer@fun
            settings <- analyzer@settings
            res <- fun(self, settings)
            print("Event has been analysed!")
            return(res)
          }
)


### PARAMETERS ESTIMATION ###
### estimator returns a list with labeled estimated parameters values
## for trajectory: total duration, center of mass, inner area of samples, ...
## pupil data: min, max of pupil size and its range, ...
## filter markers: outlies percentage, ...
## additional samples - external function
setMethod("estimateParams", "DataRecord",
          function(self, estimator)
          {
            fun <- estimator@fun
            settings <- estimator@settings
            res <- fun(self, settings)
            return(res)
          }
)

setMethod("addSubFunction", "SubFunctions",
          function(self, subFunction)
          {
            subFunctionsCnt <- length(self@subFunctionsList)
            if (subFunctionsCnt == 0)
            {
              self@subFunctionsList$ids <- 1
              self@subFunctionsList$subFunctions <- list(subFunction)
              return(self)
            }
            id <- tail(self@subFunctionsList$ids, n = 1) + 1
            self@subFunctionsList$ids <- c(self@subFunctionsList$ids, id)
            self@subFunctionsList$subFunctions <- append(self@subFunctionsList$subFunctions, list(subFunction))
            return(self)
          }
)

setMethod("getSubfunctions", "SubFunctions",
          function(self, operation = NA, applyTo = NA, event = NA)
          {
            if (is.na(event) & is.na(applyTo) & is.na(operation))
            {
              flags <- rep(T, length(self@subFunctionsList$subFunctions))
            }
            if (is.na(event) & is.na(applyTo) & !is.na(operation))
            {
              flags <- sapply(self@subFunctionsList$subFunctions, FUN = function(x) {x@operation == operation})
            }
            if (is.na(event) & !is.na(applyTo) & !is.na(operation))
            {
              flags <- sapply(self@subFunctionsList$subFunctions, FUN = function(x) {x@operation == operation & x@applyTo == applyTo})
            }
            if (!is.na(event) & !is.na(applyTo) & !is.na(operation))
            {
              flags <- sapply(self@subFunctionsList$subFunctions, FUN = function(x) {x@operation == operation & x@applyTo == applyTo & any(event == x@events)})
            }
            subFuns <- self@subFunctionsList$subFunctions[flags]
            ids <- self@subFunctionsList$ids[flags]
            sfList <- new(Class = "SubFunctions", subFunctionsList = list(ids = ids, subFunctions = subFuns))
            return(sfList)
          }
)


setMethod("asDataFrame", "SubFunctions",
          function(self)
          {
            if (length(self@subFunctionsList$ids) == 0)
            {
              df <- data.frame(Id = numeric(), Name = character(), Operation = character(), Description = character(), 
                               ApplyTo = character(), Events = character())
              return(df)
            }
            ids <- self@subFunctionsList$ids
            names <- sapply(self@subFunctionsList$subFunctions, FUN = function(x) {x@name})
            descriptions <- sapply(self@subFunctionsList$subFunctions, FUN = function(x) {x@description})
            operations <- sapply(self@subFunctionsList$subFunctions, FUN = function(x) {x@operation})
            applyTos <- sapply(self@subFunctionsList$subFunctions, FUN = function(x) {x@applyTo})
            events <- lapply(self@subFunctionsList$subFunctions, FUN = function(x) {x@events})
            df <- data.frame(Id = ids, Name = names, Operation = operations, Description = descriptions, 
                             ApplyTo = applyTos, Events = I(events), stringsAsFactors = F)
            return(df)
          }
)

### VISUALIZATIONS ###

setMethod("plotXY", "DataRecord",
          function(self, eye, filter, smoother, period, onStimulus)
          {
            
          }
)

setMethod("plotXt", "DataRecord",
          function(self, settings)
          {
            eyesData <- self@eyesDataObject
            conditions <- eyesData@conditions@conditions
            
            screenDist <- conditions$screenDist
            screenResolution <- conditions$screenResolution
            screenSize <- conditions$screenSize
            plotType <- settings$plotType
            eye <- settings$eye
            period <- settings$period
            markerType <- settings$markerType
            angular <- settings$angular
            pointsColor <- settings$pointsColor
            velType <- settings$velType
            fl <- settings$fl
            sampleRate <- conditions$sampleRate
            
            size <- length(t)
            if (is.na(sampleRate))
            {
              meandt <- mean(t[-1] - t[-size], na.rm = T)
              sampleRate <- 1/meandt
            }
            
            if (conditions$eye == "both")
            {
              dataLeft <- getDataFrame(eyesData, "left")
              dataRight <- getDataFrame(eyesData, "right")
              pointsToShowLeft <- which(dataLeft$time >= period[1] & dataLeft$time <= period[2])
              pointsToShowRight <- which(dataRight$time >= period[1] & dataRight$time <= period[2])
            }
            else
            {
              data <- getDataFrame(eyesData, conditions$eye)
              pointsToShow <- which(data$time >= period[1] & data$time <= period[2])
              markers <- switch(markerType,
                                "No markers" = pointsColor,
                                "Filter markers" = as.factor(data$filterMarkers[pointsToShow]),
                                "Event markers" = as.factor(data$eventMarkers[pointsToShow]))
              t <- data$time[pointsToShow]
              x <- data$porx[pointsToShow]
              y <- data$pory[pointsToShow]
              pupx <- data$pupxsize[pointsToShow]
              pupy <- data$pupysize[pointsToShow]
              if (plotType == "POR")
              {
                if (angular)
                {
                  angPos <- calcAngPos(x, y, screenDist, screenResolution, screenSize)
                  x <- angPos$xAng
                  y <- angPos$yAng
                }
                plot(x~t, col = markers)
                plotXt <- points(y~t, col = markers)
              }
              if (plotType == "Pupil")
              {
                if (conditions$pupilShape == "circle")
                {
                  plot(pupx~t, col = markers)
                } else
                if (conditions$pupilShape == "ellipse")
                {
                  plot(pupx~t, col = markers)
                  plotXt <- points(pupy~t, col = markers)
                }
              }
              if (plotType == "Velocity")
              {
                calcVelSettings <- list(velType = velType, angular = angular, screenDist = screenDist, 
                                        screenResolution = screenResolution, screenSize = screenSize,
                                        sampleRate = sampleRate, fl = fl)
                vel <- calcVel(t, x, y, calcVelSettings)$vels
                ## TO DO: need to use ggplot or segments() to create segmented line chart (color should depend on markers)
                plotXt <- plot(vel~t[1:length(vel)], col = markers[1:length(vel)], type = "l")
              }
              if (plotType == "Acceleration")
              {
                calcVelSettings <- list(velType = velType, angular = angular, screenDist = screenDist, 
                                        screenResolution = screenResolution, screenSize = screenSize,
                                        sampleRate = sampleRate, fl = fl)
                accel <- calcVel(t, x, y, calcVelSettings)$accels
                plotXt <- plot(accel~t[1:length(accel)], col = markers[1:length(accel)], type = "l")
              }
            }
            return(plotXt)
          }
)
