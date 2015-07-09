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
setGeneric("getConditionsById", function(self, id){standardGeneric("getConditionsById")})
setGeneric("getExperimentsNames", function(self){standardGeneric("getExperimentsNames")})
setGeneric("getExperimentIdByName", function(self, name){standardGeneric("getExperimentIdByName")})


setGeneric("addTrial", function(self, trialObject){standardGeneric("addTrial")})

setGeneric("getSubjectCodes", function(self){standardGeneric("getSubjectCodes")})
setGeneric("addSubject", function(self, subjectObject){standardGeneric("addSubject")})

setGeneric("addStimulus", function(self, stimulusObject){standardGeneric("addStimulus")})

setGeneric("addAOI", function(self, AOIObject){standardGeneric("addAOI")})

setGeneric("addAOISet", function(self, AOISetObject, orderIndex){standardGeneric("addAOISet")})

setGeneric("addAOISets", function(self, AOISetsObject){standardGeneric("addAOISets")})

setGeneric("addFactorDefinition", function(self, factor){standardGeneric("addFactorDefinition")})
setGeneric("deleteFactorDefinition", function(self, factorID){standardGeneric("deleteFactorDefinition")})
setGeneric("updateFactorDefinition", function(self, factorID, factor, FactorsData){standardGeneric("updateFactorDefinition")})
setGeneric("getFactorIDByName", function(self, factorName){standardGeneric("getFactorIDByName")})
setGeneric("getNameByFactorID", function(self, factorID){standardGeneric("getNameByFactorID")})
setGeneric("getTypeByFactorID", function(self, factorID){standardGeneric("getTypeByFactorID")})
setGeneric("getLevelsByFactorID", function(self, factorID){standardGeneric("getLevelsByFactorID")})

setGeneric("addFactorValue", function(self, availableFactors, owner, ownerID, factorID, value, replace){standardGeneric("addFactorValue")})
setGeneric("asDataFrame", function(self, owner, availableFactors){standardGeneric("asDataFrame")})



setGeneric("addDataRecord", function(self, dataRecord){standardGeneric("addDataRecord")})
setGeneric("printDataSampleKeys", function(self){standardGeneric("printDataSampleKeys")})


## data loaders

setGeneric("addLoader", function(self, loaderObject){standardGeneric("addLoader")})
setGeneric("addRawDataRecord", function(self, filepath, loader){standardGeneric("addRawDataRecord")})

setGeneric("addRawDataRecords", function(self, filesList, loader){standardGeneric("addRawDataRecords")})

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

## Cluster analysis of DataRecords
setGeneric("findClusters", function(self, clusterAnalyzer){standardGeneric("findClusters")})

## visualisations
setGeneric("plotXY", function(self, eye, filter, smoother, period, onStimulus){standardGeneric("plotXY")})
setGeneric("plotXt", function(self, eye,  filter, smoother, period, channel, angular){standardGeneric("plotXt")})

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
            id <- self@expList$ids[[which(expNames == name)]]
            return(id)
          }
)

setMethod("asDataFrame",  "Experiments",                                   
          function(self)
          {
            if (length(self@expList$ids) == 0)
            {
              df <- data.frame(ids = NA, names = NA, expDates = NA, 
                               descriptions = NA, experimenters = NA, stringsAsFactors = F)[-1,]
              colnames(df) <- c("Id", "Name", "Date", "Description", "Experimenters")
              return(df)
            }
            ids <- self@expList$ids
            names <- sapply(self@expList$experiments, FUN = function(x) {x@name})
            expDates <- sapply(self@expList$experiments, FUN = function(x) {x@expDate})
            descriptions <- sapply(self@expList$experiments, FUN = function(x) {x@description})
            experimenters <- sapply(self@expList$experiments, FUN = function(x) {x@experimenters})
            df <- data.frame(ids, names, expDates, descriptions, experimenters, stringsAsFactors = F)
            colnames(df) <- c("Id", "Name", "Date", "Description", "Experimenters")
            return(df)
          }
)

setMethod("getConditionsById",  "Experiments",                                   
          function(self, id)
          {
            if (length(self@expList$experiments) != 0)
            {
              expObj <- self@expList$experiments[[which(self@expList$ids == id)]]
              cond <- expObj@conditions
            }
            else
            {
              cond <- new(Class = "Conditions")
            }
            return(cond)
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
## Method doesn't increment ids because trials ids are obtained from datafiles
## (?) TO DO: method should check for duplicate trials
setMethod("addTrial",  "Trials",                                   
          function(self, trialObject)
          {                         
            id <- trialObject@id
            expID <- trialObject@expID
            self@trialsList$ids <- c(self@trialsList$ids, id)
            self@trialsList$expIDs <- c(self@trialsList$expIDs, expID)
            self@trialsList$trials <- c(self@trialsList$trials, trialObject)
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

# Method adds a subject object into the Subjects list with ids and subjects sublists
## Method prevents duplicating subject codes
setMethod("addSubject",  "Subjects",                                   
          function(self, subjectObject)
          {                         
            subjCnt <- length(self@subjectsList$ids)
            if (subjCnt == 0) 
            {
              self@subjectsList$ids <- 1
              self@subjectsList$subjects <- list(subjectObject)
              return(self)
            }
            if (subjectObject@code %in% getSubjectCodes(self))
            {
              stop(paste("The subject with code", subjectObject@code, "already exists!"))
            }
            id <- tail(self@subjectsList$ids, n = 1) + 1
            self@subjectsList$ids <- c(self@subjectsList$ids, id)
            self@subjectsList$subjects <- c(self@subjectsList$subjects, subjectObject)
            return(self)
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
## Method prevents duplicates in DataSample (by a composite key: expID, subjectID, trialID)
setMethod("addDataRecord",  "DataSample",                                   
          function(self, dataRecord)
          {                         
            expID <- dataRecord@expID
            subjectID <- dataRecord@subjectID
            trialID <- dataRecord@trialID
            complexKey <- list(expID = expID, subjectID = subjectID, trialID = trialID)
            if (tail(duplicated(rbind(self@keys,complexKey)),1))
            {
              warning("Such a record already exists!")
              return(self)
            } else
            {
              if (nrow(self@keys) == 0)
              {
                self@keys <- data.frame(complexKey)
                self@eyesDataObjectsList <- append(self@eyesDataObjectsList, list(dataRecord@eyesDataObject))
                self@analysisResultsList <- append(self@analysisResultsList, list(dataRecord@analysisResults))
                self@statisticsList <- append(self@statisticsList, list(dataRecord@statistics))
                return(self)
              }
              self@keys <- rbind(self@keys, complexKey)
              self@eyesDataObjectsList <- append(self@eyesDataObjectsList, list(dataRecord@eyesDataObject))
              self@analysisResultsList <- append(self@analysisResultsList, list(dataRecord@analysisResults))
              self@statisticsList <- append(self@statisticsList, list(dataRecord@statistics))
              return(self)
            }
          }
)

# Method returns a data frame with data records composite keys values (for further use for data filtering)
setMethod("printDataSampleKeys", "DataSample",                                   
          function(self)
          {                         
            return(self@keys)
          }
)

# Method adds a factor definition (object of the class Factor) into the availableFactors data frame
## Method prevents adding factors with duplicate names
setMethod("addFactorDefinition",  "AvailableFactors",                                   
          function(self, factor)
          {             
            varName <- factor@varName
            description <- factor@description
            type <- factor@type
            owner <- factor@owner
            if (length(factor@levels) == 0) {levels = NA} else {levels <- factor@levels}
            facCnt <- nrow(self@availableFactors)
            if (facCnt == 0)
            {
              self@availableFactors <- data.frame(id = 1, varName = varName, description = description, type = type, levels = I(list(levels)), owner = owner, stringsAsFactors = F)
              colnames(self@availableFactors) <- c("id", "varName", "description", "type", "levels", "owner")
              return(self)
            }
            if (any(self@availableFactors$varName == varName) & any(self@availableFactors$owner == owner))
            {
              warning(paste("A factor with name", varName, "already exists for object class", owner))
            } else
            {
              newFactorDef <- list(id = self@availableFactors[facCnt,1]+1, varName = varName, description = description, type = type, levels = I(list(levels)), owner = owner)
              self@availableFactors <- rbind(self@availableFactors, newFactorDef)
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

## TO DO: drop factor's values after deletion
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
## if owner changed then drop ownerID data in FactorsData object
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
setMethod("addFactorValue",  "FactorsData",                                   
          function(self, availableFactors, owner, ownerID, factorID, value, replace)
          {                         
            factorRecordNum <- which(availableFactors@availableFactors$id == factorID)
            factorType <- availableFactors@availableFactors$type[factorRecordNum]
            if (factorType %in% c("factor", "ordFactor"))
            {
              if (!(value %in% availableFactors@availableFactors$levels[[factorRecordNum]]))
              {
                warning("Cannot add a value because it is not a level of the selected factor!")
                return(self)
              }
            }
            if (factorType %in% c("integer", "numeric"))
            {
              if (! (is.numeric(value) | is.integer(value)))
              {
                warning("Cannot add a value because it is not a number!")
                return(self)
              } 
              else
              {
                if (factorType == "integer") {value <- round(value)}
              }
            }
            factorRecord <- data.frame(ownerID = ownerID, factorID = factorID, value = as.character(value), stringsAsFactors = F)
            if (owner == "Subject")
            {
              if (nrow(self@subjectsFactors) == 0)
              {
                self@subjectsFactors <- rbind(self@subjectsFactors, factorRecord)
                return(self)
              }
              else
              {
                if (any(self@subjectsFactors$factorID == factorID & self@subjectsFactors$ownerID == ownerID))
                {
                  if (replace)
                  {
                    recNum <- which(self@subjectsFactors$factorID == factorID & self@subjectsFactors$ownerID == ownerID)
                    if (recNum == 5) {print(factorRecord[1,])}
                    self@subjectsFactors[recNum,] <- factorRecord[1,]
                    if (recNum == 5) {print(self@subjectsFactors[recNum,])}
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
                  self@subjectsFactors <- rbind(self@subjectsFactors, factorRecord)
                  return(self)
                }
              }
            }
            
            if (owner == "Stimulus")
            {
              if (nrow(self@stimuliFactors) == 0)
              {
                self@stimuliFactors <- rbind(self@stimuliFactors, factorRecord)
                return(self)
              }
              else
              {
                if (any(self@stimuliFactors$factorID == factorID & self@stimuliFactors$ownerID == ownerID))
                {
                  if (replace)
                  {
                    recNum <- which(self@stimuliFactors$factorID == factorID & self@stimuliFactors$ownerID == ownerID)
                    self@stimuliFactors[recNum,] <- factorRecord[1,]
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
                  self@stimuliFactors <- rbind(self@stimuliFactors, factorRecord)
                  return(self)
                }
              }
            }
            
            if (owner == "Trial")
            {
              if (nrow(self@trialsFactors) == 0)
              {
                self@trialsFactors <- rbind(self@trialsFactors, factorRecord)
                return(self)
              }
              else
              {
                if (any(self@trialsFactors$factorID == factorID & self@trialsFactors$ownerID == ownerID))
                {
                  if (replace)
                  {
                    recNum <- which(self@trialsFactors$factorID == factorID & self@trialsFactors$ownerID == ownerID)
                    self@trialsFactors[recNum,] <- factorRecord[1,]
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
                  self@trialsFactors <- rbind(self@trialsFactors, factorRecord)
                  return(self)
                }
              }
            }
          }
)

setMethod("asDataFrame",  "FactorsData",                                   
          function(self, owner, availableFactors)
          {
            factorsIDs <- unique(availFactors@availableFactors$id[availableFactors@availableFactors$owner == owner])
            if (length(factorsIDs) == 0) {warning("Factors were not specified for this owner!"); return(NULL)}
            
            if (owner == "Subject") {ownersFactors <- factorsData@subjectsFactors}
            if (owner == "Stimulus") {ownersFactors <- factorsData@stimuliFactors}
            if (owner == "Trial") {ownersFactors <- factorsData@trialsFactors}

            if (nrow(ownersFactors) == 0) {warning("Factor values are not specified for this owner!"); return(NULL)}
            
            ownersIDs <- as.integer(unique(ownersFactors$ownerID))
            varCnt <- length(factorsIDs)
            factorNames <- sapply(factorsIDs, function(x) {getNameByFactorID(availableFactors,x)})
            df <- list()
            for (ownerID in ownersIDs)
            {
              ownerFactorsIDs <- ownersFactors$factorID[ownersFactors$ownerID == ownerID]
              varPositions <- sapply(ownerFactorsIDs, function(x) which(factorsIDs == x))
              values <- ownersFactors$value[which(ownersFactors$ownerID == ownerID)]
              df <- rbind(df, c(rep(NA, varCnt)))
              df[nrow(df), varPositions] <- values
            }
            df <- as.data.frame(df)
            colnames(df) <- factorNames
            factorTypes <- sapply(factorsIDs, function(x) {getTypeByFactorID(availableFactors,x)})
            factorLevels <- sapply(factorsIDs, function(x) {getLevelsByFactorID(availableFactors,x)})
            for (i in 1:length(df))
            {
              if (factorTypes[i] == "integer") {df[,i] <- as.integer(df[,i])}
              if (factorTypes[i] == "numeric") {df[,i] <- as.numeric(df[,i])}
              if (factorTypes[i] == "factor") {df[,i] <- factor(df[,i], levels = factorLevels[[i]])}
              if (factorTypes[i] == "ordFactor") {df[,i] <- ordered(df[,i], levels = factorLevels[[i]])}
            }
            df <- cbind(ownersIDs, df)
            return(df)
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
              self@rawDataRecordsList$fileNumbers <- 1
              self@rawDataRecordsList <- list(newRawDataRec)
              return(self)
            }
            self@rawDataRecordsList <- append(self@rawDataRecordsList, list(newRawDataRec))
            return(self)
          }
)


setMethod("addRawDataRecords",  "RawDataRecords",                                   
          function(self, filesList, loader)
          { 
            fun <- loader@fun
            settings <- loader@settings
            newRawDataRecords <- lapply(filesList, FUN = fun, settings = settings)
            self@rawDataRecordsList <- append(self@rawDataRecordsList, newRawDataRecords)
            return(self)
          }
)

setMethod("asDataFrame",  "RawDataRecords",                                   
          function(self)
          {
            if (length(self@rawDataRecordsList) == 0)
            {
              df <- data.frame(FileNames = character(), stringsAsFactors = F)
              names(df) <- c("FileNames")
              return(df)
            }
            fileNames <- sapply(self@rawDataRecordsList, FUN = function(x) {x@filePath})
            df <- data.frame(FileNames = fileNames, stringsAsFactors = F)
            names(df) <- c("FileNames")
            return(as.data.frame(df))
          }
)

# method returns a list with EyesData objects and additional info:
# filePath, subjectCode, trialsNums, stimDim, framesCnt
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
                 data <- append(data, list(eventMarkers = c(self@leftEventMarkers@eventMarkers, "NULL")))
                 data <- append(data, list(eventGroups = c(self@leftEventMarkers@eventGroups, 0)))
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
                  data <- append(data, list(eventMarkers = c(self@rightEventMarkers@eventMarkers, "NULL")))
                  data <- append(data, list(eventGroups = c(self@rightEventMarkers@eventGroups, 0)))
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

## TO DO: implement a method that adds a specific DataRecord object into a DataSample object 
setMethod("dataFilter", "DataRecord",
          function(self, filter)
          {
            fun <- filter@fun
            settings <- filter@settings
            res <- fun(self, settings)
            return(res)
          }
)

setMethod("dataSmoother", "DataRecord",
          function(self, smoother)
          {
            fun <- smoother@fun
            settings <- smoother@settings
            res <- fun(self, settings)
            return(res)
          }
)

setMethod("eventDetector", "DataRecord",
          function(self, detector)
          {
            fun <- detector@fun
            settings <- detector@settings
            res <- fun(self, settings)
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




### VISUALIZATIONS ###

setMethod("plotXY", "DataRecord",
          function(self, eye, filter, smoother, period, onStimulus)
          {
            
          }
)

setMethod("plotXt", "DataRecord",
          function(self, eye, filter, smoother, period, channel, angular)
          {
            # channel - 1, 2, 3, 4, 5, 6, 7, ...
            # angular is possible for 1 and 2 channels (i.e. X(t) or Y(t) plot)
            # period is period of time in sec from trial's start
            
          }
)
