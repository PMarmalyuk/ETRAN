# Classes v 1.6
####################
#class_declarations#
####################
classesFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Git\\EyeTrackingProject\\Classes"
setwd(classesFolder)
source("optionsAndSettingsClasses.R")
source("baseEyeDataClasses.R")
source("baseClasses.R")
source("listsAndTablesClasses.R")
functionsFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Git\\EyeTrackingProject\\Functions"
setwd(functionsFolder)
source("dataLoaders.R")
source("dataParsers.R")
source("filters.R")
source("smoothers.R")
source("detectors.R")
source("eventAnalyzers.R")

##############################
#generic_methods_declarations#
##############################
setGeneric("addExperiment", function(self, expObject){standardGeneric("addExperiment")})

setGeneric("addTrial", function(self, trialObject){standardGeneric("addTrial")})

setGeneric("getSubjectCodes", function(self){standardGeneric("getSubjectCodes")})
setGeneric("addSubject", function(self, subjectObject){standardGeneric("addSubject")})


setGeneric("addFactorDefinition", function(self, factor){standardGeneric("addFactorDefinition")})

setGeneric("addStimulus", function(self, stimulusObject){standardGeneric("addStimulus")})

setGeneric("addAOI", function(self, AOIObject){standardGeneric("addAOI")})

setGeneric("addAOISet", function(self, AOISetObject, orderIndex){standardGeneric("addAOISet")})

setGeneric("addAOISets", function(self, AOISetsObject){standardGeneric("addAOISets")})

setGeneric("addFactor", function(self, factorID, value){standardGeneric("addFactor")})
setGeneric("updateFactor", function(self, factorID, value){standardGeneric("updateFactor")})

setGeneric("addFactorsRecord", function(self, owner, ownerID, factors){standardGeneric("addFactorsRecord")})
setGeneric("replaceFactorsRecord", function(self, owner, ownerID, factors){standardGeneric("replaceFactorsRecord")})
setGeneric("updateFactorsRecord", function(self, owner, ownerID, factorID, value){standardGeneric("updateFactorsRecord")})


setGeneric("addDataRecord", function(self, dataRecord){standardGeneric("addDataRecord")})
setGeneric("printDataSampleKeys", function(self){standardGeneric("printDataSampleKeys")})

setGeneric("addRawDataRecord", function(self, filepath, readSettings, useExt, extFun, extSettings){standardGeneric("addRawDataRecord")})
setGeneric("addRawDataRecords", function(self, filesFolder, readSettings, useExt, extFun, extSettings){standardGeneric("addRawDataRecords")})

## Pablo: I suggest the following interface: function(self, dataFields, headerKeys, parser)
## where parser is an object that contains parsing function and its settings as the data filter, smoother and detector
setGeneric("parseDataRecord", function(self, parser){standardGeneric("parseDataRecord")})

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



## visualisations
setGeneric("plotXY", function(self, eye, period, onStimulus, smoother){standardGeneric("plotXY")})
setGeneric("plotXt", function(self, eye, period, channel, angular, smoother){standardGeneric("plotXt")})

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
            self@expList$experiments <- c(self@expList$experiments, expObject)
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
                self@eyesDataObjectsList <- append(self@eyesDataObjectsList, dataRecord@eyesDataObject)
                self@analysisResultsList <- append(self@analysisResultsList, dataRecord@analysisResults)
                self@statisticsList <- append(self@statisticsList, dataRecord@statistics)
                return(self)
              }
              self@keys <- rbind(self@keys, complexKey)
              self@eyesDataObjectsList <- append(self@eyesDataObjectsList, dataRecord@eyesDataObject)
              self@analysisResultsList <- append(self@analysisResultsList, dataRecord@analysisResults)
              self@statisticsList <- append(self@statisticsList, dataRecord@statistics)
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

# Method adds the factor id and value into Factors list
## Method prevents adding values for factors which have already been set
setMethod("addFactor",  "Factors",                                   
          function(self, factorID, value)
          {                         
            factorPosition <- which(self@factorsList$ids == factorID)
            if (factorPosition != 0)
            {
              stop(paste("Factor with ID", factorID, "has been set already! Its value is", self@factorsList$values[[factorPosition]]))
            }
            self@factorsList$ids <- c(self@factorsList$ids, factorID)
            self@factorsList$values <- c(self@factorsList$values, value)
            return(self)
          }
)

# Method updates factor's value by factorID if it exists in Factors object
setMethod("updateFactor",  "Factors",                                   
          function(self, factorID, value)
          {                         
            factorPosition <- which(self@factorsList$ids == factorID)
            if (factorPosition == 0)
            {
              stop(paste("Factor with ID", factorID, "not found!"))
            }
            self@factorsList$values[[factorPosition]] <- value
            return(self)
          }
)

# Method adds the factors object for given owner into Factors Data object
## Method prevents adding duplicate records
setMethod("addFactorsRecord",  "FactorsData",                                   
          function(self, owner, ownerID, factors)
          {
            if (any(self@owners == owner & self@ownersIDs == ownerID))
            {
              stop(paste("Factors values record for object", owner, "with ID", ownerID, "already exists! You can add factor value into record or replace a record."))
            }
            self@owner == owner
            self@ownerID = ownerID
            self@factors = factors
            return(self)
          }
)

# Method replaces a factors record in FactorsData
## Method checks if a record for specific owner and ownerID exists
## TO DO: test method, add method's documentation
setMethod("replaceFactorsRecord",  "FactorsData",                                   
          function(self, owner, ownerID, factors)
          {
            factorRecordPosition <- which(self@owners == owner & self@ownersIDs == ownerID)
            if (factorRecordPosition == 0)
            {
              stop(paste("There is no factors values record for object", owner, "with ID", ownerID, "You can add a new factor record."))
            }
            self@factors[[factorRecordPosition]] <- factors
            return(self)
          }
)

# Method updates a value of the specific factor's in existing record
## Method checks if a record for specific owner and ownerID exists
setMethod("updateFactorsRecord",  "FactorsData",                                   
          function(self, owner, ownerID, factorID, value)
          {
            factorRecordPosition <- which(self@owners == owner & self@ownersIDs == ownerID)
            if (factorRecordPosition == 0)
            {
              stop(paste("There is no factors record for object", owner, "with ID", ownerID, "You can add a new factor record."))
            }
            self@factors[[factorRecordPosition]] <- updateFactor(self = self@factors[[factorRecordPosition]], factorID = factorID, value = value)
            return(self)
          }
)

## TO DO: prevent creating duplicate records
setMethod("addRawDataRecord",  "RawDataRecords",                                   
          function(self, filepath, readSettings, useExt, extFun, extSettings)
          { 
                newRawDataRec <- createRawDataRec(filePath = filepath, readSettings = readSettings, useExt = useExt, extFun = extFun, extSettings = extSettings)
                rawDataRecCnt <- length(self@rawDataRecordsList$fileNumbers)
                if (rawDataRecCnt == 0) 
                {
                  self@rawDataRecordsList$fileNumbers <- 1
                  self@rawDataRecordsList$rawDataRecords <- list(newRawDataRec)
                  return(self)
                }
                fileNum <- tail(self@rawDataRecordsList$fileNumbers, n = 1) + 1
                self@rawDataRecordsList$fileNumbers <- c(self@rawDataRecordsList$fileNumbers, fileNum)
                self@rawDataRecordsList$rawDataRecords <- c(self@rawDataRecordsList$rawDataRecords, newRawDataRec)
                return(self)
          }
)


## TO DO: prevent creating duplicate records
setMethod("addRawDataRecords",  "RawDataRecords",                                   
          function(self, filesFolder, readSettings, useExt, extFun, extSettings)
          { 
            if (!file.exists(filesFolder))
            {
              stop("Data folder not found!")
            }
            filesToRead <- list.files(path = filesFolder, pattern = NULL, all.files = FALSE,
                       full.names = TRUE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
            rawDataRecords <- lapply(filesToRead, FUN = createRawDataRec, readSettings = readSettings, useExt = useExt, extFun = extFun, extSettings = extSettings)
            rawDataRecCnt <- length(self@rawDataRecordsList$fileNumbers)
            if (rawDataRecCnt == 0) 
            {
              firstFileNum <- 1
            }
            else
            {
              firstFileNum <- tail(self@rawDataRecordsList$fileNumbers, n = 1) + 1
            }
            filesCnt <- length(rawDataRecords)
            fileNumbers <- seq(firstFileNum, length.out = filesCnt)
            self@rawDataRecordsList$fileNumbers <- c(self@rawDataRecordsList$fileNumbers, fileNumbers)
            self@rawDataRecordsList$rawDataRecords <- c(self@rawDataRecordsList$rawDataRecords, rawDataRecords)
            return(self)
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
                 data <- append(data, list(eventGroups = c(self@leftEventMarkers@eventGroups, "0")))
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

### VISUALIZATIONS ###

setMethod("plotXY", "DataRecord",
          function(self, eye, period, onStimulus, smoother = NA)
          {
            
          }
)

setMethod("plotXt", "DataRecord",
          function(self, eye, period, channel, angular)
          {
            # channel - 1, 2, 3, 4, 5, 6, 7, ...
            # angular is possible for 1 and 2 channels (i.e. X(t) or Y(t) plot)
            # period is period of time in sec from trial's start
            
          }
)
