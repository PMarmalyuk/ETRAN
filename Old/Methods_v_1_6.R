# Classes v 1.6
####################
#class_declarations#
####################
classesFolder <- "F:\\Институт\\Проекты\\EyeTrackingPackage\\Classes"
setwd(classesFolder)
source("extFunctionsClasses.R")
source("optionsAndSettingsClasses.R")
source("baseEyeDataClasses.R")
source("baseClasses.R")
source("listsAndTablesClasses.R")

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

setGeneric("addAOIMultiSet", function(self, AOIMultiSetObject){standardGeneric("addAOIMultiSet")})


setGeneric("addFactorsRecord", function(self, owner, ownerId, factors){standardGeneric("addFactorsRecord")})
setGeneric("addFactor", function(self, factorId, value){standardGeneric("addFactor")})

setGeneric("addDataRecord", function(self, dataRecord){standardGeneric("addDataRecord")})
setGeneric("printDataSampleKeys", function(self){standardGeneric("printDataSampleKeys")})

setGeneric("addRawDataRecord", function(self, filepath, readSettings, useExt, extFun){standardGeneric("addRawDataRecord")})
setGeneric("addRawDataRecords", function(self, filesFolder, readSettings, useExt, extFun){standardGeneric("addRawDataRecords")})

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
            self@trialsList$ids <- c(self@trialsList$ids, id)
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
            name <- factor@name
            type <- factor@type
            if (length(factor@levels) == 0) {levels = NA} else {levels <- factor@levels}
            facCnt <- nrow(self@availableFactors)
            if (facCnt == 0)
            {
              self@availableFactors <- data.frame(id = 1, name = name, type = type, levels = I(list(levels)), stringsAsFactors = F)
              colnames(self@availableFactors) <- c("id", "name", "type", "levels")
              return(self)
            }
            if (any(self@availableFactors$name == name))
            {
              warning(paste("A factor with name", name, "already exists"))
            } else
            {
              newFactorDef <- list(id = self@availableFactors[facCnt,1]+1, name = name, type = type, levels = I(list(levels)))
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

# Method adds an AOISet object with orderIndex property into the AOIMultiSet object
## TO DO: method should prevent duplicates in AOIMultiSet
setMethod("addAOISet",  "AOIMultiSet",                                   
          function(self, AOISetObject, orderIndex)
          {                         
            if (orderIndex %in% self@orderIndices)
            {
              stop(paste("The AOI Set with order index", orderIndex, "already exists!"))
            }
            self@orderIndices <- c(self@orderIndices, orderIndex)
            self@AOISets <- c(self@AOISets, AOISetObject)
            return(self)
          }
)

# Method adds an AOIMultiSet object into the AOIMultiSets list with ids and multiSets sublists
## Method doesn't prevent duplicates in AOIMultiSets because their presence is not critical
setMethod("addAOIMultiSet",  "AOIMultiSets",                                   
          function(self, AOIMultiSetObject)
          {                         
            mSetsCnt <- length(self@AOIMultiSetsList$ids)
            if (mSetsCnt == 0) 
            {
              self@AOIMultiSetsList$ids <- 1
              self@AOIMultiSetsList$multiSets <- list(AOIMultiSetObject)
              return(self)
            }
            id <- tail(self@AOIMultiSetsList$ids, n = 1) + 1
            self@AOIMultiSetsList$ids <- c(self@AOIMultiSetsList$ids, id)
            self@AOIMultiSetsList$multiSets <- c(self@AOIMultiSetsList$multiSets, AOIMultiSetObject)
            return(self)
          }
)

# Method adds a dataRecord object into the DataSample object
## Method prevents duplicates in DataSample (by a composite key: expID, subjectID, trialID and class)
setMethod("addDataRecord",  "DataSample",                                   
          function(self, dataRecord)
          {                         
            expID <- dataRecord@expID
            subjectID <- dataRecord@subjectID
            trialID <- dataRecord@trialID
            class <- dataRecord@objectClass
            complexKey <- list(expID = expID, subjectID = subjectID, trialID = trialID, class = class)
            if (tail(duplicated(rbind(self@keys,complexKey)),1))
            {
              warning("Such a record already exists!")
              return(self)
            } else
            {
              if (nrow(self@keys) == 0)
              {
                self@keys <- data.frame(complexKey)
                self@objectsList <- append(self@objectsList, dataRecord@object[[1]])
                self@paramsList <- append(self@paramsList, dataRecord@parameters)
                return(self)
              }
              self@keys <- rbind(self@keys, complexKey)
              self@objectsList <- append(self@objectsList, dataRecord@object[[1]])
              self@paramsList <- append(self@paramsList, dataRecord@parameters)
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

## TO DO: Method adds the factor id and value into Factors list
setMethod("addFactor",  "Factors",                                   
          function(self, factorId, value)
          {                         
          }
)
## TO DO: Method adds the factors object for given owner into Factors Data object
setMethod("addFactorsRecord",  "FactorsData",                                   
          function(self, owner, ownerId, factors)
          {                         
          }
)

createRawDataRec <- function(filePath, readSettings, useExt, extFun)
{
  if (!file.exists(filePath))
  {
    stop("Datafile not found!")
  }
  else
  {
    if (useExt)
    {
      # implement data loading using extFun
      headerLines <- "NA"
      asIsData <- as.data.frame(NA)
      rawDataRecord <- new(Class = "RawDataRecord",
                           filePath = filePath,
                           headerLines = headerLines,
                           data = asIsData)
    }
    else
    {
      settings <- readSettings@readSettings
      headerLines <- readLines(con = filePath, n = settings$skip, encoding = settings$encoding)
      asIsData <- read.csv(filePath, sep = settings$sep,   
                           skip = settings$skip, 
                           comment.char = settings$comment.char, 
                           header = settings$header,
                           blank.lines.skip = T, 
                           check.names = F,
                           stringsAsFactors = F)
      rawDataRecord <- new(Class = "RawDataRecord",
                           filePath = filePath,
                           headerLines = headerLines,
                           data = asIsData)
    }
  }
  rawDataRecord
}

## TO DO: prevent creating duplicate records
setMethod("addRawDataRecord",  "RawDataRecords",                                   
          function(self, filepath, readSettings, useExt, extFun)
          { 
                newRawDataRec <- createRawDataRec(filePath = filepath, readSettings = readSettings, useExt = useExt, extFun = extFun)
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
          function(self, filesFolder, readSettings, useExt, extFun)
          { 
            if (!file.exists(filesFolder))
            {
              stop("Data folder not found!")
            }
            filesToRead <- list.files(path = filesFolder, pattern = NULL, all.files = FALSE,
                       full.names = TRUE, recursive = FALSE,
                       ignore.case = FALSE, include.dirs = FALSE, no.. = FALSE)
            rawDataRecords <- lapply(filesToRead, FUN = createRawDataRec, readSettings = readSettings, useExt = useExt, extFun = extFun)
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