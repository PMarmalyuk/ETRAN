# createAnalyzer function creates and returns an object of EventAnalyzer class
# EventAnalyzer object is to be used by eventAnalyzer method
createAnalyzer <- function(name, fun, settings)
{
  analyzer <- new(Class = "EventAnalyzer", name = name, fun = fun, settings = settings)
  return(analyzer)
}

# getEventMarkersAndData function just reads and returns eventMarkers, data samples and factorsData from DataRecord as a list
# returned list of eventMarkers and data for specified eye is to be used by generalEventAnalyzer/Estimator function
getEventMarkersAndData <- function(dataRecord, eye, getFactorsData = F)
{
  if (eye == "left") 
  {
    res <- list(eventMarkers = dataRecord@eyesDataObject@leftEventsMarkers,
                eyeDataFrame = getEyeDataFrame(self = dataRecord, eye = eye))
  }
  if (eye == "right") 
  {
    res <- list(eventMarkers = dataRecord@eyesDataObject@rightEventsMarkers,
                eyeDataFrame = getEyeDataFrame(self = dataRecord, eye = eye))
  }
  if (getFactorsData) 
  {
    factorsData <- getFactorsDataByEye(dataRecord@analysisResults$eventFactorsData, eye = eye)
    res <- append(res, list(factorsData = factorsData))
  }
  return(res)
}

# createFactorFromReturnedValue function creates and returns an object of Factor class
# Factor object is a definition of any factor OR REPRESENTATION returned by a subFunction or specified by a user
createFactorFromReturnedValue <- function(x)
{
  factor_new <- new(Class = "Factor")
  cls <- class(x$value)
  val <- x$value
  factor_new@name <- x$name
  factor_new@description <- x$description
  factor_new@owners <- x$owners
  if (cls == "integer")
  {
    factor_new@valueClass <- "integer"
    factor_new@levels <- as.character(NA)
  }
  if (cls == "numeric")
  {
    factor_new@valueClass <- "numeric"
    factor_new@levels <- as.character(NA)
  }
  if (cls == "factor")
  {
    factor_new@valueClass <- "factor"
    factor_new@levels <- levels(val)
  }
  if (cls == "ordered")
  {
    factor_new@valueClass <- "ordFactor"
    factor_new@levels <- levels(val)
  }
  if (extends(cls, "Representation"))
  {
    factor_new@valueClass <- cls
    factor_new@levels <- as.character(NA)
  }
  return(factor_new)
}

# createFactorsDataList function gets results of calculations made by analyzer/estimator and creates factors definitions 
# which are not present in factorsDef dictionary
createFactorsDataList <- function(calculationResults, factorsDef)
{
  uniqueFactors <- unique(calculationResults$factor)
  factorIDs <- rep(NA, nrow(calculationResults))
  for (i in 1:length(uniqueFactors))
  {
    fctrExists <- factorExists(self = factorsDef, factor = uniqueFactors[[i]])
    if (fctrExists$exists)
    {
      factorID <- fctrExists$factorID
    }
    else
    {
      # add factor definition
      fctrsDefAdded <- addFactorDefinition(self = factorsDef, factor = uniqueFactors[[i]])
      factorsDef <- fctrsDefAdded$factorsDef
      factorID <- fctrsDefAdded$factorID
    }
    factorIDs[sapply(calculationResults$factor, FUN = identical, uniqueFactors[[i]])] <- factorID
  }
  # creating a new column with factor IDs to data frame with analysis results
  calculationResults$factorID <- factorIDs
  # deleting temporary "factors" column
  calculationResults <- calculationResults[,-which(names(calculationResults) == "factors")]
  return(list(calculationResults = calculationResults, factorsDef = factorsDef))
}

# createEvent function creates an object of Event class from given part of DataRecord data samples
# createEvent function is used in getEventData function
createEvent <- function(groupData, eye, eventClass, detectorID)
{
  if (eventClass != "WindowEvent")
  {
    eventObject <- new(Class = eventClass, 
                       data = groupData,
                       eye = eye,
                       detectorID = detectorID,
                       eventID = unique(groupData$markers), 
                       group = unique(groupData$groups))
  }
  if (eventClass == "WindowEvent")
  {
    eventObject <- new(Class = eventClass, 
                       eye = eye,
                       data = groupData$dataFrame,
                       window = groupData$window, 
                       group = unique(groupData$groups))
  }
  return(eventObject)
}

# getEventData function reads and returns an object of EventData class
# which is a list consisted of event objects that correspond to eventMarkers for
# specified eye, eventClass, eventTypesIDs and detectorID (in case of eventClass == OculomotorEvent)
# getEventData function is used in generalEventAnalyzer function
getEventData <- function(eventMarkersAndData, eye, eventClass, eventTypeID, detectorID = NA)
{
  data <- eventMarkersAndData$eyeDataFrame@eyeDataFrame
  
  ## conditions on eventClass are not needed any more due to they are checked in eventMarkersSelector (?)
  if (eventClass == "FilterEvent")
  {
    
  }
  if (eventClass == "OculomotorEvent") 
  {
    selector <- createEventSelector(type = "all",
                                    event = list(eventClass = eventClass, 
                                                 eventTypeID = eventTypeID,
                                                 detectorID = detectorID))
    selected <- eventsSelector(eventMarkersList = eventMarkersAndData$eventMarkers, selector = selector)
    allSelectedGroups <- unlist(lapply(selected$selectedGroups, FUN = function(x) {x$eventGroups}))
    allData <- cbind(data, data.frame(markers = selected$eventMarkers@markers, groups = selected$eventMarkers@groups))
    allData <- allData[allData$groups %in% allSelectedGroups,]
    splittedData <- split(allData, f = allData$groups)
    createdEvents <- lapply(X = splittedData, FUN = createEvent, eye = eye, eventClass = eventClass, detectorID = detectorID)
  }
  if (eventClass == "AOIEvent")
  {
    
  }
  if (eventClass == "SyncEvent")
  {
    
  }
  if (eventClass == "WindowEvent")
  {
    
  }
  events <- new(Class = "EventData", events = createdEvents, eventClass = eventClass, detectorID = detectorID)
  return(events)
}

# calculateSubFunResultsForEvent function applies specified sub function, evaluates its value(s)/representation(s) and then
# creates and returns a data frame with following columns:
# 1. value - value(s)/representation(s) of all sub functions that have been applied
# 2. description - descriptions of factors/representations for corresponding values
# 3. name - names of factors/representations for corresponding values
# 4. owners - classes slot of a sub function (it defines potential owners of a factor/representation)
# 5. owner - class of an event and event type ID for which value(s)/representation(s) have been calculated
# 6. ownerID - event ordinal number (ID of group of samples) and detectorID (it is important in case of using several detectors: IVT, IDT, etc.)
# 7. eye - eye ("left" or "right") for which value(s)/representation(s) have been calculated
# 8. factors - factors definitions for corresponding value(s)/representation(s)
# calculateSubFunResultsForEvent function is used in generalEventAnalyzer function
# resulted data frame is to be attached to another similar data frames returned after applying another sub functions
calculateSubFunResults <- function(data, subFun)
{
  dataClass <- class(data)
  if (extends(dataClass, "Event"))
  {
    # event's factor/representation calculation
    event <- data
    valsAndInfo <- subFun@fun(event@data, subFun@settings)
    owner <- list(eventClass = as.character(class(event))[1], eventTypeID = event@eventID)
    ownerID <- list(detectorID = event@detectorID, eventGroup = event@group)
    res <- data.frame(value = I(valsAndInfo$vals), 
                      description = I(valsAndInfo$info), 
                      name = names(valsAndInfo$info),
                      owners = I(rep(subFun@classes, length(valsAndInfo$vals))),
                      owner = I(rep(list(owner), length(valsAndInfo$vals))),
                      ownerID = I(rep(list(ownerID), length(valsAndInfo$vals))),
                      eye = I(rep(event@eye, length(valsAndInfo$vals))))
    factors <- apply(res, MARGIN = 1, FUN = createFactorFromReturnedValue)
  }
  if (extends(dataClass, "Representation"))
  {
    # representation's factor/representation calculation
    representationData <- data
    valsAndInfo <- sunFun@fun(representationData@data, subFun@settings)
    owner <- list(mainClass = "Representation", subClass = as.character(dataClass)[1])
    ownerID <- list(dataRecordIdentifier = representationData@dataRecordIdentifier)
    res <- data.frame(value = I(valsAndInfo$vals), 
                      description = I(valsAndInfo$info), 
                      name = names(valsAndInfo$info),
                      owners = I(rep(subFun@classes, length(valsAndInfo$vals))),
                      owner = I(rep(list(owner), length(valsAndInfo$vals))),
                      ownerID = I(rep(list(ownerID), length(valsAndInfo$vals))),
                      eye = I(rep(event@eye, length(valsAndInfo$vals))))
    factors <- apply(res, MARGIN = 1, FUN = createFactorFromReturnedValue)
  }
  if (dataClass == "EyeData")
  {
    # eyeData's factor/representation calculation
    eyeData <- data
    valsAndInfo <- sunFun@fun(eyeData@eyeDataFrame, subFun@settings)
    owner <- list(mainClass = "EyeData")
    ownerID <- list(dataRecordIdentifier = eyeData@dataRecordIdentifier)
    res <- data.frame(value = I(valsAndInfo$vals), 
                      description = I(valsAndInfo$info), 
                      name = names(valsAndInfo$info),
                      owners = I(rep(subFun@classes, length(valsAndInfo$vals))),
                      owner = I(rep(list(owner), length(valsAndInfo$vals))),
                      ownerID = I(rep(list(ownerID), length(valsAndInfo$vals))),
                      eye = I(rep(event@eye, length(valsAndInfo$vals))))
    factors <- apply(res, MARGIN = 1, FUN = createFactorFromReturnedValue)
  }
  res$factors <- factors
  return(res)
}

# generalEventAnalyzer function figures out which sub functions are present,
# uses getEventData function to select events that are needed for a specific sub function,
# calculates sub functions results and binds them together into "res" data frame
# "description", "name" and "owners" columns are deleted because they are not needed any more when factors definitions created
generalEventAnalyzer <- function(data, settings)
{
  subFuns <- settings$subFunctions
  detectorID <- settings$detectorID
  eye <- settings$conditions$eye
  res <- list()
  for (i in 1:length(subFuns))
  {
    for (j in 1:length(subFuns[[i]]@classes))
    {
      if (subFuns[[i]]@classes[[j]]$mainClass == "EventData")
      {
        eventClass <- subFuns[[i]]@classes[[j]]$subClass
        eventTypeID <- subFuns[[i]]@classes[[j]]$eventTypeID
        events <- getEventData(eventMarkersAndData = data, 
                               eye = eye,
                               eventClass = eventClass, 
                               eventTypeID = eventTypeID,
                               detectorID = detectorID)
        res <- append(res, lapply(events@events, FUN = calculateSubFunResults, subFun = subFuns[[i]]))
      }
    }
  }
  res <- as.data.frame(do.call("rbind", res))
  res <- res[,-c(which(names(res) %in% c("description", "name", "owners")))]
  return(res)
}

# generalDataRecordAnalyzer function figures out which sub functions are present,
# selects data that are needed for a specific sub function,
# calculates sub functions results and binds them together into "res" data frame
# "description", "name" and "owners" columns are deleted because they are not needed any more when factors definitions created
generalDataRecordAnalyzer <- function(data, settings)
{
  subFuns <- settings$subFunctions
  detectorID <- settings$detectorID
  eye <- settings$conditions$eye
  res <- list()
  for (i in 1:length(subFuns))
  {
    for (j in 1:length(subFuns[[i]]@classes))
    {
      if (subFuns[[i]]@classes[[j]]$mainClass == "EyeData")
      {
        res <- append(res, calculateSubFunResults(data$eyeDataFrame, subFun = subFuns[[i]]))
      }
      if (subFuns[[i]]@classes[[j]]$mainClass == "Representation")
      {
        subClass <- subFuns[[i]]@classes[[j]]$subClass
        # extract representation of a class that corresponds to subFun's subClass
        repres <- getRepresentationData(mainFactorsData = settings$mainFactorsData,
                              eye = eye,
                              representationClass = subClass)
        # if representation is available - calculate sub fun results
        if (!is.na(repres)) 
        {
          res <- append(res, calculateSubFunResults(repres, subFun = subFuns[[i]]))
        }
      }
    }
  }
  res <- as.data.frame(do.call("rbind", res))
  res <- res[,-c(which(names(res) %in% c("description", "name", "owners")))]
  return(res)
}

## CORE ANALYZERS ##
# coreEventAnalyzer function selects data for a specific eye,
# passes all settings and data to generalAnalyzer
# It returns a list with dataRec and factorsDef, eventFactorsDef should be reassigned to factorsDef
# DataRecord@analysisResults is filled with calculated eventFactorsData
coreEventAnalyzer <- function(DataRecord, settings)
{
  
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  eye <- conditions$eye
  settings <- append(settings, list(conditions = conditions))
  
  factorsDef <- settings$factorsDef
  if (is.null(eventsFactorsDef))
  {
    factorsDef <- new(Class = "FactorsDefinitions")
  }
  if (is.null(DataRecord@analysisResults$eventFactorsData))
  {
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData")
  }
  factorsData <- DataRecord@analysisResults$eventFactorsData
  
  if (eye != "both")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = eye, getFactorsData = F)
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    for (i in 1:nrow(analyzerResults$calculationResults))
    {
      factorsData <- addFactorValue(self = factorsData,
                                    owner = analyzerResults$calculationResults[i,]$owner,
                                    ownerID = analyzerResults$calculationResults[i,]$ownerID,
                                    factorID = analyzerResults$calculationResults[i,]$factorID,
                                    eye = analyzerResults$calculationResults[i,]$eye,
                                    value = analyzerResults$calculationResults[i,]$value,
                                    replace = T)
    }
    DataRecord@analysisResults$eventFactorsData <- factorsData
    factorsDef <- analyzerResults$factorsDef
  }
  if (eye == "both")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "left", getFactorsData = F)
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    for (i in 1:nrow(analyzerResults$calculationResults))
    {
      factorsData <- addFactorValue(self = factorsData,
                                    owner = analyzerResults$calculationResults[i,]$owner,
                                    ownerID = analyzerResults$calculationResults[i,]$ownerID,
                                    factorID = analyzerResults$calculationResults[i,]$factorID,
                                    eye = analyzerResults$calculationResults[i,]$eye,
                                    value = analyzerResults$calculationResults[i,]$value,
                                    replace = T)
    }
    factorsDef <- analyzerResults$factorsDef
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "right", getFactorsData = F)
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    for (i in 1:nrow(analyzerResults$calculationResults))
    {
      factorsData <- addFactorValue(self = factorsData,
                                    owner = analyzerResults$calculationResults[i,]$owner,
                                    ownerID = analyzerResults$calculationResults[i,]$ownerID,
                                    factorID = analyzerResults$calculationResults[i,]$factorID,
                                    eye = analyzerResults$calculationResults[i,]$eye,
                                    value = analyzerResults$calculationResults[i,]$value,
                                    replace = T)
    }
    DataRecord@analysisResults$eventFactorsData <- factorsData
    factorsDef <- analyzerResults$factorsDef
  }
  return(list(dataRec = DataRecord, factorsDef = factorsDef))
}

# coreDataRecordAnalyser function selects data for a specific eye,
# passes all settings and data to generalAnalyzer,
# It returns a list with factorsData and factorsDef
# mainFactorsData is filled with calculated factorsData
# mainFactorsDef should be reassigned to factorsDef
coreDataRecordAnalyser <- function(DataRecord, settings)
{
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  eye <- conditions$eye
  settings <- append(settings, list(conditions = conditions, dataRecIdentifier = dataRecIdentifier))

  mainFactorsDef <- settings$mainFactorsDef
  mainFactorsData <- settings$mainFactorsData
  if (is.null(mainFactorsData))
  {
    mainFactorsData <- new(Class = "FactorsData")
  }
  if (is.null(mainFactorsDef))
  {
    mainFactorsDef <- new(Class = "FactorsDefinitions")
  }
  dataRecIdentifier <- list(expID = DataRecord@expID, subjectID = DataRecord@subjectID, trialID = DataRecord@trialID)

  if (eye != "both")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = eye, getFactorsData = F)
    analyzerResults <- generalDataRecordAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, mainFactorsDef)
    for (i in 1:nrow(analyzerResults$calculationResults))
    {
      mainFactorsData <- addFactorValue(self = mainFactorsData,
                                        owner = analyzerResults$calculationResults[i,]$owner,
                                        ownerID = analyzerResults$calculationResults[i,]$ownerID,
                                        factorID = analyzerResults$calculationResults[i,]$factorID,
                                        eye = analyzerResults$calculationResults[i,]$eye,
                                        value = analyzerResults$calculationResults[i,]$value,
                                        replace = T)
    }
    mainFactorsDef <- analyzerResults$factorsDef
  }
  if (eye == "both")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "left", getFactorsData = F)
    analyzerResults <- generalDataRecordAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, mainFactorsDef)
    for (i in 1:nrow(analyzerResults$calculationResults))
    {
      mainFactorsData <- addFactorValue(self = mainFactorsData,
                                        owner = analyzerResults$calculationResults[i,]$owner,
                                        ownerID = analyzerResults$calculationResults[i,]$ownerID,
                                        factorID = analyzerResults$calculationResults[i,]$factorID,
                                        eye = analyzerResults$calculationResults[i,]$eye,
                                        value = analyzerResults$calculationResults[i,]$value,
                                        replace = T)
    }
    mainFactorsDef <- analyzerResults$factorsDef
    
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "right", getFactorsData = F)
    analyzerResults <- generalDataRecordAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, mainFactorsDef)
    for (i in 1:nrow(analyzerResults$calculationResults))
    {
      mainFactorsData <- addFactorValue(self = mainFactorsData,
                                        owner = analyzerResults$calculationResults[i,]$owner,
                                        ownerID = analyzerResults$calculationResults[i,]$ownerID,
                                        factorID = analyzerResults$calculationResults[i,]$factorID,
                                        eye = analyzerResults$calculationResults[i,]$eye,
                                        value = analyzerResults$calculationResults[i,]$value,
                                        replace = T)
    }
    mainFactorsDef <- analyzerResults$factorsDef
  }
  return(list(dataRec = DataRecord, mainFactorsData = mainFactorsData, mainFactorsDef = mainFactorsDef))
}