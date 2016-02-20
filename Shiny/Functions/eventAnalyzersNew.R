# createAnalyzer function creates and returns an object of EventAnalyzer class
# EventAnalyzer object is to be used by eventAnalyzer method
createAnalyzer <- function(name, fun, settings)
{
  analyzer <- new(Class = "EventAnalyzer", name = name, fun = fun, settings = settings)
  return(analyzer)
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

# getEventMarkersAndData function just reads and returns eventMarkers and data samples from DataRecord as a list
# returned list of eventMarkers and data for specified eye is to be used by generalEventAnalyzer function
getEventMarkersAndData <- function(dataRecord, eye)
{
  if (eye == "left") {return(list(eventMarkers = dataRecord@eyesDataObject@leftEventsMarkers,
                                  data = getDataFrame(dataRecord@eyesDataObject, eye)))}
  if (eye == "right") {return(list(eventMarkers = dataRecord@eyesDataObject@rightEventsMarkers,
                                   data = getDataFrame(dataRecord@eyesDataObject, eye)))}
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
getEventData <- function(eventMarkersAndData, eye, eventClass, eventTypesIDs, detectorID = NA)
{
  idx <- sapply(eventMarkersAndData$eventMarkers, FUN = function(x) {x@eventClass == eventClass})
  eventMarkers <- eventMarkersAndData$eventMarkers[idx]
  data <- eventMarkersAndData$data
  
  if (eventClass == "FilterEvent")
  {
    
  }
  if (eventClass == "OculomotorEvent") 
  {
    idxsForSpecifiedDetectorID <- sapply(eventMarkers, FUN = function(x) {x@detectorID == detectorID})
    eventMarkers <- eventMarkers[[idxsForSpecifiedDetectorID]]
    allData <- cbind(data, data.frame(markers = eventMarkers@markers, groups = eventMarkers@groups))
    allData <- allData[allData$markers %in% eventTypesIDs,]
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
calculateSubFunResultsForEvent <- function(event, subFun)
{
  valsAndInfo <- subFun@fun(event@data, subFun)
  owner <- list(eventClass = class(event), eventTypeID = event@eventID)
  ownerID <- list(detectorID = event@detectorID, eventGroup = event@group)
  eye = event@eye
  res <- data.frame(value = I(valsAndInfo$vals), 
             description = I(valsAndInfo$info), 
             name = names(valsAndInfo$info),
             owners = I(rep(subFun@classes, length(valsAndInfo$vals))),
             owner = I(rep(list(owner), length(valsAndInfo$vals))),
             ownerID = I(rep(list(ownerID), length(valsAndInfo$vals))),
             eye = I(rep(eye, length(valsAndInfo$vals))))
  factors <- apply(res, MARGIN = 1, FUN = createFactorFromReturnedValue)
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
        eventTypesIDs <- subFuns[[i]]@classes[[j]]$eventTypeIDs
        events <- getEventData(eventMarkersAndData = data, 
                               eye = eye,
                               eventClass = eventClass, 
                               eventTypesIDs = eventTypesIDs,
                               detectorID = detectorID)
        res <- append(res, lapply(events@events, FUN = calculateSubFunResultsForEvent, subFun = subFuns[[i]]))
      }
    }
  }
  res <- as.data.frame(do.call("rbind", res))
  res <- res[,-c(which(names(res) %in% c("description", "name", "owners")))]
  return(res)
}

# createFactorsDataList function gets results of calculations made by analyzer and creates factors definitions 
# which are not present in factorsDef dictionary
createFactorsDataList <- function(analyzerResults, factorsDef)
{
  uniqueFactors <- unique(analyzerResults$factor)
  factorIDs <- rep(NA, nrow(analyzerResults))
  for (i in 1:length(uniqueFactors))
  {
    factorExists <- sapply(factorsDef@factorsDef, FUN = identical, uniqueFactors[[i]])
    if (any(factorExists))
    {
      factorID <- factorsDef@ids[[which(factorExists)]]
    }
    else
    {
      if (length(factorsDef@ids) > 0)
      {
        newFactorID <- tail(factorsDef@ids, 1)+1
      } else newFactorID <- 1
      factorsDef@factorsDef <- append(factorsDef@factorsDef, uniqueFactors[[i]])
      factorsDef@ids <- c(factorsDef@ids, newFactorID)
      factorID <- newFactorID
    }
    factorIDs[sapply(analyzerResults$factors, FUN = identical, uniqueFactors[[i]])] <- factorID
  }
  analyzerResults$factorID <- factorIDs
  analyzerResults <- analyzerResults[,-which(names(analyzerResults) == "factors")]
  return(list(analyzerResults = analyzerResults, factorsDef = factorsDef))
}

## CORE ANALYZER ##
# coreEventAnalyzer function selects data for a specific eye,
# passes all settings and data to generalEventAnalyzer,
# It returns a list with dataRec and factorsDef
# DataRecord@analysisResults is filled with calculated eventFactorsData
### TO DO: append analysisResults, not replace!!!
coreEventAnalyzer <- function(DataRecord, settings)
{
  factorsDef <- settings$factorsDef
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  if (conditions$eye == "left")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, "left")
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData", 
                                                           factorsDataList = as.data.frame(analyzerResults$analyzerResults))
    factorsDef <- analyzerResults$factorsDef
  }
  if (conditions$eye == "right")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, "right")
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData", 
                                                       factorsDataList = as.data.frame(analyzerResults$analyzerResults))
    factorsDef <- analyzerResults$factorsDef
  }
  if (conditions$eye == "both")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, "left")
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData", 
                                                       factorsDataList = as.data.frame(analyzerResults$analyzerResults))
    factorsDef <- analyzerResults$factorsDef
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, "right")
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    DataRecord@analysisResults$eventFactorsData@factorsDataList <- rbind(DataRecord@analysisResults$eventFactorsData@factorsDataList,
                                                                         as.data.frame(analyzerResults$analyzerResults))
  }
  return(list(dataRec = DataRecord, factorsDef = factorsDef))
}