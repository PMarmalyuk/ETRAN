# createAnalyzer function creates and returns an object of EventAnalyzer class
# EventAnalyzer object is to be used by eventAnalyzer method
createAnalyzer <- function(name, fun, settings)
{
  analyzer <- new(Class = "EventAnalyzer", name = name, fun = fun, settings = settings)
  return(analyzer)
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
  data <- eventMarkersAndData$eyeDataFrame@eyeDataFrame
  
  ## conditions on eventClass are not needed any more due to they are checked in eventMarkersSelector (?)
  if (eventClass == "FilterEvent")
  {
    
  }
  if (eventClass == "OculomotorEvent") 
  {
    selector <- createEventSelector(type = "all", 
                                    event = list(eventClass = eventClass, 
                                                 eventTypeIDs = eventTypesIDs,
                                                 detectorID = detectorID))
    selected <- eventsSelector(eventMarkersList = eventMarkersAndData$eventMarkers, selector = selector)
    allData <- cbind(data, data.frame(markers = selected$eventMarkers@markers, groups = selected$eventMarkers@groups))
    allData <- allData[allData$groups %in% selected$selectedGroups,]
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
        res <- append(res, lapply(events@events, FUN = calculateSubFunResults, subFun = subFuns[[i]]))
      }
      # Eyes Data factors are calculated by estimator!
#       if (subFuns[[i]]@classes[[j]]$mainClass == "EyesData")
#       {
#         
#       }
    }
  }
  res <- as.data.frame(do.call("rbind", res))
  res <- res[,-c(which(names(res) %in% c("description", "name", "owners")))]
  return(res)
}

## CORE ANALYZER ##
# coreEventAnalyzer function selects data for a specific eye,
# passes all settings and data to generalEventAnalyzer,
# It returns a list with dataRec and factorsDef
# DataRecord@analysisResults is filled with calculated eventFactorsData
### TO DO: append analysisResults with replacement, not whole replace!!!
coreEventAnalyzer <- function(DataRecord, settings)
{
  factorsDef <- settings$factorsDef
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  if (conditions$eye == "left")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "left", getFactorsData = F)
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData", 
                                                       factorsData = as.data.frame(analyzerResults$calculationResults))
    factorsDef <- analyzerResults$factorsDef
  }
  if (conditions$eye == "right")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "right", getFactorsData = F)
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData", 
                                                       factorsData = as.data.frame(analyzerResults$calculationResults))
    factorsDef <- analyzerResults$factorsDef
  }
  if (conditions$eye == "both")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "left", getFactorsData = F)
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData", 
                                                       factorsData = as.data.frame(analyzerResults$calculationResults))
    factorsDef <- analyzerResults$factorsDef
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "right", getFactorsData = F)
    settings <- append(settings, list(conditions = conditions))
    analyzerResults <- generalEventAnalyzer(eventMarkersAndData, settings)
    analyzerResults <- createFactorsDataList(analyzerResults, factorsDef)
    DataRecord@analysisResults$eventFactorsData <- rbind(DataRecord@analysisResults$eventFactorsData,
                                                                         as.data.frame(analyzerResults$calculationResults))
  }
  return(list(dataRec = DataRecord, factorsDef = factorsDef))
}