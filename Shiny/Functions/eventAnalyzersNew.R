standardAnalyzer <- function(data, eventMarkerNames, settings, conditions)
{
  data <- data[-which(is.na(data$eventGroups) | is.na(data$eventMarkers)),]
  sampleGroupsByEventType <- split(data, data$eventMarkers)
  subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@operation == "Event Analysis"})]

  sampleGroups <- split(data, f = list(data$eventMarkers, data$eventGroups), drop = T)
  eventGroupsCompexKeys <- strsplit(names(sampleGroups), ".", fixed = T)
  evTypes <- sapply(eventGroupsCompexKeys, FUN = function(x) {x[1]})
  groupIDs <- sapply(eventGroupsCompexKeys, FUN = function(x) {as.numeric(x[2])})
  allEventsData <- cbind(sampleGroups, evTypes, groupIDs)
  eventsData <- apply(X = allEventsData, MARGIN = 1, FUN = function(x)
  {
    eventSamples <- x[[1]]
    ## Get current event type (fixation, saccade, etc.)
    evType <- x[[2]]
    ## Get current event group number
    groupID <- x[[3]]
    ## Select functions to apply for current event type
    subFunsToApply <- subFunctions[sapply(subFunctions, FUN = function(x) {any(x@events == evType)})]
    ## Apply each of selected sub function to current group's samples
    functionsResults <- lapply(subFunsToApply, FUN = function(x)
    {
      settings <- append(x@settings, list(evmn = eventMarkerNames, conditions = conditions))
      functionResult <- x@fun(eventSamples, settings)
      return(functionResult)
    })
    eventData <- list(owner = evType, ownerID = groupID, values = unlist(functionsResults, recursive = F))
    return(eventData)
  })
  res <- new(Class = "FactorsData", factorsDataList = eventsData)
  return(res)
}

## CORE ANALYZER ##
coreEventAnalyzer <- function(DataRecord, settings)
{
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  if (conditions$eye == "left")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    eventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    DataRecord@analysisResults$leftEventData <- standardAnalyzer(data, eventMarkerNames, settings, conditions)
  }
  if (conditions$eye == "right")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    eventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    DataRecord@analysisResults$rightEventData <- standardAnalyzer(data, eventMarkerNames, settings, conditions)
  }
  if (conditions$eye == "both")
  {
    dataLeft <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    dataRight <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    leftEventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    rightEventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    DataRecord@analysisResults$leftEventData <- standardAnalyzer(dataLeft, leftEventMarkerNames, settings, conditions)
    DataRecord@analysisResults$rightEventData <- standardAnalyzer(dataRight, rightEventMarkerNames, settings, conditions)
  }
  return(DataRecord)
}
