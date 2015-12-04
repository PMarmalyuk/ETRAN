standardAnalyzer <- function(data, filterMarkerNames, eventMarkerNames, settings, conditions)
{
  if (any(is.na(data$eventGroups) | is.na(data$eventMarkers)))
  {
    data <- data[-which(is.na(data$eventGroups) | is.na(data$eventMarkers)),]
  }
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
      settings <- append(x@settings, list(fmn = filterMarkerNames, evmn = eventMarkerNames, conditions = conditions))
      functionResult <- x@fun(eventSamples, settings)
      return(functionResult)
    })
    eventData <- list(owner = evType, ownerID = groupID, values = unlist(functionsResults, recursive = F))
    return(eventData)
  })
  return(eventsData)
}

eventDataAsFactorsData <- function(analyzerResults, internalFactors)
{
  eventsFctrs <- list()
  for (i in 1:length(analyzerResults))
  {
    vals_record <- analyzerResults[[i]]
    owner <- c("Event", vals_record$owner)
    ownerID <- list(EventGroup = vals_record$ownerID)
    vls <- vals_record$values
    for (j in 1:length(vls))
    {
      fctr <- createFactorFromReturnedValue(x = vls[j])
      # TO DO: extract factor description from sub function output?
      fctr@description <- as.character(NA)
      fctr@owner <- "Event"
      whichFactor <- factorExists(self = internalFactors, factor = fctr)
      if (!whichFactor$exists) 
      {
        internalFactors <- addFactorDefinition(internalFactors, factor = fctr)
        factorID <- tail(internalFactors@availableFactors$id, 1)
      } else
      {
        factorID <- whichFactor$id
      }
      value = vls[[j]]
      eventsFctrs <- rbind(eventsFctrs, list(factorID = factorID, eye = "left", value = value, ownerID = ownerID, owner = owner))
    }
  }
  return(list(eventFactors = eventsFctrs, internalFactors = internalFactors))
}
  



## CORE ANALYZER ##
coreEventAnalyzer <- function(DataRecord, settings)
{
  internalFactors <- settings$internalFactors
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  if (conditions$eye == "left")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    eventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    filterMarkerNames <- DataRecord@eyesDataObject@leftFilterMarkers@markerNames
    
    analyzerResults <- standardAnalyzer(data, filterMarkerNames, eventMarkerNames, settings, conditions)
    
    factorsData <- eventDataAsFactorsData(analyzerResults, internalFactors)
    internalFactors <- factorsData$internalFactors
    eventFactors <- factorsData$eventFactors
    DataRecord@analysisResults$eventData <- new(Class = "FactorsData", factorsDataList = as.data.frame(eventFactors))
  }
  if (conditions$eye == "right")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    eventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    filterMarkerNames <- DataRecord@eyesDataObject@rightFilterMarkers@markerNames
    analyzerResults <- standardAnalyzer(data, filterMarkerNames, eventMarkerNames, settings, conditions)
    factorsData <- eventDataAsFactorsData(analyzerResults, internalFactors)
    internalFactors <- factorsData$internalFactors
    eventFactors <- factorsData$eventFactors
    DataRecord@analysisResults$eventData <- new(Class = "FactorsData", factorsDataList = as.data.frame(eventFactors))
  }
  if (conditions$eye == "both")
  {
    dataLeft <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    leftEventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    leftFilterMarkerNames <- DataRecord@eyesDataObject@leftFilterMarkers@markerNames
    leftAnalyzerResults <- standardAnalyzer(dataLeft, leftFilterMarkerNames, leftEventMarkerNames, settings, conditions)
    leftFactorsData <- eventDataAsFactorsData(leftAnalyzerResults, internalFactors)
    internalFactors <- leftFactorsData$internalFactors
    leftEventFactors <- leftFactorsData$eventFactors
    
    dataRight <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    rightEventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    rightFilterMarkerNames <- DataRecord@eyesDataObject@rightFilterMarkers@markerNames
    rightAnalyzerResults <- standardAnalyzer(dataRight, rightFilterMarkerNames, rightEventMarkerNames, settings, conditions)
    rightFactorsData <- eventDataAsFactorsData(rightAnalyzerResults, internalFactors)
    internalFactors <- rightFactorsData$internalFactors
    rightEventFactors <- rightFactorsData$eventFactors
    
    eventFactors <- rbind(leftEventFactors, rightEventFactors)
    DataRecord@analysisResults$eventData <- rbind(Class = "FactorsData", factorsDataList = as.data.frame(eventFactors))
  }
  return(list(dataRec = DataRecord, intFctrs = internalFactors))
}
