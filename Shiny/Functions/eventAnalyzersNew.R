generalEventAnalyzer <- function(data, operation, settings)
{
  if (operation == "Oculomotor Events Analysis")
  {
    if (any(is.na(data$eventGroups) | is.na(data$eventMarkers)))
    {
      data <- data[-which(is.na(data$eventGroups) | is.na(data$eventMarkers)),]
    }
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "Oculomotor Events"})]
    sampleGroups <- split(data, f = list(data$eventMarkers, data$eventGroups), drop = T)
    eventGroupsCompexKeys <- strsplit(names(sampleGroups), ".", fixed = T)
    evTypes <- sapply(eventGroupsCompexKeys, FUN = function(x) {x[1]})
    groupIDs <- sapply(eventGroupsCompexKeys, FUN = function(x) {as.numeric(x[2])})
    allEventsData <- cbind(sampleGroups, evTypes, groupIDs)
    analysisData <- apply(X = allEventsData, MARGIN = 1, FUN = function(x)
    {
      eventSamples <- x[[1]]
      ## Get current event type (fixation, saccade, etc.)
      evType <- x[[2]]
      ## Get current event group number
      groupID <- x[[3]]
      ## Select functions to apply for current event type
      subFunsToApply <- subFunctions[sapply(subFunctions, FUN = function(x) {any(x@applyWhen == evType)})]
      ## Apply each of selected sub function to current group's samples
      functionsResults <- lapply(subFunsToApply, FUN = function(x)
      {
        settings <- append(x@settings, settings)
        functionResult <- x@fun(eventSamples, settings)
        return(functionResult)
      })
      analysisDatum <- list(owner = evType, ownerID = groupID, valsAndInfo = functionsResults)
      return(analysisDatum)
    })
  }
  if (operation == "AOI Events Analysis")
  {
    #
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "AOI Events"})]
  }
  if (operation == "Frame Events Analysis")
  {
    #
    subFunctions <- settings$subFunctions[sapply(settings$subFunctions, FUN = function(x) {x@applyTo == "Frame Events"})]
  }
  return(analysisData)
}

generalEventDataAsFactorsData <- function(analysisData, operation, eventFactors, settings)
{
  fctrs <- list()
  if (operation == "Oculomotor Events Analysis")
  {
    mainOwner <- "Event"
    ownerID <- list(EventGroup = NA)
  }
  if (operation == "AOI Analysis")
  {
    mainOwner <- "AOI"
    ownerID <- list(AOIID = NA)
  }
  if (operation == "Frame Analysis")
  {
    mainOwner <- "Frame"
    ownerID <- list(FrameID = NA)
  }
  eye <- settings$conditions$eye
  for (i in 1:length(analysisData))
  {
    owner <- c(mainOwner, analysisData[[i]]$owner)
    ownerID[[1]] <- analysisData[[i]]$ownerID
    subFunVls <- analysisData[[i]]$valsAndInfo
    if (length(subFunVls) == 0) {print(subFunVls)}
    if (!(length(subFunVls) == 0))
    {
      for (j in 1:length(subFunVls))
      {
        for (k in 1:length(subFunVls[[j]]$vals))
        {
          fctr <- createFactorFromReturnedValue(x = subFunVls[[j]]$vals[[k]])  
          value <- subFunVls[[j]]$vals[[k]]
          fctr@varName <- as.character(names(subFunVls[[j]]$vals[k]))
          fctr@description <- as.character(subFunVls[[j]]$info[[k]])
          fctr@owner <- mainOwner
          whichFactor <- factorExists(self = eventFactors, factor = fctr)
          if (!whichFactor$exists) 
          {
            eventFactors <- addFactorDefinition(eventFactors, factor = fctr)
            factorID <- tail(eventFactors@availableFactors$id, 1)
          } else
          {
            factorID <- whichFactor$id
          }
          fctrs <- rbind(fctrs, list(factorID = factorID, eye = eye, value = value, ownerID = ownerID, owner = owner))
        }
      }
    }
  }

  return(list(factorsData = fctrs, eventFactorsDef = eventFactors))
}
  
## CORE ANALYZER ##
coreEventAnalyzer <- function(DataRecord, settings)
{
  eventFactorsDef <- settings$eventFactors
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  operation <- settings$operation
  if (conditions$eye == "left")
  {
    # TO DO: getDataFrame should also add AOIName and FrameID columns into returned data frame
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    eventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    filterMarkerNames <- DataRecord@eyesDataObject@leftFilterMarkers@markerNames
    settings <- append(settings, list(fmn = filterMarkerNames, evmn = eventMarkerNames, conditions = conditions))
    analyzerResults <- generalEventAnalyzer(data, operation, settings)
    factorsData <- generalEventDataAsFactorsData(analyzerResults, operation, eventFactorsDef, settings)
    eventFactorsDef <- factorsData$eventFactorsDef
    eventFactorsData <- factorsData$factorsData
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData", factorsDataList = as.data.frame(eventFactorsData))
  }
  if (conditions$eye == "right")
  {
    data <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    eventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    filterMarkerNames <- DataRecord@eyesDataObject@rightFilterMarkers@markerNames
    settings <- append(settings, list(fmn = filterMarkerNames, evmn = eventMarkerNames, conditions = conditions))
    analyzerResults <- generalEventAnalyzer(data, operation, settings)
    factorsData <- generalEventDataAsFactorsData(analyzerResults, operation, eventFactorsDef, settings)
    eventFactorsDef <- factorsData$eventFactorsDef
    eventFactorsData <- factorsData$factorsData
    DataRecord@analysisResults$eventFactorsData <- new(Class = "FactorsData", factorsDataList = as.data.frame(eventFactorsData))
  }
  if (conditions$eye == "both")
  {
    dataLeft <- getDataFrame(DataRecord@eyesDataObject, eye = "left")
    leftFilterMarkerNames <- DataRecord@eyesDataObject@leftFilterMarkers@markerNames
    leftEventMarkerNames <- DataRecord@eyesDataObject@leftEventMarkers@markerNames
    leftSettings <- append(settings, list(fmn = leftFilterMarkerNames, evmn = leftEventMarkerNames, conditions = conditions))
    leftAnalyzerResults <- generalEventAnalyzer(dataLeft, operation, leftSettings)
    leftFactorsData <- generalEventDataAsFactorsData(leftAnalyzerResults, operation, eventFactorsDef, leftSettings)
    eventFactorsDef <- leftFactorsData$eventFactorsDef
    leftEventFactorsData <- leftFactorsData$factorsData
    
    dataRight <- getDataFrame(DataRecord@eyesDataObject, eye = "right")
    rightFilterMarkerNames <- DataRecord@eyesDataObject@rightFilterMarkers@markerNames
    rightEventMarkerNames <- DataRecord@eyesDataObject@rightEventMarkers@markerNames
    rightSettings <- append(settings, list(fmn = rightFilterMarkerNames, evmn = rightEventMarkerNames, conditions = conditions))
    rightAnalyzerResults <- generalEventAnalyzer(dataRight, operation, rightSettings)
    rightFactorsData <- generalEventDataAsFactorsData(rightAnalyzerResults, operation, eventFactorsDef, rightSettings)
    eventFactorsDef <- rightFactorsData$eventFactorsDef
    rightEventFactorsData <- rightFactorsData$factorsData
    
    eventFactorsData <- rbind(leftEventFactorsData, rightEventFactorsData)
    DataRecord@analysisResults$eventFactorsData <- rbind(Class = "FactorsData", factorsDataList = as.data.frame(eventFactorsData))
  }
  return(list(dataRec = DataRecord, eventFactors = eventFactorsDef))
}
