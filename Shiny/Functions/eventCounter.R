generalEventCounter <- function(selectedEvents, settings)
{
  selector <- settings$selector
  owners <- list(mainClass = "DataRecord",
                 subClass = "EventGroup",
                 selector = selector)
  ownerID <- settings$dataRecIdentifier
  eye <- settings$eye
  eventCounts <- lapply(selectedEvents$selectedGroups, FUN = function(x) 
  {
    owner <- list(mainClass = "DataRecord", 
                  subClass = "EventGroup",
                  insideEvents = "insideEvents" %in% selector@type, 
                  byGroups = "byGroups" %in% selector@type)
    
    ownerID <- append(ownerID, head(x,length(x)-1))
    
    valsAndInfo <- list(vals = list(eventCount = as.integer(length(x$eventGroups))), 
                        info = list(eventCount = "Number of event occurencies"))
    res <- data.frame(value = I(valsAndInfo$vals), 
                      description = I(valsAndInfo$info), 
                      name = names(valsAndInfo$info),
                      owners = I(list(owners)),
                      owner = I(list(owner)),
                      ownerID = I(list(ownerID)),
                      eye = eye)
    factors <- apply(res, MARGIN = 1, FUN = createFactorFromReturnedValue)
    names(factors) <- NULL
    res$factors <- factors
    return(res)
  })
  res <- do.call("rbind", eventCounts)
  rownames(res) <- NULL
  res <- res[,-c(which(names(res) %in% c("description", "name", "owners")))]
}

# TO DO: append factors data into main factors data list
coreEventCounter <- function(DataRecord, settings)
{
  conditions <- DataRecord@eyesDataObject@conditions@conditions
  eye <- conditions$eye
  
  factorsDef <- settings$factorsDef
  if (is.null(factorsDef))
  {
    factorsDef <- new(Class = "FactorsDefinitions")
  }
  mainFactorsData <- settings$mainFactorsData
  if (is.null(mainFactorsData))
  {
    mainFactorsData <- new(Class = "FactorsData")
  }
  mainFactorsDef <- settings$mainFactorsDef
  if (is.null(mainFactorsDef))
  {
    mainFactorsDef <- new(Class = "FactorsDefinitions")
  }
  
  dataRecIdentifier <- list(expID = DataRecord@expID, subjectID = DataRecord@subjectID, trialID = DataRecord@trialID)
  settings <- append(settings, list(dataRecIdentifier = dataRecIdentifier, eye = eye))
  
  if (eye != "both")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = eye, getFactorsData = T)
    selectedEvents <- eventsSelector(eventMarkersList = eventMarkersAndData$eventMarkers, 
                                     factorsData = eventMarkersAndData$factorsData,
                                     factorsDef = factorsDef, selector = settings$selector)
    counterResults <- generalEventCounter(selectedEvents, settings)
    counterResults <- createFactorsDataList(calculationResults = counterResults, factorsDef = mainFactorsDef)
    for (i in 1:nrow(counterResults$calculationResults))
    {
      mainFactorsData <- addFactorValue(self = mainFactorsData,
                                        owner = counterResults$calculationResults[i,]$owner,
                                        ownerID = counterResults$calculationResults[i,]$ownerID,
                                        factorID = counterResults$calculationResults[i,]$factorID,
                                        eye = counterResults$calculationResults[i,]$eye,
                                        value = counterResults$calculationResults[i,]$value,
                                        replace = T)
    }
    mainFactorsDef <- counterResults$factorsDef
  }
  if (eye == "both")
  {
    settings$eye <- "left"
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "left", getFactorsData = T)
    selectedEvents <- eventsSelector(eventMarkersList = eventMarkersAndData$eventMarkers, 
                                     factorsData = eventMarkersAndData$factorsData,
                                     factorsDef = factorsDef, selector = settings$selector)
    counterResults <- generalEventCounter(selectedEvents, settings)
    counterResults <- createFactorsDataList(counterResults, factorsDef)
    for (i in 1:nrow(counterResults$calculationResults))
    {
      mainFactorsData <- addFactorValue(self = mainFactorsData,
                                        owner = counterResults$calculationResults[i,]$owner,
                                        ownerID = counterResults$calculationResults[i,]$ownerID,
                                        factorID = counterResults$calculationResults[i,]$factorID,
                                        eye = counterResults$calculationResults[i,]$eye,
                                        value = counterResults$calculationResults[i,]$value,
                                        replace = T)
    }
    mainFactorsDef <- counterResults$factorsDef
    
    settings$eye <- "right"
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "right", getFactorsData = T)
    selectedEvents <- eventsSelector(eventMarkersList = eventMarkersAndData$eventMarkers, 
                                     factorsData = eventMarkersAndData$factorsData,
                                     factorsDef = factorsDef, selector = settings$selector)
    counterResults <- generalEventCounter(selectedEvents, settings)
    counterResults <- createFactorsDataList(counterResults, factorsDef)
    for (i in 1:nrow(counterResults$calculationResults))
    {
      mainFactorsData <- addFactorValue(self = mainFactorsData,
                                        owner = counterResults$calculationResults[i,]$owner,
                                        ownerID = counterResults$calculationResults[i,]$ownerID,
                                        factorID = counterResults$calculationResults[i,]$factorID,
                                        eye = counterResults$calculationResults[i,]$eye,
                                        value = counterResults$calculationResults[i,]$value,
                                        replace = T)
    }
    mainFactorsDef <- counterResults$factorsDef
  }
  return(list(dataRec = DataRecord, mainFactorsData = mainFactorsData, mainFactorsDef = mainFactorsDef))
}

# sel <- createEventSelector(type = c("all"), 
#                            event = list(eventClass = "OculomotorEvent", eventTypeID = c(1,2), detectorID = 3),
#                            framingEvent = list(eventClass = "FilterEvent", eventTypeID = c(1)))
# res <- coreEventCounter(DataRecord = dataRec, 
#                         settings = list(selector = sel, 
#                                         eventsFactorsDef = new(Class = "FactorsDefinitions"),
#                                         mainFactorsDef = res$mainFactorsDef,
#                                         mainFactorsData = res$mainFactorsData))#new(Class = "FactorsData")))
# res$mainFactorsData@factorsData
