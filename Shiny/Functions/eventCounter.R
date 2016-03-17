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
    res$factors <- I(list(factors))
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
  mainFactorsData <- settings$mainFactorsData
  
  dataRecIdentifier <- list(expID = DataRecord@expID, subjectID = DataRecord@subjectID, trialID = DataRecord@trialID)
  if (eye == "left")
  {
    eventMarkersAndData <- getEventMarkersAndData(DataRecord, eye = "left", getFactorsData = T)
    selectedEvents <- eventsSelector(eventMarkersList = eventMarkersAndData$eventMarkers, 
                                     factorsData = eventMarkersAndData$factorsData,
                                     factorsDef = factorsDef, selector = settings$selector)
    settings <- append(settings, list(dataRecIdentifier = dataRecIdentifier, eye = eye))
    counterResults <- generalEventCounter(selectedEvents, settings)
    counterResults <- createFactorsDataList(counterResults, factorsDef)
    mainFactorsData <- new(Class = "FactorsData", factorsData = as.data.frame(counterResults$calculationResults))
    factorsDef <- counterResults$factorsDef
  }
    
#     settings$mainFactorsData <- selectedMainFactorsData
#     settings <- append(settings, list(conditions = conditions, dataRecIdentifier = dataRecIdentifier))
#     analyzerResults <- generalDataRecordAnalyzer(eventMarkersAndData, settings)
#     factorsData <- createFactorsDataList(analyzerResults, mainFactorsDef)
#     mainFactorsDef <- factorsData$mainFactorsDef
#     mainFactorsData <- new(Class = "FactorsData", factorsDataList = as.data.frame(factorsData$mainFactorsData))
#   }
  if (eye == "right")
  {
    
  }
  if (eye == "both")
  {
    
  }
  #return(list(dataRec = DataRecord, mainFactorsData = mainFactorsData, mainFactorsDef = mainFactorsDef))
  return(list(dataRec = DataRecord, mainFactorsData = mainFactorsData, factorsDef = factorsDef))
}

sel <- createEventSelector(type = c("all"), 
                           event = list(eventClass = "OculomotorEvent", eventTypeID = c(1,2), detectorID = 3),
                           framingEvent = list(eventClass = "FilterEvent", eventTypeID = c(1)))
eventsSelector(eventMarkersList = dataRec@eyesDataObject@leftEventsMarkers, selector = sel)
res <- coreEventCounter(DataRecord = dataRec, settings = list(selector = sel, factorsDef = new(Class = "FactorsDefinitions")))
res$factorsDef
res$mainFactorsData@factorsData$value
