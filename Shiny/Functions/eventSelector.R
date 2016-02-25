setClass("EventSelector",
         representation(type = "character", # "all", "insideEvent", "byFactorExpression" or c("insideEvent", "byFactorExpression")
                        events = "list", # list of eventClass, eventTypeIDs and [detectorID]
                        framingEvents = "list", # list of eventClass and eventTypeIDs
                        factorIDs = "numeric", # IDs of factors for which expressions should be checked
                        factorExpressions = "list")) # expressions to check for corresponding factors

createEventSelector <- function(type, events, 
                                framingEvents = list(NA), 
                                factorIDs = as.numeric(NA), 
                                factorExpressions = list(NA))
{
  selector <- new(Class = "EventSelector",
                  type = type, 
                  events = events, 
                  framingEvents = framingEvents, 
                  factorIDs = factorIDs, 
                  factorExpressions = factorExpressions)
  return(selector)
}

getEventsLocations <- function(eventMarkers)
{
  df <- data.frame(markers = eventMarkers@markers, 
                   groups = eventMarkers@groups)
  dfs <- split(df, df$groups)
  locations <- lapply(dfs, FUN = function(x) 
  {
    list(startIdx = head(rownames(x), 1), endIdx = tail(rownames(x), 1), typeID = unique(x$markers), group = unique(x$groups))
  })
  locations <- do.call("rbind", locations)
  locations[,c(1,2)] <- as.numeric(locations[,c(1,2)])
  return(as.data.frame(locations))
}

eventsSelector <- function(eventMarkersList, factorsData = NA, selector)
{
  type <- selector@type
  events <- selector@events
  if (type == "all")
  {
    if (events$eventClass == "FilterEvent")
    {
      eventTypeIDs <- events$eventTypeIDs
      flag <- sapply(eventMarkersList, FUN = function(x) {x@eventClass == events$eventClass})
      allEventMarkers <- eventMarkersList[[which(flag)]]
      selectedGroups <- unique(allEventMarkers@groups[allEventMarkers@markers %in% eventTypeIDs])
      selectedEvents <- list(eventMarkers = allEventMarkers, selectedGroups = selectedGroups)
    }
    if (events$eventClass == "OculomotorEvent")
    {
      eventTypeIDs <- events$eventTypeIDs
      detectorID <- events$detectorID
      flags <- sapply(eventMarkersList, FUN = function(x) {x@eventClass == events$eventClass})
      eventMarkersList <- eventMarkersList[flags]
      idxsForSpecifiedDetectorID <- sapply(eventMarkersList, FUN = function(x) {x@detectorID == detectorID})
      allEventMarkers <- eventMarkersList[[idxsForSpecifiedDetectorID]]
      selectedGroups <- unique(allEventMarkers@groups[allEventMarkers@markers %in% eventTypeIDs])
      selectedEvents <- list(eventMarkers = allEventMarkers, selectedGroups = selectedGroups)
    }
  }
  else
  {
    if (type == "insideEvents")
    {
      framingEvents <- selector@framingEvents
      selectorOfExternalEvents <- createEventSelector(type = "all", events = framingEvents)
      selectedExternalEventMarkers <- eventMarkersSelector(eventMarkersList = eventMarkersList, selector = selectorOfExternalEvents)
      selectorOfInternalEvents <- createEventSelector(type = "all", events = events)
      selectedInternalEventMarkers <- eventMarkersSelector(eventMarkersList = eventMarkersList, selector = selectorOfInternalEvents)
      
      uniqueExternalEventMarkers <- unique(selectedExternalEventMarkers$eventMarkers@markers)
      
      extEventsLocations <- getEventsLocations(eventMarkers = selectedExternalEventMarkers$eventMarkers)
      intEventsLocations <- getEventsLocations(eventMarkers = selectedInternalEventMarkers$eventMarkers)

      selExtEvLocs <- extEventsLocations[extEventsLocations$typeID %in% framingEvents$eventTypeIDs,]
      selIntEvLocs <- intEventsLocations[intEventsLocations$typeID %in% events$eventTypeIDs,]

      hits <- apply(selIntEvLocs, MARGIN = 1, FUN = function(x)
      {
        res <- apply(selExtEvLocs, MARGIN = 1, FUN = function(y)
        {
          hit <- between(x = x$startIdx, lower = y$startIdx, upper = y$endIdx, incbounds = T) &
                  between(x = x$endIdx, lower = y$startIdx, upper = y$endIdx, incbounds = T)
          return(data.frame(extEventID = y$typeID, extEventGroup = y$group, 
                            intEventID = x$typeID, intEventGroup = x$group, hit = hit))
        })
        return(do.call("rbind", res))
      })
      res <- do.call("rbind", hits)
      res <- res[res$hit,-5]
      
      selectedEvents <- res
      
    }
    if (type == "byFactorExpression")
    {
      factorIDs 
      factorExpressions
    }
  }
  return(selectedEvents)
}

RES$intGroup[RES$extEventID == 1 & RES$intEventID == 1 & RES$hit] 
sel <- createEventSelector(type = "all", events = list(eventClass = "OculomotorEvent", eventTypeIDs = 1, detectorID = 3))
eventsSelector(eventMarkersList = dataRec@eyesDataObject@leftEventsMarkers, selector = sel)
sel <- createEventSelector(type = "all", events = list(eventClass = "FilterEvent", eventTypeIDs = 1))
eventsSelector(eventMarkersList = dataRec@eyesDataObject@leftEventsMarkers, selector = sel)
sel <- createEventSelector(type = "insideEvents", 
                           events = list(eventClass = "OculomotorEvent", eventTypeIDs = c(1,2), detectorID = 3),
                           framingEvents = list(eventClass = "FilterEvent", eventTypeIDs = c(1)))
eventsSelector(eventMarkersList = dataRec@eyesDataObject@leftEventsMarkers, selector = sel)


dataRec@eyesDataObject@leftEventsMarkers$filterMarkers
