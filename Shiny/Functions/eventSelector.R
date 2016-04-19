setClass("EventSelector",
         representation(type = "character", # "all", "insideEvent", "byFactorExpression" or c("insideEvent", "byFactorExpression")
                        event = "list", # list of eventClass, eventTypeID and [detectorID]
                        framingEvent = "list", # list of eventClass and eventTypeID
                        factorIDs = "numeric", # IDs of factors for which expressions should be checked
                        factorExpressions = "list")) # expressions to check for corresponding factors

createEventSelector <- function(type, event, framingEvent = list(NA), factorIDs = as.numeric(NA), factorExpressions = list(NA))
{
  selector <- new(Class = "EventSelector",
                  type = type, 
                  event = event, 
                  framingEvent = framingEvent, 
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

expressionConstructor <- function(varName, operator, operand)
{
  
}

expressionEvaluator <- function(expression, varName, varValue)
{
  
}

eventsSelector <- function(eventMarkersList, factorsData = NA, factorsDef = NA, selector)
{
  type <- selector@type
  event <- selector@event
  if ("all" %in% type)
  {
    if (event$eventClass != "OculomotorEvent")
    {
      eventTypeID <- event$eventTypeID
      flag <- sapply(eventMarkersList, FUN = function(x) {x@eventClass == event$eventClass})
      if (all(flag == FALSE)) 
      {
        warning("There are no markers of events of specified class!")
        selectedEvents <- NA
      }
      else
      {
        allEventMarkers <- eventMarkersList[[which(flag)]]
        selectedGroups <- lapply(eventTypeID, FUN = function(x) 
        {
          groups <- unique(allEventMarkers@groups[allEventMarkers@markers == x])
          return(list(eventTypeID = x, eventGroups = groups))
        })
        selectedEvents <- list(eventMarkers = allEventMarkers, selectedGroups = selectedGroups)
      }
    }
    if (event$eventClass == "OculomotorEvent")
    {
      eventTypeID <- event$eventTypeID
      detectorID <- event$detectorID
      flags <- sapply(eventMarkersList, FUN = function(x) {x@eventClass == event$eventClass})
      if (all(flags == FALSE)) 
      {
        warning("There are no markers of events of specified class!")
        selectedEvents <- NA
      }
      else
      {
        eventMarkersForEventClass <- eventMarkersList[flags]
        idxsForSpecifiedDetectorID <- sapply(eventMarkersForEventClass, FUN = function(x) {x@detectorID == detectorID})
        if (all(idxsForSpecifiedDetectorID == FALSE)) 
        {
          warning("There are no events detected by specified detector!")
          selectedEvents <- NA
        }
        else
        {
          allEventMarkers <- eventMarkersForEventClass[[idxsForSpecifiedDetectorID]]
          selectedGroups <- lapply(eventTypeID, FUN = function(x) 
          {
            groups <- unique(allEventMarkers@groups[allEventMarkers@markers == x])
            return(list(eventTypeID = x, eventGroups = groups))
          })
          selectedEvents <- list(eventMarkers = allEventMarkers, selectedGroups = selectedGroups)
        }
      }
    }
  }
  if ("insideEvents" %in% type)
  {
    framingEvent <- selector@framingEvent
    selectorOfExternalEvents <- createEventSelector(type = "all", event = framingEvent)
    selectedExternalEventMarkers <- eventsSelector(eventMarkersList = eventMarkersList, selector = selectorOfExternalEvents)
    if (!is.na(selectedExternalEventMarkers[1]))
    {
      selectedInternalEventMarkers <- selectedEvents
      if (!is.na(selectedInternalEventMarkers[1]))
      {
        extEventsLocations <- getEventsLocations(eventMarkers = selectedExternalEventMarkers$eventMarkers)
        intEventsLocations <- getEventsLocations(eventMarkers = selectedInternalEventMarkers$eventMarkers)
        selExtEvLocs <- extEventsLocations[extEventsLocations$typeID %in% framingEvent$eventTypeID,]
        selIntEvLocs <- intEventsLocations[intEventsLocations$typeID %in% event$eventTypeID,]
        hits <- apply(selIntEvLocs, MARGIN = 1, FUN = function(x)
        {
          res <- apply(selExtEvLocs, MARGIN = 1, FUN = function(y)
          {
            hit <- between(x = x$startIdx, lower = y$startIdx, upper = y$endIdx, incbounds = T) &
                   between(x = x$endIdx, lower = y$startIdx, upper = y$endIdx, incbounds = T)
            return(data.frame(extEventTypeID = y$typeID, extEventGroup = y$group, 
                              eventTypeID = x$typeID, eventGroup = x$group, hit = hit))
          })
          return(do.call("rbind", res))
        })
        res <- do.call("rbind", hits)
        
        if ("byGroups" %in% type)
        {
          selectedGroupsUnlabeled <- split(res, list(extEventTypeID = res$extEventTypeID, 
                                                     extEventGroup = res$extEventGroup, 
                                                     eventTypeID = res$eventTypeID),
                                           drop = T)
          selectedGroups <- lapply(selectedGroupsUnlabeled, FUN = function(x)
          {
            return(list(extEventTypeID = unique(x$extEventTypeID), 
                        extEventGroup = unique(x$extEventGroup),
                        eventTypeID = unique(x$eventTypeID),
                        eventGroups = x$eventGroup[x$hit]))
          })
        }
        else
        {
          selectedGroupsUnlabeled <- split(res, list(extEventTypeID = res$extEventTypeID,
                                                     eventTypeID = res$eventTypeID),
                                           drop = F)
          selectedGroups <- lapply(selectedGroupsUnlabeled, FUN = function(x)
          {
            
            return(list(extEventTypeID = unique(x$extEventTypeID),
                        eventTypeID = unique(x$eventTypeID),
                        eventGroups = x$eventGroup[x$hit]))
          })
        }
        names(selectedGroups) <- NULL
        selectedEvents <- list(eventMarkers = allEventMarkers, selectedGroups = selectedGroups)
      }
    }
  }
  if ("byFactorExpression" %in% type)
  {
    # OUTSIDE of selector user should:
    ## 1. Select class of events
    ## 2. Select _EVENTTYPEID_
    ## 3. Select all _SCALAR_ factors defined for events of specified class and _EVENTTYPEID_
    ## 4. Choose factors to be used in expression
    ## 3. Construct expression for (some of) the factors ### OUTSIDE SELECTOR ###
    factorIDs <- selector@factorIDs #factor IDs which names are used in expressions
    factorExpressions <- selector@factorExpressions # expressions (varName, operation, operand) for corresponding factors
    # 1. Select all events of specified class and _EVENTTYPEID_ from eventMarkersList
    # 4. Select values of factors which correspond to the events
    # e.g. create data frame with factor ID, factor value, event group [and detectorID]
    # 5. Evaluate expression for each event and select events which agree with expression
    # 6. Return groups of the events
    
    
    ## typical routine with expression:
    varName <- "v"
    varValue <- 5
    varExpr <- "v >= 10"
    assign(varName, varValue)
    eval(parse(text = varExpr))
  }
  return(selectedEvents)
}

# sel <- createEventSelector(type = "all", event = list(eventClass = "OculomotorEvent", eventTypeID = c(1,2,3), detectorID = 3))
# eventsSelector(eventMarkersList = dataRec@eyesDataObject@leftEventsMarkers, selector = sel)
# 
# sel <- createEventSelector(type = "all", event = list(eventClass = "FilterEvent", eventTypeIDs = 1))
# eventsSelector(eventMarkersList = dataRec@eyesDataObject@leftEventsMarkers, selector = sel)
# 
# newMarkers <- rbinom(n = length(dataRec@eyesDataObject@leftEventsMarkers$oculomotorEventMarkers@markers), size = 1, prob = 0.999)
# transitions <- newMarkers[-1] != newMarkers[-length(newMarkers)]; groups <- c(1,cumsum(transitions)+1)
# myMarkers <- new(Class = "SyncEventMarkers", markers = newMarkers, eventClass = "SyncEvent", groups = groups)
# dataRec@eyesDataObject@leftEventsMarkers$myMarkers <- myMarkers
# 
# sel <- createEventSelector(type = c("all", "insideEvents", "byGroups"), 
#                            event = list(eventClass = "OculomotorEvent", eventTypeID = c(1,2), detectorID = 3),
#                            framingEvent = list(eventClass = "SyncEvent", eventTypeID = c(0,1)))
# eventsSelector(eventMarkersList = dataRec@eyesDataObject@leftEventsMarkers, selector = sel)
# 

