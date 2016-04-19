getAvailableEventsClasses <- function(ETD, eye)
{
  if (eye == "left")
  {
    eventMarkersList <- ETD$leftEventsData
  }
  if (eye == "right")
  {
    eventMarkersList <- ETD$rightEventsData
  }
  
  if (any(!is.na(eventMarkersList)))
  {
    availableEventsClasses <- unique(sapply(eventMarkersList, FUN = function(x) {x$eventsClass}))
  }
  else   
  {
    warning("There are no event markers in the eye tracking data for specified eye!")
    availableEventsClasses <- NA
  }
  return(c("Trial", availableEventsClasses))
}
getAvailableEventsClasses(ETD=etd, eye = "left")

getAvailableDetectorsResults <- function(ETD, eye, eventsClass)
{
  if (is.na(eventsClass))
  {
    warning("You should specify events class!")
    return(NA)
  }
  if (eye == "left")
  {
    eventMarkersList <- ETD$leftEventsData
  }
  if (eye == "right")
  {
    eventMarkersList <- ETD$rightEventsData
  }
  if (any(!is.na(eventMarkersList)))
  {
    availableDetectorsResults <- unique(sapply(eventMarkersList, FUN = function(x) {ifelse(x$eventsClass == eventsClass,
                                                                                           x$detector, NA)}))
    availableDetectorsResults <- availableDetectorsResults[!is.na(availableDetectorsResults)]
  }
  else   
  {
    warning("There are no event markers in the eye tracking data for specified events class!")
    availableDetectorsResults <- NA
  }
  return(availableDetectorsResults)
}
# getAvailableDetectorsResults(ETD=etd, eye = "left", eventsClass = "OculomotorEvents")

getMarkersGroups <- function(markers)
{
  adjacentMarkers <- data.frame(firstEv = markers[-length(markers)], secondEv = markers[-1])
  transitions <- apply(adjacentMarkers, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  groups <- c(1,cumsum(transitions)+1)
  return(groups)
}
# getMarkersGroups(etd$leftEventsData$filterEventMarkers)

getChannelsData <- function(ETD, eye)
{
  if (eye == "left" & ETD$settings$mode == "right") {stop("No data for left eye!"); return(NA)}
  if (eye == "right" & ETD$settings$mode == "left") {stop("No data for right eye!"); return(NA)}
  
  #if (!is.na(trials)) {readFlags <- ETD$commonData$trial %in% trials} else {readFlags <- T}
  
  if (eye == "left")
  {
    channelsData <- cbind(ETD$commonData, 
                          ETD$leftEyeData)
    eventMarkers <- ETD$leftEventsData
  }
  if (eye == "right")
  {
    channelsData <- cbind(ETD$commonData, 
                          ETD$rightEyeData)
    eventMarkers <- ETD$rightEventsData
  }
  return(channelsData)
}
# getChannelsData(etd, eye = "left")

getEventMarkers <- function(ETD, eye, eventsClass, detector = NA)
{
  if (eye == "left" & ETD$settings$mode == "right") {stop("No data for left eye!"); return(NA)}
  if (eye == "right" & ETD$settings$mode == "left") {stop("No data for right eye!"); return(NA)}
  
  if (eventsClass == "Trial")
  {
    markers <- ETD$commonData$trial
    eventMarkers <- list(eventMarkers = markers,
                         eventGroups = getMarkersGroups(markers),
                         eventClass = "Trial",
                         detector = NA)
  }
  else
  {
    if (eye == "left")
    {
      eventMarkersList <- ETD$leftEventsData
    }
    if (eye == "right")
    {
      eventMarkersList <- ETD$rightEventsData
    }
    if (!is.na(eventsClass))
    {
      markersReadFlags <- sapply(eventMarkersList, FUN = function(x) {x$eventsClass == eventsClass})
      if (sum(markersReadFlags) == 0) 
      {
        stop("Cannot find event markers for specified event class!")
        return(NA)
      }
      if (is.na(detector))
      {
        if (sum(markersReadFlags) == 1) 
        {
          eventMarkers <- eventMarkersList[[which(markersReadFlags)]]
        }
        if (sum(markersReadFlags) > 1) 
        {
          messagePart1 <- "Please, specify detector which has been used to detect events!\n"
          messagePart2 <- "You can select event markers for one of the following available detectors' results:\n"
          detectorsPresent <- paste0(sapply(eventMarkersList[markersReadFlags], 
                                            FUN = function(x) {paste0("- ",x$detector)}), collapse = "\n")
          message <- paste0(messagePart1, messagePart2, detectorsPresent)
          stop(message)
          return(NA)
        }
      }
      else
      {
        markersReadFlags <- sapply(eventMarkersList, FUN = function(x) {x$eventsClass == eventsClass & x$detector == detector})
        if (sum(markersReadFlags) == 0) 
        {
          stop("Cannot find event markers for specified event class and detector!")
          return(NA)
        }
        if (sum(markersReadFlags) == 1) 
        {
          eventMarkers <- eventMarkersList[[which(markersReadFlags)]]
        }
      }
    }
    else 
    {
      stop("You should specify event class!")
      return(NA)
    }
  }
  return(eventMarkers)
}
# getEventMarkers(ETD = etd, eye = "left", eventsClass = "OculomotorEvents", detector = "IDT.Detection2")

getMarkersGroupsPositions <- function(markers, groups)
{
  df <- data.frame(markers = markers, pos = 1:length(markers), groups = groups)
  dfs <- split(df, df$groups)
  locations <- lapply(dfs, FUN = function(x) 
  {
    data.frame(start = as.numeric(head(x$pos, 1)), 
               end = as.numeric(tail(x$pos, 1)), 
               type = unique(x$markers), 
               group = unique(x$groups))
  })
  locations <- do.call("rbind", locations)
  return(as.data.frame(locations))
}
# getMarkersGroupsPositions(markers = etd$leftEventsData$IDT.Detection2$eventMarkers, 
#                          groups = etd$leftEventsData$IDT.Detection2$eventGroups)

getTrialsLocations <- function(ETD, trials = NA)
{
  markers <- ETD$commonData$trial
  groups <- getMarkersGroups(markers)
  trialsLocations <- getMarkersGroupsPositions(markers = markers, groups = groups)
  if (all(is.na(trials)))
  {
    specTrialsLocations <- trialsLocations
  }
  else
  {
    specTrialsLocations <- trialsLocations[trialsLocations$type %in% trials,]
  }
  specTrialsLocations$eventsClass <- "Trial"
  specTrialsLocations$detector <- NA
  return(specTrialsLocations)
}
# getTrialsLocations(etd, 1)

getEventsLocations <- function(ETD, eye, eventsClass = NA, eventTypes = NA, detector = NA)
{
  if (all(is.na(eventsClass)))
  {
    locations <- data.frame(start = 1, end = nrow(ETD$commonData),
                            type = NA, group = NA, 
                            eventsClass = NA, detector = NA)
  }
  else
  {
    eventMarkers <- getEventMarkers(ETD = ETD, eye = eye, eventsClass = eventsClass, detector = detector)
    if (eventsClass == "WindowEvents")
    {
      recordLength <- nrow(ETD$commonData)
      width <- eventMarkers$window$width
      overlapped <- eventMarkers$window$overlap
      notOverlapped <- (width-overlapped)
      numOfWindows <- (recordLength - width) %/% notOverlapped + 1
      startPositions <- (0:(numOfWindows-1))*notOverlapped
      endPositions <- startPositions + width
      locations <- data.frame(start = startPositions, 
                              end = endPositions, 
                              type = rep("Window", numOfWindows), 
                              group = 1:numOfWindows)
    }
    if (eventsClass == "Trials")
    {
      markers <- ETD$commonData$trial
      groups <- getMarkersGroups(markers)
    }
    else
    {
      markers <- eventMarkers$eventMarkers
      groups <- eventMarkers$eventGroups
    }
    locations <- getMarkersGroupsPositions(markers = markers, groups = groups)
    if (all(is.na(eventTypes)))
    {
      locations <- locations
    }
    else
    {
      specEvTypesFlags <- locations$type %in% eventTypes
      if (all(!specEvTypesFlags))
      {
        stop("Cannot find any of specified event types!")
      }
      locations <- locations[locations$type %in% eventTypes,]
    }
    locations$eventsClass <- eventsClass
    locations$detector <- detector
  }
  return(locations)
}
# getEventsLocations(ETD = etd, eye = "left", eventsClass = "OculomotorEvents", detector = "IDT.Detection2", eventTypes = "Fixation")

getEventsHits <- function(extEvLocations, intEvLocations)
{
  res <- lapply(1:nrow(intEvLocations), function(i1)
  {
    x <- intEvLocations[i1,]
    res <- lapply(1:nrow(extEvLocations), function(i2)
    {
      y <- extEvLocations[i2,]
      hit <- between(x = x$start, lower = y$start, upper = y$end, incbounds = T) & 
        between(x = x$end, lower = y$start, upper = y$end, incbounds = T)
      return(data.frame(extEventsClass = y$eventsClass, extDetector = y$detector,
                        extEventType = y$type, extEventGroup = y$group, 
                        intEventsClass = x$eventsClass, intDetector = x$detector,
                        intEventType = x$type, intEventGroup = x$group, 
                        hit = hit))
    })
    return(do.call("rbind", res))
  })
  hitsTable <- do.call("rbind", res)
  return(hitsTable)
}

getEventsInEvents <- function(intEventLocations, extEventLocations)
{
  if (all(is.na(extEventLocations)))
  {
    return(intEventLocations)
  }
  hitsRecords <- getEventsHits(extEvLocations = extEventLocations, intEvLocations = intEventLocations)
  hits <- hitsRecords[hitsRecords$hit, ]
  res <- data.frame(intEventLocations[intEventLocations$group %in% hits$intEventGroup, ])
  res$extEventType <- hits$extEventType
  res$extEventGroup <- hits$extEventGroup
  res$extEventsClass <- hits$extEventsClass
  res$extDetector <- hits$extDetector
  return(res)
}
getEventsInEvents(intEventLocations = getEventsLocations(ETD = etd, 
                                                         eye = "left", 
                                                         eventsClass = "OculomotorEvents", 
                                                         detector = "IDT.Detection2",
                                                         eventTypes = "Saccade"),
                  extEventLocations = getTrialsLocations(ETD = etd))


# TO DO: findEventsIntersections function
# which returns d.f. with ev1type, ev1gr, ev2type, ev2gr, intersectionStart, intersectionEnd
findEventsIntersections <- function(ETD, eye, trials = NA, events1, events2)
{
  
}

getChannelsDataSubset <- function(channelsData, location)
{
  start <- location$start
  end <- location$end
  return(channelsData[start:end,])
}

getRepresentationsDataSubset <- function(representationsData, location)
{
  start <- location$start
  end <- location$end
  if (nrow(representationsData) == 0) {return(data.frame())}
  idxs <- (representationsData$start >= start) & (representationsData$end <= end)
  return(representationsData[idxs,])
}

getDataSubset <- function(channelsData, representationsData, location, eye)
{
  channelsDataSubset <- getChannelsDataSubset(channelsData, location)
  representationsDataSubset <- getRepresentationsDataSubset(representationsData, location)
  return(list(eye = eye,
              eventsClass = location$eventsClass,
              detector = location$detector,
              eventType = location$type,
              eventGroup = location$group,
              start = location$start,
              end = location$end,
              channelsData = channelsDataSubset, 
              representationsData = representationsDataSubset))
}

getRepresentationsData <- function(ETD, eye)
{
  allReprData <- ETD$representations
  
  if (nrow(allReprData) == 0) 
  {
    return(data.frame())
  }
  
  representationsData <- allReprData[allReprData$eye == eye,]
  return(representationsData)
}


getDataSubsets <- function(ETD, eye, locations)
{
  channelsData <- getChannelsData(ETD = ETD, eye = eye)
  representationsData <- getRepresentationsData(ETD = ETD, eye = eye)
  locationsList <- split(locations, f = 1:nrow(locations))
  dataSubsets <- lapply(locationsList, FUN = getDataSubset, 
                        channelsData = channelsData, 
                        representationsData = representationsData,
                        eye = eye)
  return(dataSubsets)
}

rr <- getDataSubsets(ETD = etd, eye = "left", locations = getEventsLocations(ETD = etd, 
                                                                             eye = "left", 
                                                                             eventsClass = "OculomotorEvents", 
                                                                             detector = "IDT.Detection2",
                                                                             eventTypes = "Saccade"))
rr

# TO DO: FUNCTIONS THAT EXTRACTS PARTICULAR REPRESENTATIONS FROM REPRESENTATIONS TABLE
# e.g. duration parameter of specified fixations detected by IDT.Detection2
# i.e. one has to set representation/parameter name and also eventsClass, detector, eventType and possible eventGroups
getRepresentations <- function(representations, varName, eventsClass, detector, eventType, eventGroups)
{
  
}

evaluateSubFunctions <- function(dataSubsets, subFunctions)
{
  res <- lapply(dataSubsets, FUN = function(x)
  {
    res <- lapply(subFunctions, FUN = function(y) 
    {
        y$fun(x, y$settings)$vals
    })
    res <- unlist(res, recursive = F)
    varNames <- names(res)
    res <- cbind(eye = x$eye, 
                 eventsClass = x$eventsClass, 
                 detector = x$detector, 
                 eventType = x$eventType, 
                 eventGroup = x$eventGroup, 
                 start = x$start,
                 end = x$end,
                 varName = varNames,
                 value = res)
    res
  })
  res <- do.call("rbind", res)
  rownames(res) <- NULL
  return(as.data.frame(res))
}
etd$representations <- evaluateSubFunctions(dataSubsets = rr,
                            subFunctions = list(subFunctions$duration,
                                                subFunctions$onset,
                                                subFunctions$offset,
                                                subFunctions$repres))
unlist(etd$representations[6,]$value, recursive = F)
