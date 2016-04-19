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
# getAvailableEventsClasses(ETD=etd, eye = "left")

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

getEventMarkers <- function(ETD, eye, eventsClass, detector = NA)
{
  if (is.null(detector)) {detector <- NA}
  if (eventsClass == "Trial")
  {
    markers <- ETD$commonData$trial
    eventMarkers <- list(eventMarkers = markers,
                         eventGroups = getMarkersGroups(markers),
                         eventsClass = "Trial",
                         detector = NA)
    return(eventMarkers)
  }
  else
  {
    if (eye == "left" & ETD$settings$mode == "right") {stop("No data for left eye!"); return(NA)}
    if (eye == "right" & ETD$settings$mode == "left") {stop("No data for right eye!"); return(NA)}
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
# getEventMarkers(ETD = etd, eye = "left", eventsClass = "Trial")

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

getSubsetsLocations <- function(ETD, eye, eventsClass = NA, eventTypes = NA, detector = NA)
{
  subsetsDefinition <- list()
  subsetsDefinition$eye <- eye
  subsetsDefinition$eventTypes <- eventTypes
  if (is.null(eventTypes)) {eventTypes <- NA}
  # if eventsDefinition is NA then we return location of the whole data record
  if (is.na(eventsClass))
  {
    locations <- data.frame(start = 1, end = nrow(ETD$commonData),
                            type = NA, group = NA, 
                            eventsClass = NA, detector = NA)
    subsetsDefinition$eventsClass <- NA
    subsetsDefinition$detector <- NA
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
      startPositions <- (0:(numOfWindows-1))*notOverlapped + 1
      endPositions <- startPositions + width
      locations <- data.frame(start = startPositions, 
                              end = endPositions, 
                              type = rep("Window", numOfWindows), 
                              group = 1:numOfWindows)
    }
    else
    {
        markers <- eventMarkers$eventMarkers
        groups <- eventMarkers$eventGroups
        eventsClass <- eventMarkers$eventsClass
        locations <- getMarkersGroupsPositions(markers = markers, groups = groups)
    }
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
    locations$detector <- eventMarkers$detector
    subsetsDefinition$eventsClass <- eventsClass
    subsetsDefinition$detector <- eventMarkers$detector
  }
  res <- list(query = list(subsetsDefinition), locations = locations)
  return(res)
}  

# etd$leftEventsData$Window1 <- createWindow("YoWindow", width = 1000, overlap = 0)
# getSubsetsLocations(ETD = etd, eye = "left", eventsClass = "WindowEvents")
# getSubsetsLocations(ETD = etd, eye = "left", eventTypes = "Fixation", eventsClass = "OculomotorEvents")

getEventsHitsLocations <- function(extEvLocations, intEvLocations, misses = F, hits = T, showExt = T)
{
  res <- lapply(1:nrow(intEvLocations), function(i1)
  {
    x <- intEvLocations[i1,]
    res <- lapply(1:nrow(extEvLocations), function(i2)
    {
      y <- extEvLocations[i2,]
      hit <- between(x = x$start, lower = y$start, upper = y$end, incbounds = T) & 
        between(x = x$end, lower = y$start, upper = y$end, incbounds = T)
      if (showExt)
      {
        return(data.frame(extEventsClass = y$eventsClass, extDetector = y$detector,
                          extEventType = y$type, extEventGroup = y$group, 
                          eventsClass = x$eventsClass, detector = x$detector,
                          eventType = x$type, eventGroup = x$group, 
                          hit = hit))
      }
      else
      {
        return(data.frame(eventsClass = x$eventsClass, detector = x$detector,
                          eventType = x$type, eventGroup = x$group, 
                          hit = hit))
      }
    })
    return(do.call("rbind", res))
  })
  hitsTable <- do.call("rbind", res)
  if (hits & misses) {res <- hitsTable}
  if (misses & !hits) {res <- hitsTable[!hitsTable$hit, ]}
  if (!misses & hits) {res <- hitsTable[hitsTable$hit, ]}
  if (!hits & !misses) {stop("Nothing to return! Set either hits or misses to TRUE state!")}
  locations <- data.frame(intEvLocations[intEvLocations$group %in% res$eventGroup, ])
  locations$extEventsClass <- res$extEventsClass
  locations$extDetector <- res$extDetector
  locations$extEventType <- res$extEventType
  locations$extEventGroup <- res$extEventGroup
  return(locations)
}
# getEventsHitsLocations(extEvLocations = getSubsetsLocations(ETD = etd, eye = "left",
#                                                   eventsClass = "Trial",
#                                                   eventTypes = 1)$locations,
#               intEvLocations = getSubsetsLocations(ETD = etd, 
#                                                   eye = "left", 
#                                                   eventsClass = "OculomotorEvents", 
#                                                   detector = "IDT.Detection2", 
#                                                   eventTypes = "Fixation")$locations,
#               misses = F,
#               hits = T,
#               showExt = T
#               )

selectEvents <- function(ETD, eye, queries = NA)
{
  locations <- getSubsetsLocations(ETD = etd, eye = eye)
  if (all(!is.na(queries)))
  {
    lapply(queries, FUN = function(x)
    {
      
      intLocations <- getSubsetsLocations(ETD = etd, eye = eye, 
                                          eventsClass = x$eventsClass,
                                          detector = x$detector,
                                          eventTypes = x$eventTypes)
      locations$locations <<- getEventsHitsLocations(extEvLocations = locations$locations, 
                                                     intEvLocations = intLocations$locations, 
                                                     misses = F, 
                                                     hits = T,
                                                     showExt = F)
      locations$query <<- append(locations$query, list(x))
    })
  }
  return(locations)
}
# evsel <- selectEvents(ETD = etd, eye = "left", queries = list(list(eventsClass = "Trial"),
#                                                               list(eventsClass = "WindowEvents"),
#                                                               list(eventsClass = "OculomotorEvents",
#                                                                    eventTypes = "Fixation")
# ))

getChannelsDataSubset <- function(channelsData, location, excludeFiltered = T, okMarker = "Ok")
{
  start <- location$start
  end <- location$end
  channelsDataSubset <- channelsData[start:end,]
  if (excludeFiltered)
  {
    channelsDataSubset <- channelsDataSubset[channelsDataSubset$filterMarkers == okMarker, ]
  }
  return(channelsDataSubset)
}

getRepresentationsDataSubset <- function(representationsData, location)
{
  start <- location$start
  end <- location$end
  if (nrow(representationsData) == 0) {return(data.frame())}
  idxs <- (representationsData$start >= start) & (representationsData$end <= end)
  representationsDataSubset <- representationsData[idxs,]
  return()
}

getDataSubset <- function(channelsData, representationsData, location, eye, excludeFiltered = T, okMarker = "Ok")
{
  channelsDataSubset <- getChannelsDataSubset(channelsData, location, excludeFiltered, okMarker)
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

getChannelsData <- function(ETD, eye)
{
  if (eye == "left" & ETD$settings$mode == "right") {stop("No data for left eye!"); return(NA)}
  if (eye == "right" & ETD$settings$mode == "left") {stop("No data for right eye!"); return(NA)}
  
  #if (!is.na(trials)) {readFlags <- ETD$commonData$trial %in% trials} else {readFlags <- T}
  
  if (eye == "left")
  {
    channelsData <- data.frame(ETD$commonData, 
                               ETD$leftEyeData,
                               list(filterMarkers = ETD$leftEventsData$filterEventMarkers$eventMarkers))
  }
  if (eye == "right")
  {
    channelsData <- data.frame(ETD$commonData, 
                               ETD$rightEyeData,
                               list(filterMarkers = ETD$rightEventsData$filterEventMarkers$eventMarkers))
  }
  return(channelsData)
}
# getChannelsData(etd, eye = "left")[1:10,]$filterMarkers

getRepresentationsData <- function(ETD, eye)
{
  allReprData <- ETD$representations
  if (all(is.na(allReprData)))
  {
    return(data.frame())
  }
  if (nrow(allReprData) == 0) 
  {
    return(data.frame())
  }
  
  representationsData <- allReprData[allReprData$eye == eye,]
  return(representationsData)
}

getDataSubsets <- function(ETD, eye, locations, excludeFiltered = T, okMarker = "Ok")
{
  channelsData <- getChannelsData(ETD = ETD, eye = eye)
  representationsData <- getRepresentationsData(ETD = ETD, eye = eye)
  locationsList <- split(locations, f = 1:nrow(locations))
  dataSubsets <- lapply(locationsList, FUN = getDataSubset, 
                        channelsData = channelsData, 
                        representationsData = representationsData,
                        eye = eye, excludeFiltered, okMarker)
  return(dataSubsets)
}
getDataSubsets(ETD = etd, eye = "left", locations = evsel$locations)

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
# allSaccadesLocs <- getEventsLocations(ETD = etd, 
#                                       eye = "left", 
#                                       eventsClass = "OculomotorEvents", 
#                                       detector = "IDT.Detection2",
#                                       eventTypes = "Saccade")
# allSaccadesData <- getDataSubsets(ETD = etd, 
#                                   eye = "left", 
#                                   locations = allSaccadesLocs)
# evaluateSubFunctions(dataSubsets = allSaccadesData,
#                      subFunctions = list(subFunctions$duration,
#                                          subFunctions$onset,
#                                          subFunctions$offset,
#                                          subFunctions$repres))
# 
# 

# plot(etd$leftEyeData$porx, col = etd$leftEventsData$ExtEvent1$eventMarkers)
# trial1Locs <- getEventsLocations(ETD = etd, 
#                                  eye = "left", 
#                                  eventsClass = "Trial",
#                                  eventTypes = 1)
# allFixationsLocs <- getEventsLocations(ETD = etd, 
#                                        eye = "left", 
#                                        eventsClass = "OculomotorEvents", 
#                                        detector = "IDT.Detection2",
#                                        eventTypes = "Fixation")
# etd$leftEventsData$ExtEvent1
# allEvent1Locs <- getEventsLocations(ETD = etd, 
#                                     eye = "left", 
#                                     eventsClass = "MyArtificialEvents", 
#                                     detector = "ByHandDetector",
#                                     eventTypes = "Event1")
# allFixationsInTrial1 <- getEventsHitsLocations(extEvLocations = trial1Locs,
#                                                intEvLocations = allFixationsLocs,
#                                                misses = F,
#                                                hits = T)
# testlocs <- getEventsHitsLocations(extEvLocations = allFixationsInTrial1,
#                        intEvLocations = allEvent1Locs,
#                        misses = F,
#                        hits = T)

# fixationsIn2TrialData <- getDataSubsets(ETD = etd, eye = "left", 
#                                         locations = getEventsHitsLocations(extEvLocations = trial2Locs,
#                                                                            intEvLocations = allFixationsLocs,
#                                                                            misses = F,
#                                                                            hits = T)
# )

rrr <- evaluateSubFunctions(dataSubsets = getDataSubsets(ETD = etd, eye = "left", locations = evsel$locations),
                            subFunctions = list(subFunctions$duration,
                                                subFunctions$onset,
                                                subFunctions$offset,
                                                subFunctions$repres))


# evaluateSubFunctions(dataSubsets = fixationsIn2TrialData,
#                      subFunctions = list(subFunctions$duration,
#                                          subFunctions$onset,
#                                          subFunctions$offset,
#                                          subFunctions$repres))
# unlist(etd$representations[6,]$value, recursive = F)
