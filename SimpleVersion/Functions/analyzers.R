eventsCounter <- function(events)
{
  locations <- events$locations
  query <- events$query
  if (length(query) == 0) {src <- NA; eventsClass <- NA; detector <- NA} 
  if (length(query) == 1) {src <- NA; eventsClass <- NA; detector <- NA}
  else 
  {
    # data source for counting events is determined by all queries before last query
    src <- head(query, length(query)-1)
    # if last query did not specify eventType, then we are dealing with locations of all event types 
    # and therefore should not drop absent levels when counting
    
    # if eventType has been specified by user then we need to drop absent levels when counting due to user's will
    lastQuery <- unlist(tail(query, 1), recursive = F)
    if (is.null(lastQuery$eventType))
    {
      dropAbsentEventTypes <- F
    }
    else
    {
      if (all(is.na(lastQuery$eventType)))
      {
        dropAbsentEventTypes <- F
      }
      else dropAbsentEventTypes <- T
    }
  }
  # counting phase
  splittedLocations <- split(locations, f = locations$type, drop = dropAbsentEventTypes)
  types <- names(splittedLocations)

  res <- lapply(1:length(splittedLocations),
                FUN = function(i) {
                  cnt <- nrow(splittedLocations[[i]])
                  res <- data.frame(eventsClass = lastQuery$eventsClass,
                                    detector  = lastQuery$detector,
                                    eventType = types[[i]],
                                    count = cnt,
                                    src = I(list(src)))
                  return(res)
                })
  res <- do.call("rbind", res)
  eventCounts <- data.frame(eventsClass = res$eventsClass,
                            detector = res$detector,
                            eventType = res$eventType,
                            varName = "count",
                            value = res$count,
                            src = res$src)
  return(eventCounts)
}
# qlist <- list(createQuery(eventsClass = "Trial", eye = "left", eventType = c(1)),
#               createQuery(eventsClass = "OculomotorEvents",eye = "left", eventType = c("Fixation", "Saccade")))
# selectedEvents <- selectEvents(ETD = etd, eye = "left", 
#                                queriesList = qlist,
#                                includeQuery = T)
# eventsCounter(ETD = etd, eye = "left", selectedEvents = selectedEvents)

# res <- lapply(1:2, FUN = function(x){
#   qlist <- list(createQuery(eventsClass = "Trial", eventType = x),
#                 createQuery(eventsClass = "OculomotorEvents", eventType = "Fixation")
#   )
#   eventsCounter(ETD = etd, eye = "left", queriesList = qlist)
# })
# res <- do.call("rbind", res)


evaluateSubFunctions <- function(dataSubsets, subFunctions, commonSettings)
{
  res <- lapply(dataSubsets, FUN = function(x)
  {
    res <- lapply(subFunctions, FUN = function(y) 
    {
      settings <- append(y$settings, commonSettings)
      y$fun(x, settings)$vals
    })
    varNames <- unlist(lapply(res, names))
    res <- unlist(res, recursive = F)
    res <- data.frame(eye = x$eye, 
                      eventsClass = x$eventsClass, 
                      detector = x$detector, 
                      eventType = x$eventType, 
                      eventGroup = x$eventGroup, 
                      start = x$start,
                      end = x$end,
                      varName = varNames,
                      value = I(res),
                      stringsAsFactors = F)
    res
  })
  res <- do.call("rbind", res)
  rownames(res) <- NULL
  return(res)
}

allFixQuery <- createQuery(eye = "left",
                           eventsClass = "OculomotorEvents",
                           detector = "IDT.Detection2",
                           eventType = "Fixation")
tr1Query <- createQuery(eye = "left",
                        eventsClass = "Trial",
                        eventType = 1)

allFixLocs <- selectEvents(ETD = etd, 
                           eye = "left", 
                           queriesList = list(tr1Query, allFixQuery), 
                           includeQuery = T)

allFixData <- getDataSubsets(ETD = etd, 
                             eye = "left", 
                             locations = allFixLocs$locations,
                             excludeFiltered = F)

etd$representations <- evaluateSubFunctions(dataSubsets = allFixData,
                                            subFunctions = list(subFunctions$duration,
                                                                subFunctions$pupilMeanAndSD),
                                            commonSettings = etd$settings)

statsEstimator <- function(ETD, eye, events, varNames)
{
  selectedRepresentations <- getRepresentations(ETD = ETD, 
                                                eye = eye, 
                                                varNames = varNames, 
                                                events = events)
  if (length(events$query) == 0) {src <- NA; eventsClass <- NA; detector <- NA} 
  if (length(events$query) == 1) {src <- NA; eventsClass <- NA; detector <- NA}
  else 
  {
    # data source for counting events is determined by all queries before last query
    src <- head(events$query, length(events$query)-1)
  }
  
  vars <- unique(selectedRepresentations$varName)
  res <- lapply(vars, FUN = function(x)
  {
    currentVariableData <- selectedRepresentations[selectedRepresentations$varName == x, ]
    selectedEventTypes <- unique(currentVariableData$eventType)
    
    res <- lapply(selectedEventTypes, FUN = function(y)
    {
      dataForEventType <- currentVariableData[currentVariableData$eventType == y, ]
      values <- unlist(dataForEventType$value, recursive = F)
      if (length(values) <= 1)
      {
        return(data.frame())
      }
      firstValue <- head(values, 1)
      if (is.atomic(firstValue) & !all(is.null(values)))
      {
        values <- values[!is.na(values) & !is.null(values) & !is.nan(values)]
        if (class(firstValue) %in% c("numeric", "integer"))
        {
          funs <- list(sum = sum, min = min, max = max, 
                       mean = mean, median = median, 
                       sd = sd, var = var,
                       skewness = function(x) {skewness(x)}, 
                       kurtosis = function(x) {kurtosis(x)},
                       range = function(x) {max(x) - min(x)},
                       quartile1 = function(x) {quantile(x, probs = 0.25)},
                       quartile3 = function(x) {quantile(x, probs = 0.75)},
                       IQR = function(x) {quantile(x, probs = 0.75) - 
                           quantile(x, probs = 0.25)})
        }
        else
        if (class(firstValue) == "factor")
        {
          funs <- list(mode = function(x) {tail(sort(table(x)),1)},
                       mode.count = function(x) {Mode <- tail(sort(table(x)),1)
                       return(length(which(table(x) == Mode)))},
                       mode.percent = function(x) {Mode <- tail(sort(table(x)),1)
                       return(Mode/sum(x))},
                       entropy = function(x) {shannon.entropy(table(x))})
        }
        else
        {
          return(data.frame())
        }
        varNames <- names(funs)
        res <- lapply(1:length(funs), FUN = function(z) 
        {
          data.frame(eventsClass = unique(dataForEventType$eventsClass),
                     detector = unique(dataForEventType$detector),
                     eventType = unique(dataForEventType$eventType),
                     varName = paste0(x,".",varNames[z]),
                     value = I(list(funs[[z]](x = values))),
                     src = I(list(src)),
                     stringsAsFactors = F)
        })
        res <- do.call("rbind", res)
        return(res)
      }
      else
      {
        return(data.frame())
      }
    })
    res <- do.call("rbind", res)
  })
  res <- do.call("rbind", res)
  counts <- eventsCounter(events)
  res <- rbind(res, counts)
  rownames(res) <- NULL
  res
}
rr3 <- statsEstimator(ETD = etd, eye = "left", 
                      events = allFixLocs,
                      varNames = c("duration"))
statsEstimator(ETD = etd, eye = "left", events = allFixLocs, varNames = "duration")
boxplot(log(unlist(etd$representations[etd$representations$varName == "duration" &
                                  etd$representations$eventType == "Fixation"  ,]$value, recursive = F)))

# TO DO: function that appends new representations to representations tables
# TO DO: function that appends new statistics to descriprives tables

# TO DO: function getVarValues that get values of a specific variable (in representations or descriprives table) 
## for a specific event and return them as a vector/list

# TO DO: function getVarsValues that get values of a specific variables (in representations or descriprives table) 
## for a specific event and return them as a data.frame (using getVarValues function)

# TO DO: develop new sub functions for calculcation of complex representations
