getEventsHits <- function(extEvLocations, intEvLocations, 
                          splitBy = F)
{
  res <- lapply(1:nrow(extEvLocations), function(extEvNum)
  {
    y <- extEvLocations[extEvNum,]
    x <- intEvLocations
    hits <- between(x = x$start, lower = y$start, 
                    upper = y$end, incbounds = T) & 
      between(x = x$end, lower = y$start, 
              upper = y$end, incbounds = T)
    if (any(hits))
    {
      extEvInfo <- y[,-which(names(y) %in% c("eye", "start", 
                                             "end", "event"))]
      locs <- data.frame(intEvLocations[hits,], extEvent = y$event, 
                         extEvInfo, row.names = NULL)
    } else locs <- NULL
    return(locs)
  })

  res <- rbindlist(res)
  class(res) <- "data.frame"
  # res <- do.call("rbind", res)
  if (splitBy == "group")
  {
    res <- split(res, f = 1:nrow(res))
  }
  if (splitBy == "type")
  {
    res <- split(res, f = res$extEvent)
  }
  return(res)
}

getChannelsData <- function(ETD, eye, excludeFiltered = T, 
                            okMarker = "Ok")
{
  makeFilteredNA <- function(eyeData, filterEvents, okMarker)
  {
    f <- filterEvents
    fNotOk <- f[f$event != okMarker,]
    for (i in 1:nrow(fNotOk))
    {
      eyeData[(fNotOk$start[i]):(fNotOk$end[i]), ] <- NA
    }
    return(eyeData)
  }

  if (eye == "left" & ETD$settings$mode == "right") 
    {stop("No data for left eye!"); return(NA)}
  if (eye == "right" & ETD$settings$mode == "left") 
    {stop("No data for right eye!"); return(NA)}
  if (eye == "left")
  {
    eyeData <- ETD$leftEyeData
    filterEvents <- ETD$leftEvents$filter
    if (excludeFiltered)
    {
      eyeData <- makeFilteredNA(eyeData = eyeData,
                                filterEvents = filterEvents,
                                okMarker = okMarker)
    }
    channelsData <- data.frame(ETD$commonData, 
                               eyeData)
  }
  if (eye == "right")
  {
    eyeData <- ETD$rightEyeData
    filterEvents <- ETD$rightEvents$filterMarkers
    if (excludeFiltered)
    {
      eyeData <- makeFilteredNA(eyeData = eyeData,
                                filterEvents = filterEvents,
                                okMarker = okMarker)
    }
    channelsData <- data.frame(ETD$commonData, 
                               ETD$eyeData)
  }
  return(channelsData)
}

getDataSubset <- function(channelsData, location, eye)
{
  channelsDataSubset <- channelsData[location$start:location$end, ]
  return(channelsDataSubset)
}

getDataSubsets <- function(ETD, eye, locations, 
                           excludeFiltered = T, 
                           okMarker = "Ok")
{
  channelsData <- getChannelsData(ETD = ETD, eye = eye, 
                                  excludeFiltered = excludeFiltered, 
                                  okMarker = okMarker)
  locationsList <- split(locations, f = 1:nrow(locations))
  data <- lapply(locationsList, FUN = getDataSubset, 
                 channelsData = channelsData, 
                 eye = eye)
  return(data)
}