

# INTERPOLATION IS IMPLEMENTED SUCH THAT FILTER EATS MEMORY EXPONENTIALLY!!!!
standardFilter <- function(t, x, y, settings, filterMarkerNames, interpolate)
{
  screenResolution <- settings$screenResolution
  okMarker <- filterMarkerNames$okMarker
  gapMarker <- filterMarkerNames$gapMarker
  artMarker <- filterMarkerNames$artMarker
  
  markers1 <- ifelse(x == 0 & y == 0, gapMarker, okMarker)
  if (!is.na(screenResolution)[1])
  {
    markers2 <- ifelse(x > screenResolution[1] | y > screenResolution[2], artMarker, okMarker)
    markers1[which(markers1 == okMarker)] <- markers2[which(markers1 == okMarker)]
  }
  markers <- markers1
  if (interpolate)
  {
    gapMarkers <- ifelse(markers != okMarker, gapMarker, okMarker)
    gapMarks <- data.frame(firstGap = gapMarkers[-length(gapMarkers)], secondGap = gapMarkers[-1])
    transitions <- apply(gapMarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
    group <- c(1,cumsum(transitions)+1)
    data <- data.frame(t, x, y, group)
    lastGroup <- group[length(group)]
    gapGroups <- unique(group[which(gapMarkers == gapMarker)])
    if (length(gapGroups) != 0)
    {
      data2 <- lapply(gapGroups, FUN = function(x)
      {
        if (x == 1) 
        {
          smpCnt <- nrow(data[data$group == x,])
          samplesAfterGap <- data[data$group == x + 1,]
          firstSampleAfterGap <- samplesAfterGap[1,]
          newX <- rep(firstSampleAfterGap$x, smpCnt)
          newY <- rep(firstSampleAfterGap$y, smpCnt)
        } 
        if (x == lastGroup) 
        {
          smpCnt <- nrow(data[data$group == x,])
          samplesBeforeGap <- data[data$group == x - 1,]
          lastSampleBeforeGap <- samplesBeforeGap[nrow(samplesBeforeGap),]
          newX <- rep(lastSampleBeforeGap$x, smpCnt)
          newY <- rep(lastSampleBeforeGap$y, smpCnt)
        }
        if (x != 1 && x != lastGroup) 
        {
          samplesAfterGap <- data[data$group == x + 1,]
          firstSampleAfterGap <- samplesAfterGap[1,]
          samplesBeforeGap <- data[data$group == x - 1,]
          lastSampleBeforeGap <- samplesBeforeGap[nrow(samplesBeforeGap),]
          ts <- data[data$group == x,1]
          ts2 <- c(lastSampleBeforeGap$t, ts, firstSampleAfterGap$t)
          dts <- (ts2[-1]-ts2[-length(ts2)])/(firstSampleAfterGap$t-lastSampleBeforeGap$t)
          dPos <- list(dx = firstSampleAfterGap$x-lastSampleBeforeGap$x, dy = firstSampleAfterGap$y-lastSampleBeforeGap$y)
          newX <- lastSampleBeforeGap$x+dPos$dx*cumsum(dts)[1:length(ts)]
          newY <- lastSampleBeforeGap$y+dPos$dy*cumsum(dts)[1:length(ts)]
        }
        data[data$group == x,2] <- newX
        data[data$group == x,3] <- newY
        data
      })
      filteredData <- do.call("rbind", data2)
      data[rownames(filteredData),] <- filteredData
      t <- data$t; x <- data$x; y <- data$y
    }
  }
  markersDF <- cbind(markers[-length(markers)], markers[-1])
  transitions <- apply(markersDF, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  eventGroups <- c(1,cumsum(transitions)+1)
  markers <- factor(markers, levels = filterMarkerNames)
  res <- list(t = t, x = x, y = y, filterEventMarkers = list(eventMarkers = markers, 
                                                             eventGroups = eventGroups,
                                                             eventsClass = "FilterEvents", 
                                                             detector = "StandardFilter"))
  return(res)
}

dataFilter <- function(ETD, filterMarkerNames, interpolate)
{
  mode <- etd$settings$mode
  if (mode == "left" | mode == "binocular")
  {
    filtResLeft <- standardFilter(t = ETD$commonData$time, x = ETD$leftEyeData$porx, y = ETD$leftEyeData$pory,
                                  settings = ETD$settings, filterMarkerNames = filterMarkerNames, interpolate = interpolate) 
    if (interpolate)
    {
      ETD$leftEyeData$porx <- filtResLeft$x
      ETD$leftEyeData$pory <- filtResLeft$y
    }
    if (all(is.na(ETD$leftEventsData)))
    {
      ETD$leftEventsData <- list(filterEventMarkers = filtResLeft$filterEventMarkers)
    }
    else
    {
      ETD$leftEventsData <- append(ETD$leftEventsData, list(filterEventMarkers = filtResLeft$filterEventMarkers))
    }
  }
  if (mode == "right" | mode == "binocular")
  {
    filtResRight <- standardFilter(t = ETD$commonData$time, x = ETD$rightEyeData$porx, y = ETD$rightEyeData$pory,
                                   settings = ETD$settings, filterMarkerNames = filterMarkerNames, interpolate = interpolate)
    if (interpolate)
    {
      ETD$rightEyeData$porx <- filtResRight$x
      ETD$rightEyeData$pory <- filtResRight$y
    }
    if (all(is.na(ETD$rightEventsData)))
    {
      ETD$rightEventsData <- list(filterEventMarkers = filtResRight$filterEventMarkers)
    }
    else
    {
      ETD$rightEventsData <- append(ETD$rightEventsData, list(filterEventMarkers = filtResRight$filterEventMarkers))
    }
  }
  return(ETD)
}

