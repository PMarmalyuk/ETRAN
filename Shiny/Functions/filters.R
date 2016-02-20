createFilter <- function(id, name, description, fun, settings)
{
  filter <- new(Class = "FilterEventDetector", id = id, name = name, fun = fun, description = description, settings = settings)
  return(filter)
}

# noFilter INDEPENDENT OF ETRAN CLASSES
noFilter <- function(t,x,y,settings)
{
  okMarker <- 1; gapMarker <- 2; artMarker <- 3
  markers <- rep(okMarker, length(t))
  res <- list(t = t, x = x, y = y, eventMarkers = markers)
  return(res)
}

# standardFilter INDEPENDENT OF ETRAN CLASSES
standardFilter <- function(t, x, y, settings)
{
  okMarker <- 1; gapMarker <- 2; artMarker <- 3
  
  screenRes <- settings$screenResolution
  interpolate <- settings$interpolate
  
  markers1 <- ifelse(x == 0 & y == 0, gapMarker, okMarker)
  if (!is.na(screenRes)[1])
  {
    markers2 <- ifelse(x > screenRes[1] | y > screenRes[2], artMarker, okMarker)
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
      }
      )
      filteredData <- do.call("rbind", data2)
      data[rownames(filteredData),] <- filteredData
      t <- data$t; x <- data$x; y <- data$y
    }
  }
  res <- list(t = t, x = x, y = y, eventMarkers = markers)
  return(res)
}

## CORE FILTER ##
# data filter: finds ok, gap (0,0-s) and artifact samples
# and marks them according to markersDefinition located inside settings list
coreFilter <- function(DataRecord, settings)
{
  interpolate <- ifelse(length(settings$interpolate) != 0, settings$interpolate, F)
  filter <- settings$subfun
  filterID <- settings$filterID
  t <- DataRecord@eyesDataObject@time@time
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    res <- filter(t, leftX, leftY, settings)
    filterEventMarkers <- new(Class = "FilterEventMarkers", markers = res$eventMarkers, eventClass = "FilterEvent")
    if (interpolate)
    {
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- res$x
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory <- res$y
    }
    DataRecord@eyesDataObject@leftEventsMarkers$filterMarkers <- filterEventMarkers
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    res <- filter(t, rightX, rightY, settings)
    if (interpolate)
    {
      DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx <- res$x
      DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory <- res$y
    }
    filterEventMarkers <- new(Class = "FilterEventMarkers", markers = res$eventMarkers, eventClass = "FilterEvent")
    DataRecord@eyesDataObject@rightEventsMarkers$filterMarkers <- filterEventMarkers
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    resLeft <- filter(t, leftX, leftY, settings)
    resRight <- filter(t, rightX, rightY, settings)
    if (interpolate)
    {
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- resLeft$x
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$port <- resLeft$y
      DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx <- resRight$x
      DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory <- resRight$y
    }
    leftFilterEventMarkers <- new(Class = "FilterEventMarkers", markers = resLeft$eventMarkers, eventClass = "FilterEvent")
    rightFilterEventMarkers <- new(Class = "FilterEventMarkers", markers = resRight$eventMarkers, eventClass = "FilterEvent")
    DataRecord@eyesDataObject@leftEventsMarkers$filterMarkers <- leftFilterEventMarkers
    DataRecord@eyesDataObject@leftEventsMarkers$filterMarkers <- rightFilterEventMarkers
  }
  return(DataRecord)
}