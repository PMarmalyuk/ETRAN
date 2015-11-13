createFilter <- function(name, fun, settings)
{
  filter <- new(Class = "Filter", name = name, fun = fun, settings = settings)
  return(filter)
}

# noFilter INDEPENDENT OF ETRAN CLASSES:
noFilter <- function(t,x,y,settings)
{
  markerNames <- settings$filterMarkerNames
  filterMarkers <- rep(markerNames$ok, length(t))
  res <- list(t = t, x = x, y = y, filterMarkers = filterMarkers)
  return(res)
}

# standardFilter INDEPENDENT OF ETRAN CLASSES:
standardFilter <- function(t, x, y, settings)
{
  screenRes <- settings$screenResolution
  interpolate <- settings$interpolate
  markerNames <- settings$filterMarkerNames
  markers1 <- ifelse(x == 0 & y == 0, markerNames$zeroes, markerNames$ok)
  if (!is.na(screenRes)[1])
  {
    markers2 <- ifelse(x > screenRes[1] | y > screenRes[2], markerNames$outOfBounds, markerNames$ok)
    markers1[which(markers1 == markerNames$ok)] <- markers2[which(markers1 == markerNames$ok)]
  }
  filterMarkers <- markers1
  if (interpolate)
  {
    gapMarkers <- ifelse(filterMarkers != markerNames$ok, "GAP", "NOT GAP")
    gapMarks <- data.frame(firstGap = gapMarkers[-length(gapMarkers)], secondGap = gapMarkers[-1])
    transitions <- apply(gapMarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
    group <- c(1,cumsum(transitions)+1)
    data <- data.frame(t, x, y, group)
    lastGroup <- group[length(group)]
    gapGroups <- unique(group[which(gapMarkers == "GAP")])
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
        data[data$group == x,]
      }
      )
      filteredData <- do.call("rbind", data2)
      data[rownames(filteredData),] <- filteredData
      t <- data$t; x <- data$x; y <- data$y
    }
  }
  res <- list(t = t, x = x, y = y, filterMarkers = filterMarkers)
  return(res)
}

## CORE FILTER ##
# data filter: finds (0,0)s and outOfBounds samples 
# and marks them according to markerNames data in FilterMarkers object
coreFilter <- function(DataRecord, settings)
{
  interpolate <- settings$interpolate
  filter <- settings$subfun
  t <- DataRecord@eyesDataObject@time@time
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    res <- filter(t, leftX, leftY, settings)
    if (interpolate)
    {
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- res$x
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory <- res$y
    }
    fMarkers <- new(Class = "FilterMarkers", markerNames = settings$filterMarkerNames, filterMarkers = res$filterMarkers)
    DataRecord@eyesDataObject@leftFilterMarkers <- fMarkers
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
    fMarkers <- new(Class = "FilterMarkers", markerNames = settings$filterMarkerNames, filterMarkers = res$filterMarkers)
    DataRecord@eyesDataObject@rightFilterMarkers <- fMarkers
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
    leftFMarkers <- new(Class = "FilterMarkers", markerNames = settings$filterMarkerNames, filterMarkers = resLeft$filterMarkers)
    DataRecord@eyesDataObject@leftFilterMarkers <- leftFMarkers
    rightFMarkers <- new(Class = "FilterMarkers", markerNames = settings$filterMarkerNames, filterMarkers = resRight$filterMarkers)
    DataRecord@eyesDataObject@rightFilterMarkers <- rightFMarkers
  }
  return(DataRecord)
}