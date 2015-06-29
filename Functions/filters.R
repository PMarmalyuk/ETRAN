standardFilter <- function(t, x, y, screenDim, interpolate)
{
  markers <- new(Class = "FilterMarkers")
  markers1 <- ifelse(x == 0 & y == 0, markers@markerNames$zeroes, markers@markerNames$ok)
  if (!is.na(screenDim)[1])
  {
    markers2 <- ifelse(x > screenDim[1] | y > screenDim[2], markers@markerNames$outOfBounds, markers@markerNames$ok)
    markers1[which(markers1 == markers@markerNames$ok)] <- markers2[which(markers1 == markers@markerNames$ok)]
  }
  markers@filterMarkers <- markers1
  if (interpolate)
  {
    gapMarkers <- ifelse(markers@filterMarkers != markers@markerNames$ok, "GAP", "NOT GAP")
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
  res <- list(t = t, x = x, y = y, filterMarkers = markers)
  return(res)
}

## CORE FILTER ##
# data filter: finds (0,0)s and outOfBounds samples 
# and marks them according to markerNames data in FilterMarkers object
coreFilter <- function(DataRecord, settings)
{
  scrDim <- DataRecord@eyesDataObject@conditions@conditions$screenDim
  interpolate <- settings$interpolate
  t <- DataRecord@eyesDataObject@time@time
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    res <- standardFilter(t, leftX, leftY, scrDim, interpolate)
    if (interpolate)
    {
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- res$x
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory <- res$y
    }
    DataRecord@eyesDataObject@leftFilterMarkers <- res$filterMarkers
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    res <- standardFilter(t, rightX, rightY, scrDim, interpolate)
    if (interpolate)
    {
      DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx <- res$x
      DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory <- res$y
    }
    DataRecord@eyesDataObject@rightFilterMarkers <- res$filterMarkers
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    resLeft <- standardFilter(t, leftX, leftY, scrDim, interpolate)
    resRight <- standardFilter(t, rightX, rightY, scrDim, interpolate)
    if (interpolate)
    {
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- resLeft$x
      DataRecord@eyesDataObject@leftEyeSamples@eyeData$port <- resLeft$y
      DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx <- resRight$x
      DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory <- resRight$y
    }
    DataRecord@eyesDataObject@leftFilterMarkers <- resLeft$filterMarkers
    DataRecord@eyesDataObject@rightFilterMarkers <- resRight$filterMarkers
  }
  return(DataRecord)
}