markersGroups <- function(markers) 
{
  transitions <- ifelse(markers[-length(markers)] 
                        != markers[-1], 1,0)
  groups <- c(1,cumsum(transitions)+1)
  return(groups)
}

# interpolation
interpolate <- function(t, x0, x1, y0, y1, t0, t1)
{
  dx <- x1-x0
  dy <- y1-y0
  dt <- t1-t0
  smpCnt <- length(t)
  
  # First gap
  if (is.na(x0))
  {
    x <- rep(x1, smpCnt)
    y <- rep(y1, smpCnt)
  }
  # Last gap
  if (is.na(x1))
  {
    x <- rep(x0, smpCnt)
    y <- rep(y0, smpCnt)
  }
  # Inter gap
  if (!is.na(x0) & !is.na(x1))
  {
    tRel <- t-t0
    x <- x0+dx*(tRel/dt)
    y <- y0+dy*(tRel/dt)
  }
  return(list(x = x, y = y))
}

standardFilter <- function(t, x, y, settings, 
                           filterMarkerNames)
{
  interpolateShort <- settings$interpolateShort
  blinkDetection <- settings$blinkDetection
  minGapDuration <- settings$minGapDuration
  smoothLen <- settings$smoothLen
  if (smoothLen %% 2 == 0) smoothLen <- smoothLen + 1
  screenResolution <- settings$screenResolution
  okMarker <- filterMarkerNames$okMarker
  gapMarker <- filterMarkerNames$gapMarker
  outMarker <- filterMarkerNames$outMarker
  bliMarker <- filterMarkerNames$bliMarker
  
  if (is.null(screenResolution))
  {
    stop("\r You must add 'screenResolution' element into 
          the settings list of a data record!
         \r It should be a vector with two numeric elements,
         e.g. c(1024,768).")
  }
  smpCnt <- length(t)
  df <- data.frame(t = t, x = x, y = y,
                   marker = rep(okMarker, smpCnt),
                   pos = 1:smpCnt, stringsAsFactors = F)
  
  gapFlags <- x == 0 & y == 0
  outFlags <- x < 0 | x > screenResolution[1] | 
    y < 0 | y > screenResolution[2]
  df$gapOutFlag <- gapFlags | outFlags
  df$marker[gapFlags] <- gapMarker
  df$marker[outFlags] <- outMarker
  df$group <- markersGroups(df$gapOutFlag)
  dfs <- split(df, df$group)
  posData <- rbindlist(lapply(dfs, FUN = function(x)
  {
    marker <- paste(sort(unique(x$marker)), collapse = "&")
    group <- unique(x$group)
    startPos <- head(x$pos, 1)
    endPos <- tail(x$pos, 1)
    startT <- head(x$t,1)
    endT <- tail(x$t,1)
    prevPos <- ifelse(startPos == 1, NA, startPos - 1)
    nextPos <- ifelse(endPos == smpCnt, NA, endPos + 1)
    return(list(marker = marker, group = group, 
                startPos = startPos, endPos = endPos,
                startT = startT, endT = endT,
                dur = endT-startT, smpCnt = endPos - startPos + 1,
                prevPos = prevPos, nextPos = nextPos))
  }))

  posData$prevT <- t[posData$prevPos]
  posData$nextT <- t[posData$nextPos]
  posData$prevX <- x[posData$prevPos]
  posData$nextX <- x[posData$nextPos]
  posData$prevY <- y[posData$prevPos]
  posData$nextY <- y[posData$nextPos]
  
  gapOutMarker <- paste(sort(c(gapMarker, outMarker)), 
                        collapse = "&")
  
  if (interpolateShort)
  {
    # SHORT GAPS AND OUTS INTERPOLATION
    gapPosd <- posData[posData$marker != okMarker & 
                         posData$dur < minGapDuration, ]
    if (nrow(gapPosd) > 0)
    {
      for (i in 1:nrow(gapPosd))
      {
        g <- gapPosd[i,]
        locs <- g$startPos:g$endPos
        intXY <-  interpolate(t = df$t[locs], 
                              x0 = g$prevX, x1 = g$nextX,
                              y0 = g$prevY, y1 = g$nextY,
                              t0 = g$prevT, t1 = g$nextT)
        df$x[locs] <- intXY$x
        df$y[locs] <- intXY$y
        df$marker[locs] <- okMarker
      }
    }
  }
  
  # temp smooth before detecting blinks
  if (blinkDetection)
  {
    y <- runmed(df$y, smoothLen)
    for (i in 1:nrow(posData))
    {
      g <- posData[i,]
      # LONG GAP/GAP&OUT -> find blink location
      if (g$dur > minGapDuration & g$marker %in% 
          c(gapMarker, gapOutMarker))
      {
        left <- g$prevPos; right <- g$nextPos
        if (!is.na(left) & !is.na(right))
        {
          ###Left local minimum
          while ((left - 1) > 0 & !(y[left] < y[left - 1] & 
                                    y[left] < y[left + 1]))
            left <- left - 1
          ###Right local minimum
          while ((right + 1) < smpCnt & !(y[right] < y[right + 1] & 
                                          y[right] < y[right - 1]))
            right <- right + 1
          df$marker[left:right] <- bliMarker
        }
        if (!is.na(left))
        {
          
        }
        if (!is.na(right))
        {
          
        }
      }
    }
  }

  if (blinkDetection)
  {
    markers <- factor(df$marker, levels = 
                        c(okMarker, gapMarker, gapOutMarker, 
                          outMarker, bliMarker))
  }
  else
  {
    markers <- factor(df$marker, levels = 
                        c(okMarker, gapMarker, gapOutMarker, 
                          outMarker))
  }
  
  groups <- markersGroups(markers)
  res <- list(t = df$t, x = df$x, y = df$y, filterMarkers = 
                list(eventMarkers = markers, 
                     eventGroups = groups,
                     eventsClass = "FilterEvents"))
  return(res)
  }

dataFilter <- function(ETD, filterMarkerNames, filterSettings)
{
  mode <- etd$settings$mode
  interpolateShort <- filterSettings$interpolateShort
  filterSettings <- append(filterSettings, 
                           list(screenResolution = ETD$settings$screenResolution))
  if (mode == "left" | mode == "binocular")
  {
    filtResLeft <- standardFilter(t = ETD$commonData$time, 
                                  x = ETD$leftEyeData$porx, 
                                  y = ETD$leftEyeData$pory,
                                  settings = filterSettings, 
                                  filterMarkerNames = filterMarkerNames) 
    if (interpolateShort)
    {
      ETD$leftEyeData$porx <- filtResLeft$x
      ETD$leftEyeData$pory <- filtResLeft$y
    }
    filterMarkers <- filtResLeft$filterMarkers
    filterMarkers$eye <- "left"
    ETD$leftEvents$filter <- getEventsPositions(filterMarkers)
  }
  if (mode == "right" | mode == "binocular")
  {
    filtResRight <- standardFilter(t = ETD$commonData$time, 
                                   x = ETD$rightEyeData$porx, 
                                   y = ETD$rightEyeData$pory,
                                   settings = filterSettings, 
                                   filterMarkerNames = filterMarkerNames)
    if (interpolateShort)
    {
      ETD$rightEyeData$porx <- filtResRight$x
      ETD$rightEyeData$pory <- filtResRight$y
    }
    filterMarkers <- filtResRight$filterMarkers
    filterMarkers$eye <- "right"
    ETD$rightEvents$filter <- getEventsPositions(filterMarkers)
  }
  return(ETD)
}