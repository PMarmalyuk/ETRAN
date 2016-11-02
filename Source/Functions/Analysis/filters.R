markersGroups <- function(markers) 
{
  transitions <- ifelse(markers[-length(markers)] != markers[-1], 1,0)
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

#Filter
standardFilter <- function(t, x, y, interpolateShort = T, blinkDetection = "zeros", 
                           minGapDuration = .03, smoothLen = 3, stepSize = NULL)
{
  #Проверка входных данных
  if (smoothLen < 1) stop("Smoothing length must be positive.")
  if (smoothLen %% 2 == 0) smoothLen <- smoothLen + 1
  lt <- length(t); lx <- length(x); ly <- length(y)
  if (lt != lx | lx != ly | ly != lt)
    stop("Differrent input vectors lengths.")
  possibleBlinkTypes <- c("zeros", "step", "peakY", "step+peakY", "veloThreshold", "none")
  if (!is.element(blinkDetection, possibleBlinkTypes))
    stop(paste("blinkDetection argument must be element of c(\"", paste(possibleBlinkTypes, collapse = "\", \""), "\").", sep = ""))
  #Маркеры событий
  okMarker <- "Ok"
  gapMarker <- "Gap"
  bliMarker <- "Blink"
  #Вектор флагов сэмплов с нулевыми координатами
  gapFlags <- x == 0 & y == 0
  #Вспомогательный фрейм данных
  smpCnt <- lt
  df <- data.frame(t = t, x = x, y = y,
                   marker = ifelse(gapFlags, gapMarker, okMarker),
                   pos = 1:smpCnt, stringsAsFactors = F)
  df$group <- markersGroups(df$marker)
  dfs <- split(df, df$group)
  #Фрейм данных участков событий
  posData <- rbindlist(lapply(dfs, FUN = function(x) {
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
  #Данные для интерполяции
  posData$prevT <- t[posData$prevPos]
  posData$nextT <- t[posData$nextPos]
  posData$prevX <- x[posData$prevPos]
  posData$nextX <- x[posData$nextPos]
  posData$prevY <- y[posData$prevPos]
  posData$nextY <- y[posData$nextPos]
  #Интерполяция коротких пропусков
  if (interpolateShort) {
    gapPosd <- posData[posData$marker != okMarker & posData$dur < minGapDuration, ]
    if (nrow(gapPosd) > 0) {
      for (i in 1:nrow(gapPosd)) {
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
  #Выявление морганий
  gaps <- posData[posData$marker == gapMarker, ]
  if (blinkDetection != "none") {
    if (blinkDetection == "zeros") {
      df$marker[df$marker == gapMarker] <- bliMarker
    }
    if (blinkDetection == "step") {
      if (is.null(stepSize)) 
        stop("\"stepSize\"-argument is needed to be set")
      for(i in 1:nrow(gaps)) {
        g <- gaps[i,]
        if (g$dur > minGapDuration) {
          left <- g$prevPos; right <- g$nextPos
          left <- ifelse(is.na(left) | left - stepSize < 1, g$startPos, left - stepSize)
          right <- ifelse(is.na(right) | right + stepSize > smpCnt, g$endPos, right + stepSize)
          df$marker[left:right] <- bliMarker
        }
      }
    }
    if (blinkDetection == "peakY") {
      y <- runmed(df$y, smoothLen)
      for(i in 1:nrow(gaps)) {
        g <- gaps[i,]
        if (g$dur > minGapDuration) {
          left <- g$prevPos; right <- g$nextPos
          #Левый локальный минимум или начальная позиция пропуска
          if (is.na(left)) left <- g$startPos else
            while(left > 1 & !(y[left - 1] >= y[left] & y[left + 1] >= y[left]))
              left <- left - 1
            #Правый локальный минимум или конечная позиция пропуска
            if (is.na(right)) right <- g$endPos else
              while(right < smpCnt & !(y[right - 1] >= y[right] & y[right + 1] >= y[right]))
                right <- right + 1
              df$marker[left:right] <- bliMarker
        }
      }
    }
    if (blinkDetection == "step+peakY") {
      if (is.null(stepSize)) 
        stop("\"stepSize\"-argument is needed to be set")
      y <- runmed(df$y, smoothLen)
      for(i in 1:nrow(gaps)) {
        g <- gaps[i,]
        if (g$dur > minGapDuration) {
          left <- g$prevPos; right <- g$nextPos
          #Левый локальный минимум или начальная позиция пропуска
          if (is.na(left)) left <- g$startPos else {
            stepLeft <- left - stepSize
            while(left > 1 & left > stepLeft & !(y[left - 1] >= y[left] & y[left + 1] >= y[left]))
              left <- left - 1
          }
          #Правый локальный минимум или конечная позиция пропуска
          if (is.na(right)) right <- g$endPos else {
            stepRight <- right + stepSize
            while(right < smpCnt & right < stepRight & !(y[right - 1] >= y[right] & y[right + 1] >= y[right]))
              right <- right + 1
          }
          df$marker[left:right] <- bliMarker
        }
      }
    }
    if (blinkDetection == "veloThreshold") {
      print("This method is in development.")
    }
  }
  df$group <- markersGroups(df$marker)
  names(df) <- c("t","x","y","eventMarkers","pos","eventGroups")
  return(df)
}

dataFilter <- function(ETD, interpolateShort = T, blinkDetection = "zeros", 
                       minGapDuration = .03, smoothLen = 3, stepSize = NULL)
{
  mode <- etd$settings$mode
  if (mode == "left" | mode == "binocular")
  {
    filtResLeft <- standardFilter(t = ETD$commonData$time, 
                                  x = ETD$leftEyeData$porx, 
                                  y = ETD$leftEyeData$pory, 
                                  interpolateShort = interpolateShort, 
                                  blinkDetection = blinkDetection, 
                                  minGapDuration = minGapDuration, 
                                  smoothLen = smoothLen, 
                                  stepSize = stepSize) 
    if (interpolateShort)
    {
      ETD$leftEyeData$porx <- filtResLeft$x
      ETD$leftEyeData$pory <- filtResLeft$y
    }
    filtResLeft$eye <- "left"
    ETD$leftEvents$filter <- as.data.frame(getEventsPositions(filtResLeft))
  }
  if (mode == "right" | mode == "binocular")
  {
    filtResRight <- standardFilter(t = ETD$commonData$time, 
                                   x = ETD$rightEyeData$porx, 
                                   y = ETD$rightEyeData$pory, 
                                   interpolateShort = interpolateShort, 
                                   blinkDetection = blinkDetection, 
                                   minGapDuration = minGapDuration, 
                                   smoothLen = smoothLen, 
                                   stepSize = stepSize) 
    if (interpolateShort)
    {
      ETD$rightEyeData$porx <- filtResRight$x
      ETD$rightEyeData$pory <- filtResRight$y
    }
    filtResRight$eye <- "right"
    ETD$rightEvents$filter <- as.data.frame(getEventsPositions(filtResRights))
  }
  return(ETD)
}