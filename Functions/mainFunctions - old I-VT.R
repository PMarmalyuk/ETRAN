
createParser <- function(name, fun, settings)
{
  parser <- new(Class = "Parser", name = name, fun = fun, settings = settings)
  return(parser)
}

createFilter <- function(name, fun, settings)
{
  filter <- new(Class = "Filter", name = name, fun = fun, settings = settings)
  return(filter)
}

createSmoother <- function(name, fun, settings)
{
  smoother <- new(Class = "Smoother", name = name, fun = fun, settings = settings)
  return(smoother)
}

createDetector <- function(name, fun, settings)
{
  detector <- new(Class = "EventDetector", name = name, fun = fun, settings = settings)
  return(detector)
}



# Calculates angular height and width in degrees of the eye position 
# relative to the screen/scene center or the given reference point
calcAngPos <- function(x, y, screenDist, screenDim, screenSize, refPoint = c(screenDim[1]/2, screenDim[2]/2))
{
  d <- screenDist
  w <- screenSize[1]; h <- screenSize[2]
  wPx <- screenDim[1]; hPx <- screenDim[2]
  xshift <- ((x-refPoint[1])/wPx)*w
  yshift <- ((y-refPoint[2])/hPx)*h
  xAng <- atan(xshift/d)*(180/pi)
  yAng <- atan(yshift/d)*(180/pi)
  return(list(xAng = xAng, yAng = yAng))
}
calcAngPos(x = 0, y = 1000, screenDist = 50, screenDim = c(1920, 1080), screenSize = c(52.4, 29.5), refPoint = c(100,100))

calcXYShiftsAngVel <- function(t, x, y, screenDist, screenDim, screenSize)
{
  samplesCnt <- length(t)
  dts <- abs(t[-1] - t[-samplesCnt])
  d <- screenDist
  w <- screenSize[1]; h <- screenSize[2]
  wPx <- screenDim[1]; hPx <- screenDim[2]
  x1s <- x[-samplesCnt]; x2s <- x[-1]
  y1s <- y[-samplesCnt]; y2s <- y[-1]
  xshifts <- ((x2s-x1s)/wPx)*w
  yshifts <- ((y2s-y1s)/hPx)*h
  xAngs <- atan(xshifts/d)
  yAngs <- atan(yshifts/d)
  xVels <- abs(xAngs)*(180/pi)/dts
  yVels <- abs(yAngs)*(180/pi)/dts
  return(list(xVels = xVels, yVels = yVels))
}
plot(calcXYShiftsAngVel(t, x, y, screenDist = 54, screenDim = c(1920, 1080), screenSize = c(52.4, 29.5))$xVels, type = "l")

# Calculates velocities (px/timeUnit) of the eye movements given by <t, x, y>
calcPxVel <- function(t, x, y)
{
  samplesCnt <- length(t)
  dts <- abs(t[-1] - t[-samplesCnt])
  dls <- sqrt((x[-1] - x[-samplesCnt])^2 + (y[-1] - y[-samplesCnt])^2)
  vels <- 
    res <- list(dists = dls, dts = dts, vels = dls/dts)
  res
}

# Calculates angular velocities (deg/timeUnit) of the eye movements given by <t, x, y>
calcAngVel <- function(t, x, y, screenDist, screenDim, screenSize)
{
  samplesCnt <- length(t)
  dts <- abs(t[-1] - t[-samplesCnt])
  d <- screenDist
  w <- screenSize[1]; h <- screenSize[2]
  wPx <- screenDim[1]; hPx <- screenDim[2]
  x1s <- x[-samplesCnt]; x2s <- x[-1]
  y1s <- y[-samplesCnt]; y2s <- y[-1]
  dP0P1 <- sqrt((((x1s-wPx/2)/wPx)*w)^2 + (((y1s-hPx/2)/hPx)*h)^2)
  dP0P2 <- sqrt((((x2s-wPx/2)/wPx)*w)^2 + (((y2s-hPx/2)/hPx)*h)^2)
  alphaW <- atan(dP0P1/d)
  alphaH <- atan(dP0P2/d)
  dEyeP1 <- dP0P1/sin(alphaW)
  dEyeP2 <- dP0P2/sin(alphaH)
  dP1P2 <- sqrt((((x2s-x1s)/wPx)*w)^2 + (((y2s-y1s)/hPx)*h)^2)
  angles <- acos((dEyeP1^2 + dEyeP2^2 - dP1P2^2)/(2*dEyeP1*dEyeP2))
  res <- list(dists = angles*(180/pi), dts = dts, vels = angles*(180/pi)/dts)
  res
}

t <- dataRec@eyesDataObject@time@time * dataRec@eyesDataObject@conditions@conditions$timeUnits
x <- dataRec@eyesDataObject@leftEyeSamples@eyeData$porx
y <- dataRec@eyesDataObject@leftEyeSamples@eyeData$pory

plot(t, x, type = "l")
vels <- calcAngVel(t, x, y, screenDist = 100, screenDim = c(1280, 1024), screenSize = c(33.7, 27))
plot(vels$vels, type = "l", ylim = c(0,1000), xlim = c(0, 1000))

res <- standardFilter(x, y, screenDim = c(1280, 1024))

# data filter
coreFilter <- function(DataRecord, settings)
{
  standardFilter <- function(t, x, y, screenDim, interpolate)
  {
    markers <- new(Class = "FilterMarkers")
    markers1 <- ifelse(x == 0 & y == 0, markers@markerNames$zeroes, markers@markerNames$ok)
    if (!is.na(screenDim)[1])
    {
      markers2 <- ifelse(x > screenDim[1] | y > screenDim[2], markers@markerNames$outOfBounds, markers@markerNames$ok)
      markers1[which(markers1 == markers@markerNames$ok)] <- markers2[which(markers1 == markers@markerNames$ok)]
    }
    markers@filterMarkersData <- markers1
    if (interpolate)
    {
      gapMarkers <- ifelse(markers@filterMarkersData != markers@markerNames$ok, "GAP", "NOT GAP")
      gapMarks <- data.frame(firstGap = gapMarkers[-length(gapMarkers)], secondGap = gapMarkers[-1])
      transitions <- apply(gapMarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
      group <- c(1,cumsum(transitions)+1)
      data <- data.frame(t, x, y, group)
      lastGroup <- group[length(group)]
      gapGroups <- unique(group[which(gapMarkers == "GAP")])
      if (length(gapGroups) != 1)
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
          return(data[data$group == x,])
        }
        )
        filteredData <- do.call("rbind", data2)
        data[rownames(filteredData),] <- filteredData
        t <- data$t; x <- data$x; y <- data$y
      }
      else
      {
        stop("The trajectory consists of one group of samples only!")
      }
    }
    res <- list(t = t, x = x, y = y, filterMarkers = markers)
    return(res)
  }
  
  
  
  scrDim <- DataRecord@eyesDataObject@conditions@conditions$screenDim
  interpolate <- settings$interpolate
  
  timeUnits <- DataRecord@eyesDataObject@conditions@conditions$timeUnits
  t <- DataRecord@eyesDataObject@time@time*timeUnits
  fs <- DataRecord@eyesDataObject@conditions@conditions$sampleRate
  dt <- 1/fs
  
  
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    if (length(t) == 0) 
    {
      samplesCnt <- length(leftX)
      t <- seq(from = dt, to = samplesCnt*dt, by = dt)
    }
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
    if (length(t) == 0) 
    {
      samplesCnt <- length(rightX)
      t <- seq(from = dt, to = samplesCnt*dt, by = dt)
    }
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
    if (length(t) == 0) 
    {
      samplesCnt <- length(leftX)
      t <- seq(from = dt, to = samplesCnt*dt, by = dt)
    }
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

# ----------------------------------------------------------------------
#   Savitzky-Golay Algorithm
# ----------------------------------------------------------------------
# T2 <- sav.gol(T, fl, forder=4, dorder=0);
#
# Polynomial filtering method of Savitzky and Golay
# See Numerical Recipes, 1992, Chapter 14.8, for details.
#
# x      = vector of signals to be filtered
#          (the derivative is calculated for each ROW)
# fl     = filter length (for instance fl = 51..151)
# forder = filter order (2 = quadratic filter, 4= quartic)
# dorder = derivative order (0 = smoothing, 1 = first derivative, etc.)
# ----------------------------------------------------------------------
# Note: there are problems with this smoother: it may produce negative coordinates!
# ----------------------------------------------------------------------
savGolFilt <- function(x, settings)
{
  #   *** PseudoInvers of a Matrix ***
  #   using singular value decomposition
  pinv <- function (A)
  {
    s <- svd(A)
    s$v %*% diag(1/s$d) %*% t(s$u)
  }
  
  fl <- settings$fl
  forder <- settings$forder
  dorder <- settings$dorder
  
  m <- length(x)
  dorder <- dorder + 1
  
  # -- calculate filter coefficients --
  fc <- (fl-1)/2                          # index: window left and right
  X  <- outer(-fc:fc, 0:forder, FUN="^")  # polynomial terms and coefficients
  Y  <- pinv(X);                          # pseudoinverse
  
  # -- filter via convolution and take care of the end points --
  T2 <- convolve(x, rev(Y[dorder,]), type="o")    # convolve(...)
  T2 <- T2[(fc+1):(length(T2)-fc)]
}
# ----------------------------------------------------------------------

medianFilt <- function(x, settings)
{
  k <- settings$fl
  runmed(x, k)
}

# Note: first and last fl%/%2 samples are equal to the first and last fl%/%2 samples from x
movAvgFilt <- function(x, settings)
{
  fl <- settings$fl
  filt <- rep(1/fl, fl)
  xsmoothed <- stats::filter(x, filter = filt, method = "convolution")
  xsmoothed[1:(fl%/%2)] <- x[1:(fl%/%2)]
  xsmoothed[(length(x)-(fl%/%2)+1):length(x)] <- x[(length(x)-(fl%/%2)+1):length(x)]
  as.numeric(xsmoothed)
}

# raw data record parsing
coreParser <- function(RawDataRecord, settings)
{
  self <- RawDataRecord
  filePath <- self@filePath
  dataFields <- settings$dataFields
  headerKeys <- settings$headerKeys
  sampleKey <- settings$sampleKey
  sep <- settings$sep
  
  ## Deleting all samples with sample type other than sampleKey
  if (!is.na(dataFields@availableFields$smptype))
  {
    self@data <- self@data[self@data[, dataFields@availableFields$smptype] == sampleKey,]
  }
  
  ## Reading trial number samples
  trials <- NA
  if (!is.na(dataFields@availableFields$trial))
  {
    trials <- self@data[,dataFields@availableFields$trial]
    trialsNums <- unique(trials)
  }
  ## Reading stimuli name samples as trials indicators
  if (!is.na(dataFields@availableFields$trial) & !is.na(dataFields@availableFields$stimname))
  {
    trials <- self@data[,dataFields@availableFields$stimname]
    trialsNums <- unique(trials)
  }
  ## Splitting data by trials
  if (!is.na(trials[1]))
  {
    trialsData <- split(self@data, f = trials)
  }
  
  ## Getting field names using self@data column names
  fNames <- lapply(dataFields@availableFields[-c(14,15)], FUN = function(x) {if (!is.na(x)) {colnames(self@data)[x]} else {"NA"} })
  leftAddFNames <- NA
  rightAddFNames <- NA
  if (!is.na(dataFields@availableFields[[14]][1]))
  {
    leftAddFNames <- lapply(dataFields@availableFields[[14]], FUN = function(x) {if (!is.na(x)) {colnames(self@data)[x]} else {"NA"} })  
  }
  if (!is.na(dataFields@availableFields[[15]][1]))
  {
    rightAddFNames <- lapply(dataFields@availableFields[[15]], FUN = function(x) {if (!is.na(x)) {colnames(self@data)[x]} else {"NA"} })
  }
  fieldNames <- new(Class = "DataFieldNames", 
                    fieldNames = list(fNames, 
                                      leftAdditionalFields = leftAddFNames, 
                                      rightAdditionalFields = rightAddFNames))
  
  ## Figuring out conditions
  ### Recording mode (monocular: left or right,- or binocular)
  eyeLeft <- F
  eyeRight <- F
  if (!is.na(dataFields@availableFields$lporx) & !is.na(dataFields@availableFields$lpory))
  {
    eyeLeft <- T
  }
  if (!is.na(dataFields@availableFields$rporx) & !is.na(dataFields@availableFields$rpory))
  {
    eyeRight <- T
  }
  if (!eyeLeft & !eyeRight) {stop("You must specify eye samples disposition in your dataset!")}
  if (eyeLeft & eyeRight) {eye = "both"}
  if (eyeLeft & !eyeRight) {eye = "left"}
  if (!eyeLeft & eyeRight) {eye = "right"}
  conditions <- new(Class = "Conditions")
  conditions@conditions$eye <- eye
  
  ## Determining pupil shape
  if (is.na(dataFields@availableFields$lpupxsize) & is.na(dataFields@availableFields$lpupysize) |
      is.na(dataFields@availableFields$rpupxsize) & is.na(dataFields@availableFields$rpupysize)
  )
  {
    pupilShape <- NA
  }
  if (!is.na(dataFields@availableFields$lpupxsize) & is.na(dataFields@availableFields$lpupysize) |
      !is.na(dataFields@availableFields$rpupxsize) & is.na(dataFields@availableFields$rpupysize)
  )
  {
    pupilShape <- "circle"
  }            
  if (!is.na(dataFields@availableFields$lpupxsize) & !is.na(dataFields@availableFields$lpupysize) |
      !is.na(dataFields@availableFields$rpupxsize) & !is.na(dataFields@availableFields$rpupysize)
  )
  {
    pupilShape <- "ellipse"
  }
  conditions@conditions$pupilShape <- pupilShape
  
  ## Reading keys from header lines
  keyValues <- lapply(headerKeys@keys, FUN = findKeyValue, headerLines = self@headerLines, sep = sep)
  subjectCode <- keyValues$subjectCode
  stimDim <- as.numeric(keyValues$stimDim)
  conditions@conditions$sampleRate <- as.numeric(keyValues$sampleRate)
  conditions@conditions$screenDistance <- as.numeric(keyValues$headDist)
  
  ## Deterimining frames count
  framesCnt <- NA
  if (!is.na(dataFields@availableFields$frame))
  {
    frameColumn <- self@data[,dataFields@availableFields$frame]
    samplesNum <- length(frameColumn)
    framesCnt <- frameColumn[samplesNum] 
  }
  
  ## Creating list of data to be disassembled in application layer
  eyesDataObjects <- lapply(trialsData, FUN = createDataRecordObject, dataFields = dataFields, fieldNames = fieldNames, conditions = conditions)
  res <- list(filePath = filePath, subjectCode = subjectCode, trialsNums = trialsNums, stimDim = stimDim, framesCnt = framesCnt, eyesDataObjects = eyesDataObjects)
  return(res)
}


# data smoother
coreSmoother <- function(DataRecord, settings)
{
  type <- settings$type
  
  if (type == "Median") {smoother <- medianFilt}
  if (type == "MovAvg") {smoother <- movAvgFilt}
  if (type == "SavGol") {smoother <- savGolFilt}
  
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- smoother(DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx, settings)
    DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory <- smoother(DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory, settings)
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx <- smoother(DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx, settings)
    DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory <- smoother(DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory, settings)
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx <- smoother(DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx, settings)
    DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory <- smoother(DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory, settings)
    DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx <- smoother(DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx, settings)
    DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory <- smoother(DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory, settings)
  }
  return(DataRecord)
}

IVT <- function(t, x, y, filterMarkers, VT, angular = F, screenDist, screenDim, screenSize, MaxTBetFix, MaxDistBetFix)
{
  # 1. Velocities estimation
  if (!angular)
  {
    vel <- calcPxVel(t, x, y)
  } 
  else
  {
    vel <- calcAngVel(t, x, y, screenDist, screenDim, screenSize)
  }

  # 2. Classification stage: getting raw event markers
  markers1 <- ifelse(x == 0 & y == 0, filterMarkers@markerNames$zeroes, filterMarkers@markerNames$ok)
  if (!is.na(screenDim)[1])
  {
    markers2 <- ifelse(x > screenDim[1] | y > screenDim[2], filterMarkers@markerNames$outOfBounds, filterMarkers@markerNames$ok)
    markers1[which(markers1 == filterMarkers@markerNames$ok)] <- markers2[which(markers1 == filterMarkers@markerNames$ok)]
  }
  gapMarkers <- ifelse(markers1 != filterMarkers@markerNames$ok, "GAP", "NOT GAP")
  rawEventMarkers <- ifelse(gapMarkers[-length(gapMarkers)] == "GAP", "GAP", ifelse(vel$vels <= VT, "Fixation", "Saccade"))
  #rawEventMarkers <- ifelse(vel$vels <= VT, "Fixation", "Saccade")
  
  # 3. Post-processing stage
  evmarks <- data.frame(firstEv = rawEventMarkers[-length(rawEventMarkers)], secondEv = rawEventMarkers[-1])
  transitions <- apply(evmarks, MARGIN = 1, function(x) {if (x[2] != x[1]) {1} else {0}})
  group <- c(1,cumsum(transitions)+1)
  events <- data.frame(t = t[-length(t)], x = x[-length(t)], y = y[-length(t)], evm = rawEventMarkers, gr = group)
  setwd("F:\\Институт\\Проекты\\EyeTrackingPackage\\Git\\EyeTrackingProject")
  write.csv(events, file = "sequence.txt")
  stop()
  ## 6.1 
  eventGroups <- split(events, group)
  fixationGroups <- list()
  saccadeGroups <- list()
  lastFixPos <- c()
  lastFixTime <- c()
  for (gr in 2:length(eventGroups))
  {
    previousEvent <- eventGroups[[gr-1]]$evm[1]
    currentEvent <- eventGroups[[gr]]$evm[1]
    if (previousEvent == "Fixation" && currentEvent == "Saccade") 
    {
      lastFixPos <- c(eventGroups[[gr-1]]$x[nrow(eventGroups[[gr-1]])], eventGroups[[gr-1]]$y[nrow(eventGroups[[gr-1]])])
      lastFixTime <- eventGroups[[gr-1]]$t[nrow(eventGroups[[gr-1]])]
      fixationGroup <- eventGroups[[gr-1]]
    }
    if (previousEvent == "Saccade" && currentEvent == "Fixation") 
    {
      if (length(lastFixTime) != 0)
      {
        fixPos <- c(eventGroups[[gr]]$x[1], eventGroups[[gr]]$y[1])
        fixTime <- eventGroups[[gr]]$t[1]
        if (lastFixTime - fixTime <= MaxTBetFix)
        {
          if (!angular)
          {
            pxVel <- calcPxVel(t = c(lastFixTime, fixTime),
                               x = c(lastFixPos[1], fixPos[1]),
                               y = c(lastFixPos[2], fixPos[2]))
            dist <- pxVel$dist
          }
          else
          {
            angVel <- calcAngVel(t = c(lastFixTime, fixTime), 
                                 x = c(lastFixPos[1], fixPos[1]), 
                                 y = c(lastFixPos[2], fixPos[2]),
                                 screenDist,
                                 screenDim,
                                 screenSize)
            dist <- angVel$dist
          }
          if (dist <= MaxDistBetFix)
          {
            saccadeGroup <- list()
            fixationGroup <- rbind(fixationGroup, eventGroups[[gr]])
          }
          else
          {
            fixationGroups <- append(fixationGroups, list(fixationGroup))
            saccadeGroups <- append(saccadeGroups, eventGroups[gr-1])
            fixationGroup <- eventGroups[[gr]]
          }
        }
        else
        {
          fixationGroups <- append(fixationGroups, list(fixationGroup))
          saccadeGroups <- append(saccadeGroups, eventGroups[gr-1])
          fixationGroup <- eventGroups[[gr]]
        }
      }
      else
      {
        saccadeGroups <- append(saccadeGroups, eventGroups[gr-1])
        fixationGroup <- eventGroups[[gr]]
      }
    }
  }
  
  # 6.2 Deleting short fixations, grouping corresponding saccades
  res <- list(fixations = fixationGroups, saccades = saccadeGroups)
  return(res)
  #return(NA)
  
  # 7. Events' parameters estimation stage
  
  # 8. Return eventMarkers and eventData objects
}

coreDetector <- function(DataRecord, settings)
{
  scrDim <- DataRecord@eyesDataObject@conditions@conditions$screenDim
  scrSize <- DataRecord@eyesDataObject@conditions@conditions$screenSize
  dist <- DataRecord@eyesDataObject@conditions@conditions$screenDist
  timeUnits <-DataRecord@eyesDataObject@conditions@conditions$timeUnits
  t <- DataRecord@eyesDataObject@time@time*timeUnits
  fs <- DataRecord@eyesDataObject@conditions@conditions$sampleRate
  dt <- 1/fs

  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    if (length(t) == 0) 
    {
      samplesCnt <- length(leftX)
      t <- seq(from = dt, to = samplesCnt*dt, by = dt)
    }
    filterMarkers <- DataRecord@eyesDataObject@leftFilterMarkers
    # getting settings for IVT
    VT <- settings$VT
    angular <- settings$angular
    res <- IVT(t = t, x = leftX, y = leftY, filterMarkers, VT = VT, angular = angular, screenDist = dist, screenDim = scrDim, screenSize = scrSize)
    DataRecord@eyesDataObject@leftEventMarkers <- res$eventMarkers
    DataRecord@analysisResults$leftEventData <- res$eventData
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    if (length(t) == 0) 
    {
      samplesCnt <- length(rightX)
      t <- seq(from = dt, to = samplesCnt*dt, by = dt)
    }
    filterMarkers <- DataRecord@eyesDataObject@rightFilterMarkers
    res <- IVT(t = t, x = rightX, y = rightY, filterMarkers, VT = VT, angular = angular, screenDist = dist, screenDim = scrDim, screenSize = scrSize)
    DataRecord@eyesDataObject@rightEventMarkers <- res$eventMarkers
    DataRecord@analysisResults$rightEventData <- res$eventData
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    if (length(t) == 0) 
    {
      samplesCnt <- length(leftX)
      t <- seq(from = dt, to = samplesCnt*dt, by = dt)
    }
    leftFilterMarkers <- DataRecord@eyesDataObject@leftFilterMarkers
    rightFilterMarkers <- DataRecord@eyesDataObject@rightFilterMarkers
    resLeft <- IVT(t = t, x = leftX, y = leftY, leftFilterMarkers, VT = VT, angular = angular, screenDist = dist, screenDim = scrDim, screenSize = scrSize)
    resRight <- IVT(t = t, x = rightX, y = rightY, rightFilterMarkers, VT = VT, angular = angular, screenDist = dist, screenDim = scrDim, screenSize = scrSize)
    DataRecord@eyesDataObject@leftEventMarkers <- resLeft$eventMarkers
    DataRecord@analysisResults$leftEventData <- resLeft$eventData
    DataRecord@eyesDataObject@rightEventMarkers <- resRight$eventMarkers
    DataRecord@analysisResults$rightEventData <- resRight$eventData
  }
  return(DataRecord)
}
