# data filter: finds (0,0)s and outOfBounds samples 
# and marks them according to markerNames data in FilterMarkers object
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
          data[data$group == x,]
        }
        )
        filteredData <- do.call("rbind", data2)
        data[rownames(filteredData),] <- filteredData
        t <- data$t; x <- data$x; y <- data$y
      }
      else
      {
        warning("The trajectory consists of one group of samples only!")
        t <- data$t; x <- data$x; y <- data$y
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

## CORE PARSER ##

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

## CORE SMOOTHER ##
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

## CORE DETECTOR ##

# This detector uses specified method (I-VT, I-DT, Ada-NH) to detect oculomotor events
# and also it uses eventGroupsAnalyser function to extract events' parameters
coreDetector <- function(DataRecord, settings)
{
  scrDim <- DataRecord@eyesDataObject@conditions@conditions$screenDim
  scrSize <- DataRecord@eyesDataObject@conditions@conditions$screenSize
  dist <- DataRecord@eyesDataObject@conditions@conditions$screenDist
  timeUnits <-DataRecord@eyesDataObject@conditions@conditions$timeUnits
  t <- DataRecord@eyesDataObject@time@time*timeUnits
  fs <- DataRecord@eyesDataObject@conditions@conditions$sampleRate
  dt <- 1/fs
  
  if (length(t) == 0 | is.na(timeUnits)) 
  {
    samplesCnt <- length(leftX)
    t <- seq(from = dt, to = samplesCnt*dt, by = dt)
  }
  
  algorithm <- settings$algorithm
  if (algorithm == "I-VT")
  {
    fun <- IVT
    # Settings for I-VT algorithm:
    ## VT, angular, screenDist, screenDim, screenSize, MaxTBetFix, MaxDistBetFix, 
    ## minFixLen, maxGapLen, maxVel, maxAccel, classifyGaps
  }
  if (algorithm == "I-DT")
  {
    fun <- IDT
  }
  if (algorithm == "Ada-NH")
  {
    fun <- AdaNH
  }
  
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "left")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    filterMarkers <- DataRecord@eyesDataObject@leftFilterMarkers
    res <- fun(t = t, x = leftX, y = leftY, filterMarkers, settings)
    DataRecord@eyesDataObject@leftEventMarkers <- res$eventMarkers
    DataRecord@analysisResults$leftEventData <- res$eventGroups
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "right")
  {
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    filterMarkers <- DataRecord@eyesDataObject@rightFilterMarkers
    res <- fun(t = t, x = rightX, y = rightY, filterMarkers, settings)
    DataRecord@eyesDataObject@rightEventMarkers <- res$eventMarkers
    DataRecord@analysisResults$rightEventData <- eventGroupsAnalyser(res$eventGroups)
  }
  if (DataRecord@eyesDataObject@conditions@conditions$eye == "both")
  {
    leftX <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$porx
    leftY <- DataRecord@eyesDataObject@leftEyeSamples@eyeData$pory
    rightX <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$porx
    rightY <- DataRecord@eyesDataObject@rightEyeSamples@eyeData$pory
    leftFilterMarkers <- DataRecord@eyesDataObject@leftFilterMarkers
    rightFilterMarkers <- DataRecord@eyesDataObject@rightFilterMarkers
    resLeft <- IVT(t = t, x = leftX, y = leftY, leftFilterMarkers, VT = VT, angular = angular, screenDist = dist, screenDim = scrDim, screenSize = scrSize)
    resRight <- IVT(t = t, x = rightX, y = rightY, rightFilterMarkers, VT = VT, angular = angular, screenDist = dist, screenDim = scrDim, screenSize = scrSize)
    DataRecord@eyesDataObject@leftEventMarkers <- resLeft$eventMarkers
    DataRecord@analysisResults$leftEventData <- eventGroupsAnalyser(resLeft$eventGroups)
    DataRecord@eyesDataObject@rightEventMarkers <- resRight$eventMarkers
    DataRecord@analysisResults$rightEventData <- eventGroupsAnalyser(resRight$eventGroups)
  }
  return(DataRecord)
}


